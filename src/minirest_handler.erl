%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(minirest_handler).

-export([init/2]).

-import(minirest_req, [ reply/2
                      , reply/4
                      , serialize/2
                      , serialize_detail/2
                      , server_internal_error/3]).

init(Req, Opts) ->
    try pipeline([fun make_light_weight_req/2,
                  fun validate/2,
                  fun next_handler/2], Req, Opts)
    catch
        error:Error:Stacktrace ->
            logger:error("Error: ~p, Stacktrace: ~p", [Error, Stacktrace]),
            {ok, server_internal_error(Error, Stacktrace, Req), Opts}
    end.

pipeline([], Req, Opts) ->
    {ok, Req, Opts};

pipeline([Fun | More], Req, Opts) ->
    case Fun(Req, Opts) of
        {ok, NReq} ->
            pipeline(More, NReq, Opts);
        {ok, NReq, NOpts} ->
            pipeline(More, NReq, NOpts);
        {stop, NReq} ->
            {ok, NReq, Opts};
        {stop, NReq, NOpts} ->
            {ok, NReq, NOpts}
    end.

make_light_weight_req(Req, Opts) ->
    {ok, BinBody, NReq} = read_body(Req),
    try jiffy:decode(BinBody, [return_maps]) of
        Body ->
            Bindings = cowboy_req:bindings(Req),
            NBindings = maps:fold(fun(K, V, Acc) ->
                                      maps:put(atom_to_binary(K, utf8), V, Acc)
                                  end, #{}, Bindings),
            LightWeightReq = #{bindings => http_uri_decode(NBindings),
                               qs       => http_uri_decode(maps:from_list(cowboy_req:parse_qs(Req))),
                               headers  => cowboy_req:headers(Req),
                               body     => Body},                  
            {ok, NReq, Opts#{light_weight_req => LightWeightReq}}
    catch
        error:Error:Stacktrace ->
            {stop, reply(400, #{}, #{message => <<"The request body isn't a valid json">>,
                                     detail => serialize_detail(Error, Stacktrace)}, NReq)}
    end.

validate(Req, Opts = #{light_weight_req := LightWeightReq}) ->
    Constraints = get_constraints(Req, Opts),
    try
        maps:fold(fun(K, V, Acc) ->
                    case maps:get(K, Constraints, undefined) of
                        undefined -> Acc;
                        SubConstraints ->
                            case validate_params(V, SubConstraints) of
                                {ok, NV} -> maps:update(K, NV, Acc);
                                {error, Reason} -> error(Reason)
                            end
                    end
                end, LightWeightReq, LightWeightReq) of
        NLightWeightReq ->
            {ok, Req, Opts#{light_weight_req => NLightWeightReq}}
    catch
        error:{at_most_one, Keys} ->
            Message = serialize("At most one of the following parameters can be specified: ~s", [jiffy:encode(Keys)]),
            bad_request(Req, Message);
        error:{exactly_one, Keys} ->
            Message = serialize("Exactly one of the following parameters must be specified: ~s", [jiffy:encode(Keys)]),
            bad_request(Req, Message);
        error:{at_least_one, Keys} ->
            Message = serialize("At least one of the following parameters must be specified: ~s", [jiffy:encode(Keys)]),
            bad_request(Req, Message);
        % {error, {each_one, Keys}} ->
        %     Message = serialize("Each of the following parameters must be specified: ~s", [jiffy:encode(Keys)]),
        %     bad_request(Req, Message);
        error:{missing_required_param, Key}:_ ->
            Message = serialize("Missing required parameter: ~s", [Key]),
            bad_request(Req, Message);
        error:{Key, Value, Reason}:_ ->
            Message = serialize("Invalid value for parameter: ~s", [Key]),
            Detail = serialize("Validate ~s failed with '~p'", [jiffy:encode(#{Key => Value}), Reason]),
            logger:error("~s", [Detail]),
            {stop, reply(400, #{}, #{message => Message, detail => Detail}, Req)}
    end.

next_handler(Req, Opts = #{light_weight_req := #{bindings := Bindings,
                                                 qs := QS,
                                                 headers := Headers,
                                                 body := Body}, handler := Handler}) ->
    Fun = lowercase_method(Req),
    NReq = case Handler:Fun(maps:merge(Bindings, QS), Headers, Body) of
               Status when is_integer(Status) ->
                   reply(Status, Req);
               {Status, RespBody} ->
                   reply(Status, #{}, RespBody, Req);
               {Status, Headers, RespBody} ->
                   reply(Status, Headers, RespBody, Req)
           end,
    {ok, NReq, Opts}.

read_body(Req) ->
    case read_body(Req, <<>>) of
        {ok, <<>>, NReq} -> {ok, <<"{}">>, NReq};
        {ok, Body, NReq} -> {ok, Body, NReq}
    end.

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, NReq} -> {ok, <<Acc/binary, Data/binary>>, NReq};
        {more, Data, NReq} -> read_body(NReq, <<Acc/binary, Data/binary>>)
    end.

bad_request(Req, Message) ->
    logger:error("~s", [Message]),
    {stop, reply(400, #{}, #{message => Message}, Req)}.

validate_params([], _, Acc) ->
    {ok, lists:reverse(Acc)};
validate_params([Params | More], Constraints, Acc) ->
    case validate_params(Params, Constraints) of
        {ok, NParams} -> validate_params(More, Constraints, [NParams | Acc]);
        {error, Reason} -> {error, Reason}
    end.

validate_params(Params, Constraints) when is_list(Params) ->
    validate_params(Params, Constraints, []);

validate_params(Params, []) when is_map(Params) ->
    {ok, Params};
validate_params(Params, [{Keys, [at_most_one]} | More]) when is_map(Params) andalso is_list(Keys) ->
    case maps:size(maps:with(Keys, Params)) =< 1 of
        true -> validate_params(Params, More);
        false -> {error, {at_most_one, Keys}}
    end;
validate_params(Params, [{Keys, [exactly_one]} | More]) when is_map(Params) andalso is_list(Keys) ->
    case maps:size(maps:with(Keys, Params)) =:= 1 of
        true -> validate_params(Params, More);
        false -> {error, {exactly_one, Keys}}
    end;
validate_params(Params, [{Keys, [at_least_one]} | More]) when is_map(Params) andalso is_list(Keys) ->
    case maps:size(maps:with(Keys, Params)) >= 1 of
        true -> validate_params(Params, More);
        false -> {error, {at_least_one, Keys}}
    end;
validate_params(Params, [{Key, Constraint} | More]) when is_map(Params) ->
    validate_params(Params, [{Key, required, Constraint} | More]);
validate_params(Params, [{Key, Optional, Constraint} | More]) when is_map(Params) ->
    case maps:get(Key, Params, undefined) of
        undefined ->
            case Optional of
                required ->
                    {error, {missing_required_param, Key}};
                optional ->
                    validate_params(Params, More);
                {optional, Default} ->
                    validate_params(Params#{Key => Default}, More)
            end;
        Value ->
            case validate_value(Value, Constraint) of
                {ok, Value1} ->
                    validate_params(Params#{Key => Value1}, More);
                {error, Reason} ->
                    {error, {Key, Value, Reason}}
            end
    end.

validate_value(Value, []) ->
    {ok, Value};
validate_value(Value, [Tag | More]) when is_atom(Tag) ->
    Fun = case Tag of
              int -> fun int/1;
              bool -> fun bool/1;
              atom -> fun atom/1;
              nonempty -> fun nonempty/1
          end,
    validate_value(Value, [Fun | More]);
validate_value(Value, [Fun | More]) when is_function(Fun) ->
    case erlang:apply(Fun, [Value]) of
        ok -> validate_value(Value, More);
        {ok, Value1} -> validate_value(Value1, More);
        {error, Reason} -> {error, Reason}
    end.

int(Value) when is_integer(Value) ->
    ok;
int(Value) ->
	try
		{ok, binary_to_integer(Value)}
	catch _:_ ->
		{error, not_integer}
	end.

bool(Value) when is_boolean(Value) ->
    ok;
bool(Value) ->
	try
		{ok, binary_to_atom(Value, utf8)}
	catch _:_ ->
		{error, not_boolean}
	end.

atom(Value) ->
    try
		{ok, binary_to_atom(Value, utf8)}
	catch _:_ ->
		{error, not_atom}
	end.

nonempty(<<>>) ->
	{error, empty};
nonempty(Value) when is_binary(Value) ->
	ok.

get_constraints(Req, Opts) ->
    maps:get(lowercase_method(Req), Opts, #{}).

lowercase_method(Req) ->
    list_to_atom(string:to_lower(binary_to_list(cowboy_req:method(Req)))).

http_uri_decode(Params) when is_map(Params) ->
    maps:map(fun(_, V) ->
                 http_uri:decode(V)
             end, Params).