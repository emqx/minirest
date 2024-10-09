%% Copyright (c) 2013-2022 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-export([update_log_meta/1]).

-include("minirest_http.hrl").

-include("minirest.hrl").

-define(META_KEY, {?MODULE, meta}).

%%==============================================================================================
%% cowboy callback init
init(Request0, State)->
    ReqStart = erlang:monotonic_time(),
    {Code, Request1} = handle(Request0, State),
    ReqEnd = erlang:monotonic_time(),
    Meta = get_log_meta(),
    run_log_hook(State, Meta, ReqStart, ReqEnd, Code, Request1),
    {ok, Request1, State}.

%% In previous versions, the metadata was statically derived from the `authorize` method, 
%% but some endpoints may not have an `authorize` method, 
%% or it may be an interactive endpoint whose metadata cannot be determined in one step.
%% Here, the metadata is moved from the function return to the process dictionary,
%% so that the metadata in the endpoint can be updated.
update_log_meta(New) ->
    Meta = get_log_meta(),
    erlang:put(?META_KEY, maps:merge(Meta, New)).

%%%==============================================================================================
%% internal
handle(Request, #{methods := Methods} = _State) ->
    Method = cowboy_req:method(Request),
    case maps:find(Method, Methods) of
        error ->
            StatusCode = ?RESPONSE_CODE_METHOD_NOT_ALLOWED,
            Headers = allow_method_header(maps:keys(Methods)),
            init_log_meta(Headers#{method => binary_to_existing_atom(Method)}),
            {
                StatusCode,
                cowboy_req:reply(StatusCode, Headers, <<"">>, Request)
            };
        {ok, Handler = #handler{path = Path, log_meta = LogMeta, method = MethodAtom}} ->
            init_log_meta(LogMeta#{operation_id => list_to_binary(Path), method => MethodAtom}),
            case do_authorize(Request, Handler) of
                {ok, AuthMeta} ->
                    update_log_meta(AuthMeta),
                    case do_parse_params(Request) of
                        {ok, Params, NRequest} ->
                            case do_validate_params(Params, Handler) of
                                {ok, NParams} ->
                                    prepend_log_meta(NParams),
                                    Response = apply_callback(NRequest, NParams, Handler),
                                    {StatusCode, NRequest1} = reply(Response, NRequest, Handler),
                                    {
                                        StatusCode,
                                        NRequest1
                                    };
                                FilterErr ->
                                    prepend_log_meta(Params#{failure => failed_log_meta(FilterErr)}),
                                    {StatusCode, NRequest1} = reply(FilterErr, Request, Handler),
                                    {StatusCode, NRequest1}
                            end;
                        ParseErr ->
                            prepend_log_meta(#{failure => failed_log_meta(ParseErr)}),                
                            {StatusCode, NRequest1} = reply(ParseErr, Request, Handler),
                            {StatusCode, NRequest1}
                    end;
                AuthFailed ->
                    prepend_log_meta(#{failure => failed_log_meta(AuthFailed)}),                
                    {StatusCode, NRequest1} = reply(AuthFailed, Request, Handler),
                    {StatusCode, NRequest1}
            end
    end.


failed_log_meta({Code, #{} = Meta}) when is_integer(Code) -> Meta;
failed_log_meta({Code, _Header, #{} = Meta}) when is_integer(Code) -> Meta;
failed_log_meta({_, Code, Message}) when is_atom(Code) ->
    #{code => Code, message => Message};
%% parse body failed
failed_log_meta({response, {?RESPONSE_CODE_BAD_REQUEST, Error}}) ->
    Error.

allow_method_header(Allow) ->
    #{<<"allow">> => trans_allow(Allow, <<"">>)}.

trans_allow([], Res) -> Res;
trans_allow([Method], Res) -> <<Res/binary, Method/binary>>;
trans_allow([Method | Allow], Res) ->
    trans_allow(Allow, <<Res/binary, Method/binary, ", ">>).

do_authorize(Request, #handler{authorization = {M, F}}) ->
    erlang:apply(M, F, [Request]);
do_authorize(_Request, _Handler) ->
    {ok, #{}}.

do_parse_params(Request) ->
    Params = #{
        bindings => cowboy_req:bindings(Request),
        query_string => parse_qs(Request),
        headers => cowboy_req:headers(Request),
        body => #{}
    },
    do_read_body(Request, Params).

parse_qs(Request) ->
    lists:foldl(
      fun({K, V}, MapAcc) ->
              maps:update_with(
                K,
                fun([_|_] = OldV) -> [V | OldV];
                   (OldV) -> [V, OldV]
                end,
                V,
                MapAcc)
      end,
      #{},
      cowboy_req:parse_qs(Request)
     ).

do_read_body(Request, Params) ->
    case cowboy_req:has_body(Request) of
        true ->
            case minirest_body:parse(Request) of
                {ok, Body, NRequest} ->
                    {ok, Params#{body => Body}, NRequest};
                {response, Response} ->
                    Response
            end;
        false ->
            {ok, Params, Request}
    end.

do_validate_params(Params, #handler{filter = Filter,
                                    path   = Path,
                                    module = Mod,
                                    method = Method}) when is_function(Filter, 2) ->
    Filter(Params, #{path => Path, module => Mod, method => Method});
do_validate_params(Params, Handler = #handler{filter = [Filter | Rest]}) ->
    case do_validate_params(Params, Handler#handler{filter = Filter}) of
        {ok, NParams} ->
            do_validate_params(NParams, Handler#handler{filter = Rest});
        Error -> Error
    end;
do_validate_params(Params, _Handler) ->
    {ok, Params}.

apply_callback(Request, Params, Handler) ->
    #handler{path = Path, method = Method, module = Mod, function = Fun} = Handler,
    try
        Args =
            case erlang:function_exported(Mod, Fun, 3) of
                true -> [Method, Params, Request];
                false -> [Method, Params]
            end,
        erlang:apply(Mod, Fun, Args)
    catch E:R:S ->
        ?LOG(warning, #{path => Path,
                        exception => E,
                        reason => R,
                        stacktrace => S}),
        Message = list_to_binary(io_lib:format("~p, ~0p, ~0p", [E, R, S], [])),
        {?RESPONSE_CODE_INTERNAL_SERVER_ERROR, 'INTERNAL_ERROR', Message}
    end.

%% response error
reply({ErrorStatus, #{code := Code, message := Message} = Resp}, Req, Handler)
  when (ErrorStatus < 200 orelse 300 =< ErrorStatus)
  andalso is_atom(Code) ->
    Other = maps:without([code, message], Resp),
    reply({ErrorStatus, Code, Message, Other}, Req, Handler);
reply({ErrorStatus, Code, Message}, Req, Handler)
  when (ErrorStatus < 200 orelse 300 =< ErrorStatus)
  andalso is_atom(Code) ->
    reply({ErrorStatus, Code, Message, _Other = #{}}, Req, Handler);
reply({ErrorStatus, Code, Message, Other}, Req, Handler = #handler{error_codes = Codes})
  when (ErrorStatus < 200 orelse 300 =< ErrorStatus)
  andalso is_atom(Code) andalso is_map(Other) ->
    case maybe_ignore_code_check(ErrorStatus, Code) orelse lists:member(Code, Codes) of
        true ->
            ErrorMessageStruct = {message, Other#{code => Code, message => Message}},
            {ok, Headers, Body} = minirest_body:encode(ErrorMessageStruct),
            reply({ErrorStatus, Headers, Body}, Req, Handler);
        false ->
            NewMessage =
                list_to_binary(
                    io_lib:format(
                        "Unsupported code ~p, message ~p, schema def ~p", [Code, Message, Codes])),
            reply({500, NewMessage}, Req, Handler)
    end;

%% response simple
reply(StatusCode, Req, _Handler) when is_integer(StatusCode) ->
    {StatusCode, cowboy_req:reply(StatusCode, Req)};

reply({StatusCode}, Req, _Handler) ->
    {StatusCode, cowboy_req:reply(StatusCode, Req)};

reply({StatusCode, Body0}, Req, Handler) ->
    case minirest_body:encode(Body0) of
        {ok, Headers, Body} ->
            {StatusCode, reply_with_body(StatusCode, Headers, Body, Req)};
        {response, Response} ->
            reply(Response, Req, Handler)
    end;

reply({StatusCode, Headers, Body0}, Req, Handler) ->
    case minirest_body:encode(Body0) of
        {ok, Headers1, Body} ->
            {StatusCode, reply_with_body(StatusCode, maps:merge(Headers1, Headers), Body, Req)};
        {response, Response} ->
            reply(Response, Req, Handler)
    end;

reply(BadReturn, Req, _Handler) ->
    StatusCode = ?RESPONSE_CODE_INTERNAL_SERVER_ERROR,
    Body = io_lib:format("mini rest bad return ~p", [BadReturn]),
    {StatusCode, cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"text/plain">>}, Body, Req)}.

reply_with_body(StatusCode, Headers, Body, Req) when is_binary(Body) ->
        cowboy_req:reply(StatusCode, Headers, Body, Req);
reply_with_body(StatusCode, Headers, {qlc_handle, _} = BodyQH, Req0) ->
    Req1 = cowboy_req:stream_reply(StatusCode, Headers, Req0),
    qlc:fold(
      fun(Data, Req) ->
              cowboy_req:stream_body(Data, nofin, Req),
              Req
      end,
      Req1,
      BodyQH),
    cowboy_req:stream_body(<<>>, fin, Req1),
    Req1.

maybe_ignore_code_check(401, _Code) -> true;
maybe_ignore_code_check(403, _Code) -> true;
maybe_ignore_code_check(415, _Code) -> true;
maybe_ignore_code_check(400, 'BAD_REQUEST') -> true;
maybe_ignore_code_check(500, 'INTERNAL_ERROR') -> true;
maybe_ignore_code_check(_, _) -> false.

run_log_hook(#{log := {M, F, InitMeta}}, Meta0, ReqStart, ReqEnd, Code, Req) ->
    Meta = maps:merge(InitMeta, Meta0),
    _ = M:F(Meta#{req_start => ReqStart, req_end => ReqEnd, code => Code}, Req),
    ok;
run_log_hook(#{log := Log}, Meta, ReqStart, ReqEnd, Code, Req) when is_function(Log) ->
    _ = Log(Meta#{req_start => ReqStart, req_end => ReqEnd, code => Code}, Req),
    ok;
run_log_hook(_State, _Meta, _ReqStart, _ReqEnd, _Code, _Req) ->
    ok.

init_log_meta(Init) ->
    erlang:put(?META_KEY, Init).

get_log_meta() ->
     erlang:get(?META_KEY).

prepend_log_meta(New) ->
    Meta = get_log_meta(),
    erlang:put(?META_KEY, maps:merge(New, Meta)).

