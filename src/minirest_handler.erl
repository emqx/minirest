%% Copyright (c) 2013-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-export([reply/2]).

-include("minirest_http.hrl").

-include("minirest.hrl").

-define(try_reply_json(BODY, REQ, EXPR),
    case to_json(BODY) of
        invalid_json_term ->
            cowboy_req:reply(?RESPONSE_CODE_INTERNAL_SERVER_ERROR,
                #{<<"content-type">> => <<"text/plain">>},
                list_to_binary(io_lib:format("invalid json term: ~p", [BODY])), REQ);
        JSON ->
            EXPR
    end).

%%==============================================================================================
%% cowboy callback init
init(Request0, State)->
    Response = handle(Request0, State),
    Request = reply(Response, Request0),
    {ok, Request, State}.

%%%==============================================================================================
%% internal
handle(Request, State) ->
    Method = cowboy_req:method(Request),
    case maps:get(Method, State, undefined) of
        undefined ->
            {?RESPONSE_CODE_METHOD_NOT_ALLOWED, #{<<"allow">> => [maps:keys(State)]}, <<"">>};
        Callback ->
            do_auth(Request, Callback)
    end.

do_auth(Request, Callback = #handler{authorization = {M, F}}) ->
    case erlang:apply(M, F, [Request]) of
        ok ->
            do_filter(Request, Callback);
        Response when is_tuple(Response) ->
            Response;
        _ ->
            {?RESPONSE_CODE_UNAUTHORIZED}
    end;

do_auth(Request, Callback) ->
    do_filter(Request, Callback).

do_filter(Request, Handler) ->
    #handler{filter = Filter, path = Path, module = Mod,
             method = Method} = Handler,
    case parse_params(Request) of
        #{body := {error, bad_json}} ->
            Body = #{code => <<"BAD_REQUEST">>, message => <<"Invalid json message received">>},
            {?RESPONSE_CODE_BAD_REQUEST, Body};
        Params ->
            case is_function(Filter, 2) of
                true ->
                    case Filter(Params, #{path => Path, module => Mod, method => Method}) of
                        {ok, NewParams} ->
                            apply_callback(Request, NewParams, Handler);
                        Response ->
                            Response
                    end;
                false ->
                    apply_callback(Request, Params, Handler)
            end
    end.

parse_params(Request) ->
    BodyParams =
        case cowboy_req:has_body(Request) of
            true ->
                {_, Body0, _} = cowboy_req:read_body(Request),
                try jsx:decode(Body0)
                catch _ : _ -> {error, bad_json}
                end;
            false -> #{}
        end,
    #{
        bindings => cowboy_req:bindings(Request),
        query_string => maps:from_list(cowboy_req:parse_qs(Request)),
        headers => cowboy_req:headers(Request),
        body => BodyParams
    }.

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
        Body = #{code => <<"INTERNAL_ERROR">>, message => Message},
        {?RESPONSE_CODE_INTERNAL_SERVER_ERROR, Body}
    end.

reply(StatusCode, Req) when is_integer(StatusCode) ->
    cowboy_req:reply(StatusCode, Req);
reply({StatusCode}, Req) ->
    cowboy_req:reply(StatusCode, Req);

reply({StatusCode, Body0}, Req) ->
    ?try_reply_json(Body0, Req,
        cowboy_req:reply(StatusCode,
            #{<<"content-type">> => <<"application/json">>}, JSON, Req));

reply({ErrorStatus, Code, Message}, Req)
        when (ErrorStatus < 200 orelse ErrorStatus >= 300)
             andalso is_atom(Code)
             andalso is_binary(Message) ->
    Body = #{code => Code, message => Message},
    reply({ErrorStatus, Body}, Req);

reply({StatusCode, Headers, Body0}, Req) ->
    ?try_reply_json(Body0, Req,
        cowboy_req:reply(StatusCode, Headers, JSON, Req));

reply(BadReturn, Req) ->
    StatusCode = ?RESPONSE_CODE_INTERNAL_SERVER_ERROR,
    Body = io_lib:format("mini rest bad return ~p", [BadReturn]),
    cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"text/plain">>}, Body, Req).

to_json(Data) when is_binary(Data) ->
    Data;
to_json(Data) ->
    case jsx:is_term(Data) of
        true ->
            jsx:encode(Data);
        false ->
            invalid_json_term
    end.
