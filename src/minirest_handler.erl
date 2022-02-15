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

reply(Response, Params) ->
    Request = maps:get(?REQUEST_KEY, Params),
    State = maps:get(?INTERNAL_STATE_KEY, Request),
    do_reply(Response, Request, State).

%%==============================================================================================
%% cowboy callback init
init(Request, State)->
    handle(Request, State),
    {ok, Request, State}.

%%%==============================================================================================
%% internal
handle(Request, State) ->
    Method = cowboy_req:method(Request),
    case maps:get(Method, State, undefined) of
        undefined ->
            {?RESPONSE_CODE_METHOD_NOT_ALLOWED, #{<<"allow">> => [maps:keys(State)]}, <<"">>};
        Handler ->
            do_auth(Request, Handler)
    end.

do_auth(Request, Handler = #handler{authorization = {M, F}}) ->
    case erlang:apply(M, F, [Request]) of
        ok ->
            do_parse_params(Request, Handler);
        Response when is_tuple(Response) ->
            Response;
        _ ->
            {?RESPONSE_CODE_UNAUTHORIZED}
    end;

do_auth(Request, Handler) ->
    do_parse_params(Request, Handler).

do_parse_params(Request, Handler) ->
    Params = #{
        bindings => cowboy_req:bindings(Request),
        query_string => maps:from_list(cowboy_req:parse_qs(Request)),
        headers => cowboy_req:headers(Request),
        body => #{}
    },
    do_read_body(Request, Params, Handler).

do_read_body(Request, Params, Handler) ->
    case cowboy_req:has_body(Request) of
        true ->
            case minirest_body:parse(Request) of
                {ok, Body, NRequest} ->
                    do_filter(NRequest, Params#{body => Body}, Handler);
                {response, Response} ->
                    Response
            end;
        false ->
            do_filter(Request, Params, Handler)
    end.

do_filter(Request, Params, #handler{filter = Filter,
                                    path = Path,
                                    module = Mod,
                                    method = Method} = Handler) when is_function(Filter, 2) ->
    case Filter(Params, #{path => Path, module => Mod, method => Method}) of
        {ok, NewParams} ->
            apply_callback(Request, NewParams, Handler);
        Response ->
            Response
    end;
do_filter(Request, Params, Handler) ->
    apply_callback(Request, Params, Handler).

apply_callback(Request, Params0, Handler) ->
    #handler{path = Path, method = Method, module = Mod, function = Fun} = Handler,
    try
        InternalParams =
            #{
                ?INTERNAL_STATE_KEY => Handler,
                ?REQUEST_KEY => Request
            },
        Params = maps:merge(InternalParams, Params0),
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
        Body = #{code => 'INTERNAL_ERROR', message => Message},
        {?RESPONSE_CODE_INTERNAL_SERVER_ERROR, Body}
    end.

%% response error
do_reply({ErrorStatus, #{code := Code, message := Message}}, Req, State)
  when (ErrorStatus < 200 orelse 300 =< ErrorStatus)
  andalso is_atom(Code)
  andalso is_binary(Message) ->
    do_reply({ErrorStatus, Code, Message}, Req, State);
do_reply({ErrorStatus, Code, Message}, Req, State = #handler{error_codes = Codes})
  when (ErrorStatus < 200 orelse 300 =< ErrorStatus)
  andalso is_atom(Code)
  andalso is_binary(Message) ->
    case maybe_ignore_code_check(ErrorStatus, Code) orelse lists:member(Code, Codes) of
        true ->
            Body = #{code => Code, message => Message},
            do_reply({ErrorStatus, Body}, Req, State);
        false ->
            Message =
                list_to_binary(
                    io_lib:format(
                        "not support code ~p, message ~p, schema def ~p", [Code, Message, Codes])),
            Body = #{code => 'INTERNAL_ERROR', message => Message},
            do_reply({500, Body}, Req, State)
    end;

%% response simple
do_reply(StatusCode, Req, _State) when is_integer(StatusCode) ->
    cowboy_req:reply(StatusCode, Req);

do_reply({StatusCode}, Req, _State) ->
    cowboy_req:reply(StatusCode, Req);

do_reply({StatusCode, Body0}, Req, State) ->
    case minirest_body:encode(Body0) of
        {ok, Headers, Body} ->
            cowboy_req:reply(StatusCode, Headers, Body, Req);
        {response, Response} ->
            do_reply(Response, Req, State)
    end;

do_reply({StatusCode, Headers, Body0}, Req, State) ->
    case minirest_body:encode(Body0) of
        {ok, Headers1, Body} ->
            cowboy_req:reply(StatusCode, maps:merge(Headers1, Headers), Body, Req);
        {response, Response} ->
            do_reply(Response, Req, State)
    end;

do_reply(BadReturn, Req, _State) ->
    StatusCode = ?RESPONSE_CODE_INTERNAL_SERVER_ERROR,
    Body = io_lib:format("mini rest bad return ~p", [BadReturn]),
    cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"text/plain">>}, Body, Req).

maybe_ignore_code_check(401, _Code) -> true;
maybe_ignore_code_check(400, 'BAD_REQUEST') -> true;
maybe_ignore_code_check(500, 'INTERNAL_ERROR') -> true;
maybe_ignore_code_check(_, _) -> false.
