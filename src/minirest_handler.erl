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

-include("minirest_http.hrl").

-include("minirest.hrl").

%%==============================================================================================
%% cowboy callback init
init(Request0, State)->
    {Response, Handler} = handle(Request0, State),
    Request = reply(Response, Request0, Handler),
    {ok, Request, State}.

%%%==============================================================================================
%% internal
handle(Request, State) ->
    Method = cowboy_req:method(Request),
    case maps:find(Method, State) of
        error ->
            Headers = allow_method_header(maps:keys(State)),
            {?RESPONSE_CODE_METHOD_NOT_ALLOWED, Headers, <<"">>};
        {ok, Handler} ->
            {do_auth(Request, Handler), Handler}
    end.

allow_method_header(Allow) ->
    #{<<"allow">> => trans_allow(Allow, <<"">>)}.

trans_allow([], Res) -> Res;
trans_allow([Method], Res) -> <<Res/binary, Method/binary>>;
trans_allow([Method | Allow], Res) ->
    trans_allow(Allow, <<Res/binary, Method/binary, ", ">>).

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
reply({ErrorStatus, #{code := Code, message := Message}}, Req, Handler)
  when (ErrorStatus < 200 orelse 300 =< ErrorStatus)
  andalso is_atom(Code) ->
    reply({ErrorStatus, Code, Message}, Req, Handler);
reply({ErrorStatus, Code, Message}, Req, Handler = #handler{error_codes = Codes})
  when (ErrorStatus < 200 orelse 300 =< ErrorStatus)
  andalso is_atom(Code) ->
    case maybe_ignore_code_check(ErrorStatus, Code) orelse lists:member(Code, Codes) of
        true ->
            {ok, Headers, Body} = minirest_body:encode(#{code => Code, message => Message}),
            reply({ErrorStatus, Headers, Body}, Req, Handler);
        false ->
            Message =
                list_to_binary(
                    io_lib:format(
                        "not support code ~p, message ~p, schema def ~p", [Code, Message, Codes])),
            reply({500, Message}, Req, Handler)
    end;

%% response simple
reply(StatusCode, Req, _Handler) when is_integer(StatusCode) ->
    cowboy_req:reply(StatusCode, Req);

reply({StatusCode}, Req, _Handler) ->
    cowboy_req:reply(StatusCode, Req);

reply({StatusCode, Body0}, Req, Handler) ->
    case minirest_body:encode(Body0) of
        {ok, Headers, Body} ->
            cowboy_req:reply(StatusCode, Headers, Body, Req);
        {response, Response} ->
            reply(Response, Req, Handler)
    end;

reply({StatusCode, Headers, Body0}, Req, Handler) ->
    case minirest_body:encode(Body0) of
        {ok, Headers1, Body} ->
            cowboy_req:reply(StatusCode, maps:merge(Headers1, Headers), Body, Req);
        {response, Response} ->
            reply(Response, Req, Handler)
    end;

reply(BadReturn, Req, _Handler) ->
    StatusCode = ?RESPONSE_CODE_INTERNAL_SERVER_ERROR,
    Body = io_lib:format("mini rest bad return ~p", [BadReturn]),
    cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"text/plain">>}, Body, Req).

maybe_ignore_code_check(401, _Code) -> true;
maybe_ignore_code_check(400, 'BAD_REQUEST') -> true;
maybe_ignore_code_check(500, 'INTERNAL_ERROR') -> true;
maybe_ignore_code_check(_, _) -> false.
