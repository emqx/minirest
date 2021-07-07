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

-define(LOG(Level, Format, Args), logger:Level("Minirest: " ++ Format, Args)).

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
        _ ->
            {?RESPONSE_CODE_UNAUTHORIZED}
    end;

do_auth(Request, Callback) ->
    do_filter(Request, Callback).

do_filter(Request, Callback = #handler{filter = Filter}) ->
    case Filter(Request) of
        {ok, Parameters} ->
            apply_callback(Parameters, Callback);
        Response ->
            Response
    end.

apply_callback(Parameters, #handler{method = Method, module = Mod, function = Fun, path = Path}) ->
    try
        erlang:apply(Mod, Fun, [Method, Parameters])
    catch E:R:S ->
        ?LOG(debug, "path:~p, ~p: ~p: ~p", [Path, E, R, S]),
        Message = list_to_binary(io_lib:format("~p, ~0p, ~0p", [E, R, S], [])),
        Body = #{code => <<"INTERNAL_ERROR">>, message => Message},
        {?RESPONSE_CODE_INTERNAL_SERVER_ERROR, Body}
    end.

reply({StatusCode}, Req) ->
    cowboy_req:reply(StatusCode, Req);

reply({StatusCode, Body0}, Req) ->
    Body = to_json(Body0),
    cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, Body, Req);

reply({StatusCode, Headers, Body0}, Req) ->
    Body = to_json(Body0),
    cowboy_req:reply(StatusCode, Headers, Body, Req);

reply(BadReturn, Req) ->
    StatusCode = ?RESPONSE_CODE_INTERNAL_SERVER_ERROR,
    Body = io_lib:format("mini rest bad return ~p", [BadReturn]),
    cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"text/plain">>}, Body, Req).

to_json(Data) when is_binary(Data) ->
    Data;
to_json(Data) when is_map(Data) ->
    jsx:encode(Data).
