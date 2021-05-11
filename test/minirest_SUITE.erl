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
%%---------------------------------------------------------------------
-module(minirest_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
%%--------------------------------------------------------------------
%% Setups
%%--------------------------------------------------------------------
all() ->
    [{group, http}].
groups() ->
    [{http, [t_get]}].

init_per_suite(_Cfg) ->

    _Cfg.
end_per_suite(_) ->
    % minirest:stop_listener(minirest_example),
    ok.
init_per_group(_Group , _Cfg) ->
    ok.
end_per_group(_Group, _Cfg) ->
    ok.
%%--------------------------------------------------------------------
%% Cases
%%--------------------------------------------------------------------

t_get(_Config) ->
    application:ensure_all_started(minirest),
    application:ensure_all_started(minirest_example),
    application:set_env(minirest_example, modules, [minirest_example_api]),
    minirest:start_listener("/api/v4",
                           minirest_example,
                           [{port, 9990}],
                           #{middlewares => [minirest_cors_middleware,
                                             minirest_global_middleware]},
                           [minirest_example]),
    R1 = httpc_get("http://127.0.0.1:9990/api/v4/example"),
    R2 = httpc_get("http://127.0.0.1:9990/api/v4/nosuch"),
    ct:print("GET1 -> :~p~n", [R1]),
    ct:print("GET2 -> :~p~n", [R2]).

%%--------------------------------------------------------------------
%% Private
%%--------------------------------------------------------------------

httpc_get(Url) ->
    % ct:print("Url is => :~p~n", [Url]),
    httpc:request(get, {Url, []}, [], [{body_format, binary}]).
