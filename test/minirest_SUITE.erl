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
    [{http, [t_api_test]}].

init_per_suite(_Cfg) ->

    _Cfg.
end_per_suite(_) ->
    minirest:stop_listener(minirest_example),
    ok.
init_per_group(_Group , _Cfg) ->
    ok.
end_per_group(_Group, _Cfg) ->
    ok.
%%--------------------------------------------------------------------
%% Cases
%%--------------------------------------------------------------------

t_match_path_variables(_) ->
    Test = [
    {"GET:/api/v4/bindings/*", "GET:/api/v4/bindings/1"},
    {"GET:/api/v4/bindings/*/*/*", "GET:/api/v4/bindings/1/2/3"},
    {"GET:/api/v4/bindings/*/etc/*/s1/s2/*", "GET:/api/v4/bindings/1/etc/2/s1/s2/3"},
    {"GET:/api/v4/bindings/*/etc/*/ok/*", "GET:/api/v4/bindings/1/etc/2/ok/3"},
    {"GET:/api/v4/bindings/*/etc/*/ok/*", "GET:/api/v4/bindings/1/2/ok/3"},
    {"GET:/api/v4/bindings/*/etc/*/ok/*", "GET:/api/v4/t/1/2/ok/3"}],
    Fun = fun({M, R}) ->
      io:format("params :~p~n", [minirest_utils:fetch_params(M, R)]) end,
    lists:foreach(Fun, Test).

t_api_test(_Config) ->
    application:ensure_all_started(hackney),
    application:ensure_all_started(minirest),
    application:ensure_all_started(minirest_example),
    application:set_env(minirest_example, modules, [minirest_example_api]),
    minirest:start_listener("/api/v4",
                           minirest_example,
                           [{port, 9990}],
                           #{middlewares => []},
                           [minirest_example]),
    R1 = send_request(get, "http://127.0.0.1:9990/api/v4/example?page=1&size=10"),
    R2 = send_request(post, "http://127.0.0.1:9990/api/v4/example"),
    R3 = send_request(put, "http://127.0.0.1:9990/api/v4/example"),
    R4 = send_request(delete, "http://127.0.0.1:9990/api/v4/example"),
    R5 = send_request(get, "http://127.0.0.1:9990/api/v4/no-such-things"),
    R6 = send_request(get, "http://127.0.0.1:9990/api/v4/example_invalid_return"),
    R7 = send_request(post, "http://127.0.0.1:9990/api/v4/method_not_support"),
    R8 = send_request(get, "http://127.0.0.1:9990/api/v4/bindings/hello/world"),
    R9 = send_request(get, "http://127.0.0.1:9990/api/v4/bindings?k1=1&k2=2"),
    ct:print("GET -> :~p~n", [R1]),
    ct:print("POST -> :~p~n", [R2]),
    ct:print("PUT -> :~p~n", [R3]),
    ct:print("DELETE -> :~p~n", [R4]),
    ct:print("GET2 for 404 -> :~p~n", [R5]),
    ct:print("GET3 for example invalid return -> :~p~n", [R6]),
    ct:print("POST2 for method not support -> :~p~n", [R7]),
    ct:print("GET bindings test -> :~p~n", [R8]),
    ct:print("GET bindings test -> :~p~n", [R9]).


%%--------------------------------------------------------------------
%% Private
%%--------------------------------------------------------------------

send_request(Method, Url) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(Method,
                    Url,
                    Headers,
                    <<>>,
                    [{follow_redirect, true},
                     {max_redirect, 5}]
    ),
    {ok, StatusCode, RespHeaders, hackney:body(ClientRef)}.
