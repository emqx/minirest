%% Copyright (c) 2013-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(minirest_handler_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

-define(PORT, 8088).
-define(SERVER_NAME, test_server).
-define(HANDLER_MODULE, minirest_test_handler).

all() ->
    [
        t_lazy_body,
        t_binary_body
    ].

init_per_suite(Config) ->
    application:ensure_all_started(minirest),
    RanchOptions = #{
        max_connections => 512,
        num_acceptors => 4,
        socket_opts => [{send_timeout, 5000}, {port, ?PORT}, {backlog, 512}]
    },
    Minirest = #{
        base_path => "",
        modules => [?HANDLER_MODULE],
        dispatch => [{"/[...]", ?HANDLER_MODULE, []}],
        protocol => http,
        ranch_options => RanchOptions,
        middlewares => [cowboy_router, cowboy_handler]
    },
    minirest:start(?SERVER_NAME, Minirest),
    minirest:update_dispatch(?SERVER_NAME),
    Config.

end_per_suite(_Config) ->
    minirest:stop(?SERVER_NAME).

t_lazy_body(_Config) ->
    ?assertMatch(
       {ok, {{_Version, 200, _Status}, _Headers, "firstsecond"}},
       httpc:request(address() ++ "/lazy_body")).

t_binary_body(_Config) ->
    ?assertMatch(
       {ok, {{_Version, 200, _Status}, _Headers, "alldataatonce"}},
       httpc:request(address() ++ "/binary_body")).

address() ->
    "http://localhost:" ++ integer_to_list(?PORT).



