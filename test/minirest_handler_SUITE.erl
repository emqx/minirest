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
        t_binary_body,
        t_flex_error,
        t_qs_params,
        t_auth_meta_in_filter,
        t_auth_meta_in_handler,
        t_handler_meta_in_auth
    ].

init_per_suite(Config) ->
    application:ensure_all_started(minirest),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(t_handler_meta_in_auth, Config) ->
    ok = start_minirest(
        #{authorization => {?HANDLER_MODULE, authorize2}}
    ),
    Config;
init_per_testcase(_Case, Config) ->
    ok = start_minirest(),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = stop_minirest().

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

t_lazy_body(_Config) ->
    ?assertMatch(
        {ok, {{_Version, 200, _Status}, _Headers, "firstsecond"}},
        httpc:request(address() ++ "/lazy_body")
    ).

t_binary_body(_Config) ->
    ?assertMatch(
        {ok, {{_Version, 200, _Status}, _Headers, "alldataatonce"}},
        httpc:request(address() ++ "/binary_body")
    ).

t_flex_error(_Config) ->
    {ok, {{_Version, 400, _Status}, _Headers, Body}} =
        httpc:request(address() ++ "/flex_error"),
    ?assertMatch(
        #{<<"code">> := _, <<"message">> := _, <<"hint">> := _},
        jsx:decode(iolist_to_binary(Body), [return_maps])
    ).

t_qs_params(_Config) ->
    ?assertMatch(
        {ok, {{_Version, 200, _Status}, _Headers, "OK"}},
        httpc:request(address() ++ "/qs_params?single=foo&array=foo&array=bar")
    ).

t_auth_meta_in_filter(_Config) ->
    ?assertMatch(
        {ok, {{_Version, 200, _Status}, _Headers, "hello from authorize"}},
        httpc:request(address() ++ "/auth_meta_in_filter")
    ).

t_auth_meta_in_handler(_Config) ->
    ?assertMatch(
        {ok, {{_Version, 200, _Status}, _Headers, "hello from authorize"}},
        httpc:request(address() ++ "/auth_meta_in_handler")
    ).

t_handler_meta_in_auth(_Config) ->
    ?assertMatch(
        {ok, {
            {_Version, 200, _Status},
            _Headers,
            "hello from minirest_test_handler:handler_meta_in_auth"
        }},
        httpc:request(address() ++ "/handler_meta_in_auth")
    ).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_minirest() ->
    start_minirest(#{}).

start_minirest(MinirestOptions0) ->
    RanchOptions = #{
        max_connections => 512,
        num_acceptors => 4,
        socket_opts => [{send_timeout, 5000}, {port, ?PORT}, {backlog, 512}]
    },
    MinirestOptions = maps:merge(
        #{
            base_path => "",
            modules => [?HANDLER_MODULE, minirest_info_api],
            authorization => {?HANDLER_MODULE, authorize1},
            dispatch => [{"/[...]", ?HANDLER_MODULE, []}],
            protocol => http,
            ranch_options => RanchOptions,
            middlewares => [cowboy_router, cowboy_handler]
        },
        MinirestOptions0
    ),
    minirest:start(?SERVER_NAME, MinirestOptions),
    minirest:update_dispatch(?SERVER_NAME).

stop_minirest() ->
    minirest:stop(?SERVER_NAME).

address() ->
    "http://localhost:" ++ integer_to_list(?PORT).
