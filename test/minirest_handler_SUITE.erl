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
-define(LOG_CAPTURE, minirest_handler_SUITE_log_capture).
-define(AUTH_TOKEN, "Bearer token-must-not-appear-in-output").

all() ->
    [
        t_lazy_body,
        t_binary_body,
        t_flex_error,
        t_qs_params,
        t_auth_meta_in_filter,
        t_auth_meta_in_handler,
        t_handler_meta_in_auth,
        t_route_path_in_auth,
        t_post_large_body,
        t_update_log_meta_outside_request,
        t_crash_hides_request,
        t_crash_hides_reason_data,
        t_crash_trace_keeps_location,
        t_crash_response_format
    ].

init_per_suite(Config) ->
    application:ensure_all_started(minirest),
    application:ensure_all_started(hackney),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(t_handler_meta_in_auth, Config) ->
    ok = start_minirest(
        #{authorization => {?HANDLER_MODULE, authorize2}}
    ),
    Config;
init_per_testcase(t_route_path_in_auth, Config) ->
    ok = start_minirest(
        #{authorization => {?HANDLER_MODULE, authorize_path}}
    ),
    Config;
init_per_testcase(Case, Config) ->
    ok = start_minirest(),
    case atom_to_list(Case) of
        "t_crash_" ++ _ -> ok = add_log_capture();
        _ -> ok
    end,
    Config.

end_per_testcase(_Case, _Config) ->
    _ = logger:remove_handler(?LOG_CAPTURE),
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

%% Verify that the authorize callback receives the route template path
%% (e.g. "/route_path_in_auth/:id") rather than the actual request path
%% (e.g. "/route_path_in_auth/42").
t_route_path_in_auth(_Config) ->
    ?assertMatch(
        {ok, {
            {_Version, 200, _Status},
            _Headers,
            "/route_path_in_auth/:id"
        }},
        httpc:request(address() ++ "/route_path_in_auth/42")
    ).

t_post_large_body(_Config) ->
    Data100KB = iolist_to_binary([$s || _ <- lists:seq(1, 100_000)]),
    Data100MB = [Data100KB || _ <- lists:seq(1, 1000)],
    Json100MB = jsx:encode(Data100MB),
    URL = address() ++ "/post_large_body",
    Headers = [{<<"content-type">>, <<"application/json">>}],
    {ok, 200, _, Ref} = hackney:request(post, URL, Headers, Json100MB, []),
    ?assertEqual({ok, <<"OK">>}, hackney:body(Ref)).

%% `update_log_meta/1' does nothing in a process that does not handle a
%% minirest request.
t_update_log_meta_outside_request(_Config) ->
    ?assertEqual(ok, minirest_handler:update_log_meta(#{source => <<"nobody">>})),
    ?assertEqual(undefined, erlang:get({minirest_handler, meta})).

%% A handler crash with the request in the stacktrace arguments puts
%% neither the authorization header nor its value into the response or the log.
t_crash_hides_request(_Config) ->
    {ok, {{_, 500, _}, _, Body}} =
        httpc:request(
            get,
            {address() ++ "/crash_function_clause", [{"authorization", ?AUTH_TOKEN}]},
            [],
            [{body_format, binary}]
        ),
    #{<<"code">> := <<"INTERNAL_ERROR">>, <<"message">> := Message} =
        jsx:decode(Body, [return_maps]),
    Log = receive_crash_log(),
    lists:foreach(
        fun(Output) ->
            ?assertEqual(nomatch, string:find(Output, "token-must-not-appear-in-output")),
            ?assertEqual(nomatch, string:find(Output, "authorization"))
        end,
        [Body, Message, io_lib:format("~0p", [Log])]
    ).

%% A handler crash with request data in the error reason does not put
%% that data into the response or the log.
t_crash_hides_reason_data(_Config) ->
    Secret = <<"secret-must-not-appear-in-output">>,
    {ok, {{_, 500, _}, _, Body}} =
        httpc:request(
            post,
            {
                address() ++ "/crash_badmatch",
                [],
                "application/json",
                jsx:encode(#{<<"secret">> => Secret})
            },
            [],
            [{body_format, binary}]
        ),
    #{report := #{reason := Reason}} = Log = receive_crash_log(),
    ?assertMatch({badmatch, _}, Reason),
    lists:foreach(
        fun(Output) -> ?assertEqual(nomatch, string:find(Output, Secret)) end,
        [Body, io_lib:format("~0p", [Log])]
    ).

%% The logged stacktrace still names the module, function, arity and
%% line of the crash.
t_crash_trace_keeps_location(_Config) ->
    {ok, {{_, 500, _}, _, _}} = httpc:request(address() ++ "/crash_function_clause"),
    #{report := #{exception := error, reason := function_clause, stacktrace := Stack}} =
        receive_crash_log(),
    [{?HANDLER_MODULE, crash_function_clause, 3, Location} | _] = Stack,
    ?assert(is_integer(proplists:get_value(line, Location))),
    ?assertMatch(
        "minirest_test_handler.erl", filename:basename(proplists:get_value(file, Location))
    ).

%% The response message for a crash keeps the `Class, Reason, Stacktrace' format.
t_crash_response_format(_Config) ->
    {ok, {{_, 500, _}, _, Body}} =
        httpc:request(get, {address() ++ "/crash_plain", []}, [], [{body_format, binary}]),
    #{<<"code">> := <<"INTERNAL_ERROR">>, <<"message">> := Message} =
        jsx:decode(Body, [return_maps]),
    ?assertMatch(
        <<"error, boom, [{minirest_test_handler,crash_plain,2,[{file,", _/binary>>, Message
    ),
    _ = receive_crash_log().

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

add_log_capture() ->
    logger:add_handler(?LOG_CAPTURE, ?MODULE, #{level => warning, config => #{pid => self()}}).

%% logger handler callback
log(#{msg := {report, Report}, meta := #{mfa := {minirest_handler, _, _}}}, #{
    config := #{pid := Pid}
}) ->
    Pid ! {crash_log, #{report => Report}};
log(_Event, _Config) ->
    ok.

receive_crash_log() ->
    receive
        {crash_log, Log} -> Log
    after 5000 ->
        ct:fail(no_crash_log)
    end.

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
