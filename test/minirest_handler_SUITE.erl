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
        t_set_cookie,
        t_set_cookies,
        t_json_utf8,
        t_json_utf8_invalid,
        t_json_utf8_chunked,
        t_update_log_meta_outside_request,
        t_crash_hides_request,
        t_crash_hides_reason_data,
        t_crash_trace_keeps_location,
        t_crash_trace_limits,
        t_crash_response_format,
        t_crash_bad_header_in_authorize,
        t_crash_in_authorize,
        t_crash_in_filter
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
init_per_testcase(t_crash_bad_header_in_authorize, Config) ->
    ok = start_minirest(#{authorization => {?HANDLER_MODULE, authorize_parse_header}}),
    ok = add_log_capture(),
    Config;
init_per_testcase(t_crash_in_authorize, Config) ->
    ok = start_minirest(#{authorization => {?HANDLER_MODULE, authorize_crash}}),
    ok = add_log_capture(),
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
        httpc:request(address() ++ "/qs_params?single=foo&array=foo&array=bar&array=baz")
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

%% A handler that returns a `set-cookie' header gets it sent as a cookie,
%% instead of cowboy rejecting the response.
t_set_cookie(_Config) ->
    {ok, 200, Headers, _Ref} = hackney:request(get, address() ++ "/set_cookie"),
    %% The exact attribute list depends on the cowlib version, so assert on
    %% the pair and the attributes the handler asked for.
    [Cookie] = set_cookie_headers(Headers),
    ?assertEqual(<<"one=1">>, cookie_pair(Cookie)),
    ?assertNotEqual(nomatch, binary:match(Cookie, <<"Path=/api">>)),
    ?assertNotEqual(nomatch, binary:match(Cookie, <<"HttpOnly">>)).

%% Several cookies are sent as one `set-cookie' header each, which is the
%% only correct encoding for them.
t_set_cookies(_Config) ->
    {ok, 200, Headers, _Ref} = hackney:request(get, address() ++ "/set_cookies"),
    ?assertEqual(
        [<<"one=1">>, <<"two=2">>],
        lists:sort([cookie_pair(C) || C <- set_cookie_headers(Headers)])
    ).

t_json_utf8(_Config) ->
    URL = address() ++ "/echo_json",
    Headers = [{<<"content-type">>, <<"application/json">>}],
    ValidValues = [
        %% "café": C3 A9 is the valid two-byte UTF-8 encoding of U+00E9.
        {<<"caf", 16#C3, 16#A9>>, <<"caf", 16#C3, 16#A9>>},
        %% "中文": /utf8 encodes the Unicode codepoints U+4E2D and U+6587.
        {<<16#4E2D/utf8, 16#6587/utf8>>, <<16#4E2D/utf8, 16#6587/utf8>>},
        %% "😀": U+1F600 is a valid codepoint encoded as four UTF-8 bytes.
        {<<16#1F600/utf8>>, <<16#1F600/utf8>>},
        %% The JSON surrogate pair D83D DE00 decodes to the same U+1F600 emoji.
        {<<"\\uD83D\\uDE00">>, <<16#1F600/utf8>>},
        %% A literal U+FFFD is valid Unicode; it does not indicate malformed input.
        {<<16#FFFD/utf8>>, <<16#FFFD/utf8>>},
        %% The JSON escape for U+FFFD must also decode without being rejected.
        {<<"\\uFFFD">>, <<16#FFFD/utf8>>}
    ],
    lists:foreach(
        fun({Value, Expected}) ->
            Json = <<"{\"value\":\"", Value/binary, "\"}">>,
            {ok, 200, _, Ref} = hackney:request(post, URL, Headers, Json, []),
            {ok, Response} = hackney:body(Ref),
            ?assertEqual(#{<<"value">> => Expected}, jsx:decode(Response, [return_maps]))
        end,
        ValidValues
    ).

t_json_utf8_invalid(_Config) ->
    URL = address() ++ "/echo_json",
    Headers = [{<<"content-type">>, <<"application/json">>}],
    InvalidValues = [
        <<16#30, 16#82, 16#01, 16#80, 16#A0>>,
        <<"caf", 16#C3>>,
        binary:copy(<<16#80>>, 1_048_576),
        <<(binary:copy(<<$a>>, 1_048_576))/binary, 16#80>>,
        <<16#C0, 16#AF>>,
        <<16#ED, 16#A0, 16#80>>,
        <<16#F4, 16#90, 16#80, 16#80>>,
        <<"\\uD800">>,
        <<"\\uDC00">>
    ],
    InvalidBodies = [
        <<"{\"", 16#80, "\":\"value\"}">>,
        <<"{/*", 16#80, "*/\"value\":\"ok\"}">>
        | [<<"{\"value\":\"", Value/binary, "\"}">> || Value <- InvalidValues]
    ],
    lists:foreach(
        fun(Invalid) ->
            {ok, 400, _, Ref} = hackney:request(post, URL, Headers, Invalid, []),
            {ok, Response} = hackney:body(Ref),
            assert_invalid_json(Response)
        end,
        InvalidBodies
    ).

t_json_utf8_chunked(_Config) ->
    %% UTF-8 characters and surrogate pairs can cross HTTP chunk boundaries.
    ValidChunks = [
        {[<<"{\"value\":\"caf">>, <<16#C3>>, <<16#A9, "\"}">>], <<"caf", 16#C3, 16#A9>>},
        {[<<"{\"value\":\"\\uD83D">>, <<"\\uDE00\"}">>], <<16#1F600/utf8>>}
    ],
    lists:foreach(
        fun({Chunks, Expected}) ->
            {200, Response} = json_stream_request(Chunks),
            ?assertEqual(#{<<"value">> => Expected}, jsx:decode(Response, [return_maps]))
        end,
        ValidChunks
    ),
    {400, InvalidResponse} = json_stream_request([
        <<"{\"value\":\"caf">>, <<16#C3>>, <<"\"}">>
    ]),
    assert_invalid_json(InvalidResponse).

%% `update_log_meta/1' does nothing in a process that does not handle a
%% minirest request.
t_update_log_meta_outside_request(_Config) ->
    ?assertEqual(ok, minirest_handler:update_log_meta(#{source => <<"nobody">>})),
    ?assertEqual(undefined, erlang:get({minirest_handler, meta})).

%% A handler crash with the request in the stacktrace arguments does not
%% put the authorization header value into the response or the log.
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
    #{report := #{stacktrace := [{_, _, [get, Params, _Request], _} | _]}} =
        Log = receive_crash_log(),
    TokenSize = integer_to_binary(length(?AUTH_TOKEN)),
    ?assertMatch(
        #{headers := #{<<"authorization">> := <<"...(", TokenSize:2/binary, " bytes)">>}},
        Params
    ),
    lists:foreach(
        fun(Output) ->
            ?assertEqual(nomatch, string:find(Output, "token-must-not-appear-in-output"))
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
    SecretSize = integer_to_binary(byte_size(Secret)),
    ?assertEqual({badmatch, <<"...(", SecretSize/binary, " bytes)">>}, Reason),
    lists:foreach(
        fun(Output) -> ?assertEqual(nomatch, string:find(Output, Secret)) end,
        [Body, io_lib:format("~0p", [Log])]
    ).

%% The logged stacktrace still names the module, function, file and line
%% of the crash, and shows the shape of the arguments.
t_crash_trace_keeps_location(_Config) ->
    {ok, {{_, 500, _}, _, _}} = httpc:request(address() ++ "/crash_function_clause"),
    #{report := #{exception := error, reason := function_clause, stacktrace := Stack}} =
        receive_crash_log(),
    [{?HANDLER_MODULE, crash_function_clause, [get, #{body := _}, #{} = _Request], Location} | _] =
        Stack,
    ?assert(is_integer(proplists:get_value(line, Location))),
    ?assertMatch(
        "minirest_test_handler.erl", filename:basename(proplists:get_value(file, Location))
    ).

%% The logged arguments are cut at a fixed depth and a fixed number of
%% map entries.
t_crash_trace_limits(_Config) ->
    Body = maps:from_list(
        [
            {iolist_to_binary(io_lib:format("k~2..0b", [I])), #{<<"a">> => #{<<"b">> => 1}}}
         || I <- lists:seq(1, 20)
        ]
    ),
    {ok, {{_, 500, _}, _, _}} =
        httpc:request(
            post,
            {address() ++ "/crash_deep_body", [], "application/json", jsx:encode(Body)},
            [],
            []
        ),
    #{report := #{stacktrace := [{_, _, [post, #{body := Scrubbed}], _} | _]}} =
        receive_crash_log(),
    ?assertEqual(11, maps:size(Scrubbed)),
    ?assertEqual('...', maps:get('...', Scrubbed)),
    maps:foreach(
        fun
            ('...', _) -> ok;
            (_K, V) -> ?assertEqual(#{<<"a">> => '...'}, V)
        end,
        Scrubbed
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

%% A malformed authorization header makes the header parser in the
%% authorize callback fail. The request gets 400, and no log event holds
%% the header content.
t_crash_bad_header_in_authorize(_Config) ->
    Header = "Basic " ++ base64:encode_to_string("fakekey-NOCOLON-fakesecret"),
    {ok, {{_, Status, _}, _, Body}} =
        httpc:request(
            get,
            {address() ++ "/auth_meta_in_handler", [{"authorization", Header}]},
            [],
            [{body_format, binary}]
        ),
    assert_no_log_holds(["fakesecret", base64:encode_to_string("fakekey-NOCOLON-fakesecret")]),
    ?assertEqual(400, Status),
    ?assertMatch(#{<<"code">> := <<"BAD_REQUEST">>}, jsx:decode(Body, [return_maps])).

%% A crash in the authorize callback gets 500, and the log holds the
%% scrubbed stacktrace but not the authorization header value.
t_crash_in_authorize(_Config) ->
    {ok, {{_, Status, _}, _, Body}} =
        httpc:request(
            get,
            {address() ++ "/auth_meta_in_handler", [{"authorization", ?AUTH_TOKEN}]},
            [],
            [{body_format, binary}]
        ),
    assert_no_log_holds(["token-must-not-appear-in-output"]),
    ?assertEqual(500, Status),
    ?assertMatch(#{<<"code">> := <<"INTERNAL_ERROR">>}, jsx:decode(Body, [return_maps])),
    #{report := #{stacktrace := [{?HANDLER_MODULE, authorize_crash, [_Request], _} | _]}} =
        receive_crash_log().

%% A crash in the filter gets 500, and the log holds the scrubbed
%% stacktrace but not the authorization header value.
t_crash_in_filter(_Config) ->
    {ok, {{_, Status, _}, _, Body}} =
        httpc:request(
            get,
            {address() ++ "/crash_in_filter", [{"authorization", ?AUTH_TOKEN}]},
            [],
            [{body_format, binary}]
        ),
    assert_no_log_holds(["token-must-not-appear-in-output"]),
    ?assertEqual(500, Status),
    ?assertMatch(#{<<"code">> := <<"INTERNAL_ERROR">>}, jsx:decode(Body, [return_maps])),
    #{report := #{reason := function_clause}} = receive_crash_log().

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

set_cookie_headers(Headers) ->
    [V || {K, V} <- Headers, string:lowercase(K) =:= <<"set-cookie">>].

cookie_pair(Cookie) ->
    hd(binary:split(Cookie, <<";">>)).

assert_invalid_json(Response) ->
    ?assert(byte_size(Response) < 128),
    ?assertEqual(nomatch, binary:match(Response, <<16#EF, 16#BF, 16#BD>>)),
    ?assertEqual(
        #{
            <<"code">> => <<"BAD_REQUEST">>,
            <<"message">> => <<"Invalid json message received">>
        },
        jsx:decode(Response, [return_maps])
    ).

json_stream_request(Chunks) ->
    Headers = [
        {<<"content-type">>, <<"application/json">>},
        {<<"transfer-encoding">>, <<"chunked">>}
    ],
    {ok, Ref} = hackney:request(post, address() ++ "/echo_json", Headers, stream, []),
    lists:foreach(fun(Chunk) -> ok = hackney:send_body(Ref, Chunk) end, Chunks),
    ok = hackney:finish_send_body(Ref),
    {ok, Status, _, Ref} = hackney:start_response(Ref),
    {ok, Response} = hackney:body(Ref),
    {Status, Response}.

add_log_capture() ->
    logger:add_handler(?LOG_CAPTURE, ?MODULE, #{level => warning, config => #{pid => self()}}).

%% logger handler callback
log(Event, #{config := #{pid := Pid}}) ->
    Pid ! {log_event, Event}.

receive_crash_log() ->
    receive
        {log_event, #{msg := {report, Report}, meta := #{mfa := {minirest_handler, _, _}}}} ->
            #{report => Report}
    after 5000 ->
        ct:fail(no_crash_log)
    end.

%% Check every log event that arrives within one second, including crash
%% reports from the cowboy request process. The events stay in the mailbox.
assert_no_log_holds(Values) ->
    timer:sleep(1000),
    {messages, Messages} = erlang:process_info(self(), messages),
    lists:foreach(
        fun
            ({log_event, Event}) ->
                Text = io_lib:format("~0p", [Event]),
                [?assertEqual(nomatch, string:find(Text, V), Text) || V <- Values];
            (_) ->
                ok
        end,
        Messages
    ).

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
