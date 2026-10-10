%% Copyright (c) 2013-2022 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(minirest_test_handler).

-behavior(minirest_api).

%% API
-export([api_spec/0]).

-export([
    authorize1/1,
    authorize2/2,
    authorize_path/2,
    authorize_parse_header/1,
    authorize_crash/1,
    crash_in_filter/2,
    lazy_body/2,
    binary_body/2,
    flex_error/2,
    qs_params/2,
    auth_meta_in_filter/2,
    auth_meta_in_handler/2,
    handler_meta_in_auth/2,
    route_path_in_auth/2,
    post_large_body/2,
    set_cookie/2,
    set_cookies/2,
    echo_json/2,
    crash_function_clause/3,
    crash_badmatch/2,
    crash_deep_body/2,
    crash_plain/2
]).

api_spec() ->
    {
        [
            lazy_body(),
            binary_body(),
            flex_error(),
            qs_params(),
            auth_meta_in_filter(),
            auth_meta_in_handler(),
            handler_meta_in_auth(),
            route_path_in_auth(),
            post_large_body(),
            set_cookie(),
            set_cookies(),
            echo_json(),
            crash_in_filter(),
            crash_function_clause(),
            crash_badmatch(),
            crash_deep_body(),
            crash_plain()
        ],
        []
    }.

lazy_body() ->
    MetaData = #{
        get => #{
            description => "lazy body",
            responses => text_plain_200_response()
        }
    },
    {"/lazy_body", MetaData, lazy_body}.

binary_body() ->
    MetaData = #{
        get => #{
            description => "binary body",
            responses => text_plain_200_response()
        }
    },
    {"/binary_body", MetaData, binary_body}.

qs_params() ->
    MetaData = #{
        get => #{
            description => "parse QS params",
            responses => text_plain_200_response()
        }
    },
    {"/qs_params", MetaData, qs_params}.

flex_error() ->
    MetaData = #{
        get => #{
            description => "binary body",
            responses => #{
                <<"400">> => #{
                    content => #{
                        'application/json' => #{
                            schema => #{
                                type => string
                            }
                        }
                    }
                }
            }
        }
    },
    {"/flex_error", MetaData, flex_error}.

auth_meta_in_filter() ->
    MetaData = #{
        get => #{
            description => "auth meta in filter",
            responses => text_plain_200_response(),
            security => [#{application => []}]
        }
    },
    Filter = fun(#{auth_meta := #{message := Message}}, _) ->
        {200, #{<<"content-type">> => <<"test/plain">>}, Message}
    end,
    {"/auth_meta_in_filter", MetaData, auth_meta_in_filter, #{filter => Filter}}.

auth_meta_in_handler() ->
    MetaData = #{
        get => #{
            description => "auth meta in handler",
            responses => text_plain_200_response(),
            security => [#{application => []}]
        }
    },
    {"/auth_meta_in_handler", MetaData, auth_meta_in_handler}.

handler_meta_in_auth() ->
    MetaData = #{
        get => #{
            description => "handler meta in auth",
            responses => text_plain_200_response(),
            security => [#{application => []}]
        }
    },
    {"/handler_meta_in_auth", MetaData, handler_meta_in_auth}.

route_path_in_auth() ->
    MetaData = #{
        get => #{
            description => "route path in auth",
            responses => text_plain_200_response(),
            security => [#{application => []}]
        }
    },
    {"/route_path_in_auth/:id", MetaData, route_path_in_auth}.

post_large_body() ->
    MetaData = #{
        post => #{
            description => "post large body",
            responses => text_plain_200_response(),
            security => [#{application => []}]
        }
    },
    {"/post_large_body", MetaData, post_large_body}.

set_cookie() ->
    MetaData = #{
        get => #{
            description => "set one response cookie",
            responses => text_plain_200_response()
        }
    },
    {"/set_cookie", MetaData, set_cookie}.

set_cookies() ->
    MetaData = #{
        get => #{
            description => "set two response cookies",
            responses => text_plain_200_response()
        }
    },
    {"/set_cookies", MetaData, set_cookies}.

echo_json() ->
    MetaData = #{
        post => #{
            description => "echo parsed JSON body",
            responses => #{<<"200">> => #{description => "Parsed JSON body"}}
        }
    },
    {"/echo_json", MetaData, echo_json}.

crash_in_filter() ->
    MetaData = #{
        get => #{
            description => "crash in the filter",
            responses => text_plain_200_response()
        }
    },
    Filter = fun(#{never_present := _}, _) -> {ok, #{}} end,
    {"/crash_in_filter", MetaData, crash_in_filter, #{filter => Filter}}.

crash_function_clause() ->
    MetaData = #{
        get => #{
            description => "crash with function_clause",
            responses => text_plain_200_response()
        }
    },
    {"/crash_function_clause", MetaData, crash_function_clause}.

crash_badmatch() ->
    MetaData = #{
        post => #{
            description => "crash with badmatch on a body value",
            responses => text_plain_200_response()
        }
    },
    {"/crash_badmatch", MetaData, crash_badmatch}.

crash_deep_body() ->
    MetaData = #{
        post => #{
            description => "crash with function_clause and a large body",
            responses => text_plain_200_response()
        }
    },
    {"/crash_deep_body", MetaData, crash_deep_body}.

crash_plain() ->
    MetaData = #{
        get => #{
            description => "crash with a plain error",
            responses => text_plain_200_response()
        }
    },
    {"/crash_plain", MetaData, crash_plain}.

%%--------------------------------------------------------------------
%% Handlers
%%--------------------------------------------------------------------

authorize1(_Req) ->
    {ok, #{message => <<"hello from authorize">>}}.

authorize2(_Req, #{module := Module, function := Fun}) ->
    {ok, #{
        message =>
            <<"hello from ", (atom_to_binary(Module))/binary, ":", (atom_to_binary(Fun))/binary>>
    }}.

authorize_path(_Req, #{path := Path}) ->
    {ok, #{route_path => list_to_binary(Path)}}.

authorize_parse_header(Req) ->
    _ = cowboy_req:parse_header(<<"authorization">>, Req),
    {ok, #{message => <<"hello from authorize">>}}.

%% No clause matches a request, so the call fails with `function_clause'
%% and the stacktrace carries the request.
authorize_crash(#{never_present := _}) ->
    {ok, #{}}.

lazy_body(get, _) ->
    BodyQH = qlc:table(fun() -> [<<"first">>, <<"second">>] end, []),
    {200, #{<<"content-type">> => <<"test/plain">>}, BodyQH}.

binary_body(get, _) ->
    Body = <<"alldataatonce">>,
    {200, #{<<"content-type">> => <<"test/plain">>}, Body}.

qs_params(get, #{query_string := Qs}) ->
    #{<<"single">> := <<"foo">>, <<"array">> := [<<"foo">>, <<"bar">>, <<"baz">>]} = Qs,
    {200, #{<<"content-type">> => <<"test/plain">>}, <<"OK">>}.

flex_error(get, _) ->
    {400, #{message => <<"boom">>, code => 'BAD_REQUEST', hint => <<"something went wrong">>}}.

auth_meta_in_filter(get, _) ->
    {200, #{<<"content-type">> => <<"test/plain">>}, <<"OK">>}.

auth_meta_in_handler(get, #{auth_meta := #{message := Message}}) ->
    {200, #{<<"content-type">> => <<"test/plain">>}, Message}.

handler_meta_in_auth(get, #{auth_meta := #{message := Message}}) ->
    {200, #{<<"content-type">> => <<"test/plain">>}, Message}.

route_path_in_auth(get, #{auth_meta := #{route_path := RoutePath}}) ->
    {200, #{<<"content-type">> => <<"test/plain">>}, RoutePath}.

post_large_body(post, #{body := _Body}) ->
    {200, #{<<"content-type">> => <<"test/plain">>}, <<"OK">>}.

set_cookie(get, _) ->
    Cookie = iolist_to_binary(
        cow_cookie:setcookie(<<"one">>, <<"1">>, #{path => <<"/api">>, http_only => true})
    ),
    Headers = #{<<"content-type">> => <<"test/plain">>, <<"set-cookie">> => Cookie},
    {200, Headers, <<"OK">>}.

set_cookies(get, _) ->
    Cookies = [
        iolist_to_binary(cow_cookie:setcookie(<<"one">>, <<"1">>, #{})),
        iolist_to_binary(cow_cookie:setcookie(<<"two">>, <<"2">>, #{}))
    ],
    Headers = #{<<"content-type">> => <<"test/plain">>, <<"set-cookie">> => Cookies},
    {200, Headers, <<"OK">>}.

echo_json(post, #{body := Body}) ->
    {200, #{}, Body}.

%% No clause matches `get', so the call fails with `function_clause'
%% and the stacktrace carries the arguments, the request included.
crash_function_clause(post, _Params, _Request) ->
    {200, #{<<"content-type">> => <<"test/plain">>}, <<"OK">>}.

crash_badmatch(post, #{body := #{<<"secret">> := Secret}}) ->
    ok = Secret.

crash_deep_body(get, _Params) ->
    {200, #{<<"content-type">> => <<"test/plain">>}, <<"OK">>}.

crash_in_filter(get, _) ->
    {200, #{<<"content-type">> => <<"test/plain">>}, <<"OK">>}.

crash_plain(get, _) ->
    error(boom).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

text_plain_200_response() ->
    #{
        <<"200">> => #{
            content => #{
                'text/plain' => #{
                    schema => #{
                        type => string
                    }
                }
            }
        }
    }.
