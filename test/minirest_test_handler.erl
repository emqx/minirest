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
    lazy_body/2,
    binary_body/2,
    flex_error/2,
    qs_params/2,
    auth_meta_in_filter/2,
    auth_meta_in_handler/2,
    handler_meta_in_auth/2
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
            handler_meta_in_auth()
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

authorize1(_Req) ->
    {ok, #{message => <<"hello from authorize">>}}.

authorize2(_Req, #{module := Module, function := Fun}) ->
    {ok, #{
        message =>
            <<"hello from ", (atom_to_binary(Module))/binary, ":", (atom_to_binary(Fun))/binary>>
    }}.

lazy_body(get, _) ->
    BodyQH = qlc:table(fun() -> [<<"first">>, <<"second">>] end, []),
    {200, #{<<"content-type">> => <<"test/plain">>}, BodyQH}.

binary_body(get, _) ->
    Body = <<"alldataatonce">>,
    {200, #{<<"content-type">> => <<"test/plain">>}, Body}.

qs_params(get, #{query_string := Qs}) ->
    #{<<"single">> := <<"foo">>, <<"array">> := [<<"bar">>, <<"foo">>]} = Qs,
    {200, #{<<"content-type">> => <<"test/plain">>}, <<"OK">>}.

flex_error(get, _) ->
    {400, #{message => <<"boom">>, code => 'BAD_REQUEST', hint => <<"something went wrong">>}}.

auth_meta_in_filter(get, _) ->
    {200, #{<<"content-type">> => <<"test/plain">>}, <<"OK">>}.

auth_meta_in_handler(get, #{auth_meta := #{message := Message}}) ->
    {200, #{<<"content-type">> => <<"test/plain">>}, Message}.

handler_meta_in_auth(get, #{auth_meta := #{message := Message}}) ->
    {200, #{<<"content-type">> => <<"test/plain">>}, Message}.

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
