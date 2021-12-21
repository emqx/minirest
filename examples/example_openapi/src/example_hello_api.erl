%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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
%%--------------------------------------------------------------------
-module(example_hello_api).

-behavior(minirest_api).

%% API
-export([api_spec/0]).

-export([hello/2]).

api_spec() ->
    {
        [hello_api()],
        []
    }.

hello_api() ->
    MetaData = #{
        get => #{
            description => "hello world",
            responses => #{
                <<"200">> => #{
                    content => #{
                        'application/json' => #{
                            schema => #{
                                type => object,
                                properties => #{
                                    msg => #{
                                        type => string}}}},
                        'text/plain' => #{
                            schema => #{
                                type => string}}}}}}},
    {"/hello", MetaData, hello}.

hello(get, #{headers := Headers}) ->
    Content = maps:get(<<"accept">>, Headers),
    Body = case Content of
        <<"text/plain">> ->
            <<"hello, minirest">>;
        <<"application/json">> ->
            #{msg => <<"hello minirest">>}
    end,
    {200, #{<<"content-type">> => Content},  Body}.

