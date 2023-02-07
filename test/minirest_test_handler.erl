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

-export([lazy_body/2,
         binary_body/2]).

api_spec() ->
  {
    [lazy_body(),
     binary_body()],
    []
  }.

lazy_body() ->
    MetaData = #{
        get => #{
            description => "lazy body",
            responses => #{
            <<"200">> => #{
                content => #{
                  'text/plain' => #{
                        schema => #{
                            type => string}}}}}}
                },
  {"/lazy_body", MetaData, lazy_body}.

binary_body() ->
    MetaData = #{
        get => #{
            description => "binary body",
            responses => #{
            <<"200">> => #{
                content => #{
                  'text/plain' => #{
                        schema => #{
                            type => string}}}}}}
                },
  {"/binary_body", MetaData, binary_body}.

lazy_body(get, _) ->
    BodyQH = qlc:table(fun() -> [<<"first">>, <<"second">>] end, []),
    {200, #{<<"content-type">> => <<"test/plain">>}, BodyQH}.

binary_body(get, _) ->
    Body = <<"alldataatonce">>,
    {200, #{<<"content-type">> => <<"test/plain">>}, Body}.

