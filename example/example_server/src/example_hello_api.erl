%% Copyright (c) 2013-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(example_hello_api).

-export([api_spec/0]).

-export([hello/1]).

-spec(api_spec() -> [{Path :: string(), Metadata :: map()}]).
api_spec() ->
    Path = "/hello",
    Metadata = #{
        get =>
           #{tags => ["example"],
            description => "hello world",
            operationId => hello,
            responses => 
                #{<<"200">> => 
                    #{content => 
                    #{'text/plain' =>
                        #{schema => #{type => string}}}}}}},
    [{Path, Metadata}].

hello(_Request) ->
    StatusCode = 200,
    Headers = #{<<"Content-Type">> => <<"text/plain">>},
    Body = <<"hello world !">>,
    {StatusCode, Headers, Body}.
