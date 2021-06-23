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

-module(example_echo_api).

-export([api_spec/0]).

-export([echo/1]).

-spec(api_spec() -> [{Path :: string(), Metadata :: map()}]).
api_spec() ->
    Path = "/echo/:message",
    Metadata = #{
        get =>
           #{tags => ["example"],
            description => "echo parameters",
            operationId => echo,
            parameters => [#{ name => message
                            , in => path
                            , schema =>
                               #{ type => string
                                , example => "hello"}}],
            responses => #{<<"200">> => #{
                description => <<"echo message">>,
                content => #{
                  'text/plain' =>
                    #{schema => #{type => string}}}}}}},
    [{Path, Metadata}].

echo(Request) ->
    StatusCode = 200,
    Headers = #{<<"Content-Type">> => <<"text/plain">>},
    Body = cowboy_req:binding(message, Request),
    {StatusCode, Headers, Body}.
