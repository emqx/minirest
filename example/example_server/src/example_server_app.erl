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


-module(example_server_app).

-behaviour(application).

-export([start/2, stop/1]).

-export([auth/1]).

start(_StartType, _StartArgs) ->
    application:ensure_all_started(minirest),
    Modules = [example_hello_api, example_echo_api, example_pets_api],
    Authorization = {?MODULE, auth},
    Options =
        #{port => 8088
        , root_path => "/minirest"
        , modules => Modules
        , authorization => Authorization},
    minirest:start(?MODULE, Options),
    example_server_sup:start_link().

stop(_State) ->
    minirest:stop(?MODULE),
    ok.

-spec(auth(map()) -> ok | any()).
auth(Request) ->
    io:format("auth: ~0p~n", [Request]),
    ok.