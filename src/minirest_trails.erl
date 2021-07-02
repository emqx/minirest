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

-module(minirest_trails).

-define(API_SPEC, rest_api).

-export([get_trails/4]).

get_trails(Modules, BasePath, Authorization, true) ->
    get_trails(Modules, BasePath, Authorization, false) ++ trails:trails([cowboy_swagger_handler]);

get_trails(Modules, BasePath, Authorization, false) ->
    Fun = fun(Module, CurrentTrails) -> CurrentTrails ++ module_trails(Module, BasePath, Authorization) end,
    lists:foldl(Fun, [], Modules).

module_trails(Module, BasePath, Authorization) ->
    try
        OpenApis = erlang:apply(Module, ?API_SPEC, []),
        Fun =
            fun({Path, Metadata}, Trails) ->
                Trails ++ handler_trails(Module, BasePath, Path, Metadata, Authorization)
            end,
        lists:foldl(Fun, [], OpenApis)
    catch E:R:S ->
        io:format("Callback Module ~p, ~p ~p ~p", [Module, E, R, S])
    end.

handler_trails(Module, BasePath, Path, Metadata, Authorization) ->
    HandlerState = minirest_handler:init_state(Path, Module, Metadata, Authorization),
    [trails:trail(append_base_path(BasePath, Path), minirest_handler, HandlerState, Metadata)].

append_base_path(undefined, Path)->
    Path;
append_base_path(BasePath, Path)->
    lists:append(BasePath, Path).
