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

get_trails(Modules, RootPath, GlobalFilter, true) ->
    get_trails(Modules, RootPath, GlobalFilter, false) ++ trails:trails([cowboy_swagger_handler]);

get_trails(Modules, RootPath, GlobalFilter, false) ->
    Fun = fun(Module, CurrentTrails) -> CurrentTrails ++ module_trails(Module, RootPath, GlobalFilter) end,
    lists:foldl(Fun, [], Modules).

module_trails(Module, RootPath, GlobalFilter) ->
    try
        OpenApis = erlang:apply(Module, ?API_SPEC, []),
        Fun =
            fun({Path, Metadata}, Trails) ->
                Trails ++ handler_trails(Module, RootPath, Path, Metadata, GlobalFilter)
            end,
        lists:foldl(Fun, [], OpenApis)
    catch E:R:S ->
        io:format("Callback Module ~p, ~p ~p ~p", [Module, E, R, S])
    end.

handler_trails(Module, RootPath, Path, Metadata, GlobalFilter) ->
    HandlerState = minirest_handler:init_state(Module, Metadata, GlobalFilter),
    [trails:trail(RootPath ++ Path, minirest_handler, HandlerState, Metadata)].
