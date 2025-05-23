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
-module(minirest_api).

-include("minirest.hrl").

-callback api_spec() -> api_spec().
-export([find_api_modules/1]).

find_api_modules(Apps) ->
    find_api_modules(Apps, []).

find_api_modules([], Acc) ->
    Acc;
find_api_modules([App | Apps], Acc) ->
    case application:get_key(App, modules) of
        undefined ->
            Acc;
        {ok, Modules} ->
            NewAcc = lists:filter(
                fun(Module) ->
                    Info = Module:module_info(attributes),
                    Behaviour = lists:flatten(
                        proplists:get_all_values(behavior, Info) ++
                            proplists:get_all_values(behaviour, Info)
                    ),
                    lists:member(minirest_api, Behaviour)
                end,
                Modules
            ),
            find_api_modules(Apps, NewAcc ++ Acc)
    end.
