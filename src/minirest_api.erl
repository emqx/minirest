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

-define(LOG(Level, Format, Args), logger:Level("Minirest: " ++ Format, Args)).

-include("minirest.hrl").

-callback api_spec() -> api_spec().

-export([find/1]).

-spec(find([App :: atom()]) -> [Module :: atom()]).
find(Apps) ->
    find_api_modules(Apps).

%%==============================================================================================
%% internal
find_api_modules([]) ->
    [];
find_api_modules(Apps) ->
    lists:append([app_modules(App) || App <- Apps]).

app_modules(App) ->
    app_modules_(App, behavior) ++ app_modules_(App, behaviour).

app_modules_(App, BehaviourOrBehavior) ->
    case application:get_key(App, modules) of
        undefined ->
            error({error_app, App});
        {ok, Modules} ->
            Fun =
                fun(Module, ApiModules) ->
                    case proplists:get_value(BehaviourOrBehavior, Module:module_info(attributes), undefined) of
                        [minirest_api] ->
                            [Module | ApiModules];
                        _ ->
                            ApiModules
                    end
                end,
            lists:foldl(Fun, [], Modules)
    end.
