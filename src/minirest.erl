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

-module(minirest).

-export([ start/2
        , stop/1]).

start(Name, Options) ->
    Modules = maps:get(modules, Options, []),
    RootPath = maps:get(root_path, Options, ""),
    Trails = minirest_trails:get_trails(Modules, RootPath) ++ trails:trails([cowboy_swagger_handler]),
    trails:store(Trails),
    ok = minirest_schema_manager:new(Modules),
    Dispatch = trails:single_host_compile(Trails),
    RanchOptions  = [{port, maps:get(port, Options)}],
    CowboyOptions = #{ env      => #{dispatch => Dispatch}
                     , compress => true
                     , timeout  => 12000
                     },
    {ok, _} = cowboy:start_clear(Name, RanchOptions, CowboyOptions).

stop(Name) ->
    cowboy:stop_listener(Name).
