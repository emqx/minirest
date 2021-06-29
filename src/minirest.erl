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

-define(LOG(Level, Format, Args), logger:Level("Minirest: " ++ Format, Args)).

-export([ start/2
        , stop/1]).

start(Name, Options) ->
    Modules        = maps:get(modules, Options, []),
    RootPath       = maps:get(root_path, Options, ""),
    HttpsEnable    = maps:get(https, Options, false),
    Authorization  = maps:get(authorization, Options, undefined),
    SwaggerSupport = maps:get(swagger_support, Options, true),
    Port           = maps:get(port, Options),
    Trails = minirest_trails:get_trails(Modules, RootPath, Authorization, SwaggerSupport),
    SwaggerSupport andalso trails:store(Trails),
    SwaggerSupport andalso minirest_schema_manager:new(Modules),
    Dispatch = trails:single_host_compile(Trails),
    CowboyOptions = #{ env => #{dispatch => Dispatch}},
    TransOpts = maps:to_list(maps:without([modules, root_path, https, authorization, swagger_support], Options)),
    StartFunction =
        case HttpsEnable of
            false ->
                start_clear;
            _ ->
                start_tls
        end,
    case erlang:apply(cowboy, StartFunction, [Name, TransOpts, CowboyOptions]) of
        {ok, Pid} ->
            ?LOG(info, "Start ~s listener on ~p unsuccessfully: ~0p", [Name, Port, Pid]),
            {ok, Pid};
        {error, eaddrinuse} ->
            ?LOG(error, "Start ~s listener on ~p failed: ~0p", [Name, Port, eaddrinuse]),
            error(eaddrinuse);
        {error, Any} ->
            ?LOG(error, "Start ~s listener on ~p failed: ~0p", [Name, Port, Any]),
            error(Any)
    end.

stop(Name) ->
    cowboy:stop_listener(Name).
