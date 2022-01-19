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
-module(minirest_trails).

-include("minirest.hrl").

-export([trails_schemas/1]).

-define(HANDLER, minirest_handler).

trails_schemas(Options) ->
    Modules = maps:get(modules, Options, []),
    Name = maps:get(name, Options),
    Security = maps:get(security, Options, undefined),
    ModuleApiSpecList = [api_spec(Security, Module) || Module <- Modules],
    {Trails0, Schemas} = trails_schemas(Options, ModuleApiSpecList),
    case maps:get(swagger_support, Options, true) of
        false ->
            {Trails0, Schemas};
        _ ->
            {Trails0 ++ trails:trails([{cowboy_swagger_handler, #{server => Name}}]), Schemas}
    end.

trails_schemas(Options, ModuleApiSpecList) ->
    BasePath = maps:get(base_path, Options, undefined),
    Authorization = maps:get(authorization, Options, undefined),
    Fun =
        fun(ModuleApiSpec, {Trails, Schemas}) ->
            {Trails0, Schemas0} = do_trails_schemas(BasePath, Authorization, ModuleApiSpec),
            {lists:append(Trails, Trails0), lists:append(Schemas, Schemas0)}
        end,
    lists:foldl(Fun, {[], []}, ModuleApiSpecList).

do_trails_schemas(BasePath, Authorization, {Module, Apis, Schemas}) ->
    Trails = [trails_api(BasePath, Authorization, Module, Api) || Api <- Apis],
    {Trails, Schemas}.

trails_api(BasePath, Authorization, Module, {Path, Metadata, Function}) ->
    trails_api(BasePath, Authorization, Module, {Path, Metadata, Function, #{}});
trails_api(BasePath, Authorization, Module, {Path, Metadata, Function, Options}) ->
    Fun =
        fun(Method, MethodDef, HandlerStates) ->
            HandlerState = #handler{
                method        = Method,
                path          = Path,
                module        = Module,
                function      = Function,
                authorization = maps:get(security, MethodDef, []) =/= [] andalso Authorization,
                filter        = maps:get(filter, Options, undefined),
                args          = maps:get(args, Options, [])
            },
            maps:put(binary_method(Method), HandlerState, HandlerStates)
        end,
    HandlerStates = maps:fold(Fun, #{}, Metadata),
    CompletePath  = append_base_path(BasePath, Path),
    trails:trail(CompletePath, ?HANDLER, HandlerStates, Metadata).

api_spec(Security, Module) ->
    try
        {Apis, Schemas} = erlang:apply(Module, ?API_SPEC, []),
        {Module, [generate_api(Security, Api) || Api <- Apis], Schemas}
    catch
        E:R:S ->
            erlang:raise(E, {minirest_trails_api_spec_error, R}, S)
    end.

generate_api(Security, {Path, MetaData, Function}) ->
    generate_api(Security, {Path, MetaData, Function, #{}});
generate_api(undefined, Api = {Path, _MetaData, _Function, _Options}) ->
    Default = #{tags => [root_path(Path)]},
    generate_api_(Default, Api);
generate_api(Security, Api = {Path, _MetaData, _Function, _Options}) ->
    Default = #{
        tags => [root_path(Path)],
        security => Security
    },
    generate_api_(Default, Api).

generate_api_(Default, {Path, MetaData, Function, Options}) ->
    {Path, maps:fold(
               fun(Method, MethodDef0, NextMetaData) ->
                   MethodDef =
                       lists:foldl(
                           fun(Key, NMethodDef) ->
                               case maps:is_key(Key, NMethodDef) of
                                   true ->
                                       NMethodDef;
                                   false ->
                                       maps:put(Key, maps:get(Key, Default), NMethodDef)
                               end
                           end, MethodDef0, maps:keys(Default)),
                   maps:put(Method, MethodDef, NextMetaData)
               end,
        #{}, MetaData), Function, Options}.

root_path(Path) ->
    case string:tokens(Path, "/") of
        [] ->
            "/";
        [Root | _] ->
            Root
    end.

append_base_path(undefined, Path) ->
    Path;
append_base_path(BasePath, Path) ->
    lists:append(BasePath, Path).

binary_method(get)     -> <<"GET">>;
binary_method(post)    -> <<"POST">>;
binary_method(put)     -> <<"PUT">>;
binary_method(head)    -> <<"HEAD">>;
binary_method(delete)  -> <<"DELETE">>;
binary_method(patch)   -> <<"PATCH">>;
binary_method(options) -> <<"OPTION">>;
binary_method(connect) -> <<"CONNECT">>;
binary_method(trace)   -> <<"TRACE">>.
