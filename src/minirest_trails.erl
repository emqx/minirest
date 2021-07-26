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

-define(LOG(Level, Format, Args), logger:Level("Minirest: " ++ Format, Args)).

-export([trails_schemas/1]).

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
            {Trails0, Schemas0} = trails_schemas(BasePath, Authorization, ModuleApiSpec),
            {lists:append(Trails, Trails0), lists:append(Schemas, Schemas0)}
        end,
    lists:foldl(Fun, {[], []}, ModuleApiSpecList).

trails_schemas(BasePath, Authorization, {Module, {Apis, Schemas}}) ->
    Trails = [trails_schemas(BasePath, Authorization, Module, Api) || Api <- Apis],
    {Trails, Schemas}.

trails_schemas(BasePath, Authorization, Module, {Path, Metadata, Function}) ->
    Fun =
        fun(MethodAtom, MethodDef, HandlerState) ->
            Method = trans_method(MethodAtom),
            Filter = trans_filter(Method, MethodDef),
            Security = maps:get(security, MethodDef, []),
            Callback = #handler{
                method = MethodAtom,
                path = Path,
                module = Module,
                function = Function,
                authorization = Security =/= [] andalso Authorization,
                filter = Filter},
            maps:put(Method, Callback, HandlerState)
        end,
    HandlerState = maps:fold(Fun, #{}, Metadata),
    trails:trail(append_base_path(BasePath, Path), minirest_handler, HandlerState, Metadata).

api_spec(Security, Module) ->
    try
        {Apis, Schemas} = erlang:apply(Module, ?API_SPEC, []),
        {Module, {[generate_api(Security, Api) || Api <- Apis], Schemas}}
    catch
        E:R:S ->
            ?LOG(error, "Start module ~p fail, ~p: ~p: ~p", [Module, E, R, S]),
            error({start_fail, Module, E, R, S})
    end.

generate_api(undefined, Api = {Path, _MetaData, _Function}) ->
    Default = #{tags => [root_path(Path)]},
    generate_api_(Default, Api);
generate_api(Security, Api = {Path, _MetaData, _Function}) ->
    Default = #{
        tags => [root_path(Path)],
        security => Security
    },
    generate_api_(Default, Api).

generate_api_(Default, {Path, MetaData, Function}) ->
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
        #{}, MetaData), Function}.

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

trans_method(get)     -> <<"GET">>;
trans_method(post)    -> <<"POST">>;
trans_method(put)     -> <<"PUT">>;
trans_method(head)    -> <<"HEAD">>;
trans_method(delete)  -> <<"DELETE">>;
trans_method(patch)   -> <<"PATCH">>;
trans_method(options) -> <<"OPTION">>;
trans_method(connect) -> <<"CONNECT">>;
trans_method(trace)   -> <<"TRACE">>.

%% TODO: filter by metadata
trans_filter(<<"GET">>, _Options) ->
    fun(Request) -> {ok, Request} end;
trans_filter(<<"POST">>, _Options) ->
    fun(Request) -> {ok, Request} end;
trans_filter(<<"PUT">>, _Options) ->
    fun(Request) -> {ok, Request} end;
trans_filter(<<"DELETE">>, _Options) ->
    fun(Request) -> {ok, Request} end;
trans_filter(_, _Options) ->
    fun(Request) -> {ok, Request} end.
