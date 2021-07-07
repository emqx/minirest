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
    Apps = maps:get(apps, Options, []),
    Modules = minirest_api:find(Apps),
    ModuleApiSpecList = [module_api_spec(Module) || Module <- Modules],
    {Trails0, Schemas} = trails_schemas(Options, ModuleApiSpecList),
    case maps:get(swagger_support, Options, true) of
        false ->
            {Trails0, Schemas};
        _ ->
            {Trails0 ++ trails:trails([cowboy_swagger_handler]), Schemas}
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
            Callback = #handler{
                method = MethodAtom,
                path = Path,
                module = Module,
                function = Function,
                authorization = Authorization,
                filter = Filter},
            maps:put(Method, Callback, HandlerState)
        end,
    HandlerState = maps:fold(Fun, #{}, Metadata),
    trails:trail(append_base_path(BasePath, Path), minirest_handler, HandlerState, Metadata).

module_api_spec(Module) ->
    try
        {Module, erlang:apply(Module, ?API_SPEC, [])}
    catch
        E:R:S ->
            ?LOG(error, "Start module ~p fail, ~p: ~p: ~p", [Module, E, R, S]),
            error({start_fail, Module, E, R, S})
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
