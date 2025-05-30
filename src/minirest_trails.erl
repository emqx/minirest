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

-export([atom_method/1]).

-define(HANDLER, minirest_handler).

trails_schemas(Options) ->
    Modules = modules(Options),
    Security = maps:get(security, Options, undefined),
    ModuleApiSpecList = minirest_util:pmap(
        fun(Module) -> api_spec(Security, Module) end, Modules, 30000
    ),
    assert_module_api_specs(ModuleApiSpecList),
    {Trails0, Schemas, ErrorCodes} = trails_schemas(Options, ModuleApiSpecList),
    case maps:get(swagger_support, Options, true) of
        false ->
            {Trails0, Schemas, ErrorCodes};
        _ ->
            Name = maps:get(name, Options),
            {
                Trails0 ++ trails:trails([{cowboy_swagger_handler, #{server => Name}}]),
                Schemas,
                ErrorCodes
            }
    end.

modules(Options) ->
    Modules = maps:get(modules, Options, []),
    case maps:get(server_info_api, Options, false) of
        true ->
            [minirest_info_api | Modules];
        false ->
            Modules
    end.

trails_schemas(Options, ModuleApiSpecList) ->
    BasePath = maps:get(base_path, Options, undefined),
    Authorization = maps:get(authorization, Options, undefined),
    Log = maps:get(log, Options, undefined),
    Fun =
        fun(ModuleApiSpec, {Trails, Schemas, ErrorCodes}) ->
            {Trails0, Schemas0, ErrorCodes0} = trails_schemas(
                BasePath, Authorization, Log, ModuleApiSpec
            ),
            {Trails ++ Trails0, Schemas ++ Schemas0, ErrorCodes ++ ErrorCodes0}
        end,
    {Trails, Schemas, ErrorCodes} = lists:foldl(Fun, {[], [], []}, ModuleApiSpecList),
    {Trails, Schemas, lists:usort(ErrorCodes)}.

trails_schemas(BasePath, Authorization, Log, {Module, {Apis, Schemas}}) ->
    {Trails, ErrorCodes} = lists:foldl(
        fun(Api, {TrailsAcc, ErrorCodesAcc}) ->
            {Trails0, ErrorCodes0} = trails_schemas(BasePath, Authorization, Log, Module, Api),
            {[Trails0 | TrailsAcc], ErrorCodes0 ++ ErrorCodesAcc}
        end,
        {[], []},
        Apis
    ),
    {lists:reverse(Trails), Schemas, ErrorCodes}.

trails_schemas(BasePath, Authorization, Log, Module, {Path, Metadata, Function}) ->
    trails_schemas(BasePath, Authorization, Log, Module, {Path, Metadata, Function, #{}});
trails_schemas(BasePath, Authorization, Log, Module, {Path, Metadata, Function, Options}) ->
    Fun =
        fun(Method, MethodDef, {MethodStatesAcc, ErrorCodesAcc}) ->
            #{responses := Responses} = MethodDef,
            ErrorCodes = maps:fold(fun get_error_codes/3, [], Responses),
            Security = maps:get(security, MethodDef, []),
            HandlerState = #handler{
                method = Method,
                module = Module,
                function = Function,
                authorization = authorization(Security, Authorization),
                filter = maps:get(filter, Options, undefined),
                log_meta = maps:get(log_meta, MethodDef, #{}),
                error_codes = ErrorCodes
            },
            {
                maps:put(binary_method(Method), HandlerState, MethodStatesAcc),
                ErrorCodes ++ ErrorCodesAcc
            }
        end,
    {MethodStates, ErrorCodes} = maps:fold(Fun, {#{}, []}, Metadata),
    HandlerState = #{path => Path, log => Log, methods => MethodStates},
    CompletePath = append_base_path(BasePath, Path),
    {trails:trail(CompletePath, ?HANDLER, HandlerState, Metadata), ErrorCodes}.

-define(NEST_CODE_KEYS, [
    <<"content">>,
    <<"application/json">>,
    <<"schema">>,
    <<"properties">>,
    <<"code">>
]).

get_error_codes(Status, _RespDef, Acc) when Status >= 200 andalso Status < 300 -> Acc;
get_error_codes(_Status, RespDef, Acc) when is_map(RespDef) ->
    case get_error_codes_(?NEST_CODE_KEYS, RespDef) of
        #{enum := Codes} -> format_code(Codes) ++ Acc;
        #{<<"enum">> := Codes} -> format_code(Codes) ++ Acc;
        _ -> Acc
    end.

get_error_codes_([], Data) ->
    Data;
get_error_codes_([Key | Keys], Data) when is_list(Data) ->
    case lists:keyfind(Key, 1, Data) of
        false -> [];
        {_, SubData} -> get_error_codes_(Keys, SubData)
    end;
get_error_codes_([Key | Keys], Data) when is_map(Data) ->
    case maps:get(Key, Data, undefined) of
        undefined -> [];
        SubData -> get_error_codes_(Keys, SubData)
    end.

authorization([], _Authorization) -> false;
authorization(_Security, Authorization) -> Authorization.

format_code(Codes) ->
    lists:map(
        fun
            (C) when is_binary(C) -> binary_to_atom(C);
            (C) when is_list(C) -> list_to_atom(C);
            (C) when is_atom(C) -> C
        end,
        Codes
    ).

api_spec(Security, Module) ->
    try
        {Apis, Schemas} = erlang:apply(Module, ?API_SPEC, []),
        {Module, {[generate_api(Security, Api) || Api <- Apis], Schemas}}
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
    MergeDefFun =
        fun(Method, MethodDef0, NextMetaData) ->
            GenerateMethodDef = maps:merge(Default, decs_str_to_binary(MethodDef0)),
            NextMetaData#{Method => GenerateMethodDef}
        end,
    {Path, maps:fold(MergeDefFun, #{}, MetaData), Function, Options}.

%% description must be a string(list or binary)
%% otherwise, raise minirest_description_not_format error.
%% for example: can't find description in i18n file.
decs_str_to_binary(#{description := Desc}) when is_tuple(Desc) ->
    erlang:error({minirest_description_not_formatted, Desc});
decs_str_to_binary(Data = #{description := Desc}) when is_list(Desc) ->
    decs_str_to_binary(Data#{description => list_to_binary(Desc)});
decs_str_to_binary(Data) when is_map(Data) ->
    Fun =
        fun
            (Key, Map, Res) when is_map(Map) ->
                Res#{Key => decs_str_to_binary(Map)};
            (oneOf, OneOf, Res) when is_list(OneOf) ->
                Res#{oneOf => [decs_str_to_binary(One) || One <- OneOf]};
            (parameters, Parameters, Res) ->
                Res#{parameters => [decs_str_to_binary(Parameter) || Parameter <- Parameters]};
            (_, _, Res) ->
                Res
        end,
    maps:fold(Fun, Data, Data);
decs_str_to_binary(Data) ->
    Data.

root_path(Path) ->
    case string:tokens(Path, "/") of
        [] -> "/";
        [Root | _] -> string:titlecase(Root)
    end.

append_base_path(undefined, Path) -> Path;
append_base_path(BasePath, Path) -> lists:append(BasePath, Path).

binary_method(get) -> <<"GET">>;
binary_method(post) -> <<"POST">>;
binary_method(put) -> <<"PUT">>;
binary_method(head) -> <<"HEAD">>;
binary_method(delete) -> <<"DELETE">>;
binary_method(patch) -> <<"PATCH">>;
binary_method(options) -> <<"OPTION">>;
binary_method(connect) -> <<"CONNECT">>;
binary_method(trace) -> <<"TRACE">>.

atom_method(<<"GET">>) -> get;
atom_method(<<"POST">>) -> post;
atom_method(<<"PUT">>) -> put;
atom_method(<<"HEAD">>) -> head;
atom_method(<<"DELETE">>) -> delete;
atom_method(<<"PATCH">>) -> patch;
atom_method(<<"OPTION">>) -> options;
atom_method(<<"CONNECT">>) -> connect;
atom_method(<<"TRACE">>) -> trace.

assert_module_api_specs(ModuleApiSpec) ->
    case [E || E = {error, _} <- ModuleApiSpec] of
        [] -> ok;
        Error -> erlang:error(Error)
    end.
