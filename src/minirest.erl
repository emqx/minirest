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
        , stop/1
        , ref/1]).

start(Name, Options) ->
    Protocol = maps:get(protocol, Options, http),
    SwaggerSupport = maps:get(swagger_support, Options, true),
    SwaggerSupport andalso set_swagger_global_spec(Options),
    {Trails, Schemas} = minirest_trails:trails_schemas(Options),
    SwaggerSupport andalso trails:store(Name, Trails),
    SwaggerSupport andalso [cowboy_swagger:add_definition(Schema) || Schema <- Schemas],
    Dispatch = trails:single_host_compile(Trails),
    TransOpts = trans_options(Options),
    CowboyOptions = #{env => #{dispatch => Dispatch}},
    start_listener(Protocol, Name, TransOpts, CowboyOptions).

stop(Name) ->
    cowboy:stop_listener(Name).

ref(Name) when is_atom(Name) ->
    ref(atom_to_binary(Name, utf8));

ref(Name) ->
    cowboy_swagger:schema(Name).

%%%==============================================================================================
%% internal

trans_options(Options) ->
    IgnoreKeys =
        [ security
        , base_path
        , protocol
        , authorization
        , swagger_support
        , swagger_global_spec
        , apps],
    maps:to_list(maps:without(IgnoreKeys, Options)).

start_listener(http, Name, TransOpts, CowboyOptions) ->
    start_listener_(start_clear, Name, TransOpts, CowboyOptions);
start_listener(https, Name, TransOpts, CowboyOptions) ->
    start_listener_(start_tls, Name, TransOpts, CowboyOptions).

start_listener_(StartFunction, Name, TransOpts, CowboyOptions) ->
    Port = proplists:get_value(port, TransOpts),
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

set_swagger_global_spec(Options) ->
    DefaultGlobalSpec = #{
        swagger => "2.0",
        info => #{title => "minirest API", version => " "}
    },
    GlobalSpec = maps:get(swagger_global_spec, Options, DefaultGlobalSpec),
    application:set_env(cowboy_swagger, global_spec, GlobalSpec).
