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
        , start/3
        , stop/1
        , ref/1]).

-include("minirest.hrl").

start(Name, Options) ->
    start(Name, ranch_opts(Options), maps:without([ranch_options], Options)).

start(Name, RanchOptions, Options) ->
    Protocol = maps:get(protocol, Options, http),
    SwaggerSupport = maps:get(swagger_support, Options, true),
    SwaggerSupport andalso set_swagger_global_spec(Options),
    {Trails, Schemas} = minirest_trails:trails_schemas(Options#{name => Name}),
    SwaggerSupport andalso trails:store(Name, Trails),
    SwaggerSupport andalso [cowboy_swagger:add_definition(Schema) || Schema <- Schemas],
    Dispatch = merge_dispatch(Trails, Options),
    CowboyOptions = #{env => #{dispatch => Dispatch}},
    start_listener(Protocol, Name, RanchOptions, CowboyOptions).

stop(Name) ->
    cowboy:stop_listener(Name).

ref(Name) when is_atom(Name) ->
    ref(atom_to_binary(Name, utf8));

ref(Name) ->
    cowboy_swagger:schema(Name).

%%%==============================================================================================
%% internal

ranch_opts(#{protocol := http, ranch_options := RanchOpts}) ->
    RanchOpts;
ranch_opts(#{protocol := http}) ->
    [{port, 18083}];
ranch_opts(#{protocol := https, ranch_options := RanchOpts}) ->
    RanchOpts.

merge_dispatch(Trails, #{dispatch := Dispatch0}) ->
    [{Host, CowField, RestDispatch}] = trails:single_host_compile(Trails),
    [{_, _, Dispatch}] = cowboy_router:compile([{'_', Dispatch0}]),
    [{Host, CowField, RestDispatch ++ Dispatch}];

merge_dispatch(Trails, _)->
    trails:single_host_compile(Trails).

start_listener(http, Name, TransOpts, CowboyOptions) ->
    start_listener_(start_clear, Name, TransOpts, CowboyOptions);
start_listener(https, Name, TransOpts, CowboyOptions) ->
    start_listener_(start_tls, Name, TransOpts, CowboyOptions).

start_listener_(StartFunction, Name, TransOpts, CowboyOptions) ->
    Port = maps:get(port, TransOpts),
    case erlang:apply(cowboy, StartFunction, [Name, TransOpts, CowboyOptions]) of
        {ok, Pid} ->
            ?LOG(info, #{msg => "started_listener_ok",
                         name => Name,
                         port => Port,
                         pid => Pid}),
            {ok, Pid};
        {error, Reason} ->
            LogData =  #{msg => "failed_to_start_listener",
                         port => Port,
                         reason => Reason},
            case Reason of
                eaddrinuse ->
                    ?LOG(error, LogData#{description => "the_port_is_in_use"});
                _ ->
                    ?LOG(error, LogData)
            end,
            error(Reason)
    end.

set_swagger_global_spec(Options) ->
    DefaultGlobalSpec = #{
        swagger => "2.0",
        info => #{title => "minirest API", version => " "}
    },
    GlobalSpec = maps:get(swagger_global_spec, Options, DefaultGlobalSpec),
    application:set_env(cowboy_swagger, global_spec, GlobalSpec).
