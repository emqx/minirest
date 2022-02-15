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

-export([ reply/2
        ]).

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
    CowboyOptions = middlewares(Options, #{env => #{dispatch => Dispatch}}),
    start_listener(Protocol, Name, RanchOptions, CowboyOptions).

stop(Name) ->
    cowboy:stop_listener(Name).

ref(Name) when is_atom(Name) ->
    ref(atom_to_binary(Name, utf8));

ref(Name) ->
    cowboy_swagger:schema(Name).

reply(Response, Params) ->
    minirest_handler:reply(Response, Params).

%%%==============================================================================================
%% internal

ranch_opts(#{protocol := http, ranch_options := RanchOpts}) ->
    RanchOpts;
ranch_opts(#{protocol := https, ranch_options := RanchOpts}) ->
    RanchOpts.

merge_dispatch(Trails, #{dispatch := Dispatch0}) ->
    [{Host, CowField, RestDispatch}] = trails:single_host_compile(Trails),
    [{_, _, Dispatch}] = cowboy_router:compile([{'_', Dispatch0}]),
    [{Host, CowField, RestDispatch ++ Dispatch}];

merge_dispatch(Trails, _)->
    trails:single_host_compile(Trails).

middlewares(#{middlewares := []}, CowboyOptions) -> CowboyOptions;
middlewares(#{middlewares := [cowboy_router, cowboy_handler]}, CowboyOptions) -> CowboyOptions;
middlewares(#{middlewares := Middlewares}, CowboyOptions) when is_list(Middlewares) ->
    maps:put(middlewares, Middlewares, CowboyOptions);
middlewares(_, CowboyOptions) -> CowboyOptions.

start_listener(http, Name, TransOpts, CowboyOptions) ->
    start_listener_(start_clear, Name, TransOpts, CowboyOptions);
start_listener(https, Name, TransOpts, CowboyOptions) ->
    start_listener_(start_tls, Name, TransOpts, CowboyOptions).

start_listener_(StartFunction, Name, TransOpts, CowboyOptions) ->
    Res = erlang:apply(cowboy, StartFunction, [Name, TransOpts, CowboyOptions]),
    log_start_result(Res, #{name => Name, port => get_port(TransOpts)}),
    Res.

-define(FAILED_MSG, "started_listener_failed").

log_start_result({ok, Pid}, Log) ->
    ?LOG(info, Log#{msg => "started_listener_ok", pid => Pid});
log_start_result({error, eaddrinuse = Reason}, Log) ->
    ?LOG(error, Log#{msg => ?FAILED_MSG, description => "the_port_is_in_use", reason => Reason});
log_start_result({error, eacces = Reason}, Log) ->
    ?LOG(error, Log#{msg => ?FAILED_MSG, description => "permission_denied", reason => Reason});
log_start_result({error, no_cert = Reason}, Log) ->
    ?LOG(error, Log#{msg => ?FAILED_MSG, description => "no_certificate_provided;", reason => Reason});
%% copy from ranch.erl line:162
log_start_result({error, {{shutdown, {failed_to_start_child, ranch_acceptors_sup,
    {listen_error, _, Reason}}}, _}}, Log0) ->
    Log = Log0#{msg => ?FAILED_MSG, reason => Reason},
    case Reason of
        eaddrnotavail -> ?LOG(error, Log#{description => "cannot_assign_requested_address"});
        ebusy -> ?LOG(error, Log#{description => "file_busy"});
        _ -> ?LOG(error, Log)
    end.

get_port(L) when is_list(L) -> proplists:get_value(port, L);
get_port(#{port := Port}) -> Port;
get_port(#{socket_opts := SocketOpts}) -> proplists:get_value(port, SocketOpts);
get_port(_) -> undefined.

set_swagger_global_spec(Options) ->
    DefaultGlobalSpec = #{
        openapi => "3.0.0",
        info => #{title => "minirest API", version => " "}
    },
    GlobalSpec = maps:get(swagger_global_spec, Options, DefaultGlobalSpec),
    application:set_env(cowboy_swagger, global_spec, GlobalSpec).
