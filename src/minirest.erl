%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-behaviour(gen_server).

-include_lib("include/minirest.hrl").

-export([ start_listener/4
        , start_listener/5
        , stop_listener/1
        , pipeline/3
        , find_all_routes/1
        , find_api_spec/2]).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

-record(state, {
    http_api :: map()
}).

%%-----------------------------------------------------------------------
%% get_env callback
%%-----------------------------------------------------------------------
start_listener(ServerRoot, ServerName, CowboyOpts, MinirestOpts) ->
    start_listener(ServerRoot, ServerName, CowboyOpts, MinirestOpts, []).

start_listener(ServerRoot, ServerName, CowboyOpts, MinirestOpts, Applications) ->
    {ok, _} = gen_server:start_link({local, ServerName}, ?MODULE,
    [ServerRoot, ServerName, CowboyOpts, MinirestOpts, Applications], []).

init([ServerRoot, ServerName, CowboyOpts, MinirestOpts, Applications]) ->
    process_flag(trap_exit, true),
    StartFun = case proplists:get_value(application, CowboyOpts, http) of
                   http -> start_clear;
                   https -> start_tls
               end,
    Routers = routes(ServerRoot, Applications),
    ct:print("Routers => :~p~n", [Routers]),
    %% TODO For I18N but not current version
    ServerLocal = maps:get(server_local, MinirestOpts, en),
    Dispatch = cowboy_router:compile([
        {'_', [{'_', minirest_dispatcher, #{server_name => ServerName,
                                            server_local => ServerLocal,
                                            server_root => ServerRoot}}]}
    ]),
    MiddleWares = maps:get(middlewares, MinirestOpts, []),
    Envs = case MiddleWares of
        [] -> #{env => #{dispatch => Dispatch}};
        _ -> #{env => #{dispatch => Dispatch},
               middlewares => MiddleWares}
    end,
    {ok, _} = cowboy:StartFun(ServerName, CowboyOpts, Envs),
    {ok, #state{http_api = #{routers => Routers}}}.

%% find api spec
handle_call(find_all_routes, _From,
            #state{http_api = #{routers := RoutersSpec}} = State) ->
    {reply, {ok, maps:keys(RoutersSpec)}, State};

handle_call({find_api_spec, MapKey}, _From,
            #state{http_api = #{routers := RoutersSpec}} = State) ->
    case maps:find(MapKey, RoutersSpec) of
        {ok, Router} -> {reply, {ok, Router}, State};
        error -> {reply, {not_fond, MapKey}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%---------------------------------------------------------------
%% Private functions
%%---------------------------------------------------------------

find_all_routes(GenServerName) ->
    gen_server:call(GenServerName, find_all_routes, 1000).

-spec(find_api_spec(GenServerName::atom(),
                    MapKey::list()) -> {ok, term()}).
find_api_spec(GenServerName, MapKey) ->
    gen_server:call(GenServerName, {find_api_spec, MapKey}, 1000).

-spec(stop_listener(atom()) -> ok).
stop_listener(ServerName) ->
    cowboy:stop_listener(ServerName).

%%---------------------------------------------------------------
%% Internal Functions
%%---------------------------------------------------------------

pipeline([], Request, Env) ->
    {ok, Request, Env};

pipeline([Fun | More], Request, Env) ->
    try
        case Fun(Request, Env) of
            {ok, NRequest} ->
                pipeline(More, NRequest, Env);
            {ok, NRequest, NEnv} ->
                pipeline(More, NRequest, NEnv);
            {stop, NReq} ->
                {stop, NReq}
        end
    catch
        _:Error:Stacktrace ->
            ct:print("Error:Stacktrace => :~p~n", [{Error, Stacktrace}]),
            logger:error("Error: ~p, Stacktrace: ~p", [Error, Stacktrace]),
            {stop, minirest_req:server_internal_error(Error, Stacktrace, Request), Env}
    end.


routes(ServerRoot, Applications) ->
    lists:foldl(
        fun(App, _Acc) ->
            {ok, Modules} = application:get_env(App, modules),
            paths(ServerRoot, Modules)
        end,
    [], Applications).

paths(ServerRoot, Modules) ->
    lists:foldl(
        fun(Module, _) ->
            lists:foldl(
            fun({http_api, [MetaData | _]}, Acc) ->
                Path = maps:get(path, MetaData),
                Method = maps:get(method, MetaData),
                Func = maps:get(func, MetaData),
                Parameters = maps:get(parameters, MetaData),
                ApiSpec = #{
                    path => Path,
                    method => Method,
                    handler => Module:module_info(module),
                    func => Func,
                    parameters => Parameters
                },
                %% #{Method:/api/v4/xxx => #{...}}
                ApiSpecMap = #{minirest_utils:gen_map_key(Method, ServerRoot, Path) => ApiSpec},
                maps:merge(ApiSpecMap, Acc);
            (_, Acc) ->
                Acc
            end, #{}, Module:module_info(attributes))
        end, [], Modules).
