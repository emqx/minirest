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

-export([ start_listener/2
        , start_listener/3
        , stop_listener/1
        , pipeline/3
        , default_middlewares/0]).

-export([ within/2
        , within/3]).

% -export([routes/1]).

-import(proplists, [get_value/2]).

%%============================================================================
%% API
%%============================================================================

start_listener(Ref, Opts) ->
    start_listener(Ref, Opts, default_middlewares()).

start_listener(Ref, Opts, Middlewares) ->
    {ok, Proto, Opts1} = take(proto, Opts, http),
    StartFun = case Proto of
                   http -> start_clear;
                   https -> start_tls
               end,
    {ok, CurApp} = application:get_application(),
    %% TODO: scan_list -> better name 
    {ok, ScanList, Opts2} = take(scan_list, Opts1, [CurApp]),
    Dispatch = cowboy_router:compile(routes(ScanList)),
    {ok, _} = cowboy:StartFun(Ref, Opts2, #{env => #{dispatch => Dispatch},
                                            middlewares => [cowboy_router] ++ Middlewares ++ [cowboy_handler]}),
    case proplists:get_value(port, Opts2) of
        undefined -> io:format("Start ~s listener successfully.~n", [Ref]);
        Port -> io:format("Start ~s listener on ~p successfully.~n", [Ref, Port])
    end.

-spec(stop_listener(atom()) -> ok).
stop_listener(Ref) ->
    cowboy:stop_listener(Ref).

pipeline([], Req, Env) ->
    {ok, Req, Env};

pipeline([Fun | More], Req, Env) ->
    case Fun(Req, Env) of
        {ok, NReq} ->
            pipeline(More, NReq, Env);
        {ok, NReq, NEnv} ->
            pipeline(More, NReq, NEnv);
        {stop, NReq} ->
            {stop, NReq}
    end.

default_middlewares() ->
    [minirest_middleware].

within(Value, Min, infinity) ->
    Value >= Min;
within(Value, infinity, Max) ->
    Value =< Max;
within(Value, Min, Max)  ->
    Value >= Min andalso Value =< Max.

within(Value, Enums) ->
    lists:member(Value, Enums).

%%============================================================================
%% Internal Functions
%%============================================================================

routes(Apps) ->
    Paths = lists:foldl(fun(App, Acc) ->
                            {ok, Modules} = application:get_key(App, modules),
                            paths(Modules) ++ Acc
                        end, [], Apps),
    [{'_', Paths}].

paths(Modules) when is_list(Modules) ->
    paths(Modules, []).

paths([], Paths) ->
    Paths;
paths([Module | More], Paths) ->
    NPaths = lists:foldl(
                 fun({http_api, [#{private := true}]}, Acc) ->
                     Acc;
                    ({http_api, [Meta = #{resource := Resource}]}, Acc) ->
                     Handler = maps:get(handler, Meta, Module),
                     PathMatch = "/api/v4" ++ ensure_prefix_slash(Resource),
                     [{PathMatch, minirest_handler, Meta#{handler => Handler}} | Acc];
                    (_, Acc) ->
                     Acc
                 end, [], Module:module_info(attributes)),
    paths(More, NPaths ++ Paths).

ensure_prefix_slash("/" ++ _ = Path) -> Path;
ensure_prefix_slash(Path) -> "/" ++ Path.

take(Key, List) ->
	take(Key, List, undefined).

take(Key, List, Default) ->
    take(Key, List, [], Default).

take(_, [], Acc, Default) ->
	{ok, Default, lists:reverse(Acc)};
take(Key, [{Key, Value} | More], Acc, _) ->
	{ok, Value, lists:reverse(Acc, More)};
take(Key, [Key | More], Acc, _) ->
	{ok, true, lists:reverse(Acc, More)};
take(Key, [Item | More], Acc, Default) ->
	take(Key, More, [Item | Acc], Default).
