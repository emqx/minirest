%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(minirest).

-author("Feng Lee <feng@emqtt.io>").

-export([start_http/3, start_https/3, handler/1, stop_http/1, map/1]).

%% Cowboy Callback
-export([init/2]).

-type(option() :: {authorization, fun()}).

-type(handler() :: {string(), mfa()} | {string(), mfa(), list(option())}).

-export_type([option/0, handler/0]).

-spec(start_http(atom(), list(), list()) -> {ok, pid()}).
start_http(ServerName, Options, Handlers) ->
    Dispatch = cowboy_router:compile([{'_', Handlers}]),
    {ok, _} = cowboy:start_clear(ServerName, Options, #{env => #{dispatch => Dispatch}}).

-spec(start_https(atom(), list(), list()) -> {ok, pid()}).
start_https(ServerName, Options, Handlers) ->
    Dispatch = cowboy_router:compile([{'_', Handlers}]),
    {ok, _} = cowboy:start_tls(ServerName, Options, #{env => #{dispatch => Dispatch}}).

init(Req, Opts) ->
    Req1 = handle_request(Req, Opts),
    {ok, Req1, Opts}.

map({Prefix, MFArgs}) ->
    map({Prefix, MFArgs, []});
map({Prefix, MFArgs, Options}) ->
    #{prefix => Prefix, mfargs => MFArgs, options => maps:from_list(Options)}.

-spec(handler(minirest_handler:config()) -> handler()).
handler(Config) -> minirest_handler:init(Config).

-spec(stop_http(atom()) -> ok).
stop_http(ServerName) ->
    cowboy:stop_listener(ServerName).

%% Callback
handle_request(Req, Handlers) ->
    Path0 = binary_to_list(cowboy_req:path(Req)),
    case match_handler(Path0, Handlers) of
        {ok, Path, Handler} ->
            try apply_handler(Req, Path, Handler)
            catch _:Error -> internal_error(Req, Error)
            end;
        not_found ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Not found.">>, Req)
    end.

match_handler(_Path, []) ->
    not_found;
match_handler(Path, [Handler = #{prefix := Prefix} | Handlers]) ->
    case string:prefix(Path, Prefix) of
        nomatch -> match_handler(Path, Handlers);
        RelPath -> {ok, add_slash(RelPath), Handler}
    end.

add_slash("/" ++ _ = Path) -> Path;
add_slash(Path) -> "/" ++ Path.

apply_handler(Req, Path, #{mfargs := MFArgs, options := #{authorization := AuthFun}}) ->
    case AuthFun(Req) of
        true  -> apply_handler(Req, Path, MFArgs);
        false ->
            cowboy_req:reply(400, #{<<"WWW-Authenticate">> => <<"Basic Realm=\"minirest-server\"">>},
                             <<"UNAUTHORIZED">>, Req)
    end;

apply_handler(Req, Path, #{mfargs := MFArgs}) ->
    apply_handler(Req, Path, MFArgs);

apply_handler(Req, Path, {M, F, Args}) ->
    erlang:apply(M, F, [Path, Req | Args]).

internal_error(Req, Error) ->
    error_logger:error_msg("~s ~s error: ~p", [cowboy_req:method(Req), cowboy_req:path(Req), Error]),
    error_logger:error_msg("~p", [erlang:get_stacktrace()]),
    cowboy_req:reply(500, #{<<"content-type">> => <<"text/plain">>}, <<"Internal Error">>, Req).

