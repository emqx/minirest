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

-export([start_http/4, handler/1, stop_http/2]).

%% MFArgs Callback
-export([handle_request/2]).

-type(option() :: {authorization, fun()}).

-type(handler() :: {string(), mfa()} | {string(), mfa(), list(option())}).

-export_type([option/0, handler/0]).

-spec(start_http(atom(), esockd:listen_on(), list(esockd:option()), list(handler())) -> {ok, pid()}).
start_http(ServerName, ListenOn, Options, Handlers) ->
    MFArgs = {?MODULE, handle_request, [[map(Handler) || Handler <- Handlers]]},
    mochiweb:start_http(ServerName, ListenOn, Options, MFArgs).

map({Prefix, MFArgs}) ->
    map({Prefix, MFArgs, []});
map({Prefix, MFArgs, Options}) ->
    #{prefix => Prefix, mfargs => MFArgs, options => maps:from_list(Options)}.

-spec(handler(minirest_handler:config()) -> handler()).
handler(Config) -> minirest_handler:init(Config).

-spec(stop_http(atom(), esockd:listen_on()) -> ok).
stop_http(ServerName, ListenOn) ->
    mochiweb:stop_http(ServerName, ListenOn).

%% Callback
handle_request(Req, Handlers) ->
    case match_handler(Req:get(path), Handlers) of
        {ok, Path, Handler} ->
            try apply_handler(Req, Path, Handler)
            catch _:Error -> internal_error(Req, Error)
            end;
        not_found ->
            Req:not_found()
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
        false -> Headers = [{"WWW-Authenticate", "Basic Realm=\"minirest-server\""}],
                 Req:respond({401, Headers, <<"UNAUTHORIZED">>})
    end;

apply_handler(Req, Path, #{mfargs := MFArgs}) ->
    apply_handler(Req, Path, MFArgs);

apply_handler(Req, Path, {M, F, Args}) ->
    erlang:apply(M, F, [Path, Req | Args]).

internal_error(Req, Error) ->
    error_logger:error_msg("~s ~s error: ~p", [Req:get(method), Req:get(path), Error]),
    error_logger:error_msg("~p", [erlang:get_stacktrace()]),
    Req:respond({500, [{"Content-Type", "text/plain"}], <<"Internal Error">>}).

