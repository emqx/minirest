%%%-------------------------------------------------------------------
%% @doc my_server public API
%% @end
%%%-------------------------------------------------------------------

-module(my_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ServerName = example_server,
    App = my_server, %% or your app name
    {ok, _} = application:ensure_all_started(minirest),
    Options = #{
        port => 8088,
        apps => [App]
    },
    minirest:start(ServerName, Options),
    my_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
