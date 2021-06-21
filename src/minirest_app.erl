-module(minirest_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    minirest_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
%% application:ensure_all_started(minirest).
% minirest:start(abc, #{modules => [example_api], port => 8088, root_path=>"/v5"}).

