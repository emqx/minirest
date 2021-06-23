%%%-------------------------------------------------------------------
%% @doc example_server public API
%% @end
%%%-------------------------------------------------------------------

-module(example_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:ensure_all_started(minirest),
    Modules = [ example_hello_api
                , example_echo_api
                , example_pets_api],
    Options = #{port => 8088, root_path => "/v1", modules => Modules},
    minirest:start(?MODULE, Options),
    example_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
