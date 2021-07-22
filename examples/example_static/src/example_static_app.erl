%%%-------------------------------------------------------------------
%% @doc example_static public API
%% @end
%%%-------------------------------------------------------------------

-module(example_static_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(APP, example_static).

start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(minirest),
    Dispatch = [
        {"/", cowboy_static, {priv_file, ?APP, "www/index.html"}},
        {"/static/[...]", cowboy_static, {priv_dir, ?APP, "www/static"}}
    ],
    Minirest = #{port => 8088, dispatch => Dispatch},
    {ok, _} = minirest:start(?APP, Minirest),
    example_static_sup:start_link().

stop(_State) ->
    ok.
