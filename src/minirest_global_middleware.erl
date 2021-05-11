%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------

-module(minirest_global_middleware).
-behaviour(cowboy_middleware).
-export([execute/2]).

%% ----------------------------------------------------
%% API functions
%% ----------------------------------------------------

execute(Req, Env) ->
    io:format("minirest_global_middleware => :~p~n", [Env]),
	{ok, Req, Env}.