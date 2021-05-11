%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------
-module(minirest_access_middleware).
-behaviour(cowboy_middleware).
-export([execute/2]).
-include_lib("include/minirest.hrl").

execute(Req, Env) ->
    try ensure_acceptable(Req, Env)
    catch
        error:Error:Stacktrace ->
            io:format("Error: ~p, Stacktrace: ~p", [Error, Stacktrace]),
            {ok, minirest_req:server_internal_error(Error, Stacktrace, Req), Env}
    end.

ensure_acceptable(Req, _Env) ->
    Accept = cowboy_req:parse_header(<<"accept">>, Req, [{{<<"*">>, <<"*">>, []}, 1000, []}]),
    case lists:any(fun({{<<"*">>, <<"*">>, _}, _, _}) -> true;
                      ({{<<"application">>, <<"*">>, _}, _, _}) -> true;
                      ({{<<"application">>, <<"json">>, _}, _, _}) -> true;
                      (_) -> false
                   end, Accept) of
        true ->
            {ok, Req};
        false ->
            Message = <<"Not acceptable">>,
            {stop, minirest_req:reply(406, #{}, #{message => Message}, Req)}
    end.
