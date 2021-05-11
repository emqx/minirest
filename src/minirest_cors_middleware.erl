%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------
-module(minirest_cors_middleware).
-behaviour(cowboy_middleware).
-export([execute/2]).
-include_lib("include/minirest.hrl").
%%-------------------------------------------------------------
%% Callback
%%-------------------------------------------------------------
execute(Req, Env) ->
    ct:print("minirest_cors_middleware => :~p~n", [Env]),
    try process_cors(Req, Env)
    catch
        error:Error:Stacktrace ->
            io:format("Error: ~p, Stacktrace: ~p", [Error, Stacktrace]),
            {ok, minirest_req:server_internal_error(Error, Stacktrace, Req), Env}
    end.

process_cors(Req, Env) ->
    Method = cowboy_req:method(Req),
    Headers = cowboy_req:headers(Req),
    do_process_cors(Method, Headers, Req, Env).

%% Preflight Request or Normal OPTIONS Request
do_process_cors(<<"OPTIONS">>, #{<<"origin">> := _Origin,
                                 <<"access-control-request-method">> := _Method}, Req, _Env) ->
    RespHeaders = #{<<"access-control-allow-origin">> => <<"*">>,
                    <<"access-control-allow-methods">> => <<"*">>,
                    <<"access-control-allow-headers">> => <<"*">>,
                    <<"access-control-max-age">> => <<"86400">>},
    {stop, minirest_req:reply(200, RespHeaders, Req)};
do_process_cors(<<"OPTIONS">>, #{<<"origin">> := _}, Req, _Env) ->
    {stop, minirest_req:reply(200, Req)};
do_process_cors(<<"OPTIONS">>, _, Req, #{handler_opts := #{allowed_methods := Allowed}}) ->
    RespHeaders = #{<<"allow">> => minirest_req:serialize_allowed_methods(Allowed)},
    {stop, minirest_req:reply(204, RespHeaders, Req)};
%% Simple Request
do_process_cors(_, #{<<"origin">> := _Origin}, Req, _Env) ->
    {ok, cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req)};
do_process_cors(_, _, Req, _Env) ->
    {ok, Req}.
