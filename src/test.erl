-module(test).

-export([start/0]).

start() ->
    application:ensure_all_started(minirest),
    Ranch = #{socket_opts => [{port,8088}]},
    Dispatch = [{"/[...]", minirest, http_handlers()}],
    minirest:start_http(test_server, Ranch, Dispatch).

http_handlers() ->
    [{"/", minirest:handler(#{apps => [minirest]}), []}].
