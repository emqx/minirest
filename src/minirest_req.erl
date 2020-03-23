-module(minirest_req).

-export([ reply/2
        , reply/3
        , reply/4
        , serialize/2
        , serialize_detail/2
        , server_internal_error/3]).

reply(Status, Req) ->
    reply(Status, #{}, #{}, Req).

reply(Status, Headers, Req) ->
    reply(Status, Headers, #{}, Req).

reply(Status, Headers, Body, Req) when map_size(Body) =:= 0 ->
    cowboy_req:reply(Status, Headers, <<>>, Req);
reply(Status, Headers, Body, Req) ->
    try jiffy:encode(Body) of
        BinBody ->
            cowboy_req:reply(Status, Headers#{<<"content-type">> => <<"application/json">>}, BinBody, Req)
    catch
        error:Error:Stacktrace ->
            server_internal_error(Error, Stacktrace, Req)
    end.

serialize(Format, Data) ->
    erlang:list_to_binary(io_lib:format(Format, Data)).

serialize_detail(Error, Stacktrace) ->
    serialize("~0p, ~0p", [Error, Stacktrace]).

server_internal_error(Error, Stacktrace, Req) ->
    reply(500, #{}, #{message => <<"Server internal error">>,
                      detail => serialize_detail(Error, Stacktrace)}, Req).