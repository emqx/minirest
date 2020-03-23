-module(minirest_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

-import(minirest, [pipeline/3]).

-import(minirest_req, [ reply/2
                      , reply/3
                      , reply/4
                      , serialize/2
                      , serialize_detail/2
                      , server_internal_error/3]).

execute(Req, Env) ->
    try pipeline([fun process_cors/2,
                  fun ensure_supported_method/2,
                  fun ensure_acceptable/2,
                  fun ensure_content_type/2], Req, Env)
    catch
        error:Error:Stacktrace ->
            io:format("Error: ~p, Stacktrace: ~p", [Error, Stacktrace]),
            {ok, server_internal_error(Error, Stacktrace, Req), Env}
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
    {stop, reply(200, RespHeaders, Req)};
do_process_cors(<<"OPTIONS">>, #{<<"origin">> := _}, Req, _Env) ->
    {stop, reply(200, Req)};
do_process_cors(<<"OPTIONS">>, _, Req, #{handler_opts := #{allowed_methods := Allowed}}) ->
    RespHeaders = #{<<"allow">> => serialize_allowed_methods(Allowed)},
    {stop, reply(204, RespHeaders, Req)};
%% Simple Request
do_process_cors(_, #{<<"origin">> := _Origin}, Req, _Env) ->
    {ok, cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req)};
do_process_cors(_, _, Req, _Env) ->
    {ok, Req}.

ensure_supported_method(Req, #{handler_opts := #{allowed_methods := Allowed}}) ->
    Method = Method = cowboy_req:method(Req),
    case lists:member(Method, Allowed) of
        true ->
            {ok, Req};
        false ->
            Message = serialize("Method '~s' not allowed", [Method]),
            {stop, reply(405, #{}, #{message => Message}, Req)}
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
            {stop, reply(406, #{}, #{message => Message}, Req)}
    end.

ensure_content_type(Req, _Env) ->
    case cowboy_req:has_body(Req) of
        true ->
            case cowboy_req:parse_header(<<"content-type">>, Req) of
                {<<"application">>, <<"json">>, _} ->
                    {ok, Req};
                undefined ->
                    Message = <<"Missing 'Content-Type' header">>,
                    {stop, reply(400, #{}, #{message => Message}, Req)};
                _ ->
                    Message = <<"Unsupported Content-Type">>,
                    {stop, reply(415, #{}, #{message => Message}, Req)}
            end;
        false ->
            {ok, Req}
    end.

serialize_allowed_methods(Methods) when is_list(Methods) ->
    serialize_allowed_methods(Methods, <<>>).

serialize_allowed_methods([], <<>>) ->
    <<"OPTIONS">>;
serialize_allowed_methods([], Acc) ->
    <<Acc/binary, ", OPTIONS">>;
serialize_allowed_methods([Method | More], <<>>) ->
    serialize_allowed_methods(More, Method);
serialize_allowed_methods([Method | More], Acc) ->
    serialize_allowed_methods(More, <<Acc/binary, ", ", Method/binary>>).