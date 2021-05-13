%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------
-module(minirest_dispatcher).
-export([init/2]).

%-------------------------------------------------------------------------------------
% Cowboy callback
%-------------------------------------------------------------------------------------

init(Request, Envs) ->
    ct:print("[DEBUG] minirest_dispatcher init request:~p~nInitState=> :~p~n", [Request, Envs]),
    try minirest:pipeline([fun path_filter/2,
                           fun make_light_weight_request/2,
                           fun validate_params/2,
                           fun next_handler/2], Request, Envs)
    catch
        _:Error:Stacktrace ->
            logger:error("Error: ~p, Stacktrace: ~p", [Error, Stacktrace]),
            {stop, minirest_req:server_internal_error(Error, Stacktrace, Request), Envs}
    end.
%-------------------------------------------------------------------------------------
% APIs
%-------------------------------------------------------------------------------------

%% filter
path_filter(Request, Envs) ->
    GenServerName = maps:get(server_name, Envs),
    Method = binary_to_list(maps:get(method, Request)),
    Path = binary_to_list(maps:get(path, Request)),
    case minirest:find_api_spec(GenServerName, Method, Path) of
        {ok, ApiSpec} ->
            {ok, Request, Envs#{api_spec => ApiSpec}};
        {not_fond, ErrPath} ->
            minirest_error:not_found(Request, ErrPath)
    end.

make_light_weight_request(Request, Envs) ->
    [M, P, V, NP | Bindings] = string:tokens(maps:get(path, Request), "/"),
    String = M ++ "/" ++ P ++ "/" ++ V ++ "/" ++ NP,
    SpecMapKey = lists:foldl(fun(_Bind, AccS) ->
        AccS ++ "/*"
    end, String, Bindings),
    {ok, BinBody, NextRequest} = read_body(Request),
    try jiffy:decode(BinBody, [return_maps]) of
        Body ->
            Bindings = cowboy_req:bindings(NextRequest),
            NBindings = maps:fold(fun(K, V, Acc) ->
                                      maps:put(atom_to_binary(K, utf8), V, Acc)
                                  end, #{}, Bindings),
            LightWeightRequest = #{bindings => minirest_utils:http_uri_decode(NBindings),
                                   qs       => minirest_utils:http_uri_decode(maps:from_list(cowboy_req:parse_qs(NextRequest))),
                                   headers  => cowboy_req:headers(NextRequest),
                                   body     => Body},
            {ok, NextRequest, Envs#{light_weight_req => LightWeightRequest}}
    catch
        _:Error:Stacktrace ->
            logger:error("Error: ~p, Stacktrace: ~p", [Error, Stacktrace]),
            minirest_error:invalid_json(Request)
    end.

%% validate request params
validate_params(Request, #{light_weight_req := LightWeightRequest,
                           api_spec := ApiSpec} = Envs) ->
    Parameters =  maps:get(parameters, ApiSpec),
    try
       minirest_validator:validate(Parameters, LightWeightRequest),
       Handler =  maps:get(handler, ApiSpec),
       Func =  maps:get(func, ApiSpec),
       {ok, Request, Envs#{handler => Handler,
                                func => Func}}
    catch
        _:Error:Stacktrace ->
        logger:error("Error: ~p, Stacktrace: ~p", [Error, Stacktrace]),
        minirest_error:invalid_params(Request, Error)
    end.

next_handler(Request, Envs) ->
    try
        Handler = maps:get(handler, Envs),
        Func = maps:get(func, Envs),
        Result = erlang:apply(Handler, Func, [Request]),
        {ok, minirest_req:reply(200, #{},
            #{data => minirest_utils:normalize_return_format(Result)}, Request),
        #{}}
    catch
       _:Error:Stacktrace ->
            logger:error("Error: ~p, Stacktrace: ~p", [Error, Stacktrace]),
            minirest_error:bad_request(Request, <<"Invalid request!">>)
    end.

read_body(Req) ->
    case read_body(Req, <<>>) of
        {ok, <<>>, NextRequest} -> {ok, <<"{}">>, NextRequest};
        {ok, Body, NextRequest} -> {ok, Body, NextRequest}
    end.

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, NextRequest} -> {ok, <<Acc/binary, Data/binary>>, NextRequest};
        {more, Data, NextRequest} -> read_body(NextRequest, <<Acc/binary, Data/binary>>)
    end.
