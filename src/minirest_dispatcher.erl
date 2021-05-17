%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------
-module(minirest_dispatcher).
-export([init/2]).

%-------------------------------------------------------------------------------------
% Cowboy callback
%-------------------------------------------------------------------------------------

init(Request, Envs) ->
    % ct:print("[DEBUG] minirest_dispatcher init request:~p~nInitState=> :~p~n", [Request, Envs]),
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
    {ok, Routes} = minirest:find_all_routes(GenServerName),
    Matched = lists:foldl(fun(M, Acc) ->
        case minirest_utils:match_route(M, Method ++ ":" ++ Path) of
            true -> {true, M};
            false -> Acc
        end
    end, false, Routes),
    case Matched of
        {true, M} ->
            {ok, ApiSpec} = minirest:find_api_spec(GenServerName, M),
            {ok, ParamsKey} = minirest_utils:fetch_params(M, Method ++ ":" ++ Path),
            ParamsValue = lists:foldl(fun(V, Acc) ->
                case string:prefix(V, "$") of
                    nomatch -> Acc;
                    KeyName -> Acc ++ [KeyName]
                end
            end, [], string:tokens(maps:get(path, ApiSpec), "/")),
            Bindings = minirest_utils:combine_bindings(ParamsKey, ParamsValue),
            {ok, Request, Envs#{api_spec => ApiSpec, bindings => Bindings}};
        false -> minirest_error:not_found(Request, Path)
    end.

make_light_weight_request(Request, #{bindings := Bindings} = Envs) ->
    {ok, BinBody, NextRequest} = read_body(Request),
    try jiffy:decode(BinBody, [return_maps]) of
        Body ->
            LightWeightRequest = #{bindings => Bindings,
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
                                func => Func,
                                light_weight_req => LightWeightRequest}}
    catch
        _:Error:Stacktrace ->
        logger:error("Error: ~p, Stacktrace: ~p", [Error, Stacktrace]),
        minirest_error:invalid_params(Request, Error)
    end.

next_handler(Request, #{handler := Handler,
                         func := Func,
                         light_weight_req := LightWeightRequest}) ->
    ct:print("next_handler =====> :~p~n", [Request]),
    try
        Result = erlang:apply(Handler, Func, [LightWeightRequest]),
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
