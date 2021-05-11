%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------
-module(minirest_dispatcher).
-export([init/2]).

-import(minirest_req, [ reply/4
                      , serialize_detail/2
                      , server_internal_error/3]).
%-------------------------------------------------------------------------------------
% Cowboy callback
%-------------------------------------------------------------------------------------

allowed_methods() ->
    [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"HEAD">>, <<"OPTIONS">>].

init(Request, InitState) ->
    try pipeline([fun filter_method/2,
                  fun validate_params/2,
                  fun make_light_weight_request/2,
                  fun next_handler/2], Request, InitState)
    catch
        error:Error:Stacktrace ->
            logger:error("Error: ~p, Stacktrace: ~p", [Error, Stacktrace]),
            {ok, server_internal_error(Error, Stacktrace, Request), InitState}
    end.
%-------------------------------------------------------------------------------------
% Private
%-------------------------------------------------------------------------------------
pipeline([], Request, InitState) ->
    {ok, Request, InitState};

pipeline([Fun | More], Request, InitState) ->

    case Fun(Request, InitState) of
        {ok, NextRequest} ->
            pipeline(More, NextRequest, InitState);
        {ok, NextRequest, NextState} ->
            pipeline(More, NextRequest, NextState);
        {stop, NextRequest} ->
            {ok, NextRequest, InitState};
        {stop, NextRequest, NextState} ->
            {ok, NextRequest, NextState}
    end.

filter_method(Request, InitState) ->
    GenServerName = maps:get(server_name, InitState),
    Path = binary_to_list(maps:get(path, Request)),
    RequestMethod = lowercase_method(Request),
    case minirest:find_api_spec(GenServerName, Path) of
        {ok, #{method := ApiMethod} = ApiSpec} ->
            case match_method_equal(RequestMethod, ApiMethod) of
                true ->
                    {ok, Request, InitState#{api_spec => ApiSpec}};
                false ->
                    minirest_error:method_not_support(Request, RequestMethod, Path)
            end;
        not_fond -> minirest_error:not_found(Request, Path)
    end.

%% validate request params
validate_params(Request, InitState) ->
    ApiSpec = maps:get(api_spec, InitState),
    Parameters =  maps:get(parameters, ApiSpec),
    %% minirest_validator:validate(Parameters) -> ok|throw;
    ct:print("Begin validate parameters=> :~p~n", [Parameters]),
    %% TODO For next station
    Handler =  maps:get(handler, ApiSpec),
    Func =  maps:get(func, ApiSpec),
    {ok, Request, #{handler => Handler,
                    func => Func}}.

make_light_weight_request(Request, InitState) ->
    {ok, BinBody, NextRequest} = read_body(Request),
    try jiffy:decode(BinBody, [return_maps]) of
        Body ->
            Bindings = cowboy_req:bindings(NextRequest),
            NBindings = maps:fold(fun(K, V, Acc) ->
                                      maps:put(atom_to_binary(K, utf8), V, Acc)
                                  end, #{}, Bindings),
            LightWeightRequest = #{bindings => http_uri_decode(NBindings),
                                   qs       => http_uri_decode(maps:from_list(cowboy_req:parse_qs(NextRequest))),
                                   headers  => cowboy_req:headers(NextRequest),
                                   body     => Body},
            {ok, NextRequest, #{light_weight_req => LightWeightRequest,
                                action => InitState}}
    catch
        error:Error:Stacktrace ->
            {stop, reply(400, #{}, #{message => <<"The request body isn't a valid json">>,
                                     detail => serialize_detail(Error, Stacktrace)}, NextRequest)}
    end.

next_handler(Request, #{action := #{handler := Handler, func := F}}) ->
    try
        %% TODO Maybe redesign return format
        Result = erlang:apply(Handler, F, [Request]),
        {ok, minirest_req:reply(200, #{}, #{data => normalize_return_format(Result)}, Request),
        #{}}
    catch
       _ : Reason : Stacktrace ->
        {stop, reply(400, #{}, #{message => <<"Invalid request!">>,
                                 detail => serialize_detail(Reason, Stacktrace)}, Request)}
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

lowercase_method(Req) ->
    list_to_atom(string:to_lower(binary_to_list(cowboy_req:method(Req)))).

http_uri_decode(Params) when is_map(Params) ->
    maps:map(fun(_, V) ->
                 uri_string:normalize(V)
             end, Params).

match_method_equal(A, B) when is_atom(A) andalso
                        is_atom(B) ->
    string:to_lower(atom_to_list(A)) == string:to_lower(atom_to_list(B)).
%%
normalize_return_format(T) when is_list(T) -> list_to_binary(T);
normalize_return_format(T) when is_map(T) -> T;
normalize_return_format(T) when is_binary(T) -> T;
normalize_return_format(_) ->
    throw({throw, "Return value type must one of:[list, map, binary]"}).