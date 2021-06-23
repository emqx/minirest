-module(minirest_handler).

-export([init/2]).

-export([init_state/3]).

-include("minirest_http.hrl").

-record(callback, {
    module :: atom(),
    function :: atom(),
    filter :: fun()
}).

%%==============================================================================================
%% cowboy callback init
init(Request0, State)->
    {Response, NewState} = handle(Request0, State),
    Request = reply(Response, Request0),
    {ok, Request, NewState}.

%%%==============================================================================================
%% internal
handle(Request, State = #{api_state := APIState}) ->
    Method = cowboy_req:method(Request),
    case maps:get(Method, State, undefined) of
        undefined ->
            {{?METHOD_NOT_ALLOWED}, State};
        Callback ->
           {Response, NextAPIState} =  apply_callback(Request, Callback, APIState),
           {Response, State#{api_state => NextAPIState}}
    end.

apply_callback(Request,
    Callback = #callback{filter = Filter, module = Mod, function = Fun}, APIState) ->
    case Filter(Request) of
        {ok, Parameters} ->
            try 
                Result = erlang:apply(Mod, Fun, [Parameters, APIState]),
                get_callback_response(Result)
            catch E:R:S ->
                Message = list_to_binary(io_lib:format("~p, ~0p, ~0p", [E, R, S], [])),
                Body = #{code => <<"INTERNAL_ERROR">>, message => Message},
                {{?INTERNAL_SERVER_ERROR, Body}, APIState}
            end;
        Response ->
            {Response, Callback}
    end.

get_callback_response({StatusCode, Headers, Body, APIState}) ->
    {{StatusCode, Headers, Body}, APIState};
get_callback_response({StatusCode, Body, APIState}) ->
    {{StatusCode, Body}, APIState};
get_callback_response({StatusCode, APIState}) ->
    {{StatusCode}, APIState}.

reply({StatusCode0}, Req) ->
    StatusCode = status_code(StatusCode0),
    cowboy_req:reply(StatusCode, Req);

reply({StatusCode0, Body0}, Req) ->
    StatusCode = status_code(StatusCode0),
    Body = to_json(Body0),
    cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, Body, Req);

reply({StatusCode0, Headers, Body0}, Req) ->
    StatusCode = status_code(StatusCode0),
    Body = to_json(Body0),
    cowboy_req:reply(StatusCode, Headers, Body, Req).

to_json(Data) when is_binary(Data) ->
    Data;
to_json(Data) when is_map(Data) ->
    jsx:encode(Data).

%%==============================================================================================
%% start handler
init_state(Module, Metadata, State) ->
    Fun =
        fun(Method0, Options, HandlerState) ->
            Method = trans_method(Method0),
            Function = maps:get(operationId, Options),
            Filter = trans_filter(Method, Options),
            Callback = #callback{
                module = Module,
                function = Function,
                filter = Filter},
            maps:put(Method, Callback, HandlerState)
        end,
    maps:put(api_state, State, maps:fold(Fun, #{}, Metadata)).

status_code(ok) -> <<"200">>;
status_code(Data) when is_integer(Data) -> integer_to_binary(Data);
status_code(Data) when is_list(Data) -> list_to_binary(Data);
status_code(Data) when is_binary(Data) -> Data.

trans_method(get)     -> <<"GET">>;
trans_method(post)    -> <<"POST">>;
trans_method(put)     -> <<"PUT">>;
trans_method(head)    -> <<"HEAD">>;
trans_method(delete)  -> <<"DELETE">>;
trans_method(patch)   -> <<"PATCH">>;
trans_method(options) -> <<"OPTION">>;
trans_method(connect) -> <<"CONNECT">>;
trans_method(trace)   -> <<"TRACE">>.

trans_filter(<<"GET">>, _Options) ->
    fun(Request) -> {ok, Request} end;
trans_filter(<<"POST">>, _Options) ->
    fun(Request) -> {ok, Request} end;
trans_filter(<<"PUT">>, _Options) ->
    fun(Request) -> {ok, Request} end;
trans_filter(<<"DELETE">>, _Options) ->
    fun(Request) -> {ok, Request} end;
trans_filter(_, _Options) ->
    fun(Request) -> {ok, Request} end.
