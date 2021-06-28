%% Copyright (c) 2013-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(minirest_handler).

-export([init/2]).

-export([init_state/3]).

-include("minirest_http.hrl").

-record(callback, {
    module :: atom(),
    function :: atom(),
    filter :: fun(),
    global_filter :: fun()
}).

%%==============================================================================================
%% cowboy callback init
init(Request0, State)->
    Response = handle(Request0, State),
    Request = reply(Response, Request0),
    {ok, Request, State}.

%%%==============================================================================================
%% internal
handle(Request, State) ->
    Method = cowboy_req:method(Request),
    case maps:get(Method, State, undefined) of
        undefined ->
            {?RESPONSE_CODE_METHOD_NOT_ALLOWED};
        Callback ->
            do_filter(Request, Callback)
    end.

do_filter(Request, Callback = #callback{global_filter = GlobalFilter}) when is_function(GlobalFilter) ->
    case GlobalFilter(Request) of
        ok ->
            do_filter(Request, Callback#callback{global_filter = pass});
        Response ->
            Response
    end;

do_filter(Request, Callback = #callback{filter = Filter}) ->
    case Filter(Request) of
        {ok, Parameters} ->
            apply_callback(Parameters, Callback);
        Response ->
            Response
    end.

apply_callback(Parameters, #callback{module = Mod, function = Fun}) ->
    try
        erlang:apply(Mod, Fun, [Parameters])
    catch E:R:S ->
        Message = list_to_binary(io_lib:format("~p, ~0p, ~0p", [E, R, S], [])),
        Body = #{code => <<"INTERNAL_ERROR">>, message => Message},
        {?RESPONSE_CODE_INTERNAL_SERVER_ERROR, Body}
    end.

reply(ok, Req) ->
    StatusCode  = <<"200">>,
    cowboy_req:reply(StatusCode, Req);

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
init_state(Module, Metadata, GlobalFilter) ->
    Fun =
        fun(Method0, Options, HandlerState) ->
            Method = trans_method(Method0),
            Function = maps:get(operationId, Options),
            Filter = trans_filter(Method, Options),
            Callback = #callback{
                module = Module,
                function = Function,
                global_filter = GlobalFilter,
                filter = Filter},
            maps:put(Method, Callback, HandlerState)
        end,
    maps:fold(Fun, #{}, Metadata).

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

%% TODO: filter by metadata
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
