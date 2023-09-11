%% Copyright (c) 2013-2022 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-include("minirest_http.hrl").

-include("minirest.hrl").

%%==============================================================================================
%% cowboy callback init
init(Request0, State)->
    ReqStart = erlang:monotonic_time(),
    {Code, Request1, Meta} = handle(Request0, State),
    ReqEnd = erlang:monotonic_time(),
    run_log_hook(Meta, ReqStart, ReqEnd, Code),
    {ok, Request1, State}.

%%%==============================================================================================
%% internal
handle(Request, State) ->
    Method = cowboy_req:method(Request),
    case maps:find(Method, State) of
        error ->
            StatusCode = ?RESPONSE_CODE_METHOD_NOT_ALLOWED,
            Headers = allow_method_header(maps:keys(State)),
            Meta = Headers#{method => binary_to_existing_atom(Method)},
            {StatusCode, cowboy_req:reply(StatusCode, Headers, <<"">>, Request), Meta};
        {ok, Handler = #handler{path = Path, log = Log, method = MethodAtom}} ->
            InitMeta = #{operate_id => Path, log => Log, method => MethodAtom},
            case do_authorize(Request, Handler) of
                {ok, AuthMeta} ->
                    Meta = maps:merge(InitMeta, AuthMeta),
                    case do_parse_params(Request) of
                        {ok, Params, NRequest} ->
                            case do_validate_params(Params, Handler) of
                                {ok, NParams} ->
                                    Response = apply_callback(NRequest, NParams, Handler),
                                    {StatusCode, NRequest1} = reply(Response, NRequest, Handler),
                                    {
                                        StatusCode,
                                        NRequest1,
                                        maps:merge(NParams, Meta)
                                    };
                                FilterErr ->
                                    {StatusCode, NRequest} = reply(FilterErr, Request, Handler),
                                    {StatusCode, NRequest, maps:merge(Params, Meta)}
                            end;
                        ParseErr ->
                            {StatusCode, NRequest} = reply(ParseErr, Request, Handler),
                            {StatusCode, NRequest, Meta}
                    end;
                AuthFailed ->
                    {StatusCode, NRequest} = reply(AuthFailed, Request, Handler),
                    {StatusCode, NRequest, InitMeta}
            end
    end.

allow_method_header(Allow) ->
    #{<<"allow">> => trans_allow(Allow, <<"">>)}.

trans_allow([], Res) -> Res;
trans_allow([Method], Res) -> <<Res/binary, Method/binary>>;
trans_allow([Method | Allow], Res) ->
    trans_allow(Allow, <<Res/binary, Method/binary, ", ">>).

do_authorize(Request, #handler{authorization = {M, F}}) ->
    erlang:apply(M, F, [Request]);
do_authorize(_Request, _Handler) ->
    {ok, #{}}.

do_parse_params(Request) ->
    Params = #{
        bindings => cowboy_req:bindings(Request),
        query_string => maps:from_list(cowboy_req:parse_qs(Request)),
        headers => cowboy_req:headers(Request),
        body => #{}
    },
    do_read_body(Request, Params).

do_read_body(Request, Params) ->
    case cowboy_req:has_body(Request) of
        true ->
            case minirest_body:parse(Request) of
                {ok, Body, NRequest} ->
                    {ok, Params#{body => Body}, NRequest};
                {response, Response} ->
                    Response
            end;
        false ->
            {ok, Params, Request}
    end.

do_validate_params(Params, #handler{filter = Filter,
                                    path   = Path,
                                    module = Mod,
                                    method = Method}) when is_function(Filter, 2) ->
    Filter(Params, #{path => Path, module => Mod, method => Method});
do_validate_params(Params, _Handler) ->
    {ok, Params}.

apply_callback(Request, Params, Handler) ->
    #handler{path = Path, method = Method, module = Mod, function = Fun} = Handler,
    try
        Args =
            case erlang:function_exported(Mod, Fun, 3) of
                true -> [Method, Params, Request];
                false -> [Method, Params]
            end,
        erlang:apply(Mod, Fun, Args)
    catch E:R:S ->
        ?LOG(warning, #{path => Path,
                        exception => E,
                        reason => R,
                        stacktrace => S}),
        Message = list_to_binary(io_lib:format("~p, ~0p, ~0p", [E, R, S], [])),
        {?RESPONSE_CODE_INTERNAL_SERVER_ERROR, 'INTERNAL_ERROR', Message}
    end.

%% response error
reply({ErrorStatus, #{code := Code, message := Message}}, Req, Handler)
  when (ErrorStatus < 200 orelse 300 =< ErrorStatus)
  andalso is_atom(Code) ->
    reply({ErrorStatus, Code, Message}, Req, Handler);
reply({ErrorStatus, Code, Message}, Req, Handler = #handler{error_codes = Codes})
  when (ErrorStatus < 200 orelse 300 =< ErrorStatus)
  andalso is_atom(Code) ->
    case maybe_ignore_code_check(ErrorStatus, Code) orelse lists:member(Code, Codes) of
        true ->
            ErrorMessageStruct = {message, #{code => Code, message => Message}},
            {ok, Headers, Body} = minirest_body:encode(ErrorMessageStruct),
            reply({ErrorStatus, Headers, Body}, Req, Handler);
        false ->
            NewMessage =
                list_to_binary(
                    io_lib:format(
                        "not support code ~p, message ~p, schema def ~p", [Code, Message, Codes])),
            reply({500, NewMessage}, Req, Handler)
    end;

%% response simple
reply(StatusCode, Req, _Handler) when is_integer(StatusCode) ->
    {StatusCode, cowboy_req:reply(StatusCode, Req)};

reply({StatusCode}, Req, _Handler) ->
    {StatusCode, cowboy_req:reply(StatusCode, Req)};

reply({StatusCode, Body0}, Req, Handler) ->
    case minirest_body:encode(Body0) of
        {ok, Headers, Body} ->
            {StatusCode, reply_with_body(StatusCode, Headers, Body, Req)};
        {response, Response} ->
            reply(Response, Req, Handler)
    end;

reply({StatusCode, Headers, Body0}, Req, Handler) ->
    case minirest_body:encode(Body0) of
        {ok, Headers1, Body} ->
            {StatusCode, reply_with_body(StatusCode, maps:merge(Headers1, Headers), Body, Req)};
        {response, Response} ->
            reply(Response, Req, Handler)
    end;

reply(BadReturn, Req, _Handler) ->
    StatusCode = ?RESPONSE_CODE_INTERNAL_SERVER_ERROR,
    Body = io_lib:format("mini rest bad return ~p", [BadReturn]),
    {StatusCode, cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"text/plain">>}, Body, Req)}.

reply_with_body(StatusCode, Headers, Body, Req) when is_binary(Body) ->
        cowboy_req:reply(StatusCode, Headers, Body, Req);
reply_with_body(StatusCode, Headers, {qlc_handle, _} = BodyQH, Req0) ->
    Req1 = cowboy_req:stream_reply(StatusCode, Headers, Req0),
    qlc:fold(
      fun(Data, Req) ->
              cowboy_req:stream_body(Data, nofin, Req),
              Req
      end,
      Req1,
      BodyQH),
    cowboy_req:stream_body(<<>>, fin, Req1),
    Req1.

maybe_ignore_code_check(401, _Code) -> true;
maybe_ignore_code_check(400, 'BAD_REQUEST') -> true;
maybe_ignore_code_check(500, 'INTERNAL_ERROR') -> true;
maybe_ignore_code_check(_, _) -> false.

run_log_hook(#{log := Log} = Meta0, ReqStart, ReqEnd, Code) when is_function(Log) ->
    Meta = maps:without([log], Meta0),
    _ = Log(Meta#{req_start => ReqStart, req_end => ReqEnd, code => Code}),
    ok;
run_log_hook(_Meta, _ReqStart, _ReqEnd, _Code) ->
    ok.
