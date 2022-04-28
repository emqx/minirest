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

-module(minirest_body).

-include("minirest_http.hrl").
-include_lib("kernel/include/file.hrl").

-export([ parse/1
        , parse/2
        ]).

-export([ encode/1
        , encode/2
        ]).
%%==============================================================================================
%% parse
-spec(parse(Request :: map()) ->
            {ok, Body :: map() | binary(), NRequest :: map()} |
            {response, {?RESPONSE_CODE_BAD_REQUEST, ErrorMessage :: map()}}).
parse(Request) ->
    parse(Request, decoder(Request)).

parse(Request, Decoder) ->
    Decoder(Request).
%%==============================================================================================
%% internal
encode(Body) ->
    encode(encoder(Body), Body).

encode(Encoder, Body) ->
    Encoder(Body).

%%==============================================================================================
%% internal
%% decoder
decoder(Request) ->
    case cowboy_req:parse_header(<<"content-type">>, Request, undefined) of
        {<<"application">>, <<"json">>, _} ->
            fun json_decoder/1;
        {<<"multipart">>, <<"form-data">>, _} ->
            fun forma_data_decoder/1;
        _T ->
            fun binary_decoder/1
    end.

json_decoder(Request) ->
    {ok, Body, NRequest} = cowboy_req:read_body(Request),
    try
        {ok, jsx:decode(Body, [return_maps]), NRequest}
    catch _:_:_ ->
        Error = #{code => <<"BAD_REQUEST">>, message => <<"Invalid json message received">>},
        {response, {?RESPONSE_CODE_BAD_REQUEST, Error}}
    end.

forma_data_decoder(Request) ->
    forma_data_decoder(Request, #{}).

forma_data_decoder(Request, Body) ->
    case loop_form(Request) of
        {done, NRequest} ->
            {ok, Body, NRequest};
        {Part, NRequest} ->
            forma_data_decoder(NRequest, maps:merge(Body, Part))
    end.

loop_form(Request) ->
    case cowboy_req:read_part(Request) of
        {ok, PartHeader, Request1} ->
            {ok, Data, Request2} = read_body(Request1, <<>>),
            case cow_multipart:form_data(PartHeader) of
                {data, FieldName} ->
                    {#{FieldName => Data}, Request2};
                {file, FieldName, FileName, Type} ->
                    {#{FieldName => #{
                        type => Type,
                        FileName => Data}}, Request2}
            end;
        {done, Request1} ->
            {done, Request1}
    end.

read_body(Request, Acc) ->
    case cowboy_req:read_part_body(Request) of
        {ok, Bin, Request1} ->
            {ok, <<Acc/binary, Bin/binary>>, Request1};
        {more, Bin, Request2} ->
            read_body(Request2, <<Acc/binary, Bin/binary>>)
    end.

binary_decoder(Request) ->
    binary_decoder(Request, <<>>).

binary_decoder(Request, Acc) ->
    case cowboy_req:read_body(Request) of
        {more, Bin, NewRequest} -> binary_decoder(NewRequest, <<Acc/binary, Bin/binary>>);
        {ok, Bin, DoneRequest} -> {ok, <<Acc/binary, Bin/binary>>, DoneRequest}
    end.

%%==============================================================================================
%% internal
%% encoder
%% too many encoder, so put them in minirest/body_encoder/x_encoder.erl
encoder({file, _}) ->
    fun minirest_file_encoder:encode/1;
encoder({file_binary, _, _}) ->
    fun minirest_file_encoder:encode/1;
encoder(Body) when is_binary(Body) ->
    fun minirest_binary_encoder:encode/1;
encoder(Body) when is_map(Body) ->
    fun minirest_json_encoder:encode/1;
encoder(Body) when is_list(Body) ->
    fun minirest_json_encoder:encode/1;
encoder({form_data, _}) ->
    fun minirest_form_data_encoder:encode/1;
encoder({message, _}) ->
    fun minirest_message_encoder:encode/1.
