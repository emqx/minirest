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

-export([ parse/1
        , parse/2
        ]).

parse(Request) ->
    ContentType = cowboy_req:parse_header(<<"content-type">>, Request, undefined),
    parse(ContentType, Request).

parse({<<"application">>, <<"json">>, _}, Request) -> json_decoder(Request);
parse({<<"multipart">>, <<"form-data">>, _}, Request) -> form_data_decoder(Request);
parse({<<"application">>, <<"octet-stream">>, _}, Request) -> binary_decoder(Request);
parse(_, Request) -> binary_decoder(Request).

json_decoder(Request) ->
    {ok, Body, NRequest} = cowboy_req:read_body(Request),
    try
        {ok, jsx:decode(Body), NRequest}
    catch _:_:_ ->
        Error = #{code => <<"BAD_REQUEST">>, message => <<"Invalid json message received">>},
        {response, {?RESPONSE_CODE_BAD_REQUEST, Error}}
    end.

form_data_decoder(Request) ->
    form_data_decoder(Request, #{}).

form_data_decoder(Request, Body) ->
    case loop_form(Request) of
        {done, NRequest} ->
            {ok, Body, NRequest};
        {Part, NRequest} ->
            form_data_decoder(NRequest, maps:merge(Body, Part))
    end.

loop_form(Request) ->
    case cowboy_req:read_part(Request) of
        {ok, PartHeader, Request1} ->
            {ok, Data, Request2} = cowboy_req:read_part_body(Request1),
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

binary_decoder(Req) ->
    binary_decoder(Req, <<>>).

binary_decoder(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {more, Bin, NewReq} -> binary_decoder(NewReq, <<Acc/binary, Bin/binary>>);
        {ok, Bin, DoneReq} -> {ok, <<Acc/binary, Bin/binary>>, DoneReq}
    end.
