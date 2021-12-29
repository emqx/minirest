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
        , encode/2]).
%%==============================================================================================
%% parse
-spec(parse(Request :: map()) ->
            {ok, {Body :: map() | binary(), NRequest :: map()}} |
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
        {ok, {jsx:decode(Body), NRequest}}
    catch _:_:_ ->
        Error = #{code => <<"BAD_REQUEST">>, message => <<"Invalid json message received">>},
        {response, {?RESPONSE_CODE_BAD_REQUEST, Error}}
    end.

forma_data_decoder(Request) ->
    forma_data_decoder(Request, #{}).

forma_data_decoder(Request, Body) ->
    case loop_form(Request) of
        {done, NRequest} ->
            {ok, {Body, NRequest}};
        {Part, NRequest} ->
            forma_data_decoder(NRequest, maps:merge(Body, Part))
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

binary_decoder(Request) ->
    cowboy_req:read_body(Request).

%%==============================================================================================
%% internal
%% encoder
encoder({file, _}) ->
    fun file_encoder/1;
encoder({file_binary, _}) ->
    fun file_binary_encoder/1;
encoder(Body) when is_binary(Body) ->
    fun binary_encoder/1;
encoder(Body) when is_map(Body) ->
    fun json_encoder/1;
encoder(Body) when is_list(Body) ->
    fun json_encoder/1;
encoder({form_data, _}) ->
    fun form_data_encoder/1.


file_encoder({file, Path}) ->
    case filelib:is_file(Path) andalso file:read_file_info(Path) of
        {ok, #file_info{size = Size}} ->
            FileTypeContentTypeHeaders = file_content_type_headers(Path),
            {ok, FileTypeContentTypeHeaders, {sendfile, 0, Size, Path}};
        {error, Reason} ->
            Body = io_lib:format("mini rest file api bad return ~p", [Reason]),
            {?RESPONSE_CODE_INTERNAL_SERVER_ERROR, #{<<"content-type">> => <<"text/plain">>}, Body};
        false ->
            Body = io_lib:format("mini rest file api bad return ~p", [{bad_file, Path}]),
            {?RESPONSE_CODE_INTERNAL_SERVER_ERROR, #{<<"content-type">> => <<"text/plain">>}, Body}
    end.

file_binary_encoder({file_binary, FileName, Binary}) ->
    FileTypeContentTypeHeaders = file_content_type_headers(FileName),
    {ok, FileTypeContentTypeHeaders, Binary}.

binary_encoder(Body) ->
    {ok, maps:merge(?DEFAULT_RESPONSE_HEADERS, #{<<"content-length">> => erlang:size(Body)}), Body}.

json_encoder(Body) ->
    case jsx:is_term(Body) of
        true ->
            {ok, ?DEFAULT_RESPONSE_HEADERS, jsx:encode(Body)};
        false ->
            Response = {
                ?RESPONSE_CODE_INTERNAL_SERVER_ERROR,
                #{<<"content-type">> => <<"text/plain">>},
                list_to_binary(io_lib:format("invalid json term: ~p", [Body]))
            },
            {response, Response}
    end.

form_data_encoder({form_data, Data}) when is_list(Data) ->
    case form_data_loop_encoder(Data, boundary(), []) of
        {response, Response} ->
            {response, Response};
        {ok, Headers, <<"\r\n", RealBodyBinary/binary>>} ->
            %% remove \r\n for first part
            {ok, Headers, RealBodyBinary}
    end;
form_data_encoder({form_data, Data}) ->
    Response = {
        ?RESPONSE_CODE_INTERNAL_SERVER_ERROR,
        #{<<"content-type">> => <<"text/plain">>},
        list_to_binary(io_lib:format("invalid form-data term: ~p", [Data]))
    },
    {response, Response}.

form_data_loop_encoder([], Boundary, Res) ->
    Headers = #{<<"content-type">> => <<"multipart/form-data, boundary=", Boundary/binary>>},
    {ok, Headers, iolist_to_binary(lists:reverse(Res))};

form_data_loop_encoder([Part | Tail], Boundary, Res) ->
    case form_data_part(Part, Boundary) of
        {response, Response} ->
            {response, Response};
        PartBinary ->
            form_data_loop_encoder(Tail, Boundary, [PartBinary | Res])
    end.

%% form-data file by path
%% if part name is not specified, will use file name without extension
form_data_part({file, Path}, Boundary) ->
    case to_binary(Path) of
        {error, PathError} ->
            error_form_data_response({read_file, PathError});
        PathBinary ->
            KeyName = filename:basename(filename:rootname(PathBinary)),
            form_data_part({file, KeyName, PathBinary}, Boundary)
    end;
form_data_part({file, KeyName, Path}, Boundary) ->
    case filelib:is_file(Path) andalso file:read_file_info(Path) of
        {ok, _} ->
            case file:read_file(Path) of
                {ok, Binary} ->
                    FileName = filename:basename(Path),
                    form_data_part({file_binary, KeyName, FileName, Binary}, Boundary);
                {error, Reason} ->
                    error_form_data_response({read_file, {Path, Reason}})
            end;
        {error, Reason} ->
            error_form_data_response({bad_file, {Path, Reason}});
        false ->
            error_form_data_response({bad_file, Path})
    end;

%% form-data send file by binary
%% if part name is not specified, will use file name without extension
form_data_part({file_binary, FileName, FileBinary}, Boundary) ->
    case to_binary(FileName) of
        {error, FileNameError} ->
            error_form_data_response({bad_file_part, FileNameError});
        FileNameBinary ->
            KeyName = filename:basename(filename:rootname(FileBinary)),
            form_data_part({file_binary, KeyName, FileNameBinary, FileBinary}, Boundary)
    end;
form_data_part({file_binary, KeyName, FileName, FileBinary}, Boundary) ->
    case {to_binary(KeyName), to_binary(FileName)} of
        {{error, KeyNameError}, {error, FileNameError}} ->
            error_form_data_response({bad_file_part, {KeyNameError, FileNameError}});
        {{error, KeyNameError}, _} ->
            error_form_data_response({bad_file_part, KeyNameError});
        {_, {error, FileNameError}} ->
            error_form_data_response({bad_file_part, FileNameError});
        {KeyNameBinary, FileNameBinary} ->
            Headers = [
                {<<"Content-Disposition">>,
                    <<"form-data; name=\"", KeyNameBinary/binary,
                        "\"; filename=\"", FileNameBinary/binary, "\"">>}
                | file_content_type_headers(FileName, #{return_type => prop_list})],
            PartHeaders = cow_multipart:part(Boundary, Headers),
            iolist_to_binary([PartHeaders, FileBinary])
    end;

%% key & value
form_data_part({Key, Value}, Boundary) ->
    case {to_binary(Key), to_binary(Value)} of
        {{error, {not_support, Key}}, {error, {not_support, Value}}} ->
            error_form_data_response({Key, Value});
        {{error, {not_support, Key}}, _} ->
            error_form_data_response({bad_key, Key});
        {_, {error, {not_support, Value}}} ->
            error_form_data_response({bad_value, Value});
        {KBinary, VBinary} ->
            NHeaders = [
                {<<"content-disposition">>, <<"form-data; name=\"", KBinary/binary, "\"">>},
                {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
            PartHeaders = cow_multipart:part(Boundary, NHeaders),
            iolist_to_binary([PartHeaders, VBinary])
    end.

%% file content type
file_content_type_headers(Path) ->
    file_content_type_headers(Path, #{return_type => map}).

file_content_type_headers(Path, #{return_type := RT}) ->
    FileType = filename:extension(Path),
    case {maps:get(FileType, ?FILE_CONTENT_TYPE_MAP, undefined), RT} of
        {undefined, map} ->
            #{};
        {undefined, prop_list} ->
            [];
        {Type, map} ->
            #{<<"content-type">> => Type};
        {Type, prop_list} ->
            [{<<"content-type">>, Type}]
    end.

error_form_data_response(Data) ->
    Response = {
        ?RESPONSE_CODE_INTERNAL_SERVER_ERROR,
        #{<<"content-type">> => <<"text/plain">>},
        list_to_binary(io_lib:format("invalid form-data term: ~p", [Data]))
    },
    {response, Response}.

boundary() ->
    cow_multipart:boundary().

to_binary(Data) when is_binary(Data) -> Data;
to_binary(Data) when is_list(Data) -> list_to_binary(Data);
to_binary(Data) when is_atom(Data) -> atom_to_binary(Data, utf8);
to_binary(Data) when is_float(Data) -> float_to_binary(Data, [{decimals, 10}, compact]);
to_binary(Data) when is_integer(Data) -> integer_to_binary(Data);
to_binary(true) -> <<"true">>;
to_binary(false) -> <<"false">>;
to_binary(Data) -> {error, {not_support, Data}}.
