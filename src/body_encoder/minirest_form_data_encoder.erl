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

-module(minirest_form_data_encoder).

-include("minirest_http.hrl").

-export([encode/1]).

encode({form_data, Data}) when is_list(Data) ->
    case loop_encode(Data, boundary(), []) of
        {response, Response} ->
            {response, Response};
        {ok, Headers, <<"\r\n", RealBodyBinary/binary>>} ->
            %% remove \r\n for first part
            {ok, Headers, RealBodyBinary}
    end;
encode({form_data, Data}) ->
    Response = {
        ?RESPONSE_CODE_INTERNAL_SERVER_ERROR,
        #{<<"content-type">> => <<"text/plain">>},
        list_to_binary(io_lib:format("invalid form-data term: ~p", [Data]))
    },
    {response, Response}.

loop_encode([], Boundary, Res) ->
    Headers = #{<<"content-type">> => <<"multipart/form-data, boundary=", Boundary/binary>>},
    {ok, Headers, iolist_to_binary(lists:reverse(Res))};

loop_encode([Part | Tail], Boundary, Res) ->
    case form_data_part(Part, Boundary) of
        {response, Response} ->
            {response, Response};
        PartBinary ->
            loop_encode(Tail, Boundary, [PartBinary | Res])
    end.

%% form-data file by path
form_data_part({Key, {file, Path}}, Boundary) ->
    case filelib:is_file(Path) andalso file:read_file_info(Path) of
        {ok, _} ->
            case file:read_file(Path) of
                {ok, Binary} ->
                    FileName = filename:basename(Path),
                    form_data_part({Key, {file_binary, FileName, Binary}}, Boundary);
                {error, Reason} ->
                    error_form_data_response({read_file, {Path, Reason}})
            end;
        {error, Reason} ->
            error_form_data_response({bad_file, {Path, Reason}});
        false ->
            error_form_data_response({bad_file, Path})
    end;

%% form-data send file by binary
form_data_part({Key, {file_binary, FileName, FileBinary}}, Boundary) ->
    case {to_binary(Key), to_binary(FileName)} of
        {{error, KeyError}, {error, FileNameError}} ->
            error_form_data_response({bad_file_part, {KeyError, FileNameError}});
        {{error, KeyError}, _} ->
            error_form_data_response({bad_file_part, KeyError});
        {_, {error, FileNameError}} ->
            error_form_data_response({bad_file_part, FileNameError});
        {KeyBinary, FileNameBinary} ->
            FileHeaders = minirest_body_encoder:file_content_type_headers(
                              FileName, #{return_type => prop_list}),
            Headers = [
                {<<"Content-Disposition">>,
                    <<"form-data; name=\"", KeyBinary/binary,
                        "\"; filename=\"", FileNameBinary/binary, "\"">>}
                | FileHeaders],
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
    end;

form_data_part(NotSupport, _) ->
    error_form_data_response({not_support, NotSupport}).

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
