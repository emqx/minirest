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

-module(example_file_api).

-behaviour(minirest_api).

-export([api_spec/0]).

-export([ upload_file/2
        , download_file/2
        , download_temporary_file/2
        , download_form_data_file/2
        ]).

api_spec() ->
    {
        [
        file_upload_api(),
        file_download_api(),
        temporary_file_download_api(),
        form_data_file_download_api()
        ],
        []
    }.

file_upload_api() ->
    MetaData = #{
        post => #{
            description => "file upload",
            'requestBody' => #{
                content => #{
                    'multipart/form-data' => #{
                        schema => #{
                            type => object,
                            properties => #{
                                msg => #{
                                    type => string},
                                sshKey => #{
                                    type => string,
                                    format => binary}}},
                        encoding => #{
                            sshKey => #{
                                'contentType' => 'text/plain'}}}}},
            responses => #{
                200 => #{
                    content => #{}}}}},
    {"/file/upload", MetaData, upload_file}.

file_download_api() ->
    MetaData = #{
        get => #{
            description => "file download",
            responses => #{
                200 => #{
                    content => #{}}}}},
    {"/file/download", MetaData, download_file}.

temporary_file_download_api() ->
    MetaData = #{
        get => #{
            description => "temporary file download, delete after send",
            responses => #{
                200 => #{
                    content => #{}}}}},
    {"/file/temporary/download", MetaData, download_temporary_file}.

form_data_file_download_api() ->
    MetaData = #{
        get => #{
            description => "temporary file download, delete after send",
            responses => #{
                200 => #{
                    content => #{}}}}},
    {"/file/form_data/download", MetaData, download_form_data_file}.

upload_file(M, P) ->
    io:format("method : ~p~n", [M]),
    io:format("req    : ~p~n", [P]),
    204.

download_file(M, P) ->
    io:format("method : ~p~n", [M]),
    io:format("req    : ~p~n", [P]),
    {ok, Path} = file:get_cwd(),
    File = Path ++ "/src/example_file_api.erl",
    {200, {file, File}}.

download_temporary_file(M, P) ->
    io:format("method : ~p~n", [M]),
    io:format("req    : ~p~n", [P]),
    FileName = <<"temporary.txt">>,
    Temporary = <<"demo temporary data">>,
    {200, {file_binary, FileName, Temporary}}.

download_form_data_file(M, P) ->
    io:format("method : ~p~n", [M]),
    io:format("req    : ~p~n", [P]),
    {ok, Path} = file:get_cwd(),
    FilePath = Path ++ "/src/example_file_api.erl",
    FileName = <<"temporary.txt">>,
    Temporary = <<"demo temporary data">>,
    FormData = [
        {key, value},
        {key_int, 1},
        {key_float, 1.2},
        {key_boolean, true},
        {key_file, {file, FilePath}},
        {key_file_binary, {file_binary, FileName, Temporary}}
    ],
    {200, {form_data, FormData}}.
