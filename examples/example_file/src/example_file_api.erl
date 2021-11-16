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

-module(example_file_api).

-behaviour(minirest_api).

-export([start/0]).

-export([api_spec/0]).

-export([ upload_file/2
        , download_file/2]).

start() ->
    application:ensure_all_started(minirest),
    Ranch = #{
        max_connections => 512,
        num_acceptors => 4,
        socket_opts => [ {send_timeout, 5000}
                       , {port, 8088}
                       , {backlog, 512}]},
    Minirest = #{
        modules => [?MODULE],
        protocol => http,
        swagger_global_spec =>
            #{openapi => "3.0.0", info => #{title => "EMQ X Dashboard API", version => "5.0.0"}}},
    minirest:start(?MODULE, Ranch, Minirest).

api_spec() ->
    {
        [file_upload_api(), file_download_api()],
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
                <<"200">> => #{
                    content => #{}}}}},
    {"/file/upload", MetaData, upload_file}.

file_download_api() ->
    MetaData = #{
        get => #{
            description => "file download",
            responses => #{
                <<"200">> => #{
                    content => #{}}}}},
    {"/file/download", MetaData, download_file}.

upload_file(M, P) ->
    io:format("method : ~p~n", [M]),
    io:format("req    : ~p~n", [P]),
    204.

download_file(M, P) ->
    io:format("method : ~p~n", [M]),
    io:format("req    : ~p~n", [P]),
    {ok, Path} = file:get_cwd(),
    File = Path ++ "/src/example_file_api.erl",
    {200, {sendfile, File}}.
