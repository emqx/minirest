%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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
%%--------------------------------------------------------------------
-module(example_file_api).

-export([rest_api/0]).

-export([up_file/1, down_file/1]).

rest_api() ->
    Path = "/file",
    Metadata = #{
        post =>
            #{tags => ["file"],
            description => "file upload",
            operationId => up_file,
            consumes => [<<"multipart/form-data">>],
            parameters => [
                #{name => file
                    , in => formData
                    , type => file}],
            responses => #{<<"200">> => #{
                description => <<"Upload file success">>}}},
        get =>
            #{tags => ["file"],
            description => "file download",
            produces => [<<"multipart/form-data">>],
            operationId => down_file,
            responses =>
            #{<<"200">> => #{content => #{'multipart/form-data' =>
            #{schema => #{type => file}}}}}}},
    [{Path, Metadata}].

up_file(Request) ->
    {ok, Headers, Req2} = cowboy_req:read_part(Request),
    {ok, Data, _} = cowboy_req:read_part_body(Req2),
    {file, Key, Filename, ContentType} = cow_multipart:form_data(Headers),
    io:format("file ~p ~p ~p ~p~n", [Key, Filename, ContentType, Data]),
    io:format("Headers : ~p~n", [cowboy_req:headers(Request)]),
    {200}.

down_file(_Request) ->
%%    TODO: fix download
    Headers = #{<<"content-type">> =>
    <<"multipart/form-data; boundary=---minirest_boundary">>},
    Body = <<"---minirest_boundary\r\nContent-Disposition: form-data; name=\"file\"; filename=\"demo.txt\"\r\nContent-Type: application/octet-stream\r\n\r\n!!! file content !!! \n\r\n---minirest_boundary--\r\n">>,
    {200, Headers, Body}.

