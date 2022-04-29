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

-module(minirest_file_encoder).

-include("minirest_http.hrl").
-include_lib("kernel/include/file.hrl").

-export([encode/1]).

encode({file, Path}) ->
    case filelib:is_file(Path) andalso file:read_file_info(Path) of
        {ok, #file_info{size = Size}} ->
            FileTypeContentTypeHeaders = minirest_body_encoder:file_content_type_headers(Path),
            {ok, FileTypeContentTypeHeaders, {sendfile, 0, Size, Path}};
        {error, Reason} ->
            Body = io_lib:format("mini rest file api bad return ~p", [Reason]),
            {?RESPONSE_CODE_INTERNAL_SERVER_ERROR, #{<<"content-type">> => <<"text/plain">>}, Body};
        false ->
            Body = io_lib:format("mini rest file api bad return ~p", [{bad_file, Path}]),
            {?RESPONSE_CODE_INTERNAL_SERVER_ERROR, #{<<"content-type">> => <<"text/plain">>}, Body}
    end;

%% binary file content
encode({file_binary, FileName, Binary}) ->
    {ok, minirest_body_encoder:file_content_type_headers(FileName), Binary}.
