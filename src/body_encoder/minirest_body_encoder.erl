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

-module(minirest_body_encoder).

-include("minirest_http.hrl").
-export([ file_content_type_headers/1
        , file_content_type_headers/2
        ]).

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
