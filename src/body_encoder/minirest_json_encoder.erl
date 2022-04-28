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

-module(minirest_json_encoder).
-include("minirest_http.hrl").

-export([encode/1]).

encode(Body) ->
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
