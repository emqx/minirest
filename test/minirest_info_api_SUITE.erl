%% Copyright (c) 2025 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(minirest_info_api_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        t_codes
    ].

init_per_suite(Config) ->
    application:ensure_all_started(minirest),
    minirest_handler_SUITE:start_minirest(),
    Config.

end_per_suite(_Config) ->
    minirest_handler_SUITE:stop_minirest().

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

t_codes(_Config) ->
    minirest_info_api:add_codes(['BAD_REQUEST', 'NOT_FOUND']),
    ?assertMatch(
        [<<"BAD_REQUEST">>, <<"NOT_FOUND">>],
        minirest_info_api:codes()
    ),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(minirest_handler_SUITE:address() ++ "/info/error/codes"),
    ?assertEqual(
        #{<<"codes">> => [<<"BAD_REQUEST">>, <<"NOT_FOUND">>]},
        jsx:decode(iolist_to_binary(Body), [return_maps])
    ).
