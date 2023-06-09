%% Copyright (c) 2013-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(minirest_util_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        t_pmap_exception
    ].

t_pmap_exception(_Config) ->
    process_flag(trap_exit, true),
    ct:timetrap({seconds, 2}),

    ?assertMatch(
        [
            {error, {a, {error, a, _}}},
            {error, {b, {error, b, _}}},
            {error, {c, {error, c, _}}}
        ],
        minirest_util:pmap(fun(X) -> error(X) end, [a, b, c], 1000)
    ).
