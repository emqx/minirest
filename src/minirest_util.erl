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

-module(minirest_util).

%% API
-export([pmap/3]).

-spec pmap(fun((A) -> B), list(A), timeout()) -> list(B | {error, term()}).
pmap(Fun, List, Timeout) ->
  Self = self(),
  WorkFun = fun(El) -> spawn_link(fun() -> pmap_exec(Self, Fun, El, Timeout) end) end,
  Workers = lists:map(WorkFun, List),
  pmap_gather(Workers).

pmap_exec(CallerPid, Fun, El, Timeout) ->
  ExecPid = self(),
  {Pid, Ref} = spawn_monitor(fun() -> ExecPid ! {result, self(), Fun(El)} end),
  ExecResult =
    receive
      {result, Pid, Result} -> Result;
      {'DOWN', Ref, process, Pid, Reason} -> {error, Reason}
    after Timeout ->
      true = erlang:exit(Pid, kill),
      {error, timeout}
    end,
  CallerPid ! {ExecPid, ExecResult}.

pmap_gather([Worker | Workers]) ->
  receive
    {Worker, Result} -> [Result | pmap_gather(Workers)]
  end;
pmap_gather([]) ->
  [].
