%% Copyright (c) 2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-ifndef(MINIREST_HRL).
-define(MINIREST_HRL, true).

-define(API_SPEC, api_spec).

-type path() :: string().
-type mete_data() :: map().
-type callback_function() :: atom().

-type api() :: {path(), mete_data(), callback_function()}.
-type apis() :: [api()].

-type api_schema() :: map().
-type api_schemas() :: [api_schema()].

-type api_spec() :: {apis(), api_schemas()}.

-type http_method() :: get | post | put | head | delete | patch | options | connect | trace.

-record(handler, {
    method          :: http_method(),
    path            :: string(),
    module          :: atom(),
    function        :: atom(),
    filter          :: fun(),
    authorization   :: {Module :: atom(), Function :: atom()} | undefined
}).

-define(MFA, {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}).
-define(LOG(Level, Data), logger:log(Level, Data, #{mfa => ?MFA, line => ?LINE})).

-endif.
