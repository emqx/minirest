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

-type status_code() :: integer().
-type headers() ::
    map()
    | list().

-type file_path() :: binary() | string().
-type file_name() :: binary() | string().
-type file_content() :: binary().

-type data_response_file() :: {file, file_path()} | {file_binary, file_name(), file_content()}.

-type form_data_key() :: atom() | binary() | string().
-type form_data_value() :: atom() | binary() | string() | data_response_file().

-type form_data_part() :: {form_data_key(), form_data_value()}.

-type response_body() ::
    binary()
    | jsx:json_term()
    | data_response_file()
    | {form_data, [form_data_part()]}.

-type error_code() :: atom() | binary().
-type error_message() :: binary().

-type response() ::
    status_code()
    | {status_code()}
    | {status_code(), response_body()}
    | {status_code(), headers(), response_body()}
    | {status_code(), error_code(), error_message()}.

-record(handler, {
    method :: http_method(),
    module :: atom(),
    function :: atom(),
    filter :: fun(),
    authorization :: {Module :: atom(), Function :: atom()} | undefined,
    log_meta :: map(),
    error_codes :: list(error_code())
}).

-type handler() :: #handler{}.

-type handler_state() :: #{
    path := path(),
    log := {module(), atom(), InitMeta :: map()} | undefined,
    methods := #{http_method() => handler()}
}.

-define(MFA, {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}).
-define(LOG(Level, Data), logger:log(Level, Data, #{mfa => ?MFA, line => ?LINE})).

-endif.
