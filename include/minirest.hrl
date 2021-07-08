-define(API_SPEC, api_spec).

-type path() :: string().
-type mete_data() :: map().
-type callback_function() :: atom().

-type api() :: {path(), mete_data(), callback_function()}.
-type apis() :: [api()].

-type schema_name() :: string() | binary().
-type api_schema() :: {schema_name(), map()}.
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
