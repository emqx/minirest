-define(APP, minirest).
%
-define(HTTP_API_TABLE, minirest_http_api).
%
-record(http_api, {
    path :: list(),
    method = 'GET' :: 'GET' | 'POST' | 'DELETE' | 'PUT',
    handler :: atom(),
    func :: atom(),
    parameters :: [map()]
}).

-record(openapi_parameter, {
    in :: list(),
    name :: list(),
    required = false :: boolean(),
    schema :: map()
}).

-record(openapi_schema, {
    type :: list(),
    maximum::number(),
    minimum::number(),
    maxLength::number(),
    minLength::number(),
    pattern::list(),
    required = false :: boolean(),
    enum::list()
}).