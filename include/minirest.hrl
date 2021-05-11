-define(APP, minirest).
%
-define(HTTP_API_TABLE, minirest_http_api).
%
-record(http_api, {
    path :: list(),
    method :: 'GET'|'POST'|'DELETE'|'PUT',
    handler :: map(),
    func :: atom(),
    parameters :: map()
}).
