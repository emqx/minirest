-define(RESPONSE_CODE_CONTINUE,                         100).
-define(RESPONSE_CODE_SWITCHING_PROTOCOLS,              101).
-define(RESPONSE_CODE_EARLY_HINTS,                      103).
-define(RESPONSE_CODE_OK,                               200).
-define(RESPONSE_CODE_CREATED,                          201).
-define(RESPONSE_CODE_ACCEPTED,                         202).
-define(RESPONSE_CODE_NON_AUTHORITATIVE_INFORMATION,    203).
-define(RESPONSE_CODE_NO_CONTENT,                       204).
-define(RESPONSE_CODE_RESET_CONTENT,                    205).
-define(RESPONSE_CODE_PARTIAL_CONTENT,                  206).
-define(RESPONSE_CODE_MULTIPLE_CHOICES,                 300).
-define(RESPONSE_CODE_MOVED_PERMANENTLY,                301).
-define(RESPONSE_CODE_FOUND,                            302).
-define(RESPONSE_CODE_SEE_OTHER,                        303).
-define(RESPONSE_CODE_NOT_MODIFIED,                     304).
-define(RESPONSE_CODE_TEMPORARY_REDIRECT,               307).
-define(RESPONSE_CODE_PERMANENT_REDIRECT,               308).
-define(RESPONSE_CODE_BAD_REQUEST,                      400).
-define(RESPONSE_CODE_UNAUTHORIZED,                     401).
-define(RESPONSE_CODE_PAYMENT_REQUIRED,                 402).
-define(RESPONSE_CODE_FORBIDDEN,                        403).
-define(RESPONSE_CODE_NOT_FOUND,                        404).
-define(RESPONSE_CODE_METHOD_NOT_ALLOWED,               405).
-define(RESPONSE_CODE_NOT_ACCEPTABLE,                   406).
-define(RESPONSE_CODE_PROXY_AUTHENTICATION_REQUIRED,    407).
-define(RESPONSE_CODE_REQUEST_TIMEOUT,                  408).
-define(RESPONSE_CODE_CONFLICT,                         409).
-define(RESPONSE_CODE_GONE,                             410).
-define(RESPONSE_CODE_LENGTH_REQUIRED,                  411).
-define(RESPONSE_CODE_PRECONDITION_FAILED,              412).
-define(RESPONSE_CODE_PAYLOAD_TOO_LARGE,                413).
-define(RESPONSE_CODE_URI_TOO_LONG,                     414).
-define(RESPONSE_CODE_UNSUPPORTED_MEDIA_TYPE,           415).
-define(RESPONSE_CODE_RANGE_NOT_SATISFIABLE,            416).
-define(RESPONSE_CODE_EXPECTATION_FAILED,               417).
-define(RESPONSE_CODE_IM_A_TEAPOT,                      418).
-define(RESPONSE_CODE_UNPROCESSABLE_ENTITY,             422).
-define(RESPONSE_CODE_TOO_EARLY,                        425).
-define(RESPONSE_CODE_UPGRADE_REQUIRED,                 426).
-define(RESPONSE_CODE_PRECONDITION_REQUIRED,            428).
-define(RESPONSE_CODE_TOO_MANY_REQUESTS,                429).
-define(RESPONSE_CODE_REQUEST_HEADER_FIELDS_TOO_LARGE,  431).
-define(RESPONSE_CODE_UNAVAILABLE_FOR_LEGAL_REASONS,    451).
-define(RESPONSE_CODE_INTERNAL_SERVER_ERROR,            500).
-define(RESPONSE_CODE_NOT_IMPLEMENTED,                  501).
-define(RESPONSE_CODE_BAD_GATEWAY,                      502).
-define(RESPONSE_CODE_SERVICE_UNAVAILABLE,              503).
-define(RESPONSE_CODE_GATEWAY_TIMEOUT,                  504).
-define(RESPONSE_CODE_HTTP_VERSION_NOT_SUPPORTED,       505).
-define(RESPONSE_CODE_VARIANT_ALSO_NEGOTIATES,          506).
-define(RESPONSE_CODE_INSUFFICIENT_STORAGE,             507).
-define(RESPONSE_CODE_LOOP_DETECTED,                    508).
-define(RESPONSE_CODE_NOT_EXTENDED,                     510).
-define(RESPONSE_CODE_NETWORK_AUTHENTICATION_REQUIRED,  511).


-define(DEFAULT_RESPONSE_HEADERS, #{<<"content-type">> => <<"application/json">>}).

-define(FILE_CONTENT_TYPE_MAP, #{
        <<".html">>     =>    <<"text/html">>,
        <<".config">>   =>    <<"application/octet-stream">>,
        <<".log">>      =>    <<"application/octet-stream">>,
        <<".json">>     =>    <<"application/json">>,
        <<".zip">>     =>    <<"application/zip">>,
        <<".txt">>      =>    <<"text/plain">>,
        <<".gif">>      =>    <<"image/gif">>,
        <<".jpeg">>     =>    <<"image/jpeg">>
    }).
