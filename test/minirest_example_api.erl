%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------
-module(minirest_example_api).
-compile([export_all, nowarn_export_all]).
-api_tag("example").
% API
-http_api(#{path => "/example",
            func => example_get,
            method => 'GET',
            description => "example api",
            parameters =>[
                #{
                    in => "path",
                    name => "page",
                    required => true,
                    schema => #{
                        type => "integer",
                        maximum => 10,
                        minimum => 0
                    }
                },
                #{
                    in => "query",
                    name => "size",
                    required => true,
                    schema => #{
                        type => "integer",
                        maximum => 10,
                        minimum => 0
                    }
                }
            ],
            % for next edition
            responses =>[]
            }
).
%% ====================================================================
%% API functions
%% ====================================================================
example_get(RequestContext) ->
    ct:print("RequestContext => :~p~n", [RequestContext]),
    #{k => v}.
