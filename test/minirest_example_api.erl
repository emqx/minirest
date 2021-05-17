%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------
-module(minirest_example_api).
-compile([export_all, nowarn_export_all]).

%% for openapi 'tags'
-api_tag("example").
%----------------------------------------------------------------------
%% get test
-http_api(#{path => "/example?page=1&size=10",
            func => example_get,
            method => 'GET',
            description => "example_get",
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
%% post test
-http_api(#{path => "/example",
            func => example_post,
            method => 'POST',
            description => "example_post",
            parameters =>[],
            % for next edition
            responses =>[]
            }
).
%% put test
-http_api(#{path => "/example",
            func => example_put,
            method => 'PUT',
            description => "example_put",
            parameters =>[],
            % for next edition
            responses =>[]
            }
).
%% delete test
-http_api(#{path => "/example",
            func => example_delete,
            method => 'DELETE',
            description => "example_delete",
            parameters =>[],
            % for next edition
            responses =>[]
            }
).
%% example_invalid_return test
-http_api(#{path => "/example_invalid_return",
            func => example_invalid_return,
            method => 'GET',
            description => "example_invalid_return",
            parameters =>[],
            % for next edition
            responses =>[]
            }
).
%% method not support test
-http_api(#{path => "/method_not_support",
            func => method_not_support,
            method => 'GET',
            description => "method_not_support",
            parameters =>[],
            % for next edition
            responses =>[]
            }
).
%% test path bindings
-http_api(#{path => "/bindings/$k1/$k2",
            func => bindings,
            method => 'GET',
            description => "bindings",
            parameters =>[],
            % for next edition
            responses =>[]
            }
).

%% ====================================================================
%% API functions
%% ====================================================================
example_get(_Request) ->
    ct:print("Bindings => :~p~n", [_Request]),
    #{k => example_get}.
example_post(_Request) ->
    ct:print("Bindings => :~p~n", [_Request]),
    #{k => example_post}.
example_put(_Request) ->
    ct:print("Bindings => :~p~n", [_Request]),
    #{k => example_put}.
example_delete(_Request) ->
    ct:print("Bindings => :~p~n", [_Request]),
    #{k => example_delete}.

example_invalid_return(_Request) ->
    ct:print("Bindings => :~p~n", [_Request]),
    {tuple, test}.

method_not_support(_Request) ->
    ct:print("Bindings => :~p~n", [_Request]),
    <<"method_not_support">>.

bindings(#{bindings := Bindings} = Request) ->
    ct:print("Bindings => :~p~n", [Request]),
    <<"bindings ok">>.