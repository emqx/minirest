-module(example_api).

-export([api_spec/0]).

-export([ handle_echo_get/2
        , handle_echo_post/2]).

-spec(api_spec() -> [{Path :: string(), Metadata :: map(), State :: term()}]).
api_spec() ->
    Path = "/example/:path_parameter",
    Metadata = #{
    get =>
       #{tags => ["example"],
        description => "echo parameters",
        operationId => handle_echo_get,
        parameters => [
            #{  name => "path_parameter",
                in => path,
                description => "parameter in path",
                schema => #{type => string}},
            #{  name => "query_parameter",
                in => 'query',
                description => "parameter in query",
                required => false,
                schema => #{type => integer, example => 100}},
            #{  name => "header_parameter",
                in => header,
                description => "parameter in header",
                required => false,
                schema => #{type => string, enum => ["a", "b", "c"], default => "a"}}
            ],
        responses => #{
            <<"200">> => #{
                description => <<"echo all parameters">>,
                content => #{
                  'application/json' =>
                    #{schema =>
                        #{type => object,
                          properties =>
                            #{<<"path_parameters">> => #{type => string, example => <<"path_hello">>},
                              <<"query_parameters">> => #{type => string, example => <<"query_hello">>},
                              <<"header_parameters">> => #{type => string, example => <<"header_hello">>}}
                        }}
                  }}
        }},
    post =>
       #{tags => ["example"],
        description => "echo post body",
        operationId => handle_echo_post,
        requestBody => #{
            content => #{
                  'application/json' =>
                    #{schema =>
                        #{type => object,
                         properties => #{
                                    echo => #{type => string},
                                    times => #{type => integer}
                                    }
                                }}}
                  }
            ,
        responses => #{
            <<"200">> => #{description => <<"create echo success">>}}
        }
    },
    [{Path, Metadata, example_state}].

%% operationId => CallbackFunctionName
%% callback_function(ParametersMap, State) -> Result
%%     ParametersMap = #{Key :: atom() => Value :: term()}
%%     State :: term()
%%     Result :: {StatusCode, Headers, Body, State} | {StatusCode, Headers, State} | {StatusCode, State}
%%         StatusCode :: integer() | binary() | string() | ok
%%         Headers :: map()
%%         Body :: map() | binary()
handle_echo_get(_Req, State = example_state) ->
    {<<"200">>, State}.

handle_echo_post(_Req, State = example_state) ->
    {200, State}.