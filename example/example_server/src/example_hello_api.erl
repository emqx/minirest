-module(example_hello_api).

-export([api_spec/0]).

-export([hello/1]).

-spec(api_spec() -> [{Path :: string(), Metadata :: map()}]).
api_spec() ->
    Path = "/hello",
    Metadata = #{
        get =>
           #{tags => ["example"],
            description => "hello world",
            operationId => hello,
            responses => 
                #{<<"200">> => 
                    #{content => 
                    #{'text/plain' =>
                        #{schema => #{type => string}}}}}}},
    [{Path, Metadata}].

hello(_Request) ->
    StatusCode = 200,
    Headers = #{<<"Content-Type">> => <<"text/plain">>},
    Body = <<"hello world !">>,
    {StatusCode, Headers, Body}.
