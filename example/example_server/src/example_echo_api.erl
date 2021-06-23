-module(example_echo_api).

-export([api_spec/0]).

-export([echo/1]).

-spec(api_spec() -> [{Path :: string(), Metadata :: map()}]).
api_spec() ->
    Path = "/echo/:message",
    Metadata = #{
        get =>
           #{tags => ["example"],
            description => "echo parameters",
            operationId => echo,
            parameters => [#{ name => message
                            , in => path
                            , schema =>
                               #{ type => string
                                , example => "hello"}}],
            responses => #{<<"200">> => #{
                description => <<"echo message">>,
                content => #{
                  'text/plain' =>
                    #{schema => #{type => string}}}}}}},
    [{Path, Metadata}].

echo(Request) ->
    StatusCode = 200,
    Headers = #{<<"Content-Type">> => <<"text/plain">>},
    Body = cowboy_req:binding(message, Request),
    {StatusCode, Headers, Body}.
