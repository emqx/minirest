# minirest

A mini RESTful API framework built on cowboy and swagger

## Write an API provider module

```erlang
-module(example).

-export([rest_api/0]).

-export([hello/1]).

-spec(rest_api() -> [{Path :: string(), Metadata :: map()}]).
rest_api() ->
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

```

## Start your HTTP server

```erlang
    application:ensure_all_started(minirest).
    Options = #{port => 8088, modules => [example_hello_api]}.
    minirest:start(?MODULE, Options).
```

## Example

See detail by example/example_server

## TODO

- Request filter

    query & headers

- Parameters check
  
- Test suite
