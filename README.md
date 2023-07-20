
# minirest

A mini RESTful API framework built on cowboy and jsx.

## Write a RESTful API Module

```erlang
-module(rest_api_books).

-rest_api(#{name   => list_books,
            method => 'GET',
            path   => "/books/",
            func   => list,
            descr  => "List books"}).

-rest_api(#{name   => get_book,
            method => 'GET',
            path   => "/books/:id",
            func   => get,
            descr  => "Get book by Id"}).

-export([list/2, get/2]).

list(Binding, _Params) ->
    Books = [#{id => I, name => list_to_binary("book" ++ integer_to_list(I))}
             || I <- lists:seq(1, 100)],
    {200, Books}.

get(#{id := Id}, _Params) ->
    {200, #{id => Id, name => list_to_binary("book" ++ Id)}}.
```

## Start the REST server

```erlang
application:ensure_all_started(minirest),
Dispatch = [{"/[...]", minirest, [{"/", minirest:handler(#{apps => [minirest]}), []}]}],
minirest:start_http(test, #{socket_opts => [{port, 8080}]}, Dispatch, #{}). 
```

## Test HTTP Request

```bash
curl http://127.0.0.1:8080

curl http://127.0.0.1:8080/books/1
```

## Stop the REST server

```
minirest:stop_http(demo_rest_server, 8080).
```

## License

Apache License Version 2.0

## Author

Feng Lee <feng@emqx.io>

