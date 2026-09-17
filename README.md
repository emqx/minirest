# minirest

A mini RESTful API framework built on cowboy and swagger

# UseAge
## Create erlang application

```shell
rebar3 new app my_server
```

## Add dep
open `rebar.config` and add minirest in deps

```erlang
{deps, [{minirest, {git, "https://github.com/emqx/minirest", {tag, "1.1.2"}}}]}.
```

After add dep, rebar.config file should be like
```erlang
{erl_opts, [debug_info]}.

{deps, [{minirest, {git, "https://github.com/emqx/minirest", {tag, "1.1.2"}}}]}.

{shell, [
    {apps, [my_server]}
]}.
```

## Write an API provider module, example.erl

```erlang
-module(example).

-behavior(minirest_api).

%% API
-export([api_spec/0]).

-export([hello/2]).

api_spec() ->
  {
    [hello_api()],
    []
  }.

hello_api() ->
    MetaData = #{
        get => #{
            description => "hello world",
            responses => #{
            <<"200">> => #{
                content => #{
                    'application/json' => #{
                        schema => #{
                            type => object,
                            properties => #{
                                msg => #{
                                    type => string}}}},
                  'text/plain' => #{
                        schema => #{
                            type => string}}}}}}},
  {"/hello", MetaData, hello}.

hello(get, #{bindings := Bindins,
             body := Body,
             query_string := QueryString,
             headers := Headers}) ->
    Content = maps:get(<<"accept">>, Headers),
    Body =
        case Content of
            <<"text/plain">> ->
                <<"hello, minirest">>;
             <<"application/json">> ->
                #{msg => <<"hello minirest">>}
        end,
    {200, #{<<"content-type">> => Content},  Body}.


% Supports callback functions for 2/3 parameters
% The first parameter is Method
% The second argument is the parsed parameters, including (bindings, query_string, headers, body)
% The third argument is the request of cowboy
-export([hello/3]).
hello(Method, #{bindings := Bindins,
                body := Body,
                query_string := QueryString,
                headers := Headers}, Request) ->
    Content = maps:get(<<"accept">>, Headers),
    Body =
        case Content of
            <<"text/plain">> ->
                <<"hello, minirest">>;
             <<"application/json">> ->
                #{msg => <<"hello minirest">>}
        end,
    {200, #{<<"content-type">> => Content},  Body}.

```

## Set a response cookie

Return the formatted cookie under the lower case `set-cookie` key of the
response headers. Use `cow_cookie:setcookie/3` to build it. Pass a list of
binaries to send several cookies, which minirest sends as one `set-cookie`
header each.

```erlang
login(post, _Params) ->
    Cookie = iolist_to_binary(
        cow_cookie:setcookie(<<"session">>, Token, #{
            path => <<"/api">>, http_only => true, secure => true, max_age => 3600
        })
    ),
    {200, #{<<"set-cookie">> => Cookie}, #{}}.
```

## Add log meta to a failed authorization

An `authorization` callback returns `{ok, AuthMeta}` to accept a request.
minirest merges `AuthMeta` into the log meta that the `log` hook receives.

To reject a request and still add log meta, return
`{with_log_meta, LogMeta, Response}`. minirest merges `LogMeta` into the log
meta in the same way, and sends only `Response` to the client. Use it to
record an authenticated caller that is not allowed to use the endpoint.

```erlang
authorize(Req, HandlerInfo) ->
    case check(Req, HandlerInfo) of
        {ok, User} ->
            {ok, #{source => User}};
        {forbidden, User} ->
            {with_log_meta, #{source => User},
                {403, 'FORBIDDEN', <<"Not allowed">>}}
    end.
```

A handler can also call `minirest_handler:update_log_meta/1` to add log meta.
The call does nothing in a process that does not handle a minirest request.

## Start your HTTP server

```erlang
    ServerName = example_server,
    App = my_server, %% or your app name
    {ok, _} = application:ensure_all_started(minirest),
    Options = #{
        port => 8088,
        apps => [App]
    },
    minirest:start(ServerName, Options).
```

## Now, Visit `http://localhost:8088/api-docs` and see what happened

## Example

See detail by example/my_server

## TODO

- Request filter

    query & headers

- Parameters check

- Test suite
