%%--------------------------------------------------------------------
%% Copyright (c) 2015-2017 EMQ Enterprise, Inc. (http://emqtt.io).
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(minirest_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> [{group, handler}, {group, rest}, {group, rest_app}].

groups() ->
    [{handler,  [sequence], [t_init]},
     {rest,     [sequence], [t_index, t_get, t_list]},
     {rest_app, [sequence], [t_put, t_delete, t_auth]}].

init_per_suite(_Config) ->
    [application:ensure_all_started(App) || App <- [esockd, mochiweb, minirest]].

end_per_suite(_Config) ->
    ok.

init_per_group(rest, _Config) ->
    Handlers = [{"/api/v2/", minirest:handler(#{modules => [rest_api_books]})}],
    minirest:start_http(rest_server, 8080, [], Handlers);

init_per_group(rest_app, _Config) ->
    ok = application:start(minirest_example),
    Handlers = [{"/api/v2/", minirest:handler(#{apps => [minirest_example], modules => [rest_api_books]}), [{authorization, fun authorize_appid/1}]}],
    minirest:start_http(rest_server, 8080, [], Handlers);

init_per_group(_Group, _Config) ->
    ok.

end_per_group(rest, _Config) ->
    minirest:stop_http(rest_server, 8080);

end_per_group(rest_app, _Config) ->
    ok = application:stop(minirest_example),
    minirest:stop_http(rest_server, 8080);

end_per_group(_Group, _Config) ->
    ok.

t_init(_Config) ->
    {minirest_handler, dispatch, [Routes]} = minirest:handler(#{modules => [rest_api_books]}),
    ?assertEqual(4, length(Routes)).

t_index(_Config) ->
    {ok, {{_, 200, "OK"}, _Headers, Body}} = httpc_get("http://127.0.0.1:8080/api/v2/"),
    APIs = jsx:decode(Body, [return_maps]),
    ct:print("REST APIs: ~p", [APIs]),
    ?assertEqual(4, length(APIs)).

t_get(_Config) ->
    {ok, {{_, 200, "OK"}, _Headers, Body}} = httpc_get("http://127.0.0.1:8080/api/v2/books/1"),
    ?assertEqual(#{<<"id">> => 1, <<"name">> => <<"book1">>}, jsx:decode(Body, [return_maps])).

t_list(_Config) ->
    {ok, {{_, 200, "OK"}, _Headers, Body}} = httpc_get("http://127.0.0.1:8080/api/v2/books/"),
    ?assertEqual(100, length(jsx:decode(Body, [return_maps]))).

t_put(_Config) ->
	SuccessJson = [{<<"name">>, <<"ok">>}, {<<"username">>, <<"admin">>}, {<<"password">>, <<"public">>}],
	ErrorJson = [{<<"name">>, <<"error">>}, {<<"username">>, <<"admin">>}, {<<"password">>, <<"public">>}],
    {ok, {{_, 200, "OK"}, _Headers, Body}} = json_request(put, "http://127.0.0.1:8080/api/v2/books/1", SuccessJson),
    ?assertEqual(<<"ok">>, mochijson2:decode(Body)),
    {ok, {{_, ErrorCode, _}, _Headers1, _Body}} = json_request(put, "http://127.0.0.1:8080/api/v2/books/1", ErrorJson),
    ?assertEqual(500, ErrorCode).

t_delete(_Config) ->
    {ok, {{_, 500, _}, _Headers, _Body}} = json_request(delete, "http://127.0.0.1:8080/api/v2/books/1", [{<<"username">>, <<"admin">>}, {<<"password">>, <<"public">>}]).

t_auth(_Config) ->
    {ok, {{_, 401, _}, _Headers, _Body}} = json_request(delete, "http://127.0.0.1:8080/api/v2/books/1", [{<<"username">>, <<"admin1">>}, {<<"password">>, <<"public">>}]).

httpc_get(Url) ->
    httpc:request(get, {Url, []}, [], [{body_format, binary}]).

json_request(Request, Url, Json) ->
	Headers = [],
	ContentType = "application/json",
	Body = iolist_to_binary(mochijson2:encode(Json)),
    httpc:request(Request, {Url, Headers, ContentType, Body}, [], []).

authorize_appid(Req) ->
    Params = parse_params(Req),
    UserName = proplists:get_value(<<"username">>, Params),
    Password = proplists:get_value(<<"password">>, Params),
    (UserName =:= <<"admin">>) and (Password =:= <<"public">>).

parse_params(Req) ->
    parse_params(Req:get(method), Req).

parse_params('HEAD', Req) ->
    Req:parse_qs();
parse_params('GET', Req) ->
    Req:parse_qs();
parse_params(_Method, Req) ->
    Req:parse_post().


