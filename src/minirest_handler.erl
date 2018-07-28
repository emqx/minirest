%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(minirest_handler).

-author("Feng Lee <feng@emqtt.io>").

-export([init/1, dispatch/3]).

-type(config() :: #{apps => list(atom()), modules => list(module())}).

-export_type([config/0]).

-spec(init(config()) -> {?MODULE, dispatch, [map()]}).
init(Config) ->
    Routes = lists:usort(
               [API#{module => Module, pattern => string:tokens(Path, "/")}
                || Module <- modules(Config), {rest_api, [API = #{path := Path, name := Name}]}
                   <- Module:module_info(attributes), not lists:member(Name, maps:get(except, Config, [])) ]),
    {?MODULE, dispatch, [Routes]}.

modules(Config) ->
    lists:foldl(fun(App, Acc) ->
                    {ok, Mods} = application:get_key(App, modules),
                    lists:append(Mods, Acc)
                end, maps:get(modules, Config, []), maps:get(apps, Config, [])).

%% Get API List
dispatch("/", Req, Routes) ->
    case binary_to_atom(cowboy_req:method(Req), utf8) of
        'GET' ->
            jsonify(Req, 200, [format_route(Route) || Route <- Routes]);
        _ ->
            bad_req(Req, <<"Bad Reques">>)
    end;

%% Dispatch request to REST APIs
dispatch(Path, Req, Routes) ->
    case catch match_route(binary_to_atom(cowboy_req:method(Req), utf8), Path, Routes) of
        {ok, #{module := Mod, func := Fun, bindings := Bindings}} ->
            case catch parse_params(Req) of
                {'EXIT', Reason} ->
                    error_logger:error_msg("Params error: ~p", [Reason]),
                    bad_req(Req, <<"Bad Reques">>);
                Params ->
                    jsonify(Req, erlang:apply(Mod, Fun, [Bindings, Params]))
            end;
        {'EXIT', {badarg, _}} ->
            bad_req(Req, <<"Not found.">>);
        false ->
            bad_req(Req, <<"Not found.">>)
    end.

format_route(#{name := Name, method := Method, path := Path, descr := Descr}) ->
    [{name, Name}, {method, Method}, {path, format_path(Path)}, {descr, bin(Descr)}].

%% Remove the :type field.
format_path(Path) ->
    re:replace(Path, <<":[^:]+(:[^/]+)">>, <<"\\1">>, [global, {return, binary}]).

match_route(_Method, _Path, []) ->
    false;
match_route(Method, Path, [Route|Routes]) ->
    case match_route(Method, Path, Route) of
        {ok, Bindings} ->
            {ok, Route#{bindings => Bindings}};
        false ->
            match_route(Method, Path, Routes)
    end;
match_route(Method, Path, #{method := Method, pattern := Pattern}) ->
    match_path(lists:map(fun mochiweb_util:unquote/1, string:tokens(Path, "/")), Pattern, #{});
match_route(_Method, _Path, _Route) ->
    false.

match_path([], [], Bindings) ->
    {ok, Bindings};
match_path([], [_H|_T], _) ->
    false;
match_path([_H|_T], [], _) ->
    false;
match_path([H1|T1], [":" ++ H2|T2], Bindings) ->
    match_path(T1, T2, case string:tokens(H2, ":") of
                           [Type, Name] ->
                               Bindings#{list_to_atom(Name) => parse_var(Type, H1)};
                           [Name] ->
                               Bindings#{list_to_atom(Name) => H1}
                       end);
match_path([H|T1], [H|T2], Bindings) ->
    match_path(T1, T2, Bindings);
match_path(_Path, _Pattern, _Bindings) ->
    false.

parse_params(Req) ->
    parse_params(cowboy_req:method(Req), Req).

parse_params(<<"HEAD">>, Req) ->
    cowboy_req:parse_qs(Req);
parse_params(<<"GET">>, Req) ->
    cowboy_req:parse_qs(Req);
parse_params(_Method, Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, [{PostVals, _}], _} = cowboy_req:read_urlencoded_body(Req),
            jsx:decode(PostVals);
        false ->
            []
    end.

parse_var("atom", S) -> list_to_existing_atom(S);
parse_var("int", S)  -> list_to_integer(S);
parse_var("bin", S)  -> list_to_binary(S).

jsonify(Req, ok) ->
    jsonify(Req, 200, <<"ok">>);
jsonify(Req, {ok, Response}) ->
    jsonify(Req, 200, Response);
jsonify(Req, {error, Reason}) ->
    jsonify(Req, 500, Reason);
jsonify(Req, {Code, Response}) when is_integer(Code) ->
    jsonify(Req, Code, Response);
jsonify(Req, {Code, Headers, Response}) when is_integer(Code) ->
    jsonify(Req, Code, Headers, Response).

jsonify(Req, Code, Response) ->
    jsonify(Req, Code, [], Response).
jsonify(Req, Code, Headers, Response) ->
    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Response), Req).

bin(S) -> iolist_to_binary(S).

bad_req(Req, Text) ->
    cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, Text, Req).