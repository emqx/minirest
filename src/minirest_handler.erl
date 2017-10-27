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
                || Module <- modules(Config), {rest_api, [API = #{path := Path}]}
                   <- Module:module_info(attributes)]),
    {?MODULE, dispatch, [Routes]}.

modules(Config) ->
    lists:foldl(fun(App, Acc) ->
                    {ok, Mods} = application:get_key(App, modules),
                    lists:append(Mods, Acc)
                end, maps:get(modules, Config, []), maps:get(apps, Config, [])).

%% Get API List
dispatch("/", Req, Routes) ->
    case Req:get(method) of
        'GET' ->
            jsonify(Req, 200, [format_route(Route) || Route <- Routes]);
        _ ->
            Req:respond(400, [{"Content-Type", "text/plain"}], <<"Bad Request">>)
    end;

%% Dispatch request to REST APIs
dispatch(Path, Req, Routes) ->
    case catch match_route(Req:get(method), Path, Routes) of
        {ok, #{module := Mod, func := Fun, bindings := Bindings}} ->
            case catch parse_params(Req) of
                {'EXIT', Reason} ->
                    error_logger:error_msg("Params error: ~p", [Reason]),
                    Req:respond(400, [{"Content-Type", "text/plain"}], <<"Bad Request">>);
                Params ->
                    jsonify(Req, erlang:apply(Mod, Fun, [Bindings, Params]))
            end;
        {'EXIT', {badarg, _}} ->
            Req:not_found();
        false -> Req:not_found()
    end.

format_route(#{name := Name, method := Method,path := Path, descr := Descr}) ->
    [{name, Name}, {method, Method}, {path, bin(Path)}, {descr, bin(Descr)}].

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
    match_path(string:tokens(Path, "/"), Pattern, #{});
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
    parse_params(Req:get(method), Req).

parse_params('HEAD', Req) ->
    Req:parse_qs();
parse_params('GET', Req) ->
    Req:parse_qs();
parse_params(_Method, Req) ->
    Req:parse_post().

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
    Req:respond({Code, [{"Content-type", "application/json"}|Headers], jsx:encode(Response)}).

bin(S) -> iolist_to_binary(S).

