%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------
-module(minirest_utils).

-export([make_path_prefix/2,
         lowercase_method/1,
         http_uri_decode/1,
         fetch_params/2,
         match_method_equal/2,
         gen_map_key/3,
         gen_map_key/1,
         match_route/2,
         combine_bindings/2,
         normalize_return_format/1]).
%% --------------------------------------------------------------------
%% Functions
%% --------------------------------------------------------------------

make_path_prefix(Method, Path) when is_atom(Method) ->
    atom_to_list(Method) ++ concat_path(Path);
make_path_prefix(Method, Path) when is_list(Method) ->
    Method ++ concat_path(Path).
concat_path(Path) ->
    ":" ++ ensure_prefix_slash(Path).
ensure_prefix_slash("/" ++ _ = Path) -> Path;
ensure_prefix_slash(Path) -> "/" ++ Path.

match_method_equal(A, B) when is_atom(A) andalso
                        is_atom(B) ->
    string:to_lower(atom_to_list(A))
     ==
    string:to_lower(atom_to_list(B)).


lowercase_method(Req) ->
    list_to_atom(string:to_lower(binary_to_list(cowboy_req:method(Req)))).

http_uri_decode(Params) when is_map(Params) ->
    maps:map(fun(_, V) -> uri_string:normalize(V) end, Params).
%%
gen_map_key(Method, ServerRoot, Path) ->
    FullPath = make_path_prefix(Method, ServerRoot ++ Path),
    gen_map_key(FullPath).
gen_map_key(FullPath) ->
    [M, P, V, NP | Bindings] = string:tokens(FullPath, "/"),
    StringA = M ++ "/" ++ P ++ "/" ++ V ++ "/" ++ NP,
    StringB = lists:foldl(fun(Token, Acc) ->
        case  string:prefix(Token, "$") of
            nomatch -> Acc;
            _ -> Acc ++ "/*"
        end
    end, "", Bindings),
    StringA ++ StringB.
%%
normalize_return_format(T) when is_list(T) -> list_to_binary(T);
normalize_return_format(T) when is_map(T) -> T;
normalize_return_format(T) when is_binary(T) -> T;
normalize_return_format(_) ->
    throw({throw, "Return value type must one of:[list, map, binary]"}).

%%

match_route(RouteKey, ReqPath) ->
  Tk1 = string:tokens(RouteKey, "/"),
  Tk2 = string:tokens(ReqPath, "/"),
  {ok, Len} = match_route(0, Tk1, Tk2),
  (length(Tk1) == (Len)) and (length(Tk2) == (Len)).
match_route(Acc, ["*" | L1], [_ | L2]) ->
  match_route(Acc + 1, L1, L2);
match_route(Acc, [KK | L1], [KK | L2]) ->
  match_route(Acc + 1, L1, L2);
match_route(Acc, _, _) -> {ok, Acc}.

%%
fetch_params(RouteKey, ReqPath) ->
  fetch_params([], string:tokens(RouteKey, "/"), string:tokens(ReqPath, "/")).
fetch_params(Acc, ["*" | L1], [Value | L2]) ->
  fetch_params(Acc ++ [Value], L1, L2);
fetch_params(Acc, [KK | L1], [KK | L2]) ->
  fetch_params(Acc, L1, L2);
fetch_params(Acc, _, _) -> {ok, Acc}.

%%
combine_bindings(Ks, Vs) when length(Ks) == length(Vs)->
  maps:from_list(lists:zip(Ks, Vs));
combine_bindings(_, _) -> #{}.
