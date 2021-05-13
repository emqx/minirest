%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------
-module(minirest_utils).

-export([make_path_prefix/2,
         lowercase_method/1,
         http_uri_decode/1,
         match_method_equal/2,
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
normalize_return_format(T) when is_list(T) -> list_to_binary(T);
normalize_return_format(T) when is_map(T) -> T;
normalize_return_format(T) when is_binary(T) -> T;
normalize_return_format(_) ->
    throw({throw, "Return value type must one of:[list, map, binary]"}).