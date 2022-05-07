%% Copyright (c) 2013-2022 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(minirest_message_encoder).

-include("minirest_http.hrl").

-export([ encode/1
        , encode/2
        ]).

encode({message, Message}) -> encode(Message);
encode(Message) ->
    Config = #{depth => unlimited, single_line => true},
    encode(Message, Config).

encode(Message, Config) ->
    JsonReady = best_effort_json_obj(Message, Config),
    {ok, ?DEFAULT_RESPONSE_HEADERS, jsx:encode(JsonReady)}.

best_effort_json_obj(List, Config) when is_list(List) ->
    try
        json_obj(maps:from_list(List), Config)
    catch
        _:_ ->
            [json(I, Config) || I <- List]
    end;
best_effort_json_obj(Map, Config) ->
    try
        json_obj(Map, Config)
    catch
        _:_ ->
            do_format_msg("~p", [Map], Config)
    end.

json_obj(Data, Config) ->
    maps:fold(fun(K, V, D) -> json_kv(K, V, D, Config) end, maps:new(), Data).

json_kv(mfa, {M, F, A}, Data, _Config) ->
    maps:put(
        mfa,
        <<
            (atom_to_binary(M, utf8))/binary,
            $:,
            (atom_to_binary(F, utf8))/binary,
            $/,
            (integer_to_binary(A))/binary
        >>,
        Data
    );
%% snabbkaffe
json_kv('$kind', Kind, Data, Config) ->
    maps:put(msg, json(Kind, Config), Data);
json_kv(gl, _, Data, _Config) ->
    %% drop gl because it's not interesting
    Data;
json_kv(file, _, Data, _Config) ->
    %% drop 'file' because we have mfa
    Data;
json_kv(K0, V, Data, Config) ->
    K = json_key(K0),
    case is_map(V) of
        true -> maps:put(json(K, Config), best_effort_json_obj(V, Config), Data);
        false -> maps:put(json(K, Config), json(V, Config), Data)
    end.

json_key(A) when is_atom(A) -> json_key(atom_to_binary(A, utf8));
json_key(Term) ->
    try unicode:characters_to_binary(Term, utf8) of
        OK when is_binary(OK) andalso OK =/= <<>> ->
            OK;
        _ ->
            throw({badkey, Term})
    catch
        _:_ ->
            throw({badkey, Term})
    end.

json([], _) ->
    "[]";
json(<<"">>, _) ->
    "\"\"";
json(A, _) when is_atom(A) -> atom_to_binary(A, utf8);
json(I, _) when is_integer(I) -> I;
json(F, _) when is_float(F) -> F;
json(P, C) when is_pid(P) -> json(pid_to_list(P), C);
json(P, C) when is_port(P) -> json(port_to_list(P), C);
json(F, C) when is_function(F) -> json(erlang:fun_to_list(F), C);
json(B, Config) when is_binary(B) ->
    best_effort_unicode(B, Config);
json(L, Config) when is_list(L), is_integer(hd(L)) ->
    best_effort_unicode(L, Config);
json(M, Config) when is_list(M), is_tuple(hd(M)), tuple_size(hd(M)) =:= 2 ->
    best_effort_json_obj(M, Config);
json(L, Config) when is_list(L) ->
    [json(I, Config) || I <- L];
json(Map, Config) when is_map(Map) ->
    best_effort_json_obj(Map, Config);
json(Term, Config) ->
    do_format_msg("~p", [Term], Config).

best_effort_unicode(Input, Config) ->
    try unicode:characters_to_binary(Input, utf8) of
        B when is_binary(B) -> B;
        _ -> do_format_msg("~p", [Input], Config)
    catch
        _:_ ->
            do_format_msg("~p", [Input], Config)
    end.

do_format_msg(Format0, Args, #{depth := Depth, single_line := SingleLine}) ->
    Format1 = io_lib:scan_format(Format0, Args),
    Format = reformat(Format1, Depth, SingleLine),
    Text0 = io_lib:build_text(Format, []),
    Text =
        case SingleLine of
            true -> re:replace(Text0, ",?\r?\n\s*", ", ", [{return, list}, global, unicode]);
            false -> Text0
        end,
    trim(unicode:characters_to_binary(Text, utf8)).

%% Get rid of the leading spaces.
%% leave alone the trailing spaces.
trim(<<$\s, Rest/binary>>) -> trim(Rest);
trim(Bin) -> Bin.

reformat(Format, unlimited, false) ->
    Format;
reformat([#{control_char := C} = M | T], Depth, true) when C =:= $p ->
    [limit_depth(M#{width => 0}, Depth) | reformat(T, Depth, true)];
reformat([#{control_char := C} = M | T], Depth, true) when C =:= $P ->
    [M#{width => 0} | reformat(T, Depth, true)];
reformat([#{control_char := C} = M | T], Depth, Single) when C =:= $p; C =:= $w ->
    [limit_depth(M, Depth) | reformat(T, Depth, Single)];
reformat([H | T], Depth, Single) ->
    [H | reformat(T, Depth, Single)];
reformat([], _, _) ->
    [].

limit_depth(M0, unlimited) ->
    M0;
limit_depth(#{control_char := C0, args := Args} = M0, Depth) ->
    %To uppercase.
    C = C0 - ($a - $A),
    M0#{control_char := C, args := Args ++ [Depth]}.
