%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------

-module(minirest_validator).
-export([required/2,
         range/3,
         oneof/2,
         validate_value/2
        ]).

%% ----------------------------------------------------
%% API functions
%% ----------------------------------------------------
required(Key, Params) when is_map(Params) ->
    case maps:find(Key, Params) of
        {ok, Value} -> Value;
        error -> throw({throw, "param ~p is required, current params is ~p\n", [Key, Params]})
    end.

range(Value, Min, Max) ->
    case (Value >= Min) and (Value =< Max) of
        true -> ok;
        false -> throw({throw, "value must in range:[~p - ~p]\n"})
    end.

oneof(Value, List) when is_list(List)->
    case lists:member(Value, List) of
        true -> ok;
        false -> throw({throw, "value must one of:~p\n", [List]})
    end.

validate_value(Value, []) ->
    {ok, Value};
validate_value(Value, [Tag | More]) when is_atom(Tag) ->
    Fun = case Tag of
              int -> fun int/1;
              bool -> fun bool/1;
              atom -> fun atom/1;
              nonempty -> fun nonempty/1
          end,
    validate_value(Value, [Fun | More]);
validate_value(Value, [Fun | More]) when is_function(Fun) ->
    case erlang:apply(Fun, [Value]) of
        ok -> validate_value(Value, More);
        {ok, Value1} -> validate_value(Value1, More);
        {error, Reason} -> {error, Reason}
    end.

int(Value) when is_integer(Value) ->
    ok;
int(Value) ->
	try
		{ok, binary_to_integer(Value)}
	catch _:_ ->
		{error, not_integer}
	end.

bool(Value) when is_boolean(Value) ->
    ok;
bool(Value) ->
	try
		{ok, binary_to_atom(Value, utf8)}
	catch _:_ ->
		{error, not_boolean}
	end.

atom(Value) ->
    try
		{ok, binary_to_atom(Value, utf8)}
	catch _:_ ->
		{error, not_atom}
	end.

nonempty(<<>>) ->
	{error, empty};
nonempty(Value) when is_binary(Value) ->
	ok.
