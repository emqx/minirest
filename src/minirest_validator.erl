%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------

-module(minirest_validator).
-include("include/minirest.hrl").
-export([validate/2,
         required/2,
         range/3,
         oneof/2,
         validate_value/2
        ]).

%% ----------------------------------------------------
%% API functions
%% ----------------------------------------------------

%% validate/1 throw error msg must be binary
-spec(validate(Parameters::list(), LightWeightRequest::map()) -> ok
                                                | binary()
                                                | list()).
validate(Parameters, LightWeightRequest) ->
    % throw(<<"_Parameters invalid">>),
    minirest:pipeline([
        fun required/2
    ],
    Parameters, LightWeightRequest).
% #{bindings => #{},body => #{},
%   headers =>
%       #{<<"content-type">> => <<"application/json">>,
%         <<"host">> => <<"127.0.0.1:9990">>,
%         <<"user-agent">> => <<"hackney/1.17.4">>},
%   qs =>
%       #{<<"page">> => <<"1">>,<<"size">> => <<"10">>}}

% parameters =>[
%     #{
%         in => "path",
%         name => "page",
%         required => true,
%         schema => #{
%             type => "integer",
%             maximum => 10,
%             minimum => 0
%         }
%     }
% ],

%% https://swagger.io/specification
required([], LightWeightRequest) -> {ok, [], LightWeightRequest};
required(Parameters, LightWeightRequest) when is_list(Parameters) ->
    % ct:print("Parameters => :~p~n", [Parameters]),
    ct:print("LightWeightRequest => :~p~n", [LightWeightRequest]),
    F = fun(#{in := In, name := Name,
              required := Required,
              schema := Schema} = Parameter) ->
              ct:print("Parameter >=> :~p~n", [Parameter])
        end,
    lists:foreach(F, Parameters),
    {ok, Parameters, LightWeightRequest}.

get_req_param_value("path", Name) -> ok;
    
get_req_param_value("query", Name) -> ok.




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
