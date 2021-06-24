-module(minirest_schema_manager).

-define(SCHEMAS_FUNCTION, schema_spec).

-export([new/1]).

new(Modules) when is_list(Modules) ->
    [new(Module) || Module <- Modules],
    ok;

new(Module) when is_atom(Module) ->
    case erlang:function_exported(Module, ?SCHEMAS_FUNCTION, 0) of
        true ->
            Schemas = erlang:apply(Module, ?SCHEMAS_FUNCTION, []),
            [cowboy_swagger:add_definition(Name, Def) || {Name, Def} <- Schemas],
            ok;
        false ->
            ok
    end.
