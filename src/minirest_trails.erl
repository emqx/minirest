-module(minirest_trails).

-define(API_SPEC, api_spec).

-export([get_trails/2]).

get_trails(Modules, RootPath) ->
    Fun = fun(Module, CurrentTrails) -> CurrentTrails ++ module_trails(Module, RootPath) end,
    lists:foldl(Fun, [], Modules).

module_trails(Module, RootPath) ->
    try
        OpenApis = erlang:apply(Module, ?API_SPEC, []),
        Fun =
            fun({Path, Metadata, CBState}, Trails) ->
                Trails ++ handler_trails(Module, Path, Metadata, CBState, RootPath)
            end,
        lists:foldl(Fun, [], OpenApis)
    catch E:R:S ->
        io:format("Callback Module ~p, ~p ~p ~p", [Module, E, R, S])
    end.

handler_trails(CBModule, Path, Metadata, CBState, RootPath) ->
    HandlerState = minirest_handler:init_state(CBModule, Metadata, CBState),
    [trails:trail(RootPath ++ Path, minirest_handler, HandlerState, Metadata)].
