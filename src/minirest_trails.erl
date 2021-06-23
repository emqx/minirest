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
            fun({Path, Metadata}, Trails) ->
                Trails ++ handler_trails(Module, Path, Metadata, RootPath)
            end,
        lists:foldl(Fun, [], OpenApis)
    catch E:R:S ->
        io:format("Callback Module ~p, ~p ~p ~p", [Module, E, R, S])
    end.

handler_trails(CBModule, Path, Metadata, RootPath) ->
    HandlerState = minirest_handler:init_state(CBModule, Metadata),
    [trails:trail(RootPath ++ Path, minirest_handler, HandlerState, Metadata)].
