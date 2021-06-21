-module(minirest).

-export([ start/2
        , stop/1]).

start(Name, Options) ->
    Modules = maps:get(modules, Options, []),
    RootPath = maps:get(root_path, Options, ""),
    Trails = minirest_trails:get_trails(Modules, RootPath) ++ trails:trails([cowboy_swagger_handler]),
    trails:store(Trails),
    Dispatch = trails:single_host_compile(Trails),
    RanchOptions  = [{port, maps:get(port, Options)}],
    CowboyOptions = #{ env      => #{dispatch => Dispatch}
                     , compress => true
                     , timeout  => 12000
                     },
    {ok, _} = cowboy:start_clear(Name, RanchOptions, CowboyOptions).

stop(Name) ->
    cowboy:stop_listener(Name).
