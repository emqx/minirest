{application, minirest_example,
    [{description, "Minirest Example app"},
     {vsn, "1"},
     {modules, ['minirest_example_api']},
     {registered, []},
     {applications,
        [kernel, stdlib, cowboy]},
     {mod, {minirest_example_app, []}},
     {env, []}]}.
