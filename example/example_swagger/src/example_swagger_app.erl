%%%-------------------------------------------------------------------
%% @doc example_swagger public API
%% @end
%%%-------------------------------------------------------------------

-module(example_swagger_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:ensure_all_started(minirest),
    BasePath = "/minirest_example",
    Apps = [example_swagger],
    Authorization = undefined,
    GlobalSpec =
        #{
            swagger => "2.0",
            info => #{title => "minirest example API", version => "v1"},
            basePath => list_to_binary(BasePath)
        },
    Options =
        #{
            port => 8088,
            base_path => BasePath,
            apps => Apps,
            authorization => Authorization,
            swagger_global_spec => GlobalSpec
        },
    minirest:start(?MODULE, Options),
    example_swagger_sup:start_link().

stop(_State) ->
    minirest:stop(?MODULE),
    ok.

%% internal functions
