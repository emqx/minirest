%% @doc example_openapi public API
%% @end
%%%-------------------------------------------------------------------

-module(example_openapi_app).

-behaviour(application).

-export([start/2, stop/1]).

-export([authorize_appid/1]).

-define(APP, example_openapi).



start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(minirest),
    Authorization = {?MODULE, authorize_appid},
    RanchOptions = #{port => 8088},
    BasePath = "/minirest/example/openapi",
    GlobalSpec = #{
        openapi => "3.0.0",
        info => #{title => "EMQ X API", version => "5.0.0"},
        servers => [#{url => BasePath}],
        components => #{
            schemas => #{},
            securitySchemes => #{
                application => #{
                    type => apiKey,
                    name => "authorization",
                    in => header}}}},
    Minirest = #{
        base_path => BasePath,
        apps => [?APP],
        authorization => Authorization,
        security => [#{application => []}],
        swagger_global_spec => GlobalSpec},
    MinirestOptions = maps:merge(Minirest, RanchOptions),
    minirest:start(?MODULE, MinirestOptions),
    example_openapi_sup:start_link().

stop(_State) ->
    ok.

authorize_appid(Req) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, <<"admin">>, <<"pwd">>} ->
            ok;
        _ ->
            {
                401,
                #{<<"WWW-Authenticate">> => <<"Basic Realm=\"minirest-server\"">>},
                <<"UNAUTHORIZED">>
            }
    end.