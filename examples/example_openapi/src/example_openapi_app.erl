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
    Dispatch = [
        {"/", cowboy_static, {priv_file, ?APP, "www/index.html"}},
        {"/static/[...]", cowboy_static, {priv_dir, ?APP, "www/static"}}
    ],
    GlobalSpec = #{
        openapi => "3.0.0",
        info => #{title => "Minirest Demo API", version => "1.0"},
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
        modules => minirest_api:find_api_modules([example_openapi]),
        authorization => Authorization,
        security => [#{application => []}],
        swagger_global_spec => GlobalSpec,
        dispatch => Dispatch
    },
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
