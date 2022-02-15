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
    BasePath = "/minirest/example/openapi",
    Authorization = {?MODULE, authorize_appid},
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
    Dispatch = [
        {"/", cowboy_static, {priv_file, ?APP, "www/index.html"}},
        {"/static/[...]", cowboy_static, {priv_dir, ?APP, "www/static"}}
    ],
    RanchOptions = #{
        max_connections => 512,
        num_acceptors => 4,
        socket_opts => [ {send_timeout, 5000}
                    , {port, 8088}
                    , {backlog, 512}]},
    Minirest = #{
        base_path => BasePath,
        modules => minirest_api:find_api_modules([?APP]),
        authorization => Authorization,
        security => [#{application => []}],
        swagger_global_spec => GlobalSpec,
        dispatch => Dispatch,
        protocol => http,
        ranch_options => RanchOptions
    },
    minirest:start(?MODULE, Minirest)
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
