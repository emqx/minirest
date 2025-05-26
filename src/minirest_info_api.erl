%% Copyright (c) 2013-2022 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% TODO: maybe remove this api after code run-time check
-module(minirest_info_api).

-behavior(minirest_api).

-dialyzer({nowarn_function, set_codes/1}).

%% API Spec
-export([api_spec/0]).

%% API Callback
-export([error_codes/2]).

%% For codes
-export([
    add_codes/1,
    codes/0
]).

api_spec() ->
    {
        [error_codes_api()],
        []
    }.

error_codes_api() ->
    MetaData = #{
        get => #{
            description => "Minirest API Error Codes",
            responses => #{
                200 => #{
                    content => #{
                        'application/json' => #{
                            schema => #{
                                type => array,
                                schema => minirest:ref(minirest_api_error_codes)
                            }
                        }
                    }
                }
            }
        }
    },
    {"/info/error/codes", MetaData, error_codes}.

error_codes(_, _) ->
    Body = #{codes => codes()},
    {200, Body}.

add_codes(undefined) ->
    codes();
add_codes([]) ->
    codes();
add_codes(Codes) when is_list(Codes) ->
    add_codes(Codes, codes()).

add_codes([], NowCodes0) ->
    NowCodes = lists:sort(NowCodes0),
    set_codes(NowCodes);
add_codes([Code | Codes], NowCodes) ->
    case lists:member(Code, NowCodes) of
        true ->
            add_codes(Codes, NowCodes);
        false ->
            add_codes(Codes, [Code | NowCodes])
    end.

set_codes(NewCodes) ->
    case NewCodes == codes() of
        true ->
            {ok, NewCodes};
        false ->
            Schema = error_codes_schema(NewCodes),
            cowboy_swagger:add_definition(Schema),
            {ok, codes()}
    end.

codes() ->
    case application:get_env(cowboy_swagger, global_spec, #{}) of
        #{components := #{schemas := #{minirest_api_error_codes := #{enum := Codes}}}} ->
            Codes;
        _ ->
            []
    end.

error_codes_schema(Codes) ->
    #{
        minirest_api_error_codes => #{
            type => string,
            enum => Codes
        }
    }.
