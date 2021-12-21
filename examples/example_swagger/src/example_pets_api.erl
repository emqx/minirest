%% Copyright (c) 2013-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(example_pets_api).

-behavior(minirest_api).

-export([api_spec/0]).

-export([ pets/2
        , pet/2]).

api_spec() ->
    {[pets_api(), pets_name_api()], [pet_schema()]}.

pet_schema() ->
    DefinitionName = <<"pet">>,
    DefinitionProperties = #{
    <<"name">> => #{
        type => <<"string">>,
        description => <<"Pet name, Calorie, LiBai and BaiYe">>,
        example => <<"Calorie">>},
    <<"animal">> => #{
        type => <<"string">>,
        enum => [<<"dog">>, <<"cat">>],
        default => <<"cat">>,
        description => <<"Pet type">>},
    <<"master">> => #{
        type => <<"string">>,
        description => <<"Master name">>,
        example => <<"Shawn">>}},
    {DefinitionName, DefinitionProperties}.

pets_api() ->
    Path = "/pets",
    Metadata = #{
        post => #{
            tags => ["pets"],
            description => "new pets",
            parameters => [
                #{
                    name => "pet",
                    in => body,
                    schema => minirest:ref(<<"pet">>)
                }
            ],
            responses => #{
                 <<"200">> => #{description => "new pet"}}},
        get => #{
            tags => ["pets"],
            description => "list pets",
            responses => #{
                <<"200">> => #{
                    schema => #{
                        type => array,
                        items => minirest:ref(<<"pet">>)}}}},
        put => #{
            tags => ["pets"],
            description => "new master",
            parameters =>[
                #{
                    name => newMaster,
                    in => body,
                    schema => #{
                        type => object,
                        properties => #{
                            <<"master">> => #{
                                type => string,
                                default => <<"DDD">>},
                            <<"pet_name">> => #{
                                type => string,
                                default => <<"Calorie">>}}
                    }
                }
            ],
            responses => #{
                <<"200">> => #{
                    description => "get a new master ok"},
                <<"404">> => #{
                    description => "pet name not found"}}}},
    {Path, Metadata, pets}.

pets_name_api() ->
    Path = "/pets/:pet_name",
    Metadata = #{
        get => #{
            tags => ["pets"],
            description => "get pet by id",
            parameters => [#{
                name => pet_name,
                in => path,
                required => true,
                type => string,
                default => <<"Calorie">>}],
            responses => #{
                <<"200">> => #{
                    schema => minirest:ref(<<"pet">>)},
                <<"404">> => #{
                    description => "pet name not found"}}},
        delete => #{
            tags => ["pets"],
            description => "remove pets",
            parameters => [#{
                name => pet_name,
                required => true,
                in => path,
                type => string,
                default => <<"Calorie">>
            }],
            responses => #{<<"200">> => #{description => "remove pet ok"}}}},
    {Path, Metadata, pet}.

pets(post, #{body := Body}) ->
    Name = maps:get(<<"name">>, Body),
    Pets = persistent_term:get(pets, #{}),
    NPets = maps:put(Name, Body, Pets),
    persistent_term:put(pets, NPets),
    {200};

pets(get, _Params) ->
    StatusCode = 200,
    Headers = #{<<"Content-Type">> => <<"application/json">>},
    Pets = persistent_term:get(pets, #{}),
    Body = jsx:encode(maps:values(Pets)),
    {StatusCode, Headers, Body}.

pet(put, #{body := Body}) ->
    Name = maps:get(<<"name">>, Body),
    NewMaster = maps:get(<<"master">>, Body),
    Pets0 = persistent_term:get(pets, #{}),
    case maps:get(Name, Pets0, no_found) of
        no_found ->
            {404};
        Pet0 ->
            Pet = maps:put(<<"master">>, NewMaster, Pet0),
            Pets = maps:put(Name, Pet, Pets0),
            persistent_term:put(pets, Pets),
            {200}
    end;

pet(get, #{bindings := Bindings}) ->
    PetName = maps:get(pet_name, Bindings),
    Pets = persistent_term:get(pets, #{}),
    case maps:get(PetName, Pets, no_found) of
        no_found ->
            {404};
        Pet ->
            Headers = #{<<"Content-Type">> => <<"application/json">>},
            Body = jsx:encode(Pet),
            {200, Headers, Body}
    end;

pet(delete, #{bindings := Bindings}) ->
    PetName = maps:get(pet_name, Bindings),
    Pets = persistent_term:get(pets, #{}),
    NPets = maps:remove(PetName, Pets),
    persistent_term:put(pets, NPets),
    {<<"200">>}.

%%==============================================================================================
%% internal
