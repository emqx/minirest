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

-export([ rest_api/0
        , rest_schema/0]).

-export([ new/1
        , list/1
        , new_master/1
        , get_pet/1
        , remove/1]).

-spec(rest_api() -> [{Path :: string(), Metadata :: map()}]).
rest_api() ->
    [pets_api(), pets_name_api()].

rest_schema() ->
    DefinitionName = <<"pet">>,
    DefinitionProperties = #{
    <<"name">> =>
        #{type => <<"string">>
        , description => <<"Pet name">>
        , example => <<"Calorie, LiBai and BaiYe">>},
    <<"animal">> =>
        #{type => <<"string">>
        , enum => [<<"dog">>, <<"cat">>]
        , default => <<"cat">>
        , description => <<"Pet type">>},
    <<"master">> =>
        #{type => <<"string">>
        , description => <<"Master name">>
        , example => <<"Shawn">>}
    },
    [{DefinitionName, DefinitionProperties}].

pets_api() ->
    Path = "/pets",
    Metadata = #{
        post =>
            #{tags => ["pets"],
             description => "new pets",
             operationId => new,
             requestBody => #{
                 description => "Pet",
                 content => 
                    #{'application/json' =>
                        #{schema => cowboy_swagger:schema(<<"pet">>)}}},
             responses =>
                 #{<<"200">> =>
                     #{description => "new pet"}}},
        get =>
           #{tags => ["pets"],
            description => "list pets",
            operationId => list,
            responses =>
                #{<<"200">> =>
                    #{content =>
                        #{'application/json' =>
                            #{schema =>
                                #{type => array , items => cowboy_swagger:schema(<<"pet">>)}}}}}},
        put =>
            #{tags => ["pets"],
                description => "new master",
                operationId => new_master,
                requestBody => #{
                    description => "Pet with new master",
                    content =>
                    #{'application/json' =>
                        #{schema =>
                            #{properties =>
                                #{name => #{type => string, example => <<"Tom">>}
                                , master => #{type => string, example => <<"DDD">>}}}}}},
                responses =>
                    #{<<"200">> =>
                        #{description => "get a new master ok"}
                    , <<"404">> => #{description => "pet name not found"}}}},
    {Path, Metadata}.

pets_name_api() ->
    Path = "/pets/:pet_name",
    Metadata = #{
        get =>
           #{tags => ["pets"],
            description => "get pet by id",
            operationId => get_pet,
            parameters => [
                #{name => pet_name
                , in => path
                , schema =>
                #{type => string, example => <<"Tom">>}
                }],
            responses =>
                #{<<"200">> =>
                    #{content =>
                    #{'application/json' =>
                        #{schema => cowboy_swagger:schema(<<"pet">>)}}}
                , <<"404">> => #{description => "pet name not found"}}},
        delete =>
            #{tags => ["pets"],
                description => "remove pets",
                operationId => remove,
                parameters => [
                    #{name => pet_name
                    , in => path
                    , schema =>
                    #{type => string, example => <<"Tom">>}
                    }],
                responses =>
                    #{<<"200">> =>
                        #{description => "remove pet ok"}}}},
    {Path, Metadata}.

new(Request) ->
    {ok, Body, _} = cowboy_req:read_body(Request),
    Pet = jsx:decode(Body, [return_maps]),
    Name = maps:get(<<"name">>, Pet),
    Pets = persistent_term:get(pets, #{}),
    NPets = maps:put(Name, Pet, Pets),
    persistent_term:put(pets, NPets),
    {ok}.

list(_Request) ->
    StatusCode = 200,
    Headers = #{<<"Content-Type">> => <<"application/json">>},
    Pets = persistent_term:get(pets, #{}),
    Body = jsx:encode(maps:values(Pets)),
    {StatusCode, Headers, Body}.

new_master(Request) ->
    {ok, Body, _} = cowboy_req:read_body(Request),
    PetMaster = jsx:decode(Body, [return_maps]),
    Name = maps:get(<<"name">>, PetMaster),
    NewMaster = maps:get(<<"master">>, PetMaster),
    Pets0 = persistent_term:get(pets, #{}),
    case maps:get(Name, Pets0, no_found) of
        no_found ->
            {404};
        Pet0 ->
            Pet = maps:put(<<"master">>, NewMaster, Pet0),
            Pets = maps:put(Name, Pet, Pets0),
            persistent_term:put(pets, Pets),
            {ok}
    end.

get_pet(Request) ->
    PetName = cowboy_req:binding(pet_name, Request),
    Pets = persistent_term:get(pets, #{}),
    case maps:get(PetName, Pets, no_found) of
        no_found ->
            {404};
        Pet ->
            Headers = #{<<"Content-Type">> => <<"application/json">>},
            Body = jsx:encode(Pet),
            {ok, Headers, Body}
    end.

remove(Request) ->
    PetName = cowboy_req:binding(pet_name, Request),
    Pets = persistent_term:get(pets, #{}),
    NPets = maps:remove(PetName, Pets),
    persistent_term:put(pets, NPets),
    {<<"200">>}.

%%==============================================================================================
%% internal
