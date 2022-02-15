%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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
%%--------------------------------------------------------------------
-module(example_pets_api).

-behavior(minirest_api).

-export([api_spec/0]).

-export([ pets/2
        , pet/2]).

api_spec() ->
    {
        [pets_api(), pets_name_api()],
        [pet_schema()]
    }.

pets_api() ->
    Metadata = #{
        post => #{
            description => "new pets",
            'requestBody' => #{
                content => #{
                    'application/json' => #{
                        schema => minirest:ref(<<"pet">>)}}},
            responses => #{
                <<"200">> => #{description => "new pet"}}},
        get => #{
            description => "list pets",
            responses => #{
                <<"200">> => #{
                    content => #{
                        'application/json' => #{
                            schema => #{
                                type => array,
                                items => minirest:ref(<<"pet">>)}}}}}}},
    {"/pets", Metadata, pets}.

pets_name_api() ->
    Path = "/pets/:pet_name",
    Metadata = #{
        get => #{
            description => "get pet by id",
            parameters => [#{
                name => pet_name,
                in => path,
                required => true,
                schema => #{type => string},
                example => <<"Calorie">>
            }],
            responses => #{
                '200' => #{
                    content => #{
                        'application/json' => #{
                            schema => minirest:ref(<<"pet">>)}}},
                '404' => #{
                    description => <<"pet name not found">>}}},
        delete => #{
            description => "remove pets",
            parameters => [#{
                name => pet_name,
                required => true,
                in => path,
                schema => #{ type => string},
                example => <<"Calorie">>
            }],
            responses => #{
                '200' => #{description => "Remove pet ok"}}},
        put => #{
            description => "new master",
            parameters => [#{
                name => pet_name,
                in => path,
                required => true,
                schema => #{type => string},
                example => <<"Calorie">>
            }],
            'requestBody' => #{
                content => #{
                    'application/json' => #{
                        schema => #{
                            type => object,
                            properties => #{
                                master => #{
                                    type => string,
                                    example => <<"DDD">>},
                                pet_name => #{
                                    type => string,
                                    example => <<"Calorie">>}}}}}},
            responses => #{
                <<"200">> => #{
                    description => "get a new master ok",
                    content => #{
                        'application/json' => #{
                            schema => minirest:ref(<<"pet">>)}}},
                <<"404">> => #{
                    description => "pet name not found"}}}},
    {Path, Metadata, pet}.

pet_schema() ->
    #{pet => #{
        type => object,
        properties => #{
            name => #{
                type => string,
                description => <<"Pet name, Calorie, LiBai and BaiYe">>,
                example => <<"Calorie">>},
            animal => #{
                type => string,
                enum => [dog, cat],
                default => cat,
                description => <<"Pet type">>},
            master => #{
                type => string,
                description => <<"Master name">>,
                example => <<"Shawn">>}}}}.

pets(post, Params = #{body := Pet}) ->
    Name = maps:get(<<"name">>, Pet),
    Pets = persistent_term:get(pets, #{}),
    NPets = maps:put(Name, Pet, Pets),
    persistent_term:put(pets, NPets),
    Response = {200},
    minirest:reply(Response, Params);

pets(get, Params) ->
    StatusCode = 200,
    Headers = #{<<"Content-Type">> => <<"application/json">>},
    Pets = persistent_term:get(pets, #{}),
    Body = jsx:encode(maps:values(Pets)),
    Response = {StatusCode, Headers, Body},
    minirest:reply(Response, Params).

pet(put, Params = #{body := PetMaster}) ->
    Name = maps:get(<<"name">>, PetMaster),
    NewMaster = maps:get(<<"master">>, PetMaster),
    Pets0 = persistent_term:get(pets, #{}),
    Response =
        case maps:get(Name, Pets0, no_found) of
            no_found ->
                {404};
            Pet0 ->
                Pet = maps:put(<<"master">>, NewMaster, Pet0),
                Pets = maps:put(Name, Pet, Pets0),
                persistent_term:put(pets, Pets),
                {200, Pets}
        end,
    minirest:reply(Response, Params);

pet(get, Params = #{bindings := #{pet_name := PetName}}) ->
    PetName = cowboy_req:binding(pet_name, Request),
    Pets = persistent_term:get(pets, #{}),
    Response =
        case maps:get(PetName, Pets, no_found) of
            no_found ->
                {404};
            Pet ->
                Headers = #{<<"Content-Type">> => <<"application/json">>},
                Body = jsx:encode(Pet),
                {200, Headers, Body}
        end,
    minirest:reply(Response, Params);

pet(delete, Params = #{bindings := #{pet_name := PetName}}) ->
    PetName = cowboy_req:binding(pet_name, Request),
    Pets = persistent_term:get(pets, #{}),
    NPets = maps:remove(PetName, Pets),
    persistent_term:put(pets, NPets),
    Response = 200,
    minirest:reply(Response, Params).

%%==============================================================================================
%% internal
