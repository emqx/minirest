-module(example_pets_api).

-export([api_spec/0]).

-export([ new/1
        , list/1
        , new_master/1
        , get_pet/1
        , remove/1
    ]).

-spec(api_spec() -> [{Path :: string(), Metadata :: map()}]).
api_spec() ->
    %% add schema before use ref
    add_schema(),
    [pets_api_spec(), pets_name_api_spec()].

pets_api_spec() ->
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

pets_name_api_spec() ->
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
add_schema() ->
    DefinitionName = <<"pet">>,
    DefinitionProperties = #{
    <<"name">> =>
        #{type => <<"string">>
        , description => <<"Pet name">>
        , example => <<"Tom">>},
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
    cowboy_swagger:add_definition(DefinitionName, DefinitionProperties).
