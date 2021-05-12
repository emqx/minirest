%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------

-module(minirest_error).
-export([bad_request/2]).
-export([invalid_json/1]).
-export([invalid_params/2]).
-export([not_found/2]).
-export([method_not_support/3]).


bad_request(Request, Message) ->
    bad_request(400, Request, Message).

bad_request(Code, Request, Message) when is_binary(Message) ->
    reply(Code, Message, Request);
bad_request(Code, Request, Message) when is_list(Message) ->
    reply(Code, list_to_binary(Message), Request);
bad_request(Code, Request, Message) when is_atom(Message) ->
    reply(Code, atom_to_binary(Message), Request).

invalid_params(Request, Message) ->
    bad_request(Request, Message).

invalid_json(Request) ->
    bad_request(Request, <<"The request body isn't a valid json">>).

not_found(Request, Path) when is_list(Path) ->
    Message = "Resource: " ++ Path ++" not found!",
    bad_request(404, Request, list_to_binary(Message)).

method_not_support(Request, RequestMethod, Path) when is_list(Path) ->
    Message = "Target path:" ++ Path ++" is not support for method:" ++ atom_to_list(RequestMethod),
    bad_request(Request, list_to_binary(Message)).

reply(Code, Message, Request) ->
    logger:error("~s", [Message]),
    {stop, minirest_req:reply(Code, #{},
     #{message => Message}, Request)}.
