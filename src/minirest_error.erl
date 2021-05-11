%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------

-module(minirest_error).
-export([bad_request/2]).
-export([not_found/2]).
-export([method_not_support/3]).

bad_request(Request, Message) when is_binary(Message) ->
    logger:error("~s", [Message]),
    {stop, minirest_req:reply(400, #{}, #{message => Message}, Request)}.

not_found(Request, Path) when is_list(Path) ->
    Message = "Resource:" ++ Path ++" not found!",
    logger:error("~s", [Message]),
    {stop, minirest_req:reply(404, #{}, #{message => list_to_binary(Message)}, Request)}.

method_not_support(Request, RequestMethod, Path) when is_list(Path) ->
    Message = "Target path:" ++ Path ++" is not support for method:" ++ atom_to_list(RequestMethod),
    logger:error("~s", [Message]),
    {stop, minirest_req:reply(404, #{}, #{message => list_to_binary(Message)}, Request)}.
