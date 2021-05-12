%% --------------------------------------------------------------------
%% @author: wwhai
%% --------------------------------------------------------------------

-module(minirest_app).
-behaviour(application).
-define(APP, minirest).
-export([start/2, stop/1]).
%%
start(_StartType, _StartArgs) ->
    minirest_sup:start_link().

stop(_State) -> ok.

% load_i18n() ->
%     PrivDir = code:priv_dir(?APP),
%     {ok, Files} = file:list_dir(PrivDir),
%     lists:foldl(fun(Filename, Acc) ->
%         Len = string:length(Filename),
%         case Len > 4 of
%             true ->
%                 Type = string:sub_string(Filename, Len - 3, Len),
%                 case Type == ".ini" of
%                     true -> Acc ++ [Filename];
%                     false -> Acc
%                 end;
%             false -> Acc
%         end
%     end, [], Files).

