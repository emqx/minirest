-module(minirest_utils).

-export([redact/1]).

-define(REDACT_VAL, "******").

redact(L) when is_list(L) ->
    lists:map(fun redact/1, L);
redact(M) when is_map(M) ->
    maps:map(fun(K, V) ->
                     redact(K, V)
             end, M);
redact({Key, Value}) ->
    case is_sensitive_key(Key) of
        true ->
            {Key, redact_v(Value)};
        false ->
            {redact(Key), redact(Value)}
    end;
redact(T) when is_tuple(T) ->
    Elements = erlang:tuple_to_list(T),
    Redact = redact(Elements),
    erlang:list_to_tuple(Redact);
redact(Any) ->
    Any.

redact(K, V) ->
    case is_sensitive_key(K) of
        true ->
            redact_v(V);
        false ->
            redact(V)
    end.

redact_v(V) when is_binary(V) -> <<?REDACT_VAL>>;
redact_v(_V) -> ?REDACT_VAL.

is_sensitive_key(ws_cookie) -> true;
is_sensitive_key("ws_cookie") -> true;
is_sensitive_key(<<"ws_cookie">>) -> true;
is_sensitive_key(token) -> true;
is_sensitive_key("token") -> true;
is_sensitive_key(<<"token">>) -> true;
is_sensitive_key(password) -> true;
is_sensitive_key("password") -> true;
is_sensitive_key(<<"password">>) -> true;
is_sensitive_key(secret) -> true;
is_sensitive_key("secret") -> true;
is_sensitive_key(<<"secret">>) -> true;
is_sensitive_key(passcode) -> true;
is_sensitive_key("passcode") -> true;
is_sensitive_key(<<"passcode">>) -> true;
is_sensitive_key(passphrase) -> true;
is_sensitive_key("passphrase") -> true;
is_sensitive_key(<<"passphrase">>) -> true;
is_sensitive_key(key) -> true;
is_sensitive_key("key") -> true;
is_sensitive_key(<<"key">>) -> true;
is_sensitive_key(aws_secret_access_key) -> true;
is_sensitive_key("aws_secret_access_key") -> true;
is_sensitive_key(<<"aws_secret_access_key">>) -> true;
is_sensitive_key(secret_key) -> true;
is_sensitive_key("secret_key") -> true;
is_sensitive_key(<<"secret_key">>) -> true;
is_sensitive_key(bind_password) -> true;
is_sensitive_key("bind_password") -> true;
is_sensitive_key(<<"bind_password">>) -> true;
is_sensitive_key(_) -> false.
