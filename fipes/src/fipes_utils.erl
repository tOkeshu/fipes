-module(fipes_utils).

-export([token/1]).

token(N) ->
    Token = [io_lib:format("~2.16.0b",[Byte])
             || <<Byte>> <= crypto:strong_rand_bytes(N)],
    iolist_to_binary(Token).
