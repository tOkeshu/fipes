-module(fipes_pipe).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.


handle(Req, State) ->
    Headers = [{<<"Content-Type">>, <<"application/tnetstrings">>}],
    {ok, Req2} = cowboy_http_req:reply(200, Headers, <<"19:2:id,10:1234567890,}">>, Req),
    {ok, Req2, State}.


terminate(_Req, _State) ->
    ok.

