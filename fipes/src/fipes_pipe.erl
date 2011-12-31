-module(fipes_pipe).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).


init({_Any, http}, Req, []) ->
    case cowboy_http_req:header('Upgrade', Req) of
        {undefined, Req2} -> {ok, Req2, []};
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.


handle(Req, State) ->
    {ok, Req2} = dispatch(Req),
    {ok, Req2, State}.


dispatch(Req) ->
    case cowboy_http_req:method(Req) of
        {'GET', Req} ->
            fail(Req);
        {'POST', Req} ->
            create(Req)
    end.


fail(Req) ->
    case cowboy_http_req:binding(pipe, Req) of
        %% /fipes => 405 Method Not Allowed
        {undefined, Req2} ->
            cowboy_http_req:reply(405, [], <<"">>, Req);
        %% /fipes/:fipe => Only supports websockets
        {_Pipe, Req2} ->
            Message = <<"This resource supports websockets only.">>,
            cowboy_http_req:reply(400, [], Message, Req)
    end.


create(Req) ->
    Headers = [{<<"Content-Type">>, <<"application/tnetstrings">>}],
    cowboy_http_req:reply(200, Headers, <<"19:2:id,10:1234567890,}">>, Req).



terminate(_Req, _State) ->
    ok.

%% websockets

websocket_init(_Any, Req, []) ->
    % Send a new uid to the user who opened the fipe.
    self() ! {uid, uid()},

    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, undefined, hibernate}.

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({stream, File, Downloader}, Req, State) ->
    Event = tnetstrings:encode({struct, [{type, <<"stream">>},
                                         {file, File},
                                         {downloader, Downloader}
                                        ]}),
    {reply, {text, Event}, Req, State, hibernate};
websocket_info({uid, Uid}, Req, State) ->
    ets:insert(owners, {Uid, self()}),
    Event = tnetstrings:encode({struct, [{type, <<"uid">>},
                                         {uid, Uid}
                                        ]}),
    {reply, {text, Event}, Req, State, hibernate};
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.


uid() ->
    {Mega, Sec, Micro} = erlang:now(),
    Timestamp = (Mega * 1000000 + Sec) * 1000000 + Micro,
    list_to_binary(integer_to_list(Timestamp, 16)).

