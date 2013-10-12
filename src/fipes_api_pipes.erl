-module(fipes_api_pipes).

-export([init/3, handle/2, terminate/3]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).


init({tcp, http}, Req, _Opts) ->
    case cowboy_req:header(<<"upgrade">>, Req) of
        {undefined, Req2} -> {ok, Req2, []};
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_websocket}
    end.


handle(Req, State) ->
    {ok, Req2} = dispatch(Req),
    {ok, Req2, State}.


dispatch(Req) ->
    case cowboy_req:method(Req) of
        {<<"GET">>, Req} ->
            fail(Req);
        {<<"POST">>, Req} ->
            create(Req)
    end.


fail(Req) ->
    case cowboy_req:binding(pipe, Req) of
        %% /fipes => 405 Method Not Allowed
        {undefined, Req2} ->
            cowboy_req:reply(405, [], <<"">>, Req2);
        %% /fipes/:fipe => Only supports websockets
        {_Pipe, Req2} ->
            Message = <<"This resource supports websockets only.">>,
            cowboy_req:reply(400, [], Message, Req2)
    end.


create(Req) ->
    Headers = [{<<"Content-Type">>, <<"application/tnetstrings">>}],
    Result  = tnetstrings:encode({struct, [{id, fipes_utils:token(10)}]}),
    cowboy_req:reply(200, Headers, Result, Req).



terminate(_Reason, _Req, _State) ->
    ok.

%% websockets

websocket_init(_Any, Req, []) ->
    % Send a new uid to the user who opened the fipe.
    self() ! {uid, fipes_utils:token(8)},

    {Fipe, Req} = cowboy_req:binding(pipe, Req),
    Req2 = cowboy_req:compact(Req),
    {ok, Req2, [Fipe, undefined], hibernate}.

websocket_handle({text, Msg}, Req, [Fipe, _Uid]) ->
    Event = tnetstrings:decode(Msg, [{label, atom}]),
    rpc(Fipe, Event),
    {ok, Req, [Fipe, _Uid]};
websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({stream, File, Downloader, Seek}, Req, State) ->
    Event = tnetstrings:encode({struct, [{type, <<"stream">>},
                                         {file, File},
                                         {downloader, Downloader},
                                         {seek, Seek}
                                        ]}),
    {reply, {text, Event}, Req, State, hibernate};
websocket_info({uid, Uid}, Req, [Fipe, undefined]) ->
    true = fipes_owner:register(Fipe, Uid, self()),
    Event = tnetstrings:encode({struct, [{type, <<"uid">>},
                                         {uid, Uid}
                                        ]}),
    {reply, {text, Event}, Req, [Fipe, Uid], hibernate};
websocket_info({new, File}, Req, State) ->
    Event = tnetstrings:encode({struct, [{type, <<"file.new">>},
                                         {file, File}]}),
    {reply, {text, Event}, Req, State, hibernate};
websocket_info({remove, File}, Req, State) ->
    Event = tnetstrings:encode({struct, [{type, <<"file.remove">>},
                                         {file, File}]}),
    {reply, {text, Event}, Req, State, hibernate};
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, [Fipe, Uid]) ->
    [begin
         TNetFile = fipes_file:to_tnetstring(File),
         fipes_owner:notify(Fipe, {remove, TNetFile}),
         fipes_file:delete(File)
     end || File <- fipes_file:find_by_owner(Uid)],
    true = fipes_owner:unregister(Fipe, Uid),
    ok.


rpc(Fipe, {struct, Event}) ->
    Type = proplists:get_value(type, Event),
    rpc(Fipe, Type, Event).

rpc(Fipe, <<"chunk">>, Event) ->
    Payload    = proplists:get_value(payload, Event),
    Uid        = proplists:get_value(downloader, Event),

    [{{Fipe, Uid}, Downloader}] = ets:lookup(downloaders, {Fipe, Uid}),
    Downloader ! {chunk, base64:decode(Payload)};
rpc(Fipe, <<"eos">>, Event) ->
    Uid = proplists:get_value(downloader, Event),
    [{{Fipe, Uid}, Downloader}] = ets:lookup(downloaders, {Fipe, Uid}),
    Downloader ! {chunk, eos};
rpc(_Fipe, _AnyType, _Event) ->
    ok.

