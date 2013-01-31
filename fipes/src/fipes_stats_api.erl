-module(fipes_stats_api).

-export([init/3, handle/2, terminate/2]).
-include("fipes.hrl").


init({tcp, http}, Req, []) ->
    %% timer:apply_interval(1000, fipes_stats, push, ['total-data-transfer', 3]),
    {ok, Req, []}.


handle(Req, State) ->
    {ok, Req2} = dispatch(Req),
    {ok, Req2, State}.


dispatch(Req) ->
    case cowboy_req:method(Req) of
        {<<"GET">>, Req} ->
            Headers = [{<<"Content-Type">>, <<"text/event-stream">>}],
            {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
            fipes_stats:subscribe(),
            stream(Req2);
        {<<"POST">>, Req} ->
            not_allowed(Req)
    end.

stream(Req) ->
    receive
        {event, Type, N} ->
            SSE = [<<"event: ">>, atom_to_list(Type), <<"\n">>,
                   <<"data: ">>, integer_to_list(N), <<"\n\n">>],
            ok = cowboy_req:chunk(SSE, Req),
            stream(Req);
        _ ->
            stream(Req)
    end.

not_allowed(Req) ->
    {ok, Req}.

terminate(_Req, _State) ->
    fipes_stats:unsubscribe(),
    ok.

