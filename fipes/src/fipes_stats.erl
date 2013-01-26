-module(fipes_stats).
-behaviour(gen_server).

-export([start_link/0, subscribe/0, unsubscribe/0, push/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    register(fipes_stats, Pid),
    {ok, Pid}.

subscribe() ->
    gen_server:cast(fipes_stats, {subscribe, self()}).

unsubscribe() ->
    gen_server:cast(fipes_stats, {unsubscribe, self()}).

push(Type, N) ->
    gen_server:cast(fipes_stats, {push , Type, N}).


init(_Args) ->
    Stats = [{'total-data-transfer', 0}],
    Subscribers = [],
    {ok, {Stats, Subscribers}}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unexpected_message}, State}.

handle_cast({subscribe, Subscriber}, {Stats, Subscribers}) ->
    N = proplists:get_value('total-data-transfer', Stats),
    Subscriber ! {event, 'total-data-transfer', N},
    {noreply, {Stats, [Subscriber|Subscribers]}};
handle_cast({unsubscribe, Subscriber}, {Stats, Subscribers}) ->
    {noreply, {Stats, lists:delete(Subscriber, Subscribers)}};
handle_cast({push, Type, N}, {Stats, Subscribers}) ->
    OldN = proplists:get_value(Type, Stats),
    [Subscriber ! {event, Type, OldN + N} || Subscriber <- Subscribers],

    NewStats = lists:keyreplace(Type, 1, Stats, {Type, OldN + N}),
    {noreply, {NewStats, Subscribers}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

