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
    Stats =
        case file:consult("fipes/priv/stats") of
            {error, _} ->
                [{'total-data', 0},
                 {'total-files', 0},
                 {'total-uploads', 0},
                 {'average-size', 0}];
            {ok, [Terms]} ->
                Terms
        end,
    Subscribers = [],
    {ok, {Stats, Subscribers}}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unexpected_message}, State}.

handle_cast({subscribe, Subscriber}, {Stats, Subscribers}) ->
    [Subscriber ! {event, Type, proplists:get_value(Type, Stats)} ||
        Type <- proplists:get_keys(Stats)],
    {noreply, {Stats, [Subscriber|Subscribers]}};
handle_cast({unsubscribe, Subscriber}, {Stats, Subscribers}) ->
    {noreply, {Stats, lists:delete(Subscriber, Subscribers)}};
handle_cast({push, Type = 'average-size', N}, {Stats, Subscribers}) ->
    NbFiles = proplists:get_value('total-files', Stats),
    OldN = proplists:get_value(Type, Stats),
    NewN = case NbFiles of
               1 -> N;
               _Else -> erlang:round(((NbFiles - 1) * OldN + N) / NbFiles)
           end,
    [Subscriber ! {event, Type, NewN} || Subscriber <- Subscribers],

    NewStats = lists:keyreplace(Type, 1, Stats, {Type, NewN}),

    save(NewStats),
    {noreply, {NewStats, Subscribers}};
handle_cast({push, Type, N}, {Stats, Subscribers}) ->
    OldN = proplists:get_value(Type, Stats),
    [Subscriber ! {event, Type, OldN + N} || Subscriber <- Subscribers],

    NewStats = lists:keyreplace(Type, 1, Stats, {Type, OldN + N}),

    save(NewStats),
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


save(State) ->
    file:write_file("fipes/priv/stats", io_lib:fwrite("~p.\n", [State])).

