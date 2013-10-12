-module(fipes_owner).

-define(TABLE, owners).
-export([find/2, register/3, unregister/2, notify/2]).


find(Fipe, Uid) ->
    [{{Fipe, Uid}, Owner}] = ets:lookup(?TABLE, {Fipe, Uid}),
    Owner.

register(Fipe, Uid, Owner) ->
    ets:insert(?TABLE, {{Fipe, Uid}, Owner}).

unregister(Fipe, Uid) ->
    ets:delete(?TABLE, {Fipe, Uid}).

notify(Fipe, Event) ->
    Objects = ets:match_object(?TABLE, {{Fipe, '_'}, '_'}),
    [Owner ! Event || {{_Fipe, _Uid}, Owner} <- Objects].


