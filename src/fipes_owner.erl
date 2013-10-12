-module(fipes_owner).

-export([find/2, register/3, unregister/2, notify/2]).


find(Fipe, Uid) ->
    [{{Fipe, Uid}, User}] = ets:lookup(users, {Fipe, Uid}),
    User.

register(Fipe, Uid, User) ->
    ets:insert(users, {{Fipe, Uid}, User}).

unregister(Fipe, Uid) ->
    ets:delete(users, {Fipe, Uid}).

notify(Fipe, Event) ->
    [Owner ! Event ||
        {{OtherFipe, _Uid}, Owner} <- ets:tab2list(users), OtherFipe == Fipe],
    ok.

