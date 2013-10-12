-module(fipes_user).

-export([find/2, register/3, unregister/2]).


find(Fipe, Uid) ->
    [{{Fipe, Uid}, User}] = ets:lookup(users, {Fipe, Uid}),
    User.

register(Fipe, Uid, User) ->
    ets:insert(users, {{Fipe, Uid}, User}).

unregister(Fipe, Uid) ->
    ets:delete(users, {Fipe, Uid}).

