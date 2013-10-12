-module(fipes_owners).

-export([notify/2]).

notify(Fipe, Event) ->
    [Owner ! Event ||
        {{OtherFipe, _Uid}, Owner} <- ets:tab2list(users), OtherFipe == Fipe],
    ok.

