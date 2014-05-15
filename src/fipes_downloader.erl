-module(fipes_downloader).

-define(TABLE, downloaders).
-export([find/2, register/3, unregister/2]).


find(Fipe, Uid) ->
    [{{Fipe, Uid}, Downloader}] = ets:lookup(?TABLE, {Fipe, Uid}),
    Downloader.

register(Fipe, Uid, Downloader) ->
    ets:insert(?TABLE, {{Fipe, Uid}, Downloader}).

unregister(Fipe, Uid) ->
    ets:delete(?TABLE, {Fipe, Uid}).

