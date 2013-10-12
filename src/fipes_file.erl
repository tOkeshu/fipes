-module(fipes_file).

-export([id/1, name/1, type/1, size/1, fipe/1, owner_id/1, owner/1]).
-export([to_tnetstring/1, from_proplist/1, find_by_owner/1, find_by_fipe/1]).
-export([delete/1]).

-record(file, {id       :: binary(),
               name     :: binary(),
               type     :: binary(),
               size     :: integer(),
               fipe     :: binary(),
               owner_id :: binary(),
               owner    :: pid()}).

id(File) ->
    File#file.id.

name(File) ->
    File#file.name.

type(File) ->
    File#file.type.

size(File) ->
    File#file.size.

fipe(File) ->
    File#file.fipe.

owner_id(File) ->
    File#file.owner_id.

owner(File) ->
    File#file.owner.

to_tnetstring(File) ->
    {struct, [{id,    File#file.id},
              {name,  File#file.name},
              {type,  File#file.type},
              {size,  File#file.size},
              {owner, File#file.owner_id}]}.

from_proplist(FileInfos) ->
    #file{id       = proplists:get_value(id,       FileInfos),
          name     = proplists:get_value(name,     FileInfos),
          type     = proplists:get_value(type,     FileInfos),
          size     = proplists:get_value(size,     FileInfos),
          fipe     = proplists:get_value(fipe,     FileInfos),
          owner_id = proplists:get_value(owner_id, FileInfos),
          owner    = proplists:get_value(owner,    FileInfos)}.

find_by_owner(Uid) ->
    % Find the user's files
    Match = #file{id       = '_',
                  name     = '_',
                  type     = '_',
                  size     = '_',
                  fipe     = '_',
                  owner_id = Uid,
                  owner    = '_'},
    Files = ets:match_object(files, {{'_', '_'}, Match}),
    [File || {{_Fipe, _FileId}, File} <- Files].

find_by_fipe(Fipe) ->
    Objects = ets:match_object(files, {{Fipe, '_'}, '_'}),
    Files = [File || {{_Fipe, _FileId}, File} <- Objects],
    Files.


delete(File) ->
    ets:delete(files, {fipe(File), id(File)}).

