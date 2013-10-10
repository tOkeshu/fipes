-module(fipes_files).

-export([init/3, handle/2, terminate/3]).
-export([to_tnestring_struct/1]).

-include("fipes.hrl").


init({tcp, http}, Req, []) ->
    {ok, Req, []}.


handle(Req, State) ->
    {ok, Req2} = dispatch(Req),
    {ok, Req2, State}.


dispatch(Req) ->
    {Fipe, Req} = cowboy_req:binding(pipe, Req),
    case cowboy_req:method(Req) of
        {<<"GET">>, Req} ->
            case cowboy_req:binding(file, Req) of
                {undefined, Req} ->
                    index(Fipe, Req);
                {FileId, Req} ->
                    download(Fipe, FileId, Req)
            end;
        {<<"POST">>, Req} ->
            create(Fipe, Req)
    end.


index(Fipe, Req) ->
    Objects = ets:match_object(files, {{Fipe, '_'}, '_'}),
    Files   = [to_tnestring_struct(File) ||
                  {{_Fipe, _FileId}, File} <- Objects],
    Results = tnetstrings:encode(Files, [{label, atom}]),

    Headers = [{<<"Content-Type">>, <<"application/tnetstrings">>}],
    cowboy_req:reply(200, Headers, Results, Req).


download(Fipe, FileId, Req) ->
    % Register the downloader
    Uid = fipes_utils:token(8),
    ets:insert(downloaders, {{Fipe, Uid}, self()}),

    File = find_file(Fipe, FileId),

    Headers =
        [{<<"Content-Type">>, <<"application/octet-stream">>},
         {<<"Content-Disposition">>, [<<"attachment; filename=\"">>, File#file.name, <<"\"">>]},
         {<<"Access-Control-Allow-Origin">>, <<"*">>},
         % Tell Nginx to not buffer this response
         % http://wiki.nginx.org/X-accel#X-Accel-Buffering
         {<<"X-Accel-Buffering">>, <<"no">>}],
    {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),

    % Ask the file owner to start the stream
    File#file.owner ! {stream, FileId, Uid, 0},

    fipes_stats:push('average-size', File#file.size),
    fipes_stats:push('total-uploads', 1),
    stream(File, Uid, Req2).


find_file(Fipe, FileId) ->
    [{{Fipe, FileId}, File}] = ets:lookup(files, {Fipe, FileId}),
    File.


stream(File, Uid, Req) ->
    receive
        {chunk, eos} ->
            ets:delete(downloaders, {File#file.fipe, Uid}),
            {ok, Req};
        {chunk, FirstChunk} ->
            <<SmallChunk:1/binary, NextCurrentChunk/binary>> = FirstChunk,
            send_chunk(SmallChunk, Req),
            NextSeek = size(FirstChunk),
            File#file.owner ! {stream, File#file.id, Uid, NextSeek},
            stream(File, Uid, NextCurrentChunk, NextSeek, Req)
    end.
stream(File, Uid, CurrentChunk, Seek, Req) ->
    receive
        {chunk, eos} ->
            send_chunk(CurrentChunk, Req),
            ets:delete(downloaders, {File#file.fipe, Uid}),
            {ok, Req};
        {chunk, NextChunk} ->
            send_chunk(CurrentChunk, Req),
            NextSeek = Seek + size(NextChunk),
            File#file.owner ! {stream, File#file.id, Uid, NextSeek},
            stream(File, Uid, NextChunk, NextSeek, Req)
    after
        20000 ->
            <<SmallChunk:1/binary, NexCurrentChunk/binary>> = CurrentChunk,
            send_chunk(SmallChunk, Req),
            stream(File, Uid, NexCurrentChunk, Seek, Req)
    end.

send_chunk(Chunk, Req) ->
    fipes_stats:push('total-data', size(Chunk)),
    cowboy_req:chunk(Chunk, Req).

create(Fipe, Req) ->
    File = file_from_req(Fipe, Req),
    true = ets:insert(files, {{Fipe, File#file.id}, File}),
    TnetFile = to_tnestring_struct(File),
    notify(Fipe, TnetFile),

    Headers = [{<<"Content-Type">>, <<"application/tnetstrings">>}],
    Result  = tnetstrings:encode(TnetFile),
    fipes_stats:push('total-files', 1),
    cowboy_req:reply(200, Headers, Result, Req).


to_tnestring_struct(File) ->
    {struct, [{id,    File#file.id},
              {name,  File#file.name},
              {type,  File#file.type},
              {size,  File#file.size},
              {owner, File#file.owner_id}]}.

file_from_req(Fipe, Req) ->
    FileId = fipes_utils:token(2),

    {ok, Body, _Req2} = cowboy_req:body(Req),
    {struct, FileInfos} = tnetstrings:decode(Body, [{label, atom}]),

    Uid = proplists:get_value(owner, FileInfos),
    [{{Fipe, Uid}, Owner}] = ets:lookup(owners, {Fipe, Uid}),

    Name = proplists:get_value(name, FileInfos),
    Type = proplists:get_value(type, FileInfos),
    Size = proplists:get_value(size, FileInfos),
    File = #file{id=FileId,
                 name=Name,
                 type=Type,
                 size=Size,
                 fipe=Fipe,
                 owner_id=Uid,
                 owner=Owner},
    File.

notify(Fipe, File) ->
    [Owner ! {new, File} ||
        {{OtherFipe, _Uid}, Owner} <- ets:tab2list(owners), OtherFipe == Fipe],
    ok.


terminate(_Reason, _Req, _State) ->
    ok.

