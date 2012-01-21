-module(fipes_files).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


init({_Any, http}, Req, []) ->
    {ok, Req, []}.


handle(Req, State) ->
    {ok, Req2} = dispatch(Req),
    {ok, Req2, State}.


dispatch(Req) ->
    {Fipe, Req} = cowboy_http_req:binding(pipe, Req),
    case cowboy_http_req:method(Req) of
        {'GET', Req} ->
            case cowboy_http_req:binding(file, Req) of
                {undefined, Req} ->
                    index(Fipe, Req);
                {File, Req} ->
                    download(Fipe, File, Req)
            end;
        {'POST', Req} ->
            create(Fipe, Req)
    end.


index(Fipe, Req) ->
    Headers    = [{<<"Content-Type">>, <<"application/tnetstrings">>}],

    Objects    = ets:match_object(files, {{Fipe, '_'}, '_'}),
    FilesInfos = [{struct, FileInfos} ||
                     {{Fipe, _FileId}, {_Owner, FileInfos}} <- Objects],
    Results    = tnetstrings:encode(FilesInfos, [{label, atom}]),

    cowboy_http_req:reply(200, Headers, Results, Req).


download(Fipe, File, Req) ->
    % Register the downloader
    Uid = uid(),
    ets:insert(downloaders, {{Fipe, Uid}, self()}),

    Name = name(Fipe, File),

    Headers =
        [{<<"Content-Type">>,        <<"application/octet-stream">>},
         {<<"Content-Disposition">>, [<<"attachment; filename=\"">>, Name, <<"\"">>]}
        ],
    {ok, Req2} = cowboy_http_req:chunked_reply(200, Headers, Req),

    % Ask the file owner to start the stream
    owner(Fipe, File) ! {stream, File, Uid},

    % TODO: remove the user from the downloaders table when finished.
    stream(Req2).


owner(Fipe, File) ->
    [{{Fipe, File}, {Uid, _FileInfos}}] = ets:lookup(files, {Fipe, File}),
    [{{Fipe, Uid}, Owner}] = ets:lookup(owners, {Fipe, Uid}),
    Owner.

name(Fipe, File) ->
    [{{Fipe, File}, {_Uid, FileInfos}}] = ets:lookup(files, {Fipe, File}),
    proplists:get_value(name, FileInfos).


stream(Req) ->
    receive
        {chunk, eos} ->
            {ok, Req};
        {chunk, Chunk} ->
            cowboy_http_req:chunk(Chunk, Req),
            stream(Req)
    end.


create(Fipe, Req) ->
    {FileId, Owner, FileInfos} = file_infos(Req),
    true = ets:insert(files, {{Fipe, FileId}, {Owner, FileInfos}}),

    notify(Fipe, FileInfos),

    Headers = [{<<"Content-Type">>, <<"application/tnetstrings">>}],
    Result  = tnetstrings:encode({struct, FileInfos}),
    cowboy_http_req:reply(200, Headers, Result, Req).


file_infos(Req) ->
    FileId = fid(),

    {ok, Body, Req2} = cowboy_http_req:body(Req),
    {struct, FileInfos} = tnetstrings:decode(Body, [{label, atom}]),
    Owner = proplists:get_value(owner, FileInfos),

    {FileId, Owner, [{id, FileId}|FileInfos]}.

notify(Fipe, FileInfos) ->
    [Owner ! {new, FileInfos} || {{Fipe, Uid}, Owner} <- ets:tab2list(owners)],
    ok.


fid() ->
    {Mega, Sec, Micro} = erlang:now(),
    Timestamp = (Mega * 1000000 + Sec) * 1000000 + Micro,
    list_to_binary(integer_to_list(Timestamp, 16)).


terminate(_Req, _State) ->
    ok.


% XXX: duplicated code, see fipes_pipe:uid/0.
uid() ->
    {Mega, Sec, Micro} = erlang:now(),
    Timestamp = (Mega * 1000000 + Sec) * 1000000 + Micro,
    list_to_binary(integer_to_list(Timestamp, 16)).

