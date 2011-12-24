-module(fipes_files).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


init({_Any, http}, Req, []) ->
    {ok, Req, []}.


handle(Req, State) ->
    {ok, Req2} = dispatch(Req),
    {ok, Req2, State}.


dispatch(Req) ->
    case cowboy_http_req:method(Req) of
        {'GET', Req} ->
            {Fipe, Req} = cowboy_http_req:binding(pipe, Req),
            case cowboy_http_req:binding(file, Req) of
                {undefined, Req} ->
                    index(Fipe, Req);
                {File, Req} ->
                    download(Fipe, File, Req)
            end;
        {'POST', Req} ->
            create(Req)
    end.


index(_Fipe, Req) ->
    Headers = [{<<"Content-Type">>, <<"application/tnetstrings">>}],
    % Files = [{"id": 1, "name": "Pipes_to_infinity.by_Ranjith_Siji.jpg", "size": 365254}]
    Files = <<"77:73:2:id,1:1\#4:name,37:Pipes_to_infinity.by_Ranjith_Siji.jpg,4:size,6:365254#}]">>,
    cowboy_http_req:reply(200, Headers, Files, Req).


download(_Fipe, _File, Req) ->
    cowboy_http_req:reply(405, [], <<"Not Implemented">>, Req).


create(Req) ->
    Headers = [{<<"Content-Type">>, <<"application/tnetstrings">>}],
    % Files = {"id": 1, "name": "Pipes_to_infinity.by_Ranjith_Siji.jpg", "size": 365254}
    File = <<"73:2:id,1:1#4:name,37:Pipes_to_infinity.by_Ranjith_Siji.jpg,4:size,6:365254#}">>,
    cowboy_http_req:reply(200, [], File, Req).



terminate(_Req, _State) ->
    ok.

