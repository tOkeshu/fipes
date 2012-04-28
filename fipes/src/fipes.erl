-module(fipes).
-behaviour(application).

-export([start/0, start/2, stop/1]).

-define(PUBLIC, [<<"public">>]).


start() ->
    application:start(fipes).


start(_Type, _Args) ->
    Routes =
        [{'_', [{[<<"fipes">>, pipe, <<"files">>, file], fipes_files, []},
                {[<<"fipes">>, pipe, <<"files">>],       fipes_files, []},
                {[<<"fipes">>, pipe],                    fipes_pipe,  []},
                {[<<"fipes">>],                          fipes_pipe,  []},
                {[<<"static">>, '...'],                  cowboy_http_static,
                 [{directory, ?PUBLIC},
                  {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
                {['...'],                                cowboy_http_static,
                 [{directory, ?PUBLIC},
                  {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}
               ]}],

    {ok, Port} = application:get_env(fipes, port),
    cowboy:start_listener(http,100,
                          cowboy_tcp_transport, [{port, Port}],
                          cowboy_http_protocol, [{dispatch, Routes}]
                         ),
    cowboy:start_listener(https, 100,
                          cowboy_ssl_transport, [{port,     Port + 1},
                                                 {certfile, "priv/ssl/cert.pem"},
                                                 {keyfile,  "priv/ssl/key.pem"},
                                                 {password, "cowboy"}],
                          cowboy_http_protocol, [{dispatch, Routes}]),
    ets:new(files,       [set, public, named_table]),
    ets:new(owners,      [set, public, named_table]),
    ets:new(downloaders, [set, public, named_table]),
    fipes_sup:start_link().


stop(_State) ->
    ok.

