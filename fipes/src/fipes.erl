-module(fipes).
-behaviour(application).

-export([start/0, start/2, stop/1]).

-define(PUBLIC, [<<"public">>]).


start() ->
    application:start(fipes).


start(_Type, _Args) ->
    Routes =
        [{'_', [cowboy_static:rule([{dir, ?PUBLIC}, {prefix, [<<"static">>]}]),
                cowboy_static:rule([{dir, ?PUBLIC}, {prefix, []}])
               ]}],

    cowboy:start_listener(http,100,
                          cowboy_tcp_transport, [{port, 8080}],
                          cowboy_http_protocol, [{dispatch, Routes}]
                         ),
    cowboy:start_listener(https, 100,
                          cowboy_ssl_transport, [{port,     8443},
                                                 {certfile, "priv/ssl/cert.pem"},
                                                 {keyfile,  "priv/ssl/key.pem"},
                                                 {password, "cowboy"}],
                          cowboy_http_protocol, [{dispatch, Routes}]),
    fipes_sup:start_link().


stop(_State) ->
    ok.

