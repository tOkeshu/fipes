-module(fipes).
-behaviour(application).

-export([start/0, shutdown/1, start/2, stop/1]).

-define(PUBLIC, [<<"fipes">>, <<"public">>]).
-define(STATIC_CONF, [{directory, ?PUBLIC},
                      {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]).
-define(ROUTES, [% /fipes/:pipe/:files/:file => fipes_files
                 % /fipes/:pipe/:files       => fipes_files
                 % /fipes/:pipe              => fipes_pipe
                 % /fipes                    => fipes_pipe
                 {[<<"fipes">>, pipe, <<"files">>, file], fipes_files, []},
                 {[<<"fipes">>, pipe, <<"files">>],       fipes_files, []},
                 {[<<"fipes">>, pipe],                    fipes_pipe,  []},
                 {[<<"fipes">>],                          fipes_pipe,  []},

                 % /stats
                 {[<<"stats">>],                          fipes_stats_api, []},

                 % /static/a/b/c             => cowboy_static
                 % /a/b/c                    => cowboy_static
                 {[<<"static">>, '...'], cowboy_static, ?STATIC_CONF},
                 {[], cowboy_static, ?STATIC_CONF ++ [{file, <<"index.html">>}]}]).

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(ranch),
    application:start(cowboy),
    application:start(fipes).


start(_Type, _Args) ->
    {ok, Port} = application:get_env(fipes, port),
    cowboy:start_http(fipes_http_listener, 100,
                      [{port, Port}], [{dispatch, [{'_', ?ROUTES}]}]),

    ets:new(files,       [set, public, named_table]),
    ets:new(owners,      [set, public, named_table]),
    ets:new(downloaders, [set, public, named_table]),
    fipes_stats:start_link(),
    fipes_sup:start_link().

shutdown(Node) ->
    true = net_kernel:connect_node(Node),
    rpc:call(Node, init, stop, []),
    ok.

stop(_State) ->
    ok.

