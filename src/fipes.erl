-module(fipes).
-behaviour(application).

-export([start/0, shutdown/1, start/2, stop/1]).

-define(STATIC_OPTIONS, [{directory, <<"./public">>},
                         {mimetypes, [{<<".html">>, [<<"text/html">>]},
                                      {<<".js">>,   [<<"application/javascript">>]},
                                      {<<".css">>,  [<<"text/css">>]},
                                      {<<".png">>,  [<<"image/png">>]},
                                      {<<".gif">>,  [<<"image/gif">>]}]}]).
-define(ROOT_OPTIONS, ?STATIC_OPTIONS ++ [{file, "index.html"}]).
-define(ROUTES, [{"/fipes/:pipe/files/:file", fipes_api_files, []},
                 {"/fipes/:pipe/files",       fipes_api_files, []},
                 {"/fipes/:pipe",             fipes_api_pipes, []},
                 {"/fipes",                   fipes_api_pipes, []},
                 {"/stats",                   fipes_api_stats, []},
                 {"/static/[...]", cowboy_static, ?STATIC_OPTIONS},
                 {"/",             cowboy_static, ?ROOT_OPTIONS}]).


start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(fipes),
    ok.


start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([{'_', ?ROUTES}]),

    {ok, Port} = application:get_env(fipes, port),
    {ok, _Listener} =
        cowboy:start_http(http,
                          100,
                          [{port, Port}],
                          [{env, [{dispatch, Dispatch}]}]),

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

