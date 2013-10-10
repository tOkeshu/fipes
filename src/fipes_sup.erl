-module(fipes_sup).
-behaviour(supervisor).

-export([start_link/0]). %% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

