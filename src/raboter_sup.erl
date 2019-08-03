%%%-------------------------------------------------------------------
%% @doc raboter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(raboter_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = permanent,
  Shutdown = 2000,

  Son1 = {raboter, {raboter, start_link, []},
    Restart, Shutdown, worker, [raboter]},
  {ok, {SupFlags, [Son1]}}.

%% internal functions
