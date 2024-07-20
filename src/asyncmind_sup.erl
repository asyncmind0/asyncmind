%%%-------------------------------------------------------------------
%% @doc asyncmind top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(asyncmind_sup).

-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start() -> application:start(?MODULE).

stop() -> application:stop(?MODULE).

start(_Type, _Args) -> supervisor:start_link({local, asyncmind_sup}, ?MODULE, []).

stop(_State) -> ok.

start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
%% Childspecs is a list that contains specifications for the child
%% processes that will be started by a supervisor process in Erlang. In
%% this specific case, the childspecs are defined in the init/1
%% function of a supervisor module and contain one child specification
%% for an asynchronous process with a module name of "asyncmind".
%%
%% Each child specification includes the following fields:
%%
%% - The name of the child process, in this case "asyncmind".
%% - The start function for the child process, which is "start_link" in the asyncmind module.
%% - The restart strategy, which is "permanent" in this case, meaning that if the child process crashes it will be restarted indefinitely.
%% - The maximum amount of time that the child process is allowed to take to start up, which is 10 seconds in this case.
%% - The type of child, which is "worker" in this case, meaning that it is a process that does work and not merely a supervisor process.
%% - An optional list of arguments that are passed to the child process start function.
%%
%% The ChildSpecs list is returned by the init/1 function as part of the supervisor's initialization process along with a map containing various supervision flags.

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 3, period => 10},
  {ok, Pools} = application:get_env(asyncmind, pools),
  ChildSpecs =
    [
      {
        asyncmind,
        {asyncmind, start_link, [[{local, ?SERVER}, ?MODULE, []]]},
        permanent,
        10000,
        worker,
        [asyncmind]
      },
      {i3, {i3, start_link, [[{local, ?SERVER}, ?MODULE, []]]}, permanent, 10000, worker, [i3]},
      {salt, {salt, start_link, [[{local, ?SERVER}, ?MODULE, []]]}, permanent, 10000, worker, []},
      {
        dunst_notifier_system,
        {salt, journal_monitor, [system, [<<".*error.*">>]]},
        permanent,
        10000,
        worker,
        []
      },
      {
        dunst_notifier_user,
        {salt, journal_monitor, [user, [<<".*error.*">>]]},
        permanent,
        10000,
        worker,
        []
      }
    ],
  PoolSpecs =
    lists:map(
      fun
        ({Name, SizeArgs, WorkerArgs}) ->
          PoolArgs = [{name, {local, Name}}, {worker_module, Name}] ++ SizeArgs,
          poolboy:child_spec(Name, PoolArgs, WorkerArgs)
      end,
      Pools
    ),
  logger:info("Starting childspec ~p", [ChildSpecs]),
  {ok, {SupFlags, ChildSpecs ++ PoolSpecs}}.

%% internal functions
