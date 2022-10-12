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

start(_Type, _Args) -> supervisor:start_link({local, contextual_sup}, ?MODULE, []).

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

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 3, period => 10},
  ChildSpecs =
    [
      {
        asyncmind,
        {asyncmind, start_link, [[{local, ?SERVER}, ?MODULE, []]]},
        permanent,
        10000,
        worker,
        [asyncmind]
      }
    ],
  logger:info("Starting childspec ~p", [ChildSpecs]),
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
