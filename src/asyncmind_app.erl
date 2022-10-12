%%%-------------------------------------------------------------------
%% @doc asyncmind public API
%% @end
%%%-------------------------------------------------------------------

-module(asyncmind_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, _} = application:ensure_all_started(gun),
  {ok, _} = application:ensure_all_started(erlexec),
  exec:start(),
  asyncmind_sup:start_link().


stop(_State) -> ok.

%% internal functions
