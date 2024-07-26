%%%-------------------------------------------------------------------
%% @doc asyncmind public API
%% @end
%%%-------------------------------------------------------------------

-module(asyncmind_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([start_phase/3]).

-include_lib("kernel/include/logger.hrl").

start(_StartType, _StartArgs) -> asyncmind_sup:start_link().

get_trails() ->
  Handlers = [erm_http, cowboy_swagger_handler],
  Trails =
    [
      {"/", cowboy_static, {priv_file, asyncmind, "index.html"}},
      {"/ws", websocket_handler, []},
      {"/static/[...]", cowboy_static, {priv_dir, asyncmind, "static"}} | trails:trails(Handlers)
    ],
  trails:store(Trails),
  trails:single_host_compile(Trails).


start_phase(start_trails_http, _StartType, []) ->
  {ok, _} = application:ensure_all_started(gun),
  {ok, _} = application:ensure_all_started(erlexec),
  {ok, _} = application:ensure_all_started(ipfs),
  {ok, _} = application:ensure_all_started(erlcron),
  {ok, _} = application:ensure_all_started(gproc),
  exec:start(),
  Dispatch = get_trails(),
  {ok, WsPort} = application:get_env(asyncmind, ws_port),
  {ok, _} =
    cowboy:start_clear(
      http,
      %[{ip, {0, 0, 0, 0}}, {port, WsPort}],
      [{port, WsPort}],
      #{
        env => #{dispatch => Dispatch},
        stream_handlers => [cowboy_telemetry_h, cowboy_metrics_h, cowboy_stream_h]
      }
    ),
  ?LOG_INFO("Started cowboy.");

start_phase(start_sync, _StartType, []) ->
  logger:info("Starting sync."),
  case init:get_plain_arguments() of
    [_, "shell"] ->
      ?LOG_INFO("Sourc sync enabled.", []),
      sync:go();

    _ ->
      ?LOG_INFO("Sourc sync disabled.", []),
      ok
  end.


stop(_State) ->
  ok = cowboy:stop_listener(http),
  application:stop(gun),
  ok.

%% internal functions
