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
  {ok, _} = application:ensure_all_started(ipfs),
  {ok, _} = application:ensure_all_started(erlcron),
  {ok, _} = application:ensure_all_started(yamerl),
  exec:start(),
  Dispatch =
    cowboy_router:compile(
      [
        {
          '_',
          [
            {"/", cowboy_static, {priv_file, asyncmind, "index.html"}},
            {"/ws", websocket_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, asyncmind, "static"}}
          ]
        }
      ]
    ),
  {ok, WsPort} = application:get_env(asyncmind, ws_port),
  {ok, _} = cowboy:start_clear(http, [{port, WsPort}], #{env => #{dispatch => Dispatch}}),
  asyncmind_sup:start_link().


stop(_State) -> ok = cowboy:stop_listener(http).

%% internal functions
