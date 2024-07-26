-module(asyncmind).

-author("Steven Joseph <steven@stevenjoseph.in>").

-behaviour(gen_server).

-export([start_link/1]).
-export([show_dmenu/1]).
-export([kubectl/1]).
-export([browser/1]).
-export([lnd/1]).
-export([dnd/1, pactl_list_sinks/0]).
-export([toggle_window_screen/2]).
-export([new_client/0]).
-export([terminal/1]).
-export([tmux/1]).
-export(
  [
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    run/2,
    wait_cancel/1,
    gun_get/1
  ]
).

-include_lib("kernel/include/logger.hrl").

start_link(Args) ->
  Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
  io:format("start_link: ~p~p~n", [Return, Args]),
  Return.


init([]) ->
  State = [],
  Return = {ok, State},
  io:format("init: ~p~n", [State]),
  Return.


pactl_list_sinks() ->
  {ok, [{stdout, StdoutJson}]} = exec:run("pactl --format=json -- list sinks\n", [stdout, sync]),
  [
    maps:get(index, X)
    || X <- jsx:decode(binary:list_to_bin(StdoutJson), [{labels, atom}, return_maps])
  ].


pactl_mute_all() -> pactl_set_sinks_mute(" 1").

pactl_unmute_all() -> pactl_set_sinks_mute(" 0").

pactl_set_sinks_mute(Mute) ->
  [
    {ok, []} =
      exec:run(
        lists:concat(["pactl --format=json -- set-sink-mute ", integer_to_list(X), Mute]),
        [sync]
      )
      || X <- pactl_list_sinks()
  ].

pactl_get_sinks_mute() ->
  Results =
    [
      exec:run(lists:concat(["pactl get-sink-mute ", integer_to_list(X)]), [sync, stdout])
      || X <- pactl_list_sinks()
    ],
  Res = [fast_yaml:decode(R) || {ok, [{stdout, [R]}]} <- Results],
  [binary_to_atom(S) || {ok, [[{<<"Mute">>, S}]]} <- Res].


% Check if the current workspace is the given name
i3_is_current_workspace(Name) ->
  case
  os:cmd("i3-msg -t get_workspaces | jq '.[] | select(.focused==true).name==\"" ++ Name ++ "\"'") of
    "true\n" -> true;
    _ -> false
  end.


handle_call({day, on}, _From, _State) ->
  Results = exec:run(["/usr/sbin/xset", "-dpms"], [sync, {env, [{"DISPLAY", ":0.0"}]}]),
  ?LOG_INFO("day on ~p~n", [Results]);

handle_call({night, on}, _From, _State) ->
  Results = exec:run(["/usr/sbin/xset", "+dpms"], [sync, {env, [{"DISPLAY", ":0.0"}]}]),
  ?LOG_INFO("night on ~p~n", [Results]);

handle_call({dnd, on}, _From, State) ->
  RunOpts = [sync, stderr, {env, [{"DISPLAY", ":0.0"}]}],
  ScreenSaver = exec:run(["/usr/sbin/xset", "s", "60", "180"], RunOpts),
  logger:info("dnd on xset screensaver ~p~n", [ScreenSaver]),
  Dpms = exec:run(["/usr/sbin/xset", "dpms", "600", "1800"], RunOpts),
  logger:info("dnd on xset dpms ~p~n", [Dpms]),
  SpotifyStop =
    exec:run(
      [
        "/usr/sbin/dbus-send",
        "--print-reply",
        "--dest",
        "org.mpris.MediaPlayer2.spotify",
        "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
      ],
      RunOpts
    ),
  logger:info("dnd on spotify stop ~p~n", [SpotifyStop]),
  exec:run(["/usr/sbin//notify-send", "dnd on"], RunOpts),
  Results = pactl_mute_all(),
  logger:info("dnd on ~p~n", [Results]),
  {reply, Results, State};

handle_call({dnd, off}, _From, State) ->
  ScreenSaver =
    exec:run(["/usr/sbin/xset", "s", "3600", "8800"], [sync, {env, [{"DISPLAY", ":0.0"}]}]),
  logger:info("dnd off xset screensaver ~p~n", [ScreenSaver]),
  Dpms =
    exec:run(["/usr/sbin/xset", "dpms", "10600", "10800"], [sync, {env, [{"DISPLAY", ":0.0"}]}]),
  logger:info("dnd off xset dpms ~p~n", [Dpms]),
  Results = pactl_unmute_all(),
  logger:info("dnd off ~p~n", [Results]),
  {reply, Results, State};

handle_call({dnd, state}, _From, State) ->
  Results = pactl_get_sinks_mute(),
  logger:info("dnd state ~p~n", [Results]),
  Res =
    case lists:all(fun (no) -> true; (_) -> false end, Results) of
      true -> off;
      false -> on
    end,
  {reply, Res, State};

handle_call({browser, Url, Profile}, _From, State) ->
  %exec:run(["/usr/sbin/chromium", "--user-data-dir=/home/steven/.config/chromium.home", Url], []),
  Results =
    exec:run(
      ["/usr/sbin/brave", "-–password-store=basic", "--profile-directory=" ++ Profile, Url],
      []
    ),
  {reply, Results, State};

handle_call({qutebrowser, Url, Profile}, _From, State) ->
  %exec:run(["/usr/sbin/chromium", "--user-data-dir=/home/steven/.config/chromium.home", Url], []),
  Ctx = [{home, "$HOME"}, {url, Url}, {profile, Profile}],
  Command =
    mustache:render(
      "/usr/sbin/qutebrowser --config-py ~/.config/qutebrowser/config.py --target window --basedir=~/.local/share/qutebrowser/{{profile}} --set window.title_format {{{perc}}}{{{audio}}}{{{current_title}}}{{{title_sep}}}{{{host}}}{{{title_sep}}}[{profile}]",
      Ctx
    ),
  ?LOG_DEBUG("Command ~p", [Command]),
  Results = exec:run(Command, []),
  {reply, Results, State};

handle_call({st, Title, _Args}, _From, _State) ->
  exec:run(io_lib:format("st -t ~p  htop", [Title]), [sync]);

handle_call({gun, _Args}, _From, _State) ->
  {ok, ConnPid} = gun:open("run.damagebdd.com", 443),
  gun:get("https://run.damagebdd.com/status", []),
  StreamRef =
    gun:get(
      ConnPid,
      "/status",
      [{<<"accept">>, "application/json"}, {<<"user-agent">>, "revolver/1.0"}]
    ),
  case gun:await(ConnPid, StreamRef) of
    {response, fin, _Status, _Headers} -> no_data;

    {response, nofin, _Status, _Headers} ->
      {ok, Body} = gun:await_body(ConnPid, StreamRef),
      io:format("~s~n", [Body])
  end,
  gun:close(ConnPid);

handle_call({wait_cancel, _Args}, _From, State) ->
  Watcher =
    spawn(
      fun
        F() ->
          receive
            <<"Cancel">> -> logger:info("canceling");

            Msg ->
              logger:info("Got: ~p", [Msg]),
              F()
          after
            60000 -> ok
          end
      end
    ),
  {ok, Pid, _OsPid} =
    exec:run("/usr/sbin/dmenu -l 30", [stdin, {stdout, Watcher}, {stderr, Watcher}]),
  lists:map(
    fun (I) -> exec:send(Pid, binary:list_to_bin([I, <<"\n">>])) end,
    [<<"Wait">>, <<"Cancel">>]
  ),
  exec:send(Pid, eof),
  {reply, [], State};

handle_call({kubectl, _Args}, _From, State) ->
  {ok, [{stdout, StdoutJson}]} =
    exec:run("kubectl --context zebra-blue get po -o json", [stdout, stderr, sync]),
  Results =
    [
      maps:get(name, maps:get(metadata, X))
      ||
      X
      <-
      maps:get(items, jsx:decode(binary:list_to_bin(StdoutJson), [{labels, atom}, return_maps]))
    ],
  logger:debug("response ~p~n", [Results]),
  Watcher =
    spawn(
      fun
        () ->
          receive
            Msg ->
              io:format("Got: ~p\n", [Msg]),
              ok
          after
            60000 -> ok
          end
      end
    ),
  {ok, Pid, _OsPid} =
    exec:run("/usr/sbin/dmenu -l 30", [stdin, {stdout, Watcher}, {stderr, Watcher}]),
  lists:map(fun (I) -> exec:send(Pid, binary:list_to_bin([I, <<"\n">>])) end, Results),
  exec:send(Pid, eof),
  {reply, Results, State};

handle_call({dmenu, _Args}, _From, State) ->
  Reply = ok,
  Return = {reply, Reply, State},
  io:format("handle_call dmenu: ~p~n", [Return]),
  Watcher =
    spawn(
      fun
        F() ->
          receive
            Msg ->
              io:format("Got: ~p\n", [Msg]),
              F()
          after
            60000 -> ok
          end
      end
    ),
  {ok, Pid, _OsPid} = exec:run("/usr/sbin/dmenu", [stdin, {stdout, Watcher}, {stderr, Watcher}]),
  exec:send(Pid, <<"foo\n">>),
  exec:send(Pid, <<"bar\n">>),
  exec:send(Pid, eof),
  Return;

handle_call({lnd, _Args}, _From, State) ->
  Reply = ok,
  Return = {reply, Reply, State},
  io:format("handle_call lnd: ~p~n", [Return]),
  Watcher =
    spawn(
      fun
        F() ->
          receive
            Msg ->
              io:format("Got: ~p\n", [Msg]),
              F()
          after
            60000 -> ok
          end
      end
    ),
  {ok, [{stdout, BtcRpcPassword}]} =
    exec:run("pass damagebdd/bitcoin/prod/rpc_password", [stdout, stderr, sync]),
  {ok, Pid, _OsPid} =
    exec:run(
      [
        "/usr/sbin/lnd",
        "--bitcoin.active",
        "--bitcoin.mainnet",
        "--debuglevel=debug",
        "--bitcoin.node=bitcoind",
        "--bitcoind.rpcuser=damagebdd_prod",
        "--bitcoind.rpcpass=" ++ BtcRpcPassword,
        "--bitcoind.zmqpubrawblock=tcp://razorjack.lan:28332",
        "--bitcoind.zmqpubrawtx=tcp://127.0.0.1:28333",
        "--restlisten=localhost:8011"
        %--externalip=X.X.X.X
      ],
      [{stdout, Watcher}, {stderr, Watcher}, monitor]
    ),
  logger:debug("started lnd ~p", [Pid]),
  gproc:reg_other({n, l, {?MODULE, lnd}}, Pid),
  Return;

handle_call({mpc, _Args}, _From, State) ->
  Reply = ok,
  Return = {reply, Reply, State},
  io:format("handle_call mpdc: ~p~n", [Return]),
  Watcher =
    spawn(
      fun
        F() ->
          receive
            Msg ->
              io:format("Got: ~p\n", [Msg]),
              F()
          after
            60000 -> ok
          end
      end
    ),
  {ok, Pid, _OsPid} = exec:run("/usr/sbin/mpc", [stdin, {stdout, Watcher}, {stderr, Watcher}]),
  exec:send(Pid, <<"foo\n">>),
  exec:send(Pid, <<"bar\n">>),
  exec:send(Pid, eof),
  Return;

% Switch to a workspace with the given name and open st terminal with the given title
handle_call({i3_switch_to_workspace, [Workspace, Title]}, _From, State) ->
  case os:cmd("wmctrl -l | grep \"" ++ Title ++ "\" | wc -l") of
    "0\n" ->
      os:cmd("i3-msg \"workspace " ++ Workspace ++ "; exec st -t '" ++ Title ++ "'\""),
      ok;

    _ ->
      os:cmd("i3-msg \"workspace " ++ Workspace ++ "\""),
      case i3_is_current_workspace(Workspace) of
        true ->
          os:cmd("i3-msg \"workspace back_and_forth\""),
          ok;

        false -> ok
      end
  end,
  {reply, [], State};

handle_call({new_client, _Args}, _From, _State) ->
  Frame = wxFrame:new(wx:null(), -1, "My Root Window"),
  wxFrame:show(Frame),
  application:run();

handle_call(_Request, _From, State) ->
  Reply = ok,
  Return = {reply, Reply, State},
  io:format("handle_call: ~p~n", [Return]),
  Return.


handle_cast({run, Cmd, Opts}, State) ->
  Return = {noreply, State},
  Result = exec:run(Cmd, Opts),
  logger:info("run cmd ~p Result ~p", [Cmd, Result]),
  Return;

handle_cast(_Msg, State) ->
  Return = {noreply, State},
  io:format("handle_cast: ~p~n", [Return]),
  Return.


handle_info(_Info, State) ->
  Return = {noreply, State},
  io:format("handle_info: ~p~n", [Return]),
  Return.


terminate(_Reason, _State) ->
  Return = ok,
  io:format("terminate: ~p~n", [Return]),
  ok.


code_change(_OldVsn, State, _Extra) ->
  Return = {ok, State},
  io:format("code_change: ~p~n", [Return]),
  Return.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

show_dmenu(Items) ->
  gen_server:call(asyncmind, {dmenu, Items}),
  ok.


lnd(Args) -> gen_server:call(asyncmind, {lnd, Args}).

terminal(_Args) ->
  Key = {n, l, {?MODULE, terminal}},
  gproc:reg_or_locate(
    Key,
    started,
    fun
      () -> {ok, _Pid, _OsPid} = exec:run("/usr/sbin/st", [sync, {env, [{"DISPLAY", ":0.0"}]}])
    end
  ).


tmux(outer) ->
  exec:run(
    "/usr/sbin/tmux -f %h/.tmux/outer.conf -S /tmp/steven/tmux_outer_socket0 new-session -fD -P -s %i -c %h -d"
  ).

kubectl(Args) ->
  gen_server:call(asyncmind, {kubectl, Args}),
  ok.


gun_get(Args) ->
  gen_server:call(asyncmind, {gun, Args}),
  ok.


wait_cancel(_Args) ->
  gen_server:call(asyncmind, {wait_cancel, {kubectl, []}}),
  ok.


dnd(OnOff) -> gen_server:call(asyncmind, {dnd, OnOff}).

browser(Url) ->
  gen_server:call(asyncmind, {browser, Url, "home"}),
  ok.


toggle_window_screen(Workspace, Title) ->
  gen_server:call(asyncmind, {i3_switch_to_workspace, [Workspace, Title]}),
  ok.


new_client() ->
  gen_server:call(asyncmind, {new_client, []}),
  ok.


run(Cmd, Opts) -> gen_server:cast(asyncmind, {run, [Cmd, Opts]}).
