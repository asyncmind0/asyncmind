-module(asyncmind).

-author("Steven Joseph <steven@stevenjoseph.in>").

-behaviour(gen_server).

-export([start_link/1]).
-export([show_dmenu/1]).
-export([kubectl/1]).
-export([browser/1]).
-export([dnd/1, pactl_list_sinks/0]).
-export([toggle_window_screen/2]).
-export([new_client/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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


pactl_mute_sink(Sink) -> pactl_mute_sink(Sink, " 1").

pactl_unmute_sink(Sink) -> pactl_mute_sink(Sink, " 0").

pactl_mute_sink(Sink, Mute) ->
  [
    {ok, []} =
      exec:run(
        lists:concat(["pactl --format=json -- set-sink-mute ", integer_to_list(X), Mute]),
        [sync]
      )
      || X <- pactl_list_sinks()
  ].

% Check if the current workspace is the given name
i3_is_current_workspace(Name) ->
    case os:cmd("i3-msg -t get_workspaces | jq '.[] | select(.focused==true).name==\"" ++ Name ++ "\"'") of
        "true\n" ->
            true;
        _ ->
            false
    end.


handle_call({dnd, on}, _From, State) ->
  Results = [pactl_mute_sink(X) || X <- pactl_list_sinks()],
  logger:info("dnd on ~p~n", [Results]),
  {reply, Results, State};

handle_call({dnd, off}, _From, State) ->
  Results = [pactl_unmute_sink(X) || X <- pactl_list_sinks()],
  logger:info("dnd off ~p~n", [Results]),
  {reply, Results, State};

handle_call({browser, Url}, _From, State) ->
  Results =
    exec:run(["/usr/sbin/chromium", "--user-data-dir=/home/steven/.config/chromium.home", Url], []),
  {reply, Results, State};

handle_call({st, Title, Args}, _From, State) ->
  exec:run(io_lib:format("st -t ~p  htop", [Title]), [sync]);

handle_call({gun, _Args}, _From, State) ->
  {ok, ConnPid} = gun:open("api.contextu.al", 443),
  gun:get("https://api.contextu.al/v3/debug", []),
  StreamRef =
    gun:get(
      ConnPid,
      "/v3/debug",
      [{<<"accept">>, "application/json"}, {<<"user-agent">>, "revolver/1.0"}]
    ),
  case gun:await(ConnPid, StreamRef) of
    {response, fin, Status, Headers} -> no_data;

    {response, nofin, Status, Headers} ->
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
    fun F(I) -> exec:send(Pid, binary:list_to_bin([I, <<"\n">>])) end,
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
        F() ->
          receive
            Msg ->
              io:format("Got: ~p\n", [Msg]),
              ok
          after
            60000 -> ok
          end
      end
    ),
  {ok, Pid, OsPid} =
    exec:run("/usr/sbin/dmenu -l 30", [stdin, {stdout, Watcher}, {stderr, Watcher}]),
  lists:map(fun F(I) -> exec:send(Pid, binary:list_to_bin([I, <<"\n">>])) end, Results),
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
  {ok, Pid, OsPid} = exec:run("/usr/sbin/dmenu", [stdin, {stdout, Watcher}, {stderr, Watcher}]),
  exec:send(Pid, <<"foo\n">>),
  exec:send(Pid, <<"bar\n">>),
  exec:send(Pid, eof),
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
  {ok, Pid, OsPid} = exec:run("/usr/sbin/mpc", [stdin, {stdout, Watcher}, {stderr, Watcher}]),
  exec:send(Pid, <<"foo\n">>),
  exec:send(Pid, <<"bar\n">>),
  exec:send(Pid, eof),
  Return;
% Switch to a workspace with the given name and open st terminal with the given title
handle_call({i3_switch_to_workspace, [Workspace, Title]}, From, State) ->
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
                false ->
                    ok
            end
    end,
  {reply, [], State};
handle_call({new_client, _Args}, _From, State) ->
    Frame = wxFrame:new(wx:null(), -1, "My Root Window"),
    wxFrame:show(Frame),
    application:run();

handle_call(_Request, _From, State) ->
  Reply = ok,
  Return = {reply, Reply, State},
  io:format("handle_call: ~p~n", [Return]),
  Return.


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


kubectl(Args) ->
  gen_server:call(asyncmind, {kubectl, Args}),
  ok.


gun_get(Args) ->
  gen_server:call(asyncmind, {gun, Args}),
  ok.


wait_cancel(Args) ->
  gen_server:call(asyncmind, {wait_cancel, {kubectl, []}}),
  ok.


dnd(OnOff) ->
  gen_server:call(asyncmind, {dnd, OnOff}),
  ok.


browser(Url) ->
  gen_server:call(asyncmind, {browser, Url}),
  ok.
toggle_window_screen(Workspace, Title) ->
  gen_server:call(asyncmind, {i3_switch_to_workspace, [Workspace, Title]}),
  ok.
new_client() ->
    gen_server:call(asyncmind, {new_client, []}),
    ok.
