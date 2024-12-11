-module(i3).

-behaviour(gen_server).

-export(
  [
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    connect_i3_socket/1,
    send_i3_command/1,
    get_workspaces/0,
    get_current_workspace/0,
    get_outputs/0,
    get_tree/0,
    subscribe/1
  ]
).
-export([handle_i3_message/3]).

-include_lib("kernel/include/logger.hrl").

-define(I3ENV, {env, [{"DISPLAY", ":1.0"}]}).
-define(HEARTBEAT_TIMER, 1000).

-record(
  state,
  {
    socket_pid = undefined,
    caller = undefined,
    recv_buffer = <<"">>,
    heartbeat_timer = undefined,
    focused_window_name = undefined,
   env = ?I3ENV
  }
).

%% External API

start_link(Args) ->
  Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
  ?LOG_INFO("i3 start_link:  ~p~p~n", [Return, Args]),
  Return.


subscribe_events(#state{socket_pid = SocketPid} = State) ->
  Events =
    [
      <<"binding">>,
      <<"workspace">>,
      <<"output">>,
      <<"mode">>,
      <<"window">>,
      <<"barconfig_update">>,
      <<"shutdown">>,
      <<"tick">>
    ],
  Message = format_ipc(2, jsx:encode(Events)),
  ?LOG_INFO("Sending subscribe ~p", [Message]),
  gen_tcp:send(SocketPid, Message),
  State.


start_i3(State) ->
  case connect_i3_socket(State) of
    {ok, State} -> subscribe_events(State);

    _Error ->
      %?LOG_INFO("i3 initializaion failed ~p", [Error]),
      State
  end.


init([]) ->
  HeartbeatTimer = erlang:send_after(?HEARTBEAT_TIMER, self(), heartbeat),
  gproc:reg_other({n, l, {?MODULE, i3_ipc}}, self()),
  {ok, start_i3(#state{heartbeat_timer = HeartbeatTimer})}.


handle_call(Request, _From, State) ->
  ?LOG_DEBUG("handle_call ~p ~p", [Request, State]),
  handle_i3_message(self(), Request, State),
  {reply, ok, State}.


handle_cast(Message, State) ->
  ?LOG_DEBUG("handle_cast ~p ~p", [Message, State]),
  handle_i3_message(self(), Message, State),
  {noreply, State#state{caller = self()}}.


handle_info(heartbeat, #state{socket_pid = undefined} = State) ->
  %?LOG_INFO("i3 socket disconnected.", []),
  %% Reset the heartbeat timer
  HeartbeatTimer = erlang:send_after(?HEARTBEAT_TIMER, self(), heartbeat),
  {noreply, start_i3(State#state{heartbeat_timer = HeartbeatTimer})};

handle_info(heartbeat, State) ->
  Message = format_ipc(10, <<"ping">>),
  %% Send a ping message to check the connection
  case gen_tcp:send(State#state.socket_pid, Message) of
    ok -> ?LOG_INFO("Heartbeat ok", []);
    Err -> ?LOG_INFO("Heartbeat NOT ok ~p", [Err])
  end,
  %% Reset the heartbeat timer
  HeartbeatTimer = erlang:send_after(?HEARTBEAT_TIMER, self(), heartbeat),
  {noreply, subscribe_events(#state{heartbeat_timer = HeartbeatTimer})};

handle_info(Message, State) ->
  %?LOG_DEBUG("handle_info ~p ~p", [Message, State]),
  handle_i3_reply_package(self(), Message, State).


terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions

connect_i3_socket(#state{env = Env} = State) ->
  case exec:run("i3 --get-socketpath", [stdout, stderr, sync, Env]) of
    {ok, [{stdout, [SockPath0]}]} ->
      SockPath = lists:nth(1, string:split(SockPath0, "\n")),
      %?LOG_INFO("i3 socket ~p", [SockPath]),
      case gen_tcp:connect({local, SockPath}, 0, [local]) of
        {ok, SocketPid} ->
          inet:setopts(SocketPid, [{active, true}, {packet, raw}, binary]),
          {ok, State#state{socket_pid = SocketPid}};

        Error -> Error
      end;

    Error ->
      ?LOG_ERROR("Failed to get i3 socket path ~p", [Error]),
      Error
  end.


send_i3_command(Command) ->
  Pid = gproc:lookup_local_name({?MODULE, i3_ipc}),
  gen_server:call(Pid, {send_command, Command}).


get_workspaces() ->
  Pid = gproc:lookup_local_name({?MODULE, i3_ipc}),
  gen_server:call(Pid, get_workspaces).


get_current_workspace() -> send_i3_command(get_current_workspace).

get_tree() ->
  Pid = gproc:lookup_local_name({?MODULE, i3_ipc}),
  gen_server:call(Pid, tree).


subscribe(Events) ->
  Pid = gproc:lookup_local_name({?MODULE, i3_ipc}),
  gen_server:call(Pid, {subscribe, Events}).


get_outputs() -> gen_server:cast(?MODULE, get_outputs).

format_ipc(Type, Msg) ->
  Len = byte_size(Msg),
  <<"i3-ipc", Len:32/little - unsigned - integer, Type:32/little - unsigned - integer, Msg/binary>>.


format_ipc_command(Msg) -> format_ipc(0, Msg).

handle_i3_message(_Pid, {subscribe, Events}, #state{socket_pid = SocketPid} = State) ->
  Message = format_ipc(2, jsx:encode(Events)),
  ?LOG_INFO("Sending subscribe ~p", [Message]),
  gen_tcp:send(SocketPid, Message),
  {noreply, State};

handle_i3_message(_Pid, {send_command, Command}, #state{socket_pid = SocketPid} = State) ->
  Mesg = format_ipc_command(Command),
  gen_tcp:send(SocketPid, Mesg),
  {noreply, State};

handle_i3_message(_Pid, get_outputs, #state{socket_pid = SocketPid} = State) ->
  Mesg = <<"i3-ipc", 0 : 32/little - unsigned - integer, 3 : 32/little - unsigned - integer>>,
  gen_tcp:send(SocketPid, Mesg),
  {noreply, State};

handle_i3_message(_Pid, get_workspaces, #state{socket_pid = SocketPid} = State) ->
  Mesg = <<"i3-ipc", 0 : 32/little - unsigned - integer, 1 : 32/little - unsigned - integer>>,
  gen_tcp:send(SocketPid, Mesg),
  {noreply, State};

handle_i3_message(_Pid, tree, #state{socket_pid = SocketPid} = State) ->
  Mesg = <<"i3-ipc", 0 : 32/little - unsigned - integer, 4 : 32/little - unsigned - integer>>,
  gen_tcp:send(SocketPid, Mesg),
  {noreply, State}.


%    %{ok, Data} = do_recv(SocketPid, []),
%  {ok, Data} = gen_tcp:recv(SocketPid, 0),
%    <<"i3-ipc", _Len:32/little - unsigned - integer, _Type:32/little - unsigned - integer, Msg/binary>> = Data,
handle_i3_reply_package(
  _Pid,
  {
    tcp,
    _SockPid,
    <<
      "i3-ipc",
      Len:32/little - unsigned - integer,
      Type:32/little - unsigned - integer,
      Msg/binary
    >> = Data
  },
  #state{recv_buffer = Buffer} = State
) ->
  RecvLen = byte_size(Data),
  case Len of
    RecvLen ->
      %% Whole message received
      {ok, State0} = handle_i3_reply(Type, jsx:decode(Msg), State),
      {noreply, State0#state{recv_buffer = <<>>}};

    _ ->
      %% Partial message received, append to buffer
      NewBuffer = <<Buffer/binary, Data/binary>>,
      BuffLen = byte_size(NewBuffer),
      case Len of
        BuffLen ->
          %logger:debug("BuffLen ~p Mesg ~p ~p", [Len, BuffLen, Msg]),
          {ok, State0} = handle_i3_reply(Type, jsx:decode(Msg), State),
          {noreply, State0#state{recv_buffer = <<>>}};

        Other when Other < BuffLen ->
          <<Msg0:Other/binary, Rest/binary>> = Msg,
          %logger:debug("Other<BuffLen ~p Mesg ~p ~p", [Len, BuffLen, Msg]),
          {ok, State0} = handle_i3_reply(Type, jsx:decode(Msg0), State),
          {noreply, State0#state{recv_buffer = Rest}};

        _Other ->
          %logger:debug("Other Len ~p Mesg ~p ~p", [Len, BuffLen, Msg]),
          {noreply, State#state{recv_buffer = NewBuffer}}
      end
  end;

handle_i3_reply_package(_Pid, {tcp, _SockPid, Data}, #state{recv_buffer = Buffer} = State) ->
  %% Append received data to buffer
  {noreply, State#state{recv_buffer = <<Buffer/binary, Data/binary>>}};

%    Workspaces;
handle_i3_reply_package(_SocketPid, UnknownMessage, State) ->
  % Handle any other message as per your requirements
  ?LOG_DEBUG("Unknown message State ~p Mesg ~p", [State, UnknownMessage]),
  ok.


handle_i3_reply(2147483655, [{<<"first">>, false}, {<<"payload">>, <<"ping">>}] = _Message, State) ->
  %?LOG_DEBUG("i3 ping reply ~p", [Message]),
  {ok, State};

handle_i3_reply(
  _Type,
  [{<<"change">>, <<"run">>}, {<<"mode">>, <<"default">>}, {<<"binding">>, Bindings}] = _Message,
  State
) ->
  ?LOG_DEBUG("i3 binding message ~p", [Bindings]),
  Symbol = proplists:get_value(<<"symbol">>, Bindings, undefined),
  Mods = proplists:get_value(<<"mods">>, Bindings, undefined),
  Command = proplists:get_value(<<"command">>, Bindings, undefined),
  handle_i3_binding(Symbol, Mods, Command, State);

handle_i3_reply(
  _Type,
  [{<<"change">>, <<"focus">>}, {<<"container">>, Container} | _Rest] = Message,
  State
) ->
  WindowName = proplists:get_value(<<"name">>, Container, undefined),
  ?LOG_DEBUG("i3 change focus window name ~p ~p", [WindowName, Message]),
  {ok, State#state{focused_window_name = WindowName}};

handle_i3_reply(
  _Type,
  [{<<"change">>, <<"title">>}, {<<"container">>, Container} | _Rest] = _Message,
  State
) ->
  WindowProps = proplists:get_value(<<"window_properties">>, Container, undefined),
  Title = proplists:get_value(<<"title">>, WindowProps, undefined),
  %Title0 = io_lib:format("~ts~n", [Title]),
  ?LOG_DEBUG("i3 change title message ~ts", [Title]),
  {ok, State};

handle_i3_reply(Type, Message, State) ->
  ?LOG_DEBUG("i3 message ~p of type ~p", [Message, Type]),
  {ok, State}.


handle_i3_binding(<<"F11">>, _Mods, Command, #state{focused_window_name = Focused} = State) ->
  ?LOG_DEBUG("i3 termwindow ~p ~p", [Focused, Command]),
  {ok, State};

handle_i3_binding(Symbol, Mods, Command, State) ->
  ?LOG_DEBUG("i3 got symbol ~p with mods ~p and command ~p", [Symbol, Mods, Command]),
  {ok, State}.

