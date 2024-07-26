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
    connect_i3_socket/0,
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

-define(SOCKET_PATH, "/run/user/1000/i3/ipc-socket.507945").
-define(I3ENV, {env, [{"DISPLAY", ":1.0"}]}).

-record(state, {socket_pid = undefined, caller = undefined, recv_buffer = <<"">>}).

%% External API

start_link(Args) ->
  Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
  ?LOG_INFO("i3 start_link:  ~p~p~n", [Return, Args]),
  Return.


init([]) ->
  case connect_i3_socket() of
    {ok, State} ->
      gproc:reg_other({n, l, {?MODULE, i3_ipc}}, self()),
      {ok, State};

    Error ->
      ?LOG_INFO("i3 initializaion failed ~p", [Error]),
      ignore
  end.


handle_call(Request, _From, State) ->
  ?LOG_DEBUG("handle_call ~p ~p", [Request, State]),
  handle_i3_message(self(), Request, State),
  {reply, ok, State}.


handle_cast(Message, State) ->
  ?LOG_DEBUG("handle_cast ~p ~p", [Message, State]),
  %handle_i3_message(self(), Message, State),
  {noreply, State#state{caller = self()}}.


handle_info(Message, State) ->
  %?LOG_DEBUG("handle_info ~p ~p", [Message, State]),
  handle_i3_reply_package(self(), Message, State),
  {noreply, State}.


terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions

connect_i3_socket() ->
  case exec:run("i3 --get-socketpath", [stdout, stderr, sync, ?I3ENV]) of
    {ok, [{stdout, [SockPath0]}]} ->
      SockPath = lists:nth(1, string:split(SockPath0, "\n")),
      ?LOG_INFO("i3 socket ~p", [SockPath]),
      case gen_tcp:connect({local, SockPath}, 0, [local]) of
        {ok, SocketPid} ->
          inet:setopts(SocketPid, [{active, true}, {packet, raw}, binary]),
          {ok, #state{socket_pid = SocketPid}};

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


subscribe(Events) -> gen_server:cast(?MODULE, {subscribe, Events}).

get_outputs() -> gen_server:cast(?MODULE, get_outputs).

format_ipc(Type, Msg) ->
  Len = byte_size(Msg),
  <<"i3-ipc", Len:32/little - unsigned - integer, Type:32/little - unsigned - integer, Msg/binary>>.


format_ipc_command(Msg) -> format_ipc(0, Msg).

handle_i3_message(_Pid, {subscribe, Events}, #state{socket_pid = SocketPid} = State) ->
  Message = format_ipc(2, jsx:encode(Events)),
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
      handle_i3_reply(Type, jsx:decode(Msg)),
      {noreply, State#state{recv_buffer = <<>>}};

    _ ->
      %% Partial message received, append to buffer
      NewBuffer = <<Buffer/binary, Data/binary>>,
      BuffLen = byte_size(NewBuffer),
      case Len of
        BuffLen ->
          logger:debug("BuffLen ~p Mesg ~p ~p", [Len, BuffLen, Msg]),
          handle_i3_reply(Type, jsx:decode(Msg)),
          {noreply, State#state{recv_buffer = <<>>}};

        Other when Other < BuffLen ->
          <<Msg0:Other/binary, Rest/binary>> = Msg,
          logger:debug("Other<BuffLen ~p Mesg ~p ~p", [Len, BuffLen, Msg]),
          handle_i3_reply(Type, jsx:decode(Msg0)),
          {noreply, State#state{recv_buffer = Rest}};

        _Other ->
          logger:debug("Other Len ~p Mesg ~p ~p", [Len, BuffLen, Msg]),
          {noreply, State#state{recv_buffer = NewBuffer}}
      end
  end;

handle_i3_reply_package(_Pid, {tcp, _SockPid, Data}, #state{recv_buffer = Buffer} = State) ->
  %% Append received data to buffer
  {noreply, State#state{recv_buffer = <<Buffer/binary, Data/binary>>}};

%    Workspaces;
handle_i3_reply_package(_SocketPid, UnknownMessage, State) ->
  % Handle any other message as per your requirements
  logger:debug("Unknown message State ~p Mesg ~p", [State, UnknownMessage]),
  ok.


handle_i3_reply(Type, Message) ->
  logger:debug("i3 message ~p of type ~p", [Message, Type]),
  ok.
