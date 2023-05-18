-module(salt).

-behaviour(gen_server).

% API functions
-export([start_link/1, stop/1, service_running/1, service_monitor/1]).

% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API Functions
start_link(_Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Pid) -> gen_server:cast(Pid, stop).

service_running(ServiceName) -> gen_server:call(?MODULE, {service_running, ServiceName}).

service_monitor(ServiceName) -> gen_server:call(?MODULE, {service_monitor, ServiceName}).

% GenServer Callbacks
init([]) -> {ok, undefined}.

handle_call({service_monitor, ServiceName}, _From, _State) ->
  {Reply, NewState} = execute_service_monitor(ServiceName),
  {reply, Reply, NewState};

handle_call({service_running, ServiceName}, _From, _State) ->
  {Reply, NewState} = execute_service_running(ServiceName),
  {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
  Reply = unknown_request,
  {reply, Reply, State}.


handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% Internal Functions
execute_service_running(ServiceName) ->
  {ok, [{stdout, StdoutJson}]} =
    exec:run("systemd status -o json-pretty " ++ ServiceName, [stdout, stderr, sync]),
  {StdoutJson, undefined}.


dunstify_journal_json(StdoutJson, _OsPid, Data) ->
  logger:debug("response ~p ~p~n", [StdoutJson, Data]),
  Message = try jsx:decode(Data, [{labels, atom}, return_maps]) of Json -> maps:get(message, Json) catch
      _:Reason ->
        logger:error("Error processing task. ~p ~p~n", [Reason, Data]),
        Data end,
  exec:run(
    "dunstify -a 'Systemd Log Monitor' -t 5000 -i error '"++ binary_to_list(Message) ++ "'",
    [sync, stdout, {env, [{"DISPLAY", ":0"}]}]
  ).


journalctl_follow_notify(Extra) ->
  exec:run("journalctl -p 3 -f -o cat " ++ Extra, [{stdout, fun dunstify_journal_json/3}]),
  {<<"ok">>, undefined}.


execute_service_monitor(system) -> journalctl_follow_notify("");
execute_service_monitor(user) -> journalctl_follow_notify("--user").
