-module(salt).

-behaviour(gen_server).

% API functions
-export([start_link/1, stop/1, service_running/1, service_monitor/1]).
-export([apply/1]).

% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([journal_monitor/2]).
-export([cmd_run/2]).
-export([ping/1]).
-export([ping/0]).
-export([test_get_state/0]).
-include_lib("kernel/include/logger.hrl").

% API Functions
start_link(_Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Pid) -> gen_server:cast(Pid, stop).

ping(Args) -> gen_server:call(?MODULE, {ping, Args}).

service_running(ServiceName) -> gen_server:call(?MODULE, {service_running, ServiceName}).

service_monitor(ServiceName) -> gen_server:call(?MODULE, {service_monitor, ServiceName}).

% GenServer Callbacks
init([]) -> {ok, undefined}.

handle_call({ping, "*"}, _From, State) -> {reply, pong, State};

handle_call({service_monitor, ServiceName}, _From, _State) ->
  {Reply, NewState} = journal_monitor(ServiceName, []),
  {reply, Reply, NewState};

handle_call({service_running, ServiceName}, _From, _State) ->
  {Reply, NewState} = execute_service_running(ServiceName),
  {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
  Reply = unknown_request,
  {reply, Reply, State}.


handle_cast({apply_state, YamlFile}, _State) -> apply_state(read_state_yaml(YamlFile));
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
        ?LOG_DEBUG("Error processing task. ~p ~p~n", [Reason, Data]),
        Data end,
  exec:run(
    "dunstify -a 'Systemd Log Monitor' -t 5000 -i error '" ++ binary_to_list(Message) ++ "'",
    [sync, stdout, {env, [{"DISPLAY", ":0"}]}]
  ).


journalctl_follow_notify(Extra) ->
  exec:run("journalctl -p 5 -f -o cat " ++ Extra, [{stdout, fun dunstify_journal_json/3}, monitor]).

apply_state({file_deployed, #{destination_nodes := DestinationNodes, source := Source} = _Kwargs}) ->
  {ok, FileContent} = file:read_file(Source),
  distributed_file:send_file(self(), DestinationNodes, Source, FileContent);

apply_state({ipfs_deployed, #{ipfs_host := IpfsHost, release_dir := ReleaseDir} = _Kwargs}) ->
  {ok, Pid} = ipfs:start_link(#{ip => IpfsHost}),
  {ok, #{<<"Hash">> := Hash, <<"Size">> := Size}} = ipfs:add(Pid, ReleaseDir),
  logger:info("Added release to ipfs ~p ~p.", [Hash, Size]).


journal_monitor(system, _Args) -> journalctl_follow_notify("");
journal_monitor(user, _Args) -> journalctl_follow_notify("--user");
journal_monitor(Journal, Args) -> logger:info("~p Args ~p", [Journal,Args]).

read_state_yaml(File) ->
  {ok, Binary} = file:read_file(File),
  {ok, [State]} = fast_yaml:decode(Binary, [maps]),
  logger:info("Got state ~p", [State]),
  get_state_module_args(State).


apply(YamlFile) -> gen_server:cast(salt, {apply_state, [YamlFile]}).

cmd_run(_Target, Cmd) ->
  {ok, [{stdout, Results}]} = exec:run(Cmd, [stdout, stderr, sync]),
  Results.


get_args([Arg | Rest], Acc) -> get_args(Rest, maps:merge(Arg, Acc));
get_args([], Acc) -> Acc.

get_state_module_args(State) ->
  [{Name0, Args}] = maps:to_list(State),
  [{Mod, MArgs}] = maps:to_list(Args),
  logger:info("Got name ~p mod ~p MArgs ~p.", [Name0, Mod, MArgs]),
  Args0 = get_args(MArgs, #{}),
  {Mod, maps:get(<<"name">>, Args0, Name0), Name0, Args0}.


ping() -> pong.

test_get_state() ->
  {
    <<"file.managed">>,
    <<"/tmp/asyncmind.tar.gz">>,
    <<"deploy_asyncmind">>,
    #{
      <<"name">> := <<"/tmp/asyncmind.tar.gz">>,
      <<"source">> := <<"salt://asyncmind.tar.gz">>,
      <<"makedirs">> := <<"True">>,
      <<"user">> := <<"root">>,
      <<"group">> := <<"root">>,
      <<"mode">> := 644
    }
  } = read_state_yaml("states/deploy.yaml"),
  ok.
