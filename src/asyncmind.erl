-module(asyncmind).

-author("Steven Joseph <steven@stevenjoseph.in>").

-behaviour(gen_server).

-export([start_link/1]).
-export([show_dmenu/1]).
-export([kubectl/1]).
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
handle_call({st, Title, Args}, _From, State)->
    exec:run(io_lib:format("st -t ~p  htop",[Title]), [stdout, stderr, sync]);

handle_call({gun, _Args}, _From, State) ->
    {ok,ConnPid} = gun:open("api.contextu.al", 443),
    gun:get("https://api.contextu.al/v3/debug",[]),
    StreamRef = gun:get(ConnPid, "/v3/debug", [
    {<<"accept">>, "application/json"},
    {<<"user-agent">>, "revolver/1.0"}
                                              ]),
    case gun:await(ConnPid, StreamRef) of
    {response, fin, Status, Headers} ->
        no_data;
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
            <<"Cancel">> ->
              logger:info("canceling");
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
  lists:map(fun F(I) -> exec:send(Pid, binary:list_to_bin([I, <<"\n">>])) end, [<<"Wait">>, <<"Cancel">>]),
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
    gen_server:call(asyncmind,{wait_cancel, {kubectl,  []}}),
    ok.
