-module(x11).

-behaviour(gen_server).

-export(
  [
    switch_window/1,
    switch_desktop/1,
    list_windows/0,
    set_window_property/3,
    kill_window/1,
    start_event_listener/1,
    stop_event_listener/0
  ]
).

-nifs(
  [
    switch_window/1,
    switch_desktop/1,
    list_windows/0,
    set_window_property/3,
    kill_window/1,
    start_event_listener/1,
    stop_event_listener/0
  ]
).

-export(
  [start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]
).

-include_lib("kernel/include/logger.hrl").

-on_load(init/0).

%% gen_server functions
%% Start the gen_server

start_link([]) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks

init([]) ->
  %% Initialize any required state
  gproc:reg_other({n, l, {?MODULE, x11}}, self()),
  Pid = spawn(x11, start_event_listener, [self()]),
  {ok, #{listner => Pid}}.


handle_call(Message, _From, State) ->
  ?LOG_INFO("handle_call ~p ~p", [Message, State]),
  {reply, ok, State}.


handle_cast(Msg, State) ->
  ?LOG_INFO("handle_cast ~p ~p", [Msg, State]),
  {noreply, State}.


handle_info(Info, State) ->
  ?LOG_INFO("handle_info ~p ~p", [Info, State]),
  {noreply, State}.


terminate(_Reason, _State) ->
  stop_event_listener(),
  ok.


code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% NIF FUnctions

init() ->
  PrivDir = code:priv_dir(asyncmind),
  NifPath = filename:join([PrivDir, "x11"]),
  ok = erlang:load_nif(NifPath, 0).


list_windows() -> erlang:nif_error(nif_library_not_loaded).

switch_window(_WindowId) -> erlang:nif_error(nif_library_not_loaded).
switch_desktop(_DesktopId) -> erlang:nif_error(nif_library_not_loaded).

kill_window(_Y) -> erlang:nif_error(nif_library_not_loaded).

set_window_property(_WindowId, _PropertyTerm, _PropertyValue) ->
  erlang:nif_error(nif_library_not_loaded).

start_event_listener(_Pid) -> erlang:nif_error(nif_library_not_loaded).

stop_event_listener() -> erlang:nif_error(nif_library_not_loaded).
