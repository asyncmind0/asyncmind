%%% @doc
%%% asyncmind Controller
%%%
%%% This process is a in charge of maintaining the program's state.
%%% @end

-module(a_con).
-vsn("0.1.0").
-author("Steven Joseph <steven@stevenjoseph.in>").
-copyright("Steven Joseph <steven@stevenjoseph.in>").
-license("GPL-3.0-or-later").

-behavior(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).
-include("$zx_include/zx_logger.hrl").


%%% Type and Record Definitions


-record(s,
        {window = none :: none | wx:wx_object()}).

-type state() :: #s{}.



%% Interface

-spec stop() -> ok.

stop() ->
    gen_server:cast(?MODULE, stop).



%%% Startup Functions


-spec start_link() -> Result
    when Result :: {ok, pid()}
                 | {error, Reason},
         Reason :: {already_started, pid()}
                 | {shutdown, term()}
                 | term().
%% @private
%% Called by a_sup.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).


-spec init(none) -> {ok, state()}.

init(none) ->
    ok = log(info, "Starting"),
    Window = a_gui:start_link("Hello, WX!"),
    ok = log(info, "Window: ~p", [Window]),
    State = #s{window = Window},
    ArgV = zx_daemon:argv(),
    ok = a_gui:show(ArgV),
    {ok, State}.



%%% gen_server Message Handling Callbacks


-spec handle_call(Message, From, State) -> Result
    when Message  :: term(),
         From     :: {pid(), reference()},
         State    :: state(),
         Result   :: {reply, Response, NewState}
                   | {noreply, State},
         Response :: ok
                   | {error, {listening, inet:port_number()}},
         NewState :: state().
%% @private
%% The gen_server:handle_call/3 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_call-3

handle_call(Unexpected, From, State) ->
    ok = log(warning, "Unexpected call from ~tp: ~tp~n", [From, Unexpected]),
    {noreply, State}.


-spec handle_cast(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().
%% @private
%% The gen_server:handle_cast/2 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2

handle_cast(stop, State) ->
    ok = log(info, "Received a 'stop' message."),
%   {noreply, State};
    {stop, normal, State};
handle_cast(Unexpected, State) ->
    ok = log(warning, "Unexpected cast: ~tp~n", [Unexpected]),
    {noreply, State}.


-spec handle_info(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().
%% @private
%% The gen_server:handle_info/2 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_info-2

handle_info(Unexpected, State) ->
    ok = log(warning, "Unexpected info: ~tp~n", [Unexpected]),
    {noreply, State}.



%% @private
%% gen_server callback to handle state transformations necessary for hot
%% code updates. This template performs no transformation.

code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    zx:stop().
