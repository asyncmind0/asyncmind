%%% @doc
%%% asyncmind GUI
%%%
%%% This process is responsible for creating the main GUI frame displayed to the user.
%%%
%%% Reference: http://erlang.org/doc/man/wx_object.html
%%% @end

-module(a_gui).
-vsn("0.1.0").
-author("Steven Joseph <steven@stevenjoseph.in>").
-copyright("Steven Joseph <steven@stevenjoseph.in>").
-license("GPL-3.0-or-later").

-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-export([show/1]).
-export([start_link/1]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2, handle_event/2]).
-include("$zx_include/zx_logger.hrl").


-record(s,
        {frame = none :: none | wx:wx_object(),
         text  = none :: none | wx:wx_object()}).


-type state() :: term().



%%% Interface functions

show(Terms) ->
    wx_object:cast(?MODULE, {show, Terms}).



%%% Startup Functions

start_link(Title) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, Title, []).


init(Title) ->
    ok = log(info, "GUI starting..."),
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, Title),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    TextC = wxTextCtrl:new(Frame, ?wxID_ANY, [{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    wxSizer:add(MainSz, TextC, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxFrame:setSizer(Frame, MainSz),
    wxSizer:layout(MainSz),

    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),
    State = #s{frame = Frame, text = TextC},
    {Frame, State}.


-spec handle_call(Message, From, State) -> Result
    when Message  :: term(),
         From     :: {pid(), reference()},
         State    :: state(),
         Result   :: {reply, Response, NewState}
                   | {noreply, State},
         Response :: ok
                   | {error, {listening, inet:port_number()}},
         NewState :: state().

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

handle_cast({show, Terms}, State) ->
    ok = do_show(Terms, State),
    {noreply, State};
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


-spec handle_event(Event, State) -> {noreply, NewState}
    when Event    :: term(),
         State    :: state(),
         NewState :: state().
%% @private
%% The wx_object:handle_event/2 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_info-2

handle_event(#wx{event = #wxClose{}}, State = #s{frame = Frame}) ->
    ok = a_con:stop(),
    ok = wxWindow:destroy(Frame),
    {noreply, State};
handle_event(Event, State) ->
    ok = log(info, "Unexpected event ~tp State: ~tp~n", [Event, State]),
    {noreply, State}.



code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    wx:destroy().



do_show(Terms, #s{text = TextC}) ->
    String = io_lib:format("Received args: ~tp", [Terms]),
    wxTextCtrl:changeValue(TextC, String).
