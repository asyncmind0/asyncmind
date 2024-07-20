-module(erm).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% wx callback
-export([handle_event/2]).

-include_lib("wx/include/wx.hrl").

-record(state, {frame, list_ctrl}).

%% API
start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% gen_server callbacks
init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), -1, "List with Icons", [{size, {400, 300}}]),
    wxFrame:show(Frame),

    ListCtrl = wxListCtrl:new(Frame, [{style, ?wxLC_REPORT bor ?wxLC_ICON bor ?wxLC_SINGLE_SEL}]),
    wxListCtrl:insertColumn(ListCtrl, 0, "Item"),
    wxListCtrl:insertColumn(ListCtrl, 1, "Icon"),

    %% Example items
    wxListCtrl:insertItem(ListCtrl, 0, "Item 1"),
    wxListCtrl:insertItem(ListCtrl, 1, "Item 2"),
    wxListCtrl:insertItem(ListCtrl, 2, "Item 3"),

    wxListCtrl:connect(ListCtrl, command_list_item_selected),
    wxListCtrl:connect(ListCtrl, command_list_item_activated),

    {ok, #state{frame = Frame, list_ctrl = ListCtrl}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    ?LOG_INFO("Got call ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?LOG_INFO("Got cast ~p", [Msg]),
    {noreply, State}.

handle_info({wx, _ListCtrl, Event}, State) ->
    %% Handle short click
    ?LOG_INFO("Item selected ~p ~n", [Event]),
    {noreply, State};
%handle_info({wx, _ListCtrl, _Event=#wx{type = command_list_item_activated}}, State) ->
    %% Handle long click (double click or enter key)
%    io:format("Item activated~n"),
%    {noreply, State};
handle_info(Info, State) ->
    ?LOG_INFO("info ~p ~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event(Event, State) ->
    io:format("Event: ~p~n", [Event]),
    {noreply, State}.
