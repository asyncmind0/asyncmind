-module(erm_windowlist).

-include_lib("wx/include/wx.hrl").
-include_lib("kernel/include/logger.hrl").

-behaviour(wx_object).

-export(
  [
    start/1,
    init/1,
    terminate/2,
    code_change/3,
    handle_info/2,
    handle_call/3,
    handle_cast/2,
    handle_event/2
  ]
).
-export([show/0]).
-export([close/0]).
-export([populate_window_list/1]).
-export([filter_windows/2]).

-record(state, {parent, config, list, windows}).

start(Config) -> wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(WINDOW_WIDTH, 900).
-define(WINDOW_HEIGHT, 800).
init(Config) ->
  wx:new(),
  wx:batch(fun () -> do_init(Config) end).


do_init(Config) ->
  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "erm_windowlist", []),
  Button = wxButton:new(Frame, ?wxID_ANY, [{label, "Go"}, {style, ?wxBU_LEFT}]),
  Panel = wxPanel:new(Frame),
  % Create a ListCtrl to display windows
  ListCtrl = wxListCtrl:new(Panel, [{style, ?wxLC_REPORT}, {size, {900, 850}}]),
  wxListCtrl:insertColumn(ListCtrl, 0, "Windows", []),
  % Create a TextCtrl for filtering
  FilterTextBox = wxTextCtrl:new(Panel, ?wxID_ANY, []),
  % Create a sizer to arrange the controls
  Sizer = wxBoxSizer:new(?wxVERTICAL),
  SzFlags = [{proportion, 0}, {border, 4}, {flag, ?wxALL bor ?wxEXPAND}],
  wxSizer:add(Sizer, FilterTextBox, SzFlags),
  wxSizer:add(Sizer, Button, SzFlags),
  wxSizer:add(Sizer, ListCtrl, SzFlags),
  wxPanel:setSizer(Panel, Sizer),
  wxSizer:fit(Sizer, Panel),
  wxFrame:fit(Frame),
  wxFrame:show(Frame),
  IL = wxImageList:new(16, 16),
  wxImageList:add(IL, wxArtProvider:getBitmap("wxART_COPY", [{size, {16, 16}}])),
  wxImageList:add(IL, wxArtProvider:getBitmap("wxART_MISSING_IMAGE", [{size, {16, 16}}])),
  wxImageList:add(IL, wxArtProvider:getBitmap("wxART_QUESTION", [{size, {16, 16}}])),
  wxImageList:add(IL, wxArtProvider:getBitmap("wxART_WARNING", [{size, {16, 16}}])),
  wxListCtrl:assignImageList(ListCtrl, IL, ?wxIMAGE_LIST_SMALL),
  % Populate the ListCtrl with window data
  wxListCtrl:setColumnWidth(ListCtrl, 0, ?WINDOW_WIDTH),
  WindowList = populate_window_list(ListCtrl),
  wxWindow:setFocus(FilterTextBox),
  % Bind events
  wxEvtHandler:connect(FilterTextBox, command_text_updated, []),
  wxListCtrl:connect(ListCtrl, command_list_item_activated, []),
  wxButton:connect(Button, key_up, []),
  wxTextCtrl:connect(FilterTextBox, key_up, []),
  wxListCtrl:connect(ListCtrl, key_up, []),
  wxPanel:connect(Panel, key_up, []),
  wxFrame:connect(Frame, key_up, []),
  % Start the main event loop
  wxFrame:show(Frame),
  gproc:reg_other({n, l, {?MODULE, erm_keypad}}, self()),
  {Frame, #state{parent = Frame, config = Config, list = ListCtrl, windows = WindowList}}.


close() ->
  case gproc:lookup_local_name({?MODULE, erm_keypad}) of
    undefined -> ok;
    Pid -> wx_object:call(Pid, shutdown)
  end.


show() ->
  case gproc:lookup_local_name({?MODULE, erm_bar}) of
    undefined -> ok;
    Pid -> wx_object:call(Pid, show)
  end.


add_list_item(_, #{'QTILE_INTERNAL' := _} = _Item) -> ok;

add_list_item(ListCtrl, #{'_NET_WM_NAME' := Title} = _Item) ->
  wxListCtrl:insertItem(ListCtrl, 0, ""),
  wxListCtrl:setItem(ListCtrl, 0, 0, Title);

add_list_item(_, _Item) -> ok.


populate_window_list(ListCtrl) ->
  % Call the NIF function to get window list
  WindowList = x11:list_windows(),
  lists:foreach(
    fun
      ({WindowId, Properties}) ->
        ?LOG_DEBUG("List item ~p ~p", [WindowId, Properties]),
        add_list_item(ListCtrl, maps:from_list(Properties))
    end,
    WindowList
  ),
WindowList.


filter_windows(FilterTextBox, ListCtrl) ->
  FilterText = wxTextCtrl:getValue(FilterTextBox),
  FilterString = unicode:characters_to_list(FilterText),
  wxListCtrl:deleteAllItems(ListCtrl),
  {ok, WindowList} = x11:list_windows(),
  lists:foreach(
    fun
      (Window) ->
        case string:contains(integer_to_list(Window), FilterString) of
          true ->
            wxListCtrl:insertItem(
              ListCtrl,
              [{index, wxListCtrl:getItemCount(ListCtrl)}, {text, integer_to_list(Window)}]
            );

          _ -> ok
        end
    end,
    WindowList
  ).



%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info

handle_event(
  #wx{id = Id, event = #wxCommand{type = command_button_clicked}},
  State = #state{parent = Parent}
) ->
  B0 = wxWindow:findWindowById(Id, [{parent, Parent}]),
  Butt = wx:typeCast(B0, wxButton),
  Label = wxButton:getLabel(Butt),
  ?LOG_DEBUG("Button: '~ts' clicked~n", [Label]),
  {noreply, State};

handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked}}, State = #state{}) ->
  ?LOG_DEBUG("Button: You toggled the 'Toggle button' ~n", []),
  {noreply, State};

handle_event(_Ev = #wx{event = #wxKey{keyCode = 27}}, State = #state{parent = Frame}) ->
  ?LOG_DEBUG("Got Escape Key ~n", []),
  wxWindow:close(Frame),
  {noreply, State};

handle_event(_Ev = #wx{event = #wxKey{keyCode = KeyCode}}, State = #state{}) ->
  ?LOG_DEBUG("Got Key Event ~p~n", [KeyCode]),
  {noreply, State};
handle_event(_Ev = #wx{event = #wxList{itemIndex = ItemIndex}}, State = #state{parent = Frame, windows= Windows}) ->
  ?LOG_DEBUG("Got ListItemIndex ~p~n", [ItemIndex]),
    {WindowId, Props} = Item = lists:nth(ItemIndex, Windows),
  ?LOG_DEBUG("Got ListItem ~p~n", [Item]),
    case proplists:get_value('_NET_WM_DESKTOP', Props) of
        undefined ->
            ok;
        DesktopId ->  
  ?LOG_DEBUG("Got desktopid ~p~n", [DesktopId]),
    x11:switch_desktop(DesktopId)
end,
    x11:switch_window(WindowId),
  ?LOG_DEBUG("Got windowid ~p~n", [WindowId]),
  wxWindow:close(Frame),
  {noreply, State};

handle_event(Ev = #wx{}, State = #state{}) ->
  ?LOG_DEBUG("Got Event ~p~n", [Ev]),
  {noreply, State}.

%% Callbacks handled as normal gen_server callbacks

handle_info(Msg, State) ->
  ?LOG_DEBUG("Got Info ~p~n", [Msg]),
  {noreply, State}.


handle_call(shutdown, _From, State = #state{parent = Panel}) ->
  wxWindow:close(Panel),
  {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
  ?LOG_DEBUG("Got Call ~p~n", [Msg]),
  {reply, ok, State}.


handle_cast(Msg, State) ->
  ?LOG_DEBUG("Got cast ~p~n", [Msg]),
  {noreply, State}.


code_change(_, _, State) -> {stop, ignore, State}.

terminate(_Reason, _State = #state{parent = Frame}) ->
  wxFrame:destroy(Frame),
  wx:destroy().
