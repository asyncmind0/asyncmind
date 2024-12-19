-module(erm_dose).

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

-record(state, {parent, config, panel, font, status, text}).

-define(TIMER_INTERVAL, 1000).

%% 1 second update interval

start(Config) -> wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Config) ->
  wx:new(),
  wx:batch(fun () -> do_init(Config) end).


do_init(Config) ->
  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "erm_dose", [{size, {1440, 720}}]),
  Panel = wxPanel:new(Frame, []),
  wxWindow:connect(Panel, paint, []),
  wxWindow:connect(Panel, activate, []),
  Button = wxButton:new(Panel, ?wxID_ANY, [{label, "Dose"}]),
  %% Setup sizers
  SzFlags = [{proportion, 0}, {border, 4}, {flag, ?wxEXPAND bor ?wxALL}],
  % 4 rows, 10 columns, spacing 5
  Sizer = wxFlexGridSizer:new(1),
  %% Setup slider with range from 0 to 100
  %% and a start value of 25
  Min = 0,
  Max = 100,
  StartValue = 50,
  %% Horizontal slider (default) with label
  Slider =
    wxSlider:new(
      Panel,
      1,
      StartValue,
      Min,
      Max,
      [{style, ?wxSL_HORIZONTAL bor ?wxSL_LABELS bor ?wxSL_AUTOTICKS}]
    ),
  wxSlider:setPageSize(Slider, 1),
  wxSlider:setLineSize(Slider, 10),
  wxSlider:setThumbLength(Slider, 10),
  wxSlider:connect(Slider, command_slider_updated, []),
  Label = wxStaticText:new(Panel, ?wxID_ANY, "Dosage in milli grams (mg)"),
  Dose = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, integer_to_list(StartValue)}]),
  Font = wxFont:new(18, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
  wxTextCtrl:setFont(Dose, Font),
  wxTextCtrl:setFont(Label, Font),
  wxTextCtrl:setFont(Slider, Font),
  wxTextCtrl:setFont(Button, Font),
  wxTextCtrl:connect(Dose, command_text_updated, []),
  wxTextCtrl:connect(Dose, command_text_enter, []),
  wxTextCtrl:connect(Dose, text_maxlen, []),
  wxSizer:add(Sizer, Label),
  wxSizer:add(Sizer, Dose, SzFlags),
  wxSizer:add(Sizer, Slider, SzFlags),
  wxSizer:add(Sizer, Button, SzFlags),
  wxFrame:setSizer(Panel, Sizer),
  wxTextCtrl:connect(Dose, key_up, []),
  wxSlider:connect(Slider, key_up, []),
  wxButton:connect(Button, key_up, []),
  wxButton:connect(Button, command_button_clicked, []),
  wxPanel:connect(Panel, key_up, []),
  wxFrame:connect(Frame, key_up, []),
  %wxSizer:setSizeHints(Sizer, Frame),
  wxFrame:show(Frame),
  ?LOG_DEBUG("Frame ~p", [Frame]),
  gproc:reg_other({n, l, {?MODULE, erm_dose}}, self()),
  State = #state{parent = Frame, config = Config, panel = Panel, status = "stared", text = Dose},
  wx:batch(fun () -> update_panel(State) end),
  {Frame, State}.


close() ->
  case gproc:lookup_local_name({?MODULE, erm_keypad}) of
    undefined -> ok;
    Pid -> wx_object:call(Pid, shutdown)
  end.


show() ->
  case gproc:lookup_local_name({?MODULE, erm_bar}) of
    undefined -> start([]);
    Pid -> wx_object:call(Pid, show)
  end.

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
  {stop, normal, save_dose(State)};

handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked}}, State = #state{}) ->
  ?LOG_DEBUG("Button: You toggled the 'Toggle button' ~n", []),
  {noreply, State};

handle_event(_Ev = #wx{event = #wxKey{keyCode = 27}}, State = #state{parent = Frame}) ->
  ?LOG_DEBUG("Got Escape Key ~n", []),
  wxWindow:close(Frame),
  {noreply, State};

handle_event(
  _Ev = #wx{event = #wxCommand{type = command_slider_updated, commandInt = Value}},
  State = #state{text = TextCtrl}
) ->
  ?LOG_DEBUG("Got slider update  ~p~n", [Value]),
  wxTextCtrl:setValue(TextCtrl, integer_to_list(Value)),
  {noreply, State};

handle_event(Ev = #wx{}, State = #state{}) ->
  ?LOG_DEBUG("Got Event ~p~n", [Ev]),
  {noreply, State}.

%% Callbacks handled as normal gen_server callbacks

handle_info(Msg, State) ->
  ?LOG_DEBUG("Got Info ~p~n", [Msg]),
  {noreply, State}.


handle_call(shutdown, _From, State = #state{parent = Panel}) ->
  wxFrame:destroy(Panel),
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


update_panel(State) -> ?LOG_DEBUG("update panel ~p", [State]).

save_dose(State = #state{text = TextCtrl}) ->
  ?LOG_DEBUG("save dose ~p", [State]),
  OrgPath = "Org",
  {ok, Timestamp} = datestring:format("<Y-m-d a H:M>", erlang:localtime()),
  Value = wxTextCtrl:getValue(TextCtrl),
  Entry = "\n"++Timestamp ++ " 0." ++ Value ++ "g",
  file:write_file(filename:join([os:getenv("HOME"), OrgPath, "dose.org"]), Entry, [append]),
State.
