-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) -> {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  erlang:start_timer(1000, self(), <<"Hello!">>),
  logger:error("init ", []),
  {[], State}.


websocket_handle({text, <<"launchers">>}, State) ->
  Launchers =
    [
      #{
        <<"label">> => <<"Spotify">>,
        <<"key">> => <<"spotify">>,
        <<"command">>
        =>
        <<
          "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
        >>
      },
      #{<<"label">> => <<"Slack">>, <<"key">> => <<"spotify">>, <<"command">> => <<"Slack">>},
      #{<<"label">> => <<"Discord">>, <<"key">> => <<"discord">>, <<"command">> => <<"discord">>},
      #{<<"label">> => <<"Steam">>, <<"key">> => <<"steam">>, <<"command">> => <<"steam">>},
      #{<<"label">> => <<"Chrome">>, <<"key">> => <<"chrome">>, <<"command">> => <<"Chrome">>},
      #{<<"label">> => <<"Climate">>, <<"key">> => <<"climate">>, <<"command">> => <<"climate">>}
    ],
  Encoded = jsx:encode(#{event => <<"launchers">>, launchers => Launchers}),
  {[{text, Encoded}], State};

websocket_handle({text, Data}, State) ->
  Msg = jsx:decode(Data),
  logger:error("rec ~p", [Msg]),
  {[{text, jsx:encode(#{<<"status">> => <<"ok">>})}], State}.


websocket_info({timeout, _Ref, Msg}, State) -> {[], State};
websocket_info(_Info, State) -> {[], State}.
