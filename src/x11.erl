-module(x11).
-export([switch_window/1, list_windows/0, kill_window/1]).
-nifs([switch_window/1, list_windows/0, kill_window/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./c_src/x11_nif", 0).


list_windows() ->
    erlang:nif_error(nif_library_not_loaded).
switch_window(_WindowId) ->
    erlang:nif_error(nif_library_not_loaded).
kill_window(_Y) ->
    erlang:nif_error(nif_library_not_loaded).
