static ERL_NIF_TERM list_windows(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM switch_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM switch_desktop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM kill_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM set_window_property(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
// Function prototypes
static ERL_NIF_TERM start_event_listener(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM stop_event_listener(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static void handle_x11_events(void);
static void send_event_to_erlang(XEvent *xevent);

static Display *display;
static ErlNifPid gen_server_pid;
