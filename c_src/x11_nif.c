#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>  // For XTextProperty
#include <X11/Xresource.h>
#include <unistd.h> // Include for usleep


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "erl_nif.h"
#include "x11_nif.h"
#include <pthread.h>




static ErlNifFunc nif_funcs[] = {
    {"list_windows", 0, list_windows},
    {"switch_window", 1, switch_window},
    {"switch_desktop", 1, switch_desktop},
    {"kill_window", 1, kill_window},
	{"set_window_property", 3, set_window_property},
    {"start_event_listener", 1, start_event_listener},
    {"stop_event_listener", 0, stop_event_listener}

};

// Called when the NIF library is loaded
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    display = XOpenDisplay(NULL);
    if (display == NULL) {
        return -1; // Error opening display
    }
    return 0; // Success
}

// Called when the NIF library is unloaded
static void unload(ErlNifEnv* env, void* priv_data) {
    if (display != NULL) {
        XCloseDisplay(display);
        display = NULL;
    }
}

// Called when the NIF library is upgraded (for hot code swapping)
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
    // Optionally handle any state migration here
    return 0; // Success
}

static int handle_x_error(Display *display, XErrorEvent *error) {
    fprintf(stderr, "X Error: Error opcode of failed request: %d\n", error->error_code);
    fprintf(stderr, "X Error: Minor opcode of failed request: %d\n", error->minor_code);
    fprintf(stderr, "X Error: Serial number of failed request: %lu\n", error->serial);
    fprintf(stderr, "X Error: Current serial number: %lu\n", XLastKnownRequestProcessed(display));
    return 0; // Return 0 to ignore the error
}

// Convert an X11 atom data to an Erlang term
static ERL_NIF_TERM atom_to_term(ErlNifEnv* env, Atom atom, unsigned char *data, int format) {
    ERL_NIF_TERM term;
    if (format == 8) { // 8-bit data
        term = enif_make_string(env, (char *)data, ERL_NIF_LATIN1);
    } else if (format == 16) { // 16-bit data
        term = enif_make_string(env, (char *)data, ERL_NIF_LATIN1);
    } else if (format == 32) { // 32-bit data
        term = enif_make_int(env, *(int *)data);
    } else {
        // For binary data
        ErlNifBinary bin;
        bin.size = sizeof(data);
        bin.data = data;
        term = enif_make_binary(env, &bin);
    }
    return term;
}

// Get all properties for a given window
static ERL_NIF_TERM get_window_properties(ErlNifEnv* env, Window win) {
    Atom atom, actual_type;
    int format;
    unsigned long nitems, bytes_after;
    unsigned char *data;
    Atom *atom_list;
    int nproperties;
    ERL_NIF_TERM props_list = enif_make_list(env, 0);
    int i;

    // Get a list of all property atoms
    atom_list = XListProperties(display, win, &nproperties);
    if (atom_list == NULL) {
        return props_list; // No properties found
    }

    for (i = 0; i < nproperties; i++) {
        atom = atom_list[i];
        if (XGetWindowProperty(display, win, atom, 0, (~0L), False, AnyPropertyType,
                               &actual_type, &format, &nitems, &bytes_after, &data) == Success) {
            if (data != NULL) {
                ERL_NIF_TERM property = atom_to_term(env, atom, data, format);
                props_list = enif_make_list_cell(env, 
                    enif_make_tuple2(env, 
                        enif_make_atom(env, XGetAtomName(display, atom)), 
                        property), 
                    props_list);
                XFree(data);
            }
        }
    }
    XFree(atom_list);
    return props_list;
}

// Convert an Erlang term to an X11 Atom
static Atom term_to_atom(ErlNifEnv* env, const ERL_NIF_TERM term) {
    char atom_name[256];
    if (enif_get_atom(env, term, atom_name, sizeof(atom_name), ERL_NIF_LATIN1)) {
        return XInternAtom(display, atom_name, False);
    }
    return None;
}

// Convert an Erlang term to a property value
static void term_to_property_value(ErlNifEnv* env, ERL_NIF_TERM term, Atom *atom, int *format, unsigned char **data, unsigned long *size) {
    ErlNifBinary bin;
    if (enif_inspect_binary(env, term, &bin)) {
        *data = bin.data;
        *size = bin.size;
        *format = 8; // Assuming 8-bit data
        *atom = None; // Not an Atom
    } else if (enif_get_int(env, term, (int*)data)) {
        *size = sizeof(int);
        *format = 32; // Assuming 32-bit data
        *atom = None; // Not an Atom
    } else {
        *atom = term_to_atom(env, term);
        *data = NULL;
        *size = 0;
        *format = 0; // No specific format
    }
}

// Set a window property
static ERL_NIF_TERM set_window_property(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Window window_id;
    ERL_NIF_TERM property_term;
    ERL_NIF_TERM value_term;
    Atom property;
    Atom type;
    int format;
    unsigned char *data;
    unsigned long size;

    if (!enif_get_int(env, argv[0], (int*)&window_id) ||
        !enif_get_atom(env, argv[1], (char *)&property_term, sizeof(property_term), ERL_NIF_LATIN1) ||
        !enif_get_int(env, argv[2], (int*)&value_term)) { // Use enif_get_int for integers
        return enif_make_badarg(env);
    }

    type = XInternAtom(display, "STRING", False); // Default type
    property = term_to_atom(env, property_term);

    term_to_property_value(env, value_term, &property, &format, &data, &size);

    if (data) {
        XChangeProperty(display, window_id, property, type, format, PropModeReplace, data, size / (format / 8));
    } else {
        XChangeProperty(display, window_id, property, type, 32, PropModeReplace, (unsigned char*)&property, 1);
    }

    XFlush(display);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM list_windows(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Window root;
    Window parent;
    Window *children;
    unsigned int nchildren;
    ERL_NIF_TERM window_list = enif_make_list(env, 0);
    int i;

    if (display == NULL) {
        return enif_make_badarg(env);
    }

    root = DefaultRootWindow(display);
    if (XQueryTree(display, root, &root, &parent, &children, &nchildren)) {
        for (i = 0; i < nchildren; i++) {
            ERL_NIF_TERM window_props = get_window_properties(env, children[i]);
            window_list = enif_make_list_cell(env, 
                enif_make_tuple2(env, 
                    enif_make_int(env, (int)children[i]), 
                    window_props),
                window_list);
        }
        XFree(children);
    } else {
        return enif_make_badarg(env);
    }

    return window_list;
}
static ERL_NIF_TERM switch_desktop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	Window root;
	Atom atom;
	    int desktop_number;


	// Get the desktop number from the argument
    if (!enif_get_int(env, argv[0], &desktop_number)) {
        return enif_make_badarg(env);
    }

    if (display == NULL) {
        display = XOpenDisplay(NULL);
        if (display == NULL) {
            return enif_make_badarg(env);
        }
    }
	// Set up an error handler
    XSetErrorHandler(handle_x_error);


	 root = DefaultRootWindow(display);

    // Atom for changing desktop (you may need to customize this depending on your WM)
    atom = XInternAtom(display, "_NET_WM_DESKTOP", False);

    // Change desktop (set the property for the root window)
    XChangeProperty(
		display, root, atom, XA_CARDINAL, 32, PropModeReplace, (unsigned char *)&desktop_number, 1);


    XFlush(display);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM switch_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Window window_id;

    if (enif_get_int(env, argv[0], (int*)&window_id) == 0) {
        return enif_make_badarg(env);
    }

    if (display == NULL) {
        display = XOpenDisplay(NULL);
        if (display == NULL) {
            return enif_make_badarg(env);
        }
    }
	// Set up an error handler
    XSetErrorHandler(handle_x_error);


    XRaiseWindow(display, window_id);
    XSetInputFocus(display, window_id, RevertToNone, CurrentTime);

    XFlush(display);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM kill_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Window window_id;

    if (enif_get_int(env, argv[0], (int*)&window_id) == 0) {
        return enif_make_badarg(env);
    }

    if (display == NULL) {
        display = XOpenDisplay(NULL);
        if (display == NULL) {
            return enif_make_badarg(env);
        }
    }

    XKillClient(display, window_id);
    XFlush(display);
    return enif_make_atom(env, "ok");
}


void *event_loop(void *arg) {
    XEvent event;
    while (1) {
        if (XPending(display) > 0) {
            XNextEvent(display, &event);
            // Send event to Erlang gen_server
            ErlNifEnv *env = enif_alloc_env();
            ERL_NIF_TERM event_term = enif_make_tuple2(env,
                enif_make_int(env, event.type), // Example: send event type
                enif_make_string(env, "Event details", ERL_NIF_LATIN1));
            enif_send(NULL, &gen_server_pid, env, event_term);
            enif_free_env(env);
        }
        usleep(10000); // Sleep for 10 ms
    }
    return NULL;
}

static ERL_NIF_TERM start_event_listener(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1 || !enif_get_local_pid(env, argv[0], &gen_server_pid)) {
        return enif_make_badarg(env);
    }

    display = XOpenDisplay(NULL);
    if (!display) {
        return enif_make_atom(env, "error");
    }

    pthread_t thread;
    if (pthread_create(&thread, NULL, event_loop, NULL) != 0) {
        return enif_make_atom(env, "error");
    }

    return enif_make_atom(env, "ok");
}

// Function to stop the event listener
static ERL_NIF_TERM stop_event_listener(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // Clean up code if needed
    return enif_make_atom(env, "ok");
}

ERL_NIF_INIT(x11, nif_funcs, load, NULL, upgrade, unload)

