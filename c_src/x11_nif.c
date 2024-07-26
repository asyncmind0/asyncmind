#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>  // For XTextProperty

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "erl_nif.h"

static ERL_NIF_TERM list_windows(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM switch_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM kill_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"list_windows", 0, list_windows},
    {"switch_window", 1, switch_window},
    {"kill_window", 1, kill_window}
};

static Display *display;
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

static ERL_NIF_TERM switch_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Window window_id;
    XEvent xev;

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

ERL_NIF_INIT(x11, nif_funcs, load, NULL, upgrade, unload)

