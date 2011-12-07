/* Copyright (c) 2011, Michael Santos <michael.santos@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the author nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include "erl_nif.h"
#include "erl_driver.h"
#include "everl.h"
#include "everl_ev.h"


static ERL_NIF_TERM error_tuple(ErlNifEnv *env, int errnum);
void alloc_free(ErlNifEnv *env, void *obj);

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_eagain;
static ERL_NIF_TERM atom_event;

static ErlNifResourceType *EVERL_WATCHER_RESOURCE;

typedef struct _event_state {
    ErlNifTid       tid;
    ErlNifMutex    *mutex;
    struct ev_loop *env;
    ev_async        kick;
    ev_async        die;
} EVENT_STATE;

typedef struct _alloc_state {
    size_t size;
    void *buf;
} ALLOC_STATE;

typedef struct _watcher_state {
    /* will segfault if ev_io is not
     * the first field of the struct */
    ev_io         io;

    struct ev_loop *loop;
    ev_async        kick;
    ErlNifEnv    *env;
    ErlNifPid     pid;
} WATCHER_STATE;


/* libev processing
 * 
 * We set up a separate thread to listen for libev registered events
 *
 */
    static void
kick_cb(struct ev_loop *loop, ev_async *w, int revents)
{
    // just used for the side effects..this callback forces the loop
    // to re-eval the current set of watchers
}

    static void
die_cb(struct ev_loop *loop, ev_async *w, int revents)
{
    ev_break(loop, EVBREAK_ALL);
}

    static void
l_release(struct ev_loop *loop)
{
    EVENT_STATE *el = NULL;


    if (loop == NULL)
        return;

    el = ev_userdata(loop);

    if (el == NULL)
        return;

    enif_mutex_unlock(el->mutex);
}

    static void
l_acquire(struct ev_loop *loop)
{
    EVENT_STATE *el = NULL;


    if (loop == NULL)
        return;

    el = ev_userdata(loop);

    if (el == NULL)
        return;

    enif_mutex_lock(el->mutex);
}

    void*
event_loop(void *arg)
{
    EVENT_STATE *el = arg;

    l_acquire (el->env);
    ev_run (el->env, 0);
    l_release (el->env);

    return NULL;
}

    static void
watcher_cb(struct ev_loop *loop, ev_io *w_, int revents)
{
    WATCHER_STATE *w = (WATCHER_STATE *)w_;
    EVENT_STATE *el = ev_userdata(loop);
    ERL_NIF_TERM msg;


    if (el == NULL)
        return;

    ev_io_stop(el->env, &w->io);

    msg = enif_make_tuple3(w->env,
                           atom_event,
                           enif_make_int(w->env, w->io.fd),
                           enif_make_int(w->env, revents)
        );

    (void)enif_send(NULL, &w->pid, w->env, msg);

    enif_clear_env(w->env);
}

    void
watcher_free(ErlNifEnv *env, void *obj)
{
    WATCHER_STATE *w = obj;


    if (w == NULL || w->loop == NULL)
        return;

    l_acquire(w->loop);
    ev_io_stop(w->loop, &w->io);
    ev_async_send(w->loop, &w->kick);
    l_release(w->loop);

    enif_free_env(w->env);
}

    static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    EVENT_STATE *el = NULL;


    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_eagain = enif_make_atom(env, "eagain");
    atom_event = enif_make_atom(env, "everl_watcher");

    if ( (EVERL_WATCHER_RESOURCE = enif_open_resource_type(env, NULL,
        "everl_watcher_resource", watcher_free,
        ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    el = enif_alloc(sizeof(*el));
    (void)memset(el, 0, sizeof(*el));

    el->mutex = enif_mutex_create("libev");
    if (!el->mutex)
        return -1;

    el->env = ev_default_loop(0);

    ev_async_init(&el->kick, kick_cb);
    ev_async_start(el->env, &el->kick);

    ev_async_init(&el->die, die_cb);
    ev_async_start(el->env, &el->die);

    ev_set_userdata(el->env, el);
    ev_set_loop_release_cb(el->env, l_release, l_acquire);

    if (enif_thread_create("libev", &el->tid, event_loop, el, NULL) < 0)
        return -1;

    *priv_data = el;

    return (0);
}

    static void
unload(ErlNifEnv* env, void *priv_data)
{
    EVENT_STATE *el = priv_data;


    if (el == NULL)
        return;

    /* send an asynchronous ev_break */
    l_acquire(el->env);
    ev_async_send(el->env, &el->die);
    l_release(el->env);
    
    /* block here before cleaning up remaining resources */
    enif_thread_join(el->tid, NULL);

    enif_mutex_destroy(el->mutex);
}

   static void
watcher_arm(ErlNifEnv *env, WATCHER_STATE *w)
{
    EVENT_STATE *el = enif_priv_data(env);


    if (el == NULL)
        return;

    l_acquire(el->env);
    ev_io_start(el->env, &w->io);
    ev_async_send(el->env, &el->kick);
    l_release(el->env);
} 

/* 0: fd, 1: flags, 2: term */
    static ERL_NIF_TERM
nif_watcher_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    WATCHER_STATE *w = NULL;
    ERL_NIF_TERM ret = enif_make_badarg(env);
    int fd;
    int flags;

    EVENT_STATE *el = enif_priv_data(env);


    if (!enif_get_int(env, argv[0], &fd))
      goto out;

    if (!enif_get_int(env, argv[1], &flags))
      goto out;

    if (flags & ~(EV_READ | EV_WRITE))
      goto out;

    w = enif_alloc_resource(EVERL_WATCHER_RESOURCE, sizeof(WATCHER_STATE));
    if (w == NULL)
      return error_tuple(env, ENOMEM);

    w->env = enif_alloc_env();
    if (!w->env) {
      ret = error_tuple(env, ENOMEM);
      goto out;
    }

    w->loop = el->env;
    (void)memcpy(&w->kick, &el->kick, sizeof(ev_async));

    enif_self(env, &w->pid);

    ev_io_init(&w->io, watcher_cb, fd, flags);
    watcher_arm(env, w);

    ret = enif_make_tuple2(env, atom_ok, enif_make_resource(env, w));

 out:
    if (w)
      enif_release_resource(w);

    return ret;

}

/* 0: watcher */
    static ERL_NIF_TERM
nif_watcher_arm(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    WATCHER_STATE *w;

    if(!enif_get_resource(env, argv[0], EVERL_WATCHER_RESOURCE, (void**)&w))
      return enif_make_badarg(env);
 
    watcher_arm(env, w);

    return atom_ok;
}

/* 0: watcher */
    static ERL_NIF_TERM
nif_watcher_disarm(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    EVENT_STATE *el = enif_priv_data(env);
    WATCHER_STATE *w;

    if(!enif_get_resource(env, argv[0], EVERL_WATCHER_RESOURCE, (void**)&w))
      return enif_make_badarg(env);

    l_acquire(el->env);
    ev_io_stop(el->env, &w->io);
    ev_async_send(el->env, &el->kick);
    l_release(el->env);

    return atom_ok;
}

    static ERL_NIF_TERM
error_tuple(ErlNifEnv *env, int errnum)
{
    return enif_make_tuple(env, 2,
            atom_error,
            enif_make_atom(env, erl_errno_id(errnum)));
}


static ErlNifFunc nif_funcs[] = {
    {"watcher_create", 2, nif_watcher_create},
    {"watcher_arm", 1, nif_watcher_arm},
    {"watcher_disarm", 1, nif_watcher_disarm}
};

ERL_NIF_INIT(everl, nif_funcs, load, NULL, NULL, unload)
