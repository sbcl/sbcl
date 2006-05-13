/* An approximation of Linux futexes implemented using pthread mutexes
 * and pthread condition variables.
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * The software is in the public domain and is provided with
 * absolutely no warranty. See the COPYING and CREDITS files for more
 * information.
 */

#include "sbcl.h"

#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_SB_LUTEX)

#include <stdlib.h>

#include "runtime.h"
#include "arch.h"
#include "target-arch-os.h"
#include "os.h"

#include "genesis/lutex.h"

typedef unsigned long tagged_lutex_t;

/* FIXME: Add some real error checking. */

pthread_mutex_t lutex_register_lock = PTHREAD_MUTEX_INITIALIZER;

int
lutex_init (tagged_lutex_t tagged_lutex)
{
    int ret;
    struct lutex *lutex = (struct lutex*) native_pointer(tagged_lutex);

    lutex->mutex = malloc(sizeof(pthread_mutex_t));
    ret = pthread_mutex_init(lutex->mutex, NULL);

    thread_mutex_lock(&lutex_register_lock);
    gencgc_register_lutex(lutex);
    thread_mutex_unlock(&lutex_register_lock);

    if (ret)
        return ret;

    lutex->condition_variable = malloc(sizeof(pthread_cond_t));
    ret = pthread_cond_init(lutex->condition_variable, NULL);

    return ret;
}

int
lutex_wait (tagged_lutex_t tagged_lutex)
{
    int ret;
    struct lutex *lutex = (struct lutex*) native_pointer(tagged_lutex);

    pthread_mutex_lock(lutex->mutex);
    ret = pthread_cond_wait(lutex->condition_variable, lutex->mutex);
    pthread_mutex_unlock(lutex->mutex);

    return ret;
}

int
lutex_wake (tagged_lutex_t tagged_lutex, int n)
{
    int ret = 0;
    struct lutex *lutex = (struct lutex*) native_pointer(tagged_lutex);

    pthread_mutex_lock(lutex->mutex);

    /* The lisp-side code passes N=2**29-1 for a broadcast. */
    if (n >= ((1 << 29) - 1)) {
        /* CONDITION-BROADCAST */
        ret = pthread_cond_broadcast(lutex->condition_variable);
    } else{
        /* We're holding the lutex mutex, so a thread we're waking can't
         * re-enter the wait between to calls to pthread_cond_signal. Thus
         * we'll wake N different threads, instead of the same thread
         * N times.
         */
        while (n--) {
            ret = pthread_cond_signal(lutex->condition_variable);
            if (ret)
                return ret;
        }
    }
    pthread_mutex_unlock(lutex->mutex);

    return ret;
}

int
lutex_lock (tagged_lutex_t tagged_lutex)
{
    struct lutex *lutex = (struct lutex*) native_pointer(tagged_lutex);

    return pthread_mutex_lock(lutex->mutex);
}

int
lutex_unlock (tagged_lutex_t tagged_lutex)
{
    struct lutex *lutex = (struct lutex*) native_pointer(tagged_lutex);

    return pthread_mutex_unlock(lutex->mutex);
}

int
lutex_destroy (tagged_lutex_t tagged_lutex)
{
    struct lutex *lutex = (struct lutex*) native_pointer(tagged_lutex);

    if (lutex->condition_variable) {
        pthread_cond_destroy(lutex->condition_variable);
        free(lutex->condition_variable);
        lutex->condition_variable = NULL;
    }

    if (lutex->mutex) {
        pthread_mutex_destroy(lutex->mutex);
        free(lutex->mutex);
        lutex->mutex = NULL;
    }

    return 0;
}
#endif
