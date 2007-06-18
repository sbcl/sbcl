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

#include <errno.h>
#include <stdlib.h>

#include "runtime.h"
#include "arch.h"
#include "target-arch-os.h"
#include "os.h"
#include "pthread-lutex.h"
#include "gencgc.h"

#include "genesis/lutex.h"

#if 1
# define lutex_assert(ex)                                              \
do {                                                                   \
    if (!(ex)) lutex_abort();                                          \
} while (0)
# define lutex_assert_verbose(ex, fmt, ...)                            \
do {                                                                   \
    if (!(ex)) {                                                       \
        fprintf(stderr, fmt, ## __VA_ARGS__);                          \
        lutex_abort();                                                 \
    }                                                                  \
} while (0)
#else
# define lutex_assert(ex)
# define lutex_assert_verbose(ex, fmt, ...)
#endif

#define lutex_abort()                                                  \
  lose("Lutex assertion failure, file \"%s\", line %d\n", __FILE__, __LINE__)


pthread_mutex_t lutex_register_lock = PTHREAD_MUTEX_INITIALIZER;

int
lutex_init (tagged_lutex_t tagged_lutex)
{
    int ret;
    struct lutex *lutex = (struct lutex*) native_pointer(tagged_lutex);

    lutex->mutexattr = malloc(sizeof(pthread_mutexattr_t));
    lutex_assert(lutex->mutexattr != 0);

    ret = pthread_mutexattr_init(lutex->mutexattr);
    lutex_assert(ret == 0);

    /* The default type of mutex is implementation dependent.
     * We use PTHREAD_MUTEX_ERRORCHECK so that locking on mutexes
     * locked by the same thread does not cause deadlocks. */
    /* FIXME: pthread_mutexattr_settype is available on SUSv2 level
     * implementations.  Can be used without checking? */
    ret = pthread_mutexattr_settype(lutex->mutexattr,
                                    PTHREAD_MUTEX_ERRORCHECK);
    lutex_assert(ret == 0);

    lutex->mutex = malloc(sizeof(pthread_mutex_t));
    lutex_assert(lutex->mutex != 0);

    ret = pthread_mutex_init(lutex->mutex, lutex->mutexattr);
    lutex_assert(ret == 0);

    lutex->condition_variable = malloc(sizeof(pthread_cond_t));
    lutex_assert(lutex->condition_variable != 0);

    ret = pthread_cond_init(lutex->condition_variable, NULL);
    lutex_assert(ret == 0);

    ret = thread_mutex_lock(&lutex_register_lock); lutex_assert(ret == 0);

    gencgc_register_lutex(lutex);

    ret = thread_mutex_unlock(&lutex_register_lock); lutex_assert(ret == 0);

    return ret;
}

int
lutex_wait (tagged_lutex_t tagged_queue_lutex, tagged_lutex_t tagged_mutex_lutex)
{
    int ret;
    struct lutex *queue_lutex = (struct lutex*) native_pointer(tagged_queue_lutex);
    struct lutex *mutex_lutex = (struct lutex*) native_pointer(tagged_mutex_lutex);

    ret = pthread_cond_wait(queue_lutex->condition_variable, mutex_lutex->mutex);
    lutex_assert(ret == 0);

    return ret;
}

int
lutex_wake (tagged_lutex_t tagged_lutex, int n)
{
    int ret = 0;
    struct lutex *lutex = (struct lutex*) native_pointer(tagged_lutex);

    /* The lisp-side code passes N=2**29-1 for a broadcast. */
    if (n >= ((1 << 29) - 1)) {
        /* CONDITION-BROADCAST */
        ret = pthread_cond_broadcast(lutex->condition_variable);
        lutex_assert(ret == 0);
    } else{
        /* We're holding the condition variable mutex, so a thread
         * we're waking can't re-enter the wait between to calls to
         * pthread_cond_signal. Thus we'll wake N different threads,
         * instead of the same thread N times.
         */
        while (n--) {
            ret = pthread_cond_signal(lutex->condition_variable);
            lutex_assert(ret == 0);
        }
    }

    return ret;
}

int
lutex_lock (tagged_lutex_t tagged_lutex)
{
    int ret = 0;
    struct lutex *lutex = (struct lutex*) native_pointer(tagged_lutex);

    ret = thread_mutex_lock(lutex->mutex);
    /* The mutex is locked by the same thread.
     *
     * FIXME: Usually when POSIX says that "an error value is returned"
     * it actually refers to errno...
     */
    if (ret == EDEADLK)
        return ret;
    lutex_assert(ret == 0);

    return ret;
}

int
lutex_trylock (tagged_lutex_t tagged_lutex)
{
    int ret = 0;
    struct lutex *lutex = (struct lutex*) native_pointer(tagged_lutex);

    ret = pthread_mutex_trylock(lutex->mutex);
    /* The mutex is locked */
    if (ret == EDEADLK || ret == EBUSY)
        return ret;
    lutex_assert(ret == 0);

    return ret;
}

int
lutex_unlock (tagged_lutex_t tagged_lutex)
{
    int ret = 0;
    struct lutex *lutex = (struct lutex*) native_pointer(tagged_lutex);

    ret = thread_mutex_unlock(lutex->mutex);
    /* Unlocking unlocked mutex would occur as:
     * (with-mutex (mutex) (cond-wait cond mutex)) */
    if (ret == EPERM)
        return ret;
    lutex_assert(ret == 0);

    return ret;
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

    if (lutex->mutexattr) {
        pthread_mutexattr_destroy(lutex->mutexattr);
        free(lutex->mutexattr);
        lutex->mutexattr = NULL;
    }

    return 0;
}
#endif
