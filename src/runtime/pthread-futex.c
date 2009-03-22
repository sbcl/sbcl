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

#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_SB_PTHREAD_FUTEX)

#include <errno.h>
#include <pthread.h>
#include <stdlib.h>

#include "runtime.h"
#include "arch.h"
#include "target-arch-os.h"
#include "os.h"

#define FUTEX_WAIT_NSEC (10000000) /* 10 msec */

#if 1
# define futex_assert(ex)                                              \
do {                                                                   \
    if (!(ex)) futex_abort();                                          \
} while (0)
# define futex_assert_verbose(ex, fmt, ...)                            \
do {                                                                   \
    if (!(ex)) {                                                       \
        fprintf(stderr, fmt, ## __VA_ARGS__);                          \
        futex_abort();                                                 \
    }                                                                  \
} while (0)
#else
# define futex_assert(ex)
# define futex_assert_verbose(ex, fmt, ...)
#endif

#define futex_abort()                                                  \
  lose("Futex assertion failure, file \"%s\", line %d\n", __FILE__, __LINE__)

struct futex {
    struct futex *prev;
    struct futex *next;
    int *lock_word;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    int count;
};

static pthread_mutex_t futex_lock = PTHREAD_MUTEX_INITIALIZER;

static struct futex *futex_head = NULL;
static struct futex *futex_free_head = NULL;

static struct futex *
futex_add(struct futex *head, struct futex *futex)
{
    futex->prev = NULL;
    futex->next = head;
    if (head != NULL)
        head->prev = futex;
    head = futex;

    return head;
}

static struct futex *
futex_delete(struct futex *head, struct futex *futex)
{
    if (head == futex)
        head = futex->next;
    if (futex->prev != NULL)
        futex->prev->next = futex->next;
    if (futex->next != NULL)
        futex->next->prev = futex->prev;

    return head;
}

static struct futex *
futex_find(struct futex *head, int *lock_word)
{
    struct futex *futex;

    for (futex = head; futex != NULL; futex = futex->next) {
        if (futex->lock_word == lock_word)
            break;
    }

    return futex;
}

static struct futex *
futex_get(int *lock_word)
{
    int ret;
    struct futex *futex;

    ret = pthread_mutex_lock(&futex_lock);
    futex_assert(ret == 0);

    futex = futex_find(futex_head, lock_word);

    if (futex != NULL)
        futex->count++;

    ret = pthread_mutex_unlock(&futex_lock);
    futex_assert(ret == 0);

    if (futex != NULL) {
        ret = pthread_mutex_lock(&futex->mutex);
        futex_assert(ret == 0);
    }

    return futex;
}

static struct futex *
futex_allocate(int *lock_word)
{
    int ret;
    struct futex *futex;

    ret = pthread_mutex_lock(&futex_lock);
    futex_assert(ret == 0);

    futex = futex_free_head;

    if (futex != NULL)
        futex_free_head = futex_delete(futex_free_head, futex);

    ret = pthread_mutex_unlock(&futex_lock);
    futex_assert(ret == 0);

    if (futex == NULL) {
        futex = malloc(sizeof(struct futex));
        futex_assert(futex != NULL);

        ret = pthread_mutex_init(&futex->mutex, NULL);
        futex_assert(ret == 0);

        ret = pthread_cond_init(&futex->cond, NULL);
        futex_assert(ret == 0);
    }

    futex->lock_word = lock_word;
    futex->count = 1;

    /* Lock mutex before register to avoid race conditions. */
    ret = pthread_mutex_lock(&futex->mutex);
    futex_assert(ret == 0);

    ret = pthread_mutex_lock(&futex_lock);
    futex_assert(ret == 0);

    futex_head = futex_add(futex_head, futex);

    ret = pthread_mutex_unlock(&futex_lock);
    futex_assert(ret == 0);

    return futex;
}

static void
futex_cleanup(void *p)
{
    struct futex *futex = (struct futex *)p;
    int ret, count;

    ret = pthread_mutex_lock(&futex_lock);
    futex_assert(ret == 0);

    count = --futex->count;
    if (count <= 0) {
        futex_head = futex_delete(futex_head, futex);
        futex_free_head = futex_add(futex_free_head, futex);
    }

    ret = pthread_mutex_unlock(&futex_lock);
    futex_assert(ret == 0);

    ret = pthread_mutex_unlock(&futex->mutex);
    futex_assert(ret == 0);
}

static int
futex_relative_to_abs(struct timespec *tp, int relative)
{
    int ret;
    struct timeval tv;

    ret = gettimeofday(&tv, NULL);
    if (ret != 0)
        return ret;
    tp->tv_sec = tv.tv_sec + (tv.tv_usec * 1000 + relative) / 1000000000;
    tp->tv_nsec = (tv.tv_usec * 1000 + relative) % 1000000000;
    return 0;
}

static int
futex_istimeout(struct timeval *timeout)
{
    int ret;
    struct timeval tv;

    if (timeout == NULL)
        return 0;

    ret = gettimeofday(&tv, NULL);
    if (ret != 0)
        return ret;

    return (tv.tv_sec > timeout->tv_sec) ||
        ((tv.tv_sec == timeout->tv_sec) && tv.tv_usec > timeout->tv_usec);
}

int
futex_wait(int *lock_word, int oldval, long sec, unsigned long usec)
{
    int ret, result;
    struct futex *futex;
    sigset_t oldset;
    struct timeval tv, *timeout;

again:
    if (sec < 0)
        timeout = NULL;
    else {
        ret = gettimeofday(&tv, NULL);
        if (ret != 0)
            return ret;
        tv.tv_sec = tv.tv_sec + sec + (tv.tv_usec + usec) / 1000000;
        tv.tv_usec = (tv.tv_usec + usec) % 1000000;
        timeout = &tv;
    }

    block_deferrable_signals(0, &oldset);

    futex = futex_get(lock_word);

    if (futex == NULL)
        futex = futex_allocate(lock_word);

    pthread_cleanup_push(futex_cleanup, futex);

    /* Compare lock_word after the lock is aquired to avoid race
     * conditions. */
    if (*(volatile int *)lock_word != oldval) {
        result = EWOULDBLOCK;
        goto done;
    }

    /* It's not possible to unwind frames across pthread_cond_wait(3). */
    for (;;) {
        int i;
        sigset_t pendset;
        struct timespec abstime;

        ret = futex_relative_to_abs(&abstime, FUTEX_WAIT_NSEC);
        futex_assert(ret == 0);

        result = pthread_cond_timedwait(&futex->cond, &futex->mutex,
                                        &abstime);
        futex_assert(result == 0 || result == ETIMEDOUT);

        if (result != ETIMEDOUT || futex_istimeout(timeout))
            break;

        /* futex system call of Linux returns with EINTR errno when
         * it's interrupted by signals.  Check pending signals here to
         * emulate this behaviour. */
        sigpending(&pendset);
        for (i = 1; i < NSIG; i++) {
            if (sigismember(&pendset, i) && sigismember(&newset, i)) {
                result = EINTR;
                goto done;
            }
        }
    }
done:
    ; /* Null statement is required between label and pthread_cleanup_pop. */
    pthread_cleanup_pop(1);
    pthread_sigmask(SIG_SETMASK, &oldset, NULL);

    /* futex_wake() in linux-os.c loops when futex system call returns
     * EINTR.  */
    if (result == EINTR) {
        sched_yield();
        goto again;
    }

    if (result == ETIMEDOUT)
        return 1;

    return result;
}

int
futex_wake(int *lock_word, int n)
{
    int ret;
    struct futex *futex;
    sigset_t oldset;

    block_deferrable_signals(0, &oldset);

    futex = futex_get(lock_word);

    if (futex != NULL) {
        pthread_cleanup_push(futex_cleanup, futex);

        /* The lisp-side code passes N=2**29-1 for a broadcast. */
        if (n >= ((1 << 29) - 1)) {
            /* CONDITION-BROADCAST */
            ret = pthread_cond_broadcast(&futex->cond);
            futex_assert(ret == 0);
        } else {
            while (n-- > 0) {
                ret = pthread_cond_signal(&futex->cond);
                futex_assert(ret == 0);
            }
        }

        pthread_cleanup_pop(1);
    }

    pthread_sigmask(SIG_SETMASK, &oldset, NULL);

    return 0;
}
#endif
