/*
 * This is the Darwin incarnation of OS-dependent routines. See also
 * "bsd-os.c".
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#include "thread.h"
#include "genesis/sbcl.h"
#include "globals.h"
#include "runtime.h"
#include "interr.h"
#include <signal.h>
#include <limits.h>
#include <mach-o/dyld.h>
#include <stdio.h>
#include <errno.h>
#include <dlfcn.h>
#include <pthread.h>

char *os_get_runtime_executable_path()
{
    char path[PATH_MAX + 1];
    uint32_t size = sizeof(path);

    if (_NSGetExecutablePath(path, &size) == -1)
        return NULL;

    return copied_string(path);
}

void
darwin_reinit() {
#ifdef LISP_FEATURE_SB_THREAD
    struct extra_thread_data *extra_data = thread_extra_data(get_sb_vm_thread());
#ifndef LISP_FEATURE_SB_SAFEPOINT
    os_sem_init(&extra_data->state_sem, 1);
    os_sem_init(&extra_data->state_not_running_sem, 0);
    os_sem_init(&extra_data->state_not_stopped_sem, 0);
#endif
    os_sem_init(&extra_data->sprof_sem, 0);
#endif
}

void darwin_init(void)
{

}


#if defined LISP_FEATURE_SB_THREAD && defined USE_DARWIN_GCD_SEMAPHORES

inline void
os_sem_init(os_sem_t *sem, unsigned int value)
{
    if (!(*sem = dispatch_semaphore_create(value)))
        lose("os_sem_init(%p): %s", sem, strerror(errno));
}

inline void
os_sem_wait(os_sem_t *sem)
{
    dispatch_semaphore_wait(*sem, DISPATCH_TIME_FOREVER);
}

void
os_sem_post(os_sem_t *sem)
{
    dispatch_semaphore_signal(*sem);
}

void
os_sem_destroy(os_sem_t *sem)
{
    dispatch_release(*sem);
}

#ifdef LISP_FEATURE_SB_FUTEX
// ulock (~futex) junk from xnu.  timeout=0 means wait forever
int __ulock_wait(uint32_t operation, void *addr, uint64_t value, uint32_t timeout);             // timeout in us
int __ulock_wait2(uint32_t operation, void *addr, uint64_t value, uint64_t timeout, uint64_t value2); // timeout in ns.  only available as of macos 11
int __ulock_wake(uint32_t operation, void *addr, uint64_t wake_value);

// operation bits [7, 0] contain the operation code.
#define UL_COMPARE_AND_WAIT             1
#define UL_UNFAIR_LOCK                  2
#define UL_COMPARE_AND_WAIT_SHARED      3
#define UL_UNFAIR_LOCK64_SHARED         4
#define UL_COMPARE_AND_WAIT64           5
#define UL_COMPARE_AND_WAIT64_SHARED    6

// operation bits [15, 8] contain the flags for __ulock_wake
#define ULF_WAKE_ALL                    0x00000100 // wake all waiting threads (default is to just wake one)
#define ULF_WAKE_THREAD                 0x00000200 // thread id specified in wake_value
#define ULF_WAKE_ALLOW_NON_OWNER        0x00000400 // allow lock to be released other than by its owner, only for UL_UNFAIR_LOCK

// operation bits [23, 16] contain the flags for __ulock_wait
#define ULF_WAIT_WORKQ_DATA_CONTENTION  0x00010000 // The waiter is contending on this lock for synchronization around global data.  This causes the workqueue subsystem to not create new threads to offset for waiters on this lock.
#define ULF_WAIT_CANCEL_POINT           0x00020000 // This wait is a cancelation point.
#define ULF_WAIT_ADAPTIVE_SPIN          0x00040000 // Use adaptive spinning when the thread that currently holds the unfair lock is on core.

// operation bits [31, 24] contain the generic flags, which can be used with both __ulock_wait and __ulock_wake
#define ULF_NO_ERRNO                    0x01000000 // return errors as negative codes instead of setting errno

int
futex_wait(int *lock_word, int oldval, long sec, unsigned long usec)
{
    unsigned long timeout;
    if (sec == 0 && usec == 0)
        return 1;

    if (sec < 0) {
        timeout = 0;
    } else {
        if (__builtin_umull_overflow((unsigned long)sec, 1000000000ul, &timeout)
            || __builtin_umull_overflow(usec, 1000ul, &usec)
            || __builtin_uaddl_overflow(usec, timeout, &timeout)) {
            timeout = 0xfffffffffffffffful;
        }
    }

    int ret = __ulock_wait2(UL_COMPARE_AND_WAIT | ULF_NO_ERRNO, lock_word, oldval, timeout, 0);
    if (ret == 0)
        return 0;
    else if (ret == -ETIMEDOUT)
        return 1;
    else if (ret == -EINTR)
        return 2;
    else
        return -1;
}

int
futex_wake(int *lock_word, int n)
{
    __ulock_wake(UL_COMPARE_AND_WAIT | ULF_NO_ERRNO | (n == 1 ? 0 : ULF_WAKE_ALL), lock_word, 0);
    return 0;
}
#endif

#elif defined LISP_FEATURE_SB_THREAD && defined CANNOT_USE_POSIX_SEM_T

inline void
os_sem_init(os_sem_t *sem, unsigned int value)
{
    if (KERN_SUCCESS!=semaphore_create(mach_task_self(), sem, SYNC_POLICY_FIFO, (int)value))
        lose("os_sem_init(%p): %s", sem, strerror(errno));
}

inline void
os_sem_wait(os_sem_t *sem)
{
    kern_return_t ret;
  restart:
    ret = semaphore_wait(*sem);
    switch (ret) {
    case KERN_SUCCESS:
        return;
        /* It is unclear just when we can get this, but a sufficiently
         * long wait seems to do that, at least sometimes.
         *
         * However, a wait that long is definitely abnormal for the
         * GC, so we complain before retrying.
         */
    case KERN_OPERATION_TIMED_OUT:
        fprintf(stderr, "os_sem_wait(%p): %s", sem, strerror(errno));
        /* This is analogous to POSIX EINTR. */
    case KERN_ABORTED:
        goto restart;
    default:
        lose("os_sem_wait(%p): %lu, %s", sem, (long unsigned)ret, strerror(errno));
    }
}

void
os_sem_post(os_sem_t *sem)
{
    if (KERN_SUCCESS!=semaphore_signal(*sem))
        lose("os_sem_post(%p): %s", sem, strerror(errno));
}

void
os_sem_destroy(os_sem_t *sem)
{
    if (-1==semaphore_destroy(mach_task_self(), *sem))
        lose("os_sem_destroy(%p): %s", sem, strerror(errno));
}

#endif
