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
#include "sbcl.h"
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
#include <mach/mach.h>
#include <mach/clock.h>
#include <stdlib.h>
#include <time.h>
#include <sys/syscall.h>

char *os_get_runtime_executable_path()
{
    char path[PATH_MAX + 1];
    uint32_t size = sizeof(path);

    if (_NSGetExecutablePath(path, &size) == -1)
        return NULL;

    return copied_string(path);
}


semaphore_t clock_sem = MACH_PORT_NULL;
mach_port_t clock_port = MACH_PORT_NULL;

void init_mach_clock() {
    if (host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, &clock_port)
        != KERN_SUCCESS) {
        lose("Error initializing clocks");
    }

    if (semaphore_create(mach_task_self_, &clock_sem, SYNC_POLICY_FIFO, 0)
        != KERN_SUCCESS) {
        lose("Error initializing clocks");
    }
}

void
darwin_reinit() {
    init_mach_clock();
#ifdef LISP_FEATURE_SB_THREAD
    struct extra_thread_data *extra_data = thread_extra_data(get_sb_vm_thread());
    os_sem_init(&extra_data->state_sem, 1);
    os_sem_init(&extra_data->state_not_running_sem, 0);
    os_sem_init(&extra_data->state_not_stopped_sem, 0);
    os_sem_init(&extra_data->sprof_sem, 0);
#endif
}

void darwin_init(void)
{
    init_mach_clock();
}


#ifdef LISP_FEATURE_SB_THREAD

inline void
os_sem_init(os_sem_t *sem, unsigned int value)
{
    if (!(*sem = dispatch_semaphore_create(value)))
        lose("os_sem_init(%p): %s", sem, strerror(errno));
}

inline void
os_sem_wait(os_sem_t *sem, char *what)
{
    dispatch_semaphore_wait(*sem, DISPATCH_TIME_FOREVER);
}

void
os_sem_post(os_sem_t *sem, char *what)
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
#endif

/* nanosleep() is not re-entrant on some versions of Darwin,
 * reimplement it using the underlying syscalls. */
int
sb_nanosleep(time_t sec, int nsec) {
    int ret;
    mach_timespec_t current_time;
    mach_timespec_t start_time;

    if (sec < 0 || nsec >= (int)NSEC_PER_SEC) {
        errno = EINVAL;
        return -1;
    }

    ret = clock_get_time(clock_port, &start_time);
    if (ret != KERN_SUCCESS) {
            lose("%s", mach_error_string(ret));
    }

    for (;;) {

      /* Older version do not have a wrapper. */
      ret = syscall(SYS___semwait_signal, (int)clock_sem, (int)MACH_PORT_NULL, (int)1, (int)1,
                    (__int64_t)sec, (__int32_t)nsec);
        if (ret < 0) {
            if (errno == ETIMEDOUT) {
                return 0;
            }
            if (errno == EINTR) {
                ret = clock_get_time(clock_port, &current_time);
                if (ret != KERN_SUCCESS) {
                    lose("%s", mach_error_string(ret));
                }
                time_t elapsed_sec = current_time.tv_sec - start_time.tv_sec;
                int elapsed_nsec = current_time.tv_nsec - start_time.tv_nsec;
                if (elapsed_nsec < 0) {
                    elapsed_sec--;
                    elapsed_nsec += NSEC_PER_SEC;
                }
                sec -= elapsed_sec;
                nsec -= elapsed_nsec;
                if (nsec < 0) {
                    sec--;
                    nsec += NSEC_PER_SEC;
                }
                if (sec < 0 || (sec == 0 && nsec == 0)) {
                    return 0;
                }
                start_time = current_time;
            } else {
                errno = EINVAL;
                return -1;
            }
        } else {
            return -1;
        }
    }
}
