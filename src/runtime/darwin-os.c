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

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
#include <libkern/OSAtomic.h>
#endif

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

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER

/* exc_server handles mach exception messages from the kernel and
 * calls catch exception raise. We use the system-provided
 * mach_msg_server, which, I assume, calls exc_server in a loop.
 *
 */
extern boolean_t exc_server();

void *
mach_exception_handler(void *port)
{
  mach_msg_server(exc_server, 2048, (mach_port_t) port, 0);
  /* mach_msg_server should never return, but it should dispatch mach
   * exceptions to our catch_exception_raise function
   */
  lose("mach_msg_server returned");
}

/* Sets up the thread that will listen for mach exceptions. note that
   the exception handlers will be run on this thread. This is
   different from the BSD-style signal handling situation in which the
   signal handlers run in the relevant thread directly. */

mach_port_t mach_exception_handler_port_set = MACH_PORT_NULL;

pthread_t
setup_mach_exception_handling_thread()
{
    kern_return_t ret;
    pthread_t mach_exception_handling_thread = NULL;
    pthread_attr_t attr;

    /* allocate a mach_port for this process */
    ret = mach_port_allocate(mach_task_self(),
                             MACH_PORT_RIGHT_PORT_SET,
                             &mach_exception_handler_port_set);

    /* create the thread that will receive the mach exceptions */

    FSHOW((stderr, "Creating mach_exception_handler thread!\n"));

    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN);
    pthread_create(&mach_exception_handling_thread,
                   &attr,
                   mach_exception_handler,
                   (void*)(long)mach_exception_handler_port_set);
    pthread_attr_destroy(&attr);

    return mach_exception_handling_thread;
}

/* tell the kernel that we want EXC_BAD_ACCESS exceptions sent to the
   exception port (which is being listened to do by the mach
   exception handling thread). */
kern_return_t
mach_lisp_thread_init(struct thread * thread)
{
    kern_return_t ret;
    mach_port_t current_mach_thread, thread_exception_port;

    if (mach_port_allocate(mach_task_self(),
                           MACH_PORT_RIGHT_RECEIVE,
                           &thread_exception_port) != KERN_SUCCESS) {
        lose("Cannot allocate thread_exception_port");
    }

    if (mach_port_set_context(mach_task_self(), thread_exception_port,
                              (mach_vm_address_t)thread)
        != KERN_SUCCESS) {
        lose("Cannot set thread_exception_port context");
    }
    thread->mach_port_name = thread_exception_port;

    /* establish the right for the thread_exception_port to send messages */
    ret = mach_port_insert_right(mach_task_self(),
                                 thread_exception_port,
                                 thread_exception_port,
                                 MACH_MSG_TYPE_MAKE_SEND);
    if (ret) {
        lose("mach_port_insert_right failed with return_code %d", ret);
    }

    current_mach_thread = mach_thread_self();
    ret = thread_set_exception_ports(current_mach_thread,
                                     EXC_MASK_BAD_ACCESS | EXC_MASK_BAD_INSTRUCTION | EXC_MASK_BREAKPOINT,
                                     thread_exception_port,
                                     EXCEPTION_DEFAULT,
                                     THREAD_STATE_NONE);
    if (ret) {
        lose("thread_set_exception_ports failed with return_code %d", ret);
    }

    ret = mach_port_deallocate (mach_task_self(), current_mach_thread);
    if (ret) {
        lose("mach_port_deallocate failed with return_code %d", ret);
    }

    ret = mach_port_move_member(mach_task_self(),
                                thread_exception_port,
                                mach_exception_handler_port_set);
    if (ret) {
        lose("mach_port_move_member failed with return_code %d", ret);
    }

    return ret;
}

void
mach_lisp_thread_destroy(struct thread *thread) {
    mach_port_t port = thread->mach_port_name;
    FSHOW((stderr, "Deallocating mach port %x\n", port));
    if (mach_port_move_member(mach_task_self(), port, MACH_PORT_NULL)
        != KERN_SUCCESS) {
        lose("Error destroying an exception port");
    }
    if (mach_port_deallocate(mach_task_self(), port) != KERN_SUCCESS) {
        lose("Error destroying an exception port");
    }

    if (mach_port_destroy(mach_task_self(), port) != KERN_SUCCESS) {
        lose("Error destroying an exception port");
    }
}
#endif

void
darwin_reinit() {
    init_mach_clock();
#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
    setup_mach_exception_handling_thread();
    mach_lisp_thread_init(all_threads);
#endif

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
#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
    setup_mach_exception_handling_thread();
#endif
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
