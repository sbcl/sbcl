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
#include <signal.h>
#include <limits.h>
#include <mach-o/dyld.h>
#include <errno.h>
#include <dlfcn.h>

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
#include <mach/mach.h>
#endif

char *
os_get_runtime_executable_path(int external)
{
    char path[PATH_MAX + 1];
    uint32_t size = sizeof(path);

    if (_NSGetExecutablePath(path, &size) == -1)
        return NULL;

    return copied_string(path);
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
mach_port_t current_mach_task = MACH_PORT_NULL;

pthread_t
setup_mach_exception_handling_thread()
{
    kern_return_t ret;
    pthread_t mach_exception_handling_thread = NULL;
    pthread_attr_t attr;

    current_mach_task = mach_task_self();

    /* allocate a mach_port for this process */
    ret = mach_port_allocate(current_mach_task,
                             MACH_PORT_RIGHT_PORT_SET,
                             &mach_exception_handler_port_set);

    /* create the thread that will receive the mach exceptions */

    FSHOW((stderr, "Creating mach_exception_handler thread!\n"));

    pthread_attr_init(&attr);
    pthread_create(&mach_exception_handling_thread,
                   &attr,
                   mach_exception_handler,
                   (void*) mach_exception_handler_port_set);
    pthread_attr_destroy(&attr);

    return mach_exception_handling_thread;
}

/* tell the kernel that we want EXC_BAD_ACCESS exceptions sent to the
   exception port (which is being listened to do by the mach
   exception handling thread). */
kern_return_t
mach_thread_init(mach_port_t thread_exception_port)
{
    kern_return_t ret;
    mach_port_t current_mach_thread;

    /* allocate a named port for the thread */
    FSHOW((stderr, "Allocating mach port %x\n", thread_exception_port));
    ret = mach_port_allocate_name(current_mach_task,
                                  MACH_PORT_RIGHT_RECEIVE,
                                  thread_exception_port);
    if (ret) {
        lose("mach_port_allocate_name failed with return_code %d\n", ret);
    }

    /* establish the right for the thread_exception_port to send messages */
    ret = mach_port_insert_right(current_mach_task,
                                 thread_exception_port,
                                 thread_exception_port,
                                 MACH_MSG_TYPE_MAKE_SEND);
    if (ret) {
        lose("mach_port_insert_right failed with return_code %d\n", ret);
    }

    current_mach_thread = mach_thread_self();
    ret = thread_set_exception_ports(current_mach_thread,
                                     EXC_MASK_BAD_ACCESS | EXC_MASK_BAD_INSTRUCTION,
                                     thread_exception_port,
                                     EXCEPTION_DEFAULT,
                                     THREAD_STATE_NONE);
    if (ret) {
        lose("thread_set_exception_ports failed with return_code %d\n", ret);
    }

    ret = mach_port_deallocate (current_mach_task, current_mach_thread);
    if (ret) {
        lose("mach_port_deallocate failed with return_code %d\n", ret);
    }

    ret = mach_port_move_member(current_mach_task,
                                thread_exception_port,
                                mach_exception_handler_port_set);
    if (ret) {
        lose("mach_port_move_member failed with return_code %d\n", ret);
    }

    return ret;
}

void
setup_mach_exceptions() {
    setup_mach_exception_handling_thread();
    mach_thread_init(THREAD_STRUCT_TO_EXCEPTION_PORT(all_threads));
}

pid_t
mach_fork() {
    pid_t pid = fork();
    if (pid == 0) {
        setup_mach_exceptions();
        return pid;
    } else {
        return pid;
    }
}

#endif

