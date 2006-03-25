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

#include "sbcl.h"
#include "globals.h"
#include "runtime.h"
#include <signal.h>
#include <ucontext.h>
#include <limits.h>
#include <mach-o/dyld.h>
#include "bsd-os.h"
#include <errno.h>

#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_SB_LUTEX)
#include <sys/semaphore.h>
#endif

#if defined(LISP_FEATURE_CARBON_THREADS)
#include <CoreServices/CoreServices.h>
#endif

char *
os_get_runtime_executable_path()
{
    char path[PATH_MAX + 1];
    uint32_t size = sizeof(path);

    if (_NSGetExecutablePath(path, &size) == -1)
        return NULL;
    else
        path[size] = '\0';

    return copied_string(path);
}

#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_SB_LUTEX)

#if defined(LISP_FEATURE_MACH_SEMAPHORES)

/* DEBUGGING CRAP THAT NEEDS TO GO AWAY! */
/*
int wait_attempt_count = 0;
int wake_attempt_count = 0;

    if (wait_attempt_count++ > 1000)
        exit(1);
*/

int futex_init(os_sem_t *semaphore)
{
    kern_return_t ret;
    FSHOW_SIGNAL((stderr, "/initializing semaphore @ %p\n", semaphore));
    ret = semaphore_create(current_task(), semaphore, SYNC_POLICY_FIFO, 1);
    FSHOW_SIGNAL((stderr, "/semaphore_create said %d\n", ret));
    return ret;
}

int futex_wait(os_sem_t *semaphore)
{
    kern_return_t ret;
    FSHOW_SIGNAL((stderr, "/waiting on semaphore @ %p\n", semaphore));
    ret = semaphore_wait(*semaphore);
    FSHOW_SIGNAL((stderr, "/semaphore_wait said %d\n", ret));
    return ret;
}

int futex_wake(os_sem_t *semaphore)
{
    kern_return_t ret;
    FSHOW_SIGNAL((stderr, "/waking semaphore @ %p, *semaphore=%p\n", semaphore, (void*)*semaphore));
    ret = semaphore_signal(*semaphore);
    FSHOW_SIGNAL((stderr, "/semaphore_signal said %d\n", ret));
    return ret;
}

int futex_destroy(os_sem_t *semaphore)
{
    kern_return_t ret;
    FSHOW_SIGNAL((stderr, "/destroying semaphore @ %p\n", semaphore));
    ret = semaphore_destroy(current_task(), *semaphore);
    FSHOW_SIGNAL((stderr, "/semaphore_destroy said %d\n", ret));
    return ret;
}

#elif defined(LISP_FEATURE_CARBON_SEMAPHORES)

int futex_init(os_sem_t *semaphore)
{
    OSStatus ret;
    FSHOW_SIGNAL((stderr, "/initializing semaphore @ %p\n", semaphore));
    ret = MPCreateSemaphore(255, 1, semaphore);
    FSHOW_SIGNAL((stderr, "/MP said %d\n", ret));
    return ret;
}

int futex_wait(os_sem_t *semaphore)
{
    kern_return_t ret;
    FSHOW_SIGNAL((stderr, "/waiting on semaphore @ %p\n", semaphore));
    ret = MPWaitOnSemaphore(*semaphore, kDurationForever);
    FSHOW_SIGNAL((stderr, "/MPWaitOnSemaphore said %d\n", ret));
    return ret;
}

int futex_wake(os_sem_t *semaphore)
{
    kern_return_t ret;
    FSHOW_SIGNAL((stderr, "/waking semaphore @ %p, *semaphore=%p\n", semaphore, (void*)*semaphore));
    ret = MPSignalSemaphore(*semaphore);
    FSHOW_SIGNAL((stderr, "/MPSignalSemaphore said %d\n", ret));
    return ret;
}

int futex_destroy(os_sem_t *semaphore)
{
    kern_return_t ret;
    FSHOW_SIGNAL((stderr, "/destroying semaphore @ %p\n", semaphore));
    ret = MPDeleteSemaphore(*semaphore);
    FSHOW_SIGNAL((stderr, "/MPDeleteSemaphore said %d\n", ret));
    return ret;
}

#else

int futex_init(os_sem_t *semaphore)
{
    int ret;
    printf("Initializing semaphore @ %p\n", semaphore);
    ret = sem_init(semaphore, 0, 1);
    printf("sem_init said %d, errno: %d\n", ret, errno);
    return ret;
}

int futex_wait(os_sem_t *semaphore)
{
    int ret;
    printf("Waiting on semaphore %p\n", semaphore);
    ret = sem_wait(semaphore);
    printf("sem_wait said %d, errno: %d\n", ret, errno);
    return ret;
}

int futex_wake(os_sem_t *semaphore)
{
    int ret;
    printf("Waking on semaphore %p\n", semaphore);
    ret = sem_post(semaphore);
    printf("sem_post said %d, errno: %d\n", ret, errno);
    return ret;
}

int futex_destroy(os_sem_t *semaphore)
{
    return sem_destroy(semaphore);
}

#endif

#endif

