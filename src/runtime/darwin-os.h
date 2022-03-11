#ifndef _DARWIN_OS_H
#define _DARWIN_OS_H

/* this is meant to be included from bsd-os.h */

#include <mach/mach_init.h>
#include <mach/task.h>
#include <AvailabilityMacros.h>
#include <sys/cdefs.h>

/* man pages claim that the third argument is a sigcontext struct,
   but ucontext_t is defined, matches sigcontext where sensible,
   offers better access to mcontext, and is of course the SUSv2-
   mandated type of the third argument, so we use that instead.
   If Apple is going to break ucontext_t out of spite, I'm going
   to be cross with them ;) -- PRM */

#if defined(LISP_FEATURE_X86)
#include <sys/ucontext.h>
#include <sys/_types.h>

#if __DARWIN_UNIX03
typedef struct __darwin_ucontext os_context_t;
#else
typedef struct ucontext os_context_t;
#endif


#else
typedef ucontext_t os_context_t;
#endif

#define SIG_MEMORY_FAULT SIGBUS

#define SIG_STOP_FOR_GC (SIGUSR2)

void darwin_init(void);

#ifdef LISP_FEATURE_SB_THREAD
#define USE_DARWIN_GCD_SEMAPHORES
#include <dispatch/dispatch.h>
typedef dispatch_semaphore_t os_sem_t;
#endif

#endif /* _DARWIN_OS_H */
