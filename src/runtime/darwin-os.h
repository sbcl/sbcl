#ifndef _DARWIN_OS_H
#define _DARWIN_OS_H

/* this is meant to be included from bsd-os.h */

#include <mach/mach_init.h>
#include <mach/task.h>
#include <mach/semaphore.h>

#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_SB_LUTEX)
#include <sys/semaphore.h>
#endif

/* man pages claim that the third argument is a sigcontext struct,
   but ucontext_t is defined, matches sigcontext where sensible,
   offers better access to mcontext, and is of course the SUSv2-
   mandated type of the third argument, so we use that instead.
   If Apple is going to break ucontext_t out of spite, I'm going
   to be cross with them ;) -- PRM */

#if defined(LISP_FEATURE_X86)
#include <sys/ucontext.h>
#include <sys/_types.h>
typedef struct ucontext os_context_t;

#if defined(LISP_FEATURE_MACH_SEMAPHORES)
typedef semaphore_t os_sem_t;
#else
typedef sem_t os_sem_t;
#endif

#else
#include <ucontext.h>
typedef ucontext_t os_context_t;
#endif

#define SIG_MEMORY_FAULT SIGBUS

#define SIG_INTERRUPT_THREAD (SIGWINCH)
#define SIG_STOP_FOR_GC (SIGINFO)

#endif /* _DARWIN_OS_H */
