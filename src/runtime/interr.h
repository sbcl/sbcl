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

#ifndef _INTERR_H_
#define _INTERR_H_
#include "os.h"
extern void lose(char *fmt, ...)
#ifndef LISP_FEATURE_WIN32
 __attribute__((format(printf,1,2))) // clang and gcc support this, MSVC doesn't
#endif
 never_returns;
extern void tprintf(char *fmt, ...);
extern boolean lose_on_corruption_p;
extern void corruption_warning_and_maybe_lose(char *fmt, ...);
extern void enable_lossage_handler(void);
extern void disable_lossage_handler(void);
extern void describe_internal_error(os_context_t *context);
extern void skip_internal_error (os_context_t *context);

#ifdef LISP_FEATURE_WIN32
/* thread ID is a more useful identifer than thread handle */
#define UNKNOWN_STACK_POINTER_ERROR(function_name, thread) \
    lose(function_name": no SP known for thread %p (ID %p)", \
         thread, (void*)thread->os_kernel_tid);
#else
/* Portably printf()ing a pthread_t is tricky. It has to be printed
 * as opaque bytes by taking &id and sizeof id.
 * As the man page says:
 *  "POSIX.1 allows an implementation wide freedom in choosing the
 *   type used to represent a thread ID; for example, representation using either
 *   an arithmetic type or a structure is permitted. Therefore, variables of type
 *   pthread_t can't portably be compared using the C equality operator (==)"
 * and elsewhere
 *  "IEEE IEEE Std 1003.1-2001/Cor 2-2004, item XBD/TC2/D6/26 is applied, adding
 *   pthread_t to the list of types that are not required to be arithmetic types,
 *   thus allowing pthread_t to be defined as a structure."
 *
 * We assume that it can be cast to 'void*'
 */
#define UNKNOWN_STACK_POINTER_ERROR(function_name, thread) \
    lose(function_name": no SP known for thread %p (pthread %p)", \
         thread, (void*)thread->os_thread);
#endif

#endif
