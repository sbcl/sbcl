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

#ifndef _INCLUDED_GLOBALS_H_
#define _INCLUDED_GLOBALS_H_

#ifndef LANGUAGE_ASSEMBLY
# include <sys/types.h>
# include <unistd.h>
# include "runtime.h"
# include "runtime-options.h"
#endif

#include "sbcl.h"

#ifndef LANGUAGE_ASSEMBLY

#ifdef LISP_FEATURE_SB_THREAD
#define foreign_function_call_active_p(thread) \
    (thread->foreign_function_call_active)
#else
extern int foreign_function_call_active;
#define foreign_function_call_active_p(thread) \
    foreign_function_call_active
#endif

extern os_vm_size_t dynamic_space_size;
extern os_vm_size_t thread_control_stack_size;

extern struct runtime_options *runtime_options;

#ifdef LISP_FEATURE_WIN32
#define ENVIRON _environ
#else
#define ENVIRON environ
#endif
extern char **ENVIRON;

#if defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_GCC_TLS)
extern pthread_key_t specials;
#endif

#if !defined(LISP_FEATURE_SB_THREAD)
extern lispobj *current_control_stack_pointer;
#endif
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || !defined(LISP_FEATURE_SB_THREAD)
extern lispobj *current_control_frame_pointer;
#endif
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64) && !defined(LISP_FEATURE_SB_THREAD)
extern lispobj *current_binding_stack_pointer;
#endif

#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
/* This is unused on X86 and X86_64, but is used as the global
 *  allocation pointer by the cheney GC, and, in some instances, as
 *  the global allocation pointer on PPC/GENCGC. This should probably
 *  be cleaned up such that it only needs to exist on cheney. At the
 *  moment, it is also used by the GENCGC, to hold the pseudo_atomic
 *  bits, and is tightly coupled to reg_ALLOC by the assembly
 *  routines. */
extern lispobj *dynamic_space_free_pointer;
#endif

# ifndef LISP_FEATURE_GENCGC
extern lispobj *current_auto_gc_trigger;
# endif

extern lispobj *current_dynamic_space;

extern void globals_init(void);

#else /* LANGUAGE_ASSEMBLY */

# ifdef LISP_FEATURE_MIPS
#  ifdef __linux__
#   define EXTERN(name,bytes) .globl name
#  else
#   define EXTERN(name,bytes) .extern name bytes
#  endif
# endif
/**/
# ifdef LISP_FEATURE_SPARC
#  ifdef SVR4
#   define EXTERN(name,bytes) .global name
#  else
#   define EXTERN(name,bytes) .global _ ## name
#  endif
# endif
/**/
# ifdef LISP_FEATURE_ALPHA
#  ifdef __linux__
#   define EXTERN(name,bytes) .globl name
#  endif
# endif
/**/
# ifdef LISP_FEATURE_PPC
#  ifdef LISP_FEATURE_DARWIN
#   define EXTERN(name,bytes) .globl _ ## name
#  else
#   define EXTERN(name,bytes) .globl name
#  endif
# endif
/**/
# if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
#  define EXTERN(name,bytes) .global name
# endif
/**/
# if defined(LISP_FEATURE_ARM) || defined(LISP_FEATURE_ARM64)
#   define EXTERN(name,bytes) .global name
# endif

# if defined(LISP_FEATURE_ALPHA) || defined(LISP_FEATURE_X86_64)
#  define POINTERSIZE 8
# else
#  define POINTERSIZE 4
# endif

#ifndef LISP_FEATURE_SB_THREAD
EXTERN(foreign_function_call_active, 4)
#endif

#if !defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
EXTERN(current_control_stack_pointer, POINTERSIZE)
#endif
EXTERN(current_control_frame_pointer, POINTERSIZE)
# if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
EXTERN(current_binding_stack_pointer, POINTERSIZE)
# endif
# ifndef LISP_FEATURE_GENCGC
EXTERN(dynamic_space_free_pointer, POINTERSIZE)
# endif

#endif /* LANGUAGE_ASSEMBLY */

#endif /* _INCLUDED_GLOBALS_H_ */
