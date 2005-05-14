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
#endif

#include "sbcl.h"

#ifndef LANGUAGE_ASSEMBLY
extern int foreign_function_call_active;
extern boolean stop_the_world;

extern lispobj *current_control_stack_pointer;
extern lispobj *current_control_frame_pointer;
# if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
extern lispobj *current_binding_stack_pointer;
# endif

# ifndef LISP_FEATURE_GENCGC
/* Beware! gencgc has also a (non-global) dynamic_space_free_pointer. */
extern lispobj *dynamic_space_free_pointer;
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
#   define EXTERN(name,bytes) .globl _/**/name
#  else
#   define EXTERN(name,bytes) .globl name 
#  endif
# endif
/**/
# if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
#  define EXTERN(name,bytes) .global name
# endif

# if defined(LISP_FEATURE_ALPHA) || defined(LISP_FEATURE_X86_64)
#  define POINTERSIZE 8
# else
#  define POINTERSIZE 4
# endif

EXTERN(foreign_function_call_active, 4)

EXTERN(current_control_stack_pointer, POINTERSIZE)
EXTERN(current_control_frame_pointer, POINTERSIZE)
# if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
EXTERN(current_binding_stack_pointer, POINTERSIZE)
# endif
# ifndef LISP_FEATURE_GENCGC
EXTERN(dynamic_space_free_pointer, POINTERSIZE)
# endif

#endif /* LANGUAGE_ASSEMBLY */

#endif /* _INCLUDED_GLOBALS_H_ */
