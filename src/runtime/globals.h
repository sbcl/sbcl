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

#if !defined(_INCLUDE_GLOBALS_H_)
#define _INCLUDED_GLOBALS_H_

#ifndef LANGUAGE_ASSEMBLY

#include <sys/types.h>
#include <unistd.h>
#include "runtime.h"

extern int foreign_function_call_active;
extern boolean stop_the_world;

extern lispobj *current_control_stack_pointer;
extern lispobj *current_control_frame_pointer;
#if !defined(__i386__)
extern lispobj *current_binding_stack_pointer;
#endif

#if !defined(__i386__)
/* FIXME: Why doesn't the x86 need this? */
extern lispobj *dynamic_space_free_pointer;
extern lispobj *current_auto_gc_trigger;
#endif

extern lispobj *current_dynamic_space;
extern pid_t parent_pid;
extern boolean stop_the_world;

extern void globals_init(void);

#else /* LANGUAGE_ASSEMBLY */

#ifdef mips
#define EXTERN(name,bytes) .extern name bytes
#endif
/**/
#ifdef sparc
#ifdef SVR4
#define EXTERN(name,bytes) .global name
#else
#define EXTERN(name,bytes) .global _ ## name
#endif
#endif
/**/
#ifdef alpha
#ifdef __linux__
#define EXTERN(name,bytes) .globl name 
#endif
#endif
#ifdef ppc
#define EXTERN(name,bytes) .globl name 
#endif
#ifdef __i386__
#ifdef __linux__
/* I'm very dubious about this.  Linux hasn't used _ on external names
 * since ELF became prevalent - i.e. about 1996, on x86    -dan 20010125 */
#define EXTERN(name,bytes) .globl _/**/name
#else
#define EXTERN(name,bytes) .global _ ## name
#endif
#endif

/* FIXME : these sizes are, incidentally, bogus on Alpha.  But the
 * EXTERN macro doesn't use its second arg anyway, so no immediate harm
 * done   -dan 2002.05.07
 */

EXTERN(foreign_function_call_active, 4)

EXTERN(current_control_stack_pointer, 4)
EXTERN(current_control_frame_pointer, 4)
EXTERN(current_binding_stack_pointer, 4)
EXTERN(dynamic_space_free_pointer, 4)
EXTERN(current_dynamic_space, 4)

#ifdef mips
EXTERN(current_flags_register, 4)
#endif

#endif /* LANGUAGE_ASSEMBLY */

#endif /* _INCLUDED_GLOBALS_H_ */
