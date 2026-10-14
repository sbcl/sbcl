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

#if !defined(_INCLUDE_VALIDATE_H_)
#define _INCLUDE_VALIDATE_H_

#define BINDING_STACK_SIZE (1024*1024)   /* chosen at random */
#define ALIEN_STACK_SIZE (1024*1024)     /* chosen at random */

/* eventually choosable per-thread: */
#ifndef DEFAULT_CONTROL_STACK_SIZE // CFLAGS=-D... can set this
#define DEFAULT_CONTROL_STACK_SIZE (2*1024*1024)
#endif

#if !defined(__ASSEMBLER__)
#include <stdbool.h>
#include "thread.h"

#if defined LISP_FEATURE_WIN32 && defined LISP_FEATURE_C_STACK_IS_CONTROL_STACK
#define STACK_GUARD_SIZE (win32_page_size + win32_stack_guarantee)
#else
#define STACK_GUARD_SIZE os_vm_page_size
#endif

#ifdef LISP_FEATURE_STACK_GROWS_DOWNWARD_NOT_UPWARD

#define CONTROL_STACK_HARD_GUARD_PAGE(th) \
    ((os_vm_address_t)(th->control_stack_start))
#define CONTROL_STACK_GUARD_PAGE(th) \
    (CONTROL_STACK_HARD_GUARD_PAGE(th) + STACK_GUARD_SIZE)
#define CONTROL_STACK_RETURN_GUARD_PAGE(th) \
    (CONTROL_STACK_GUARD_PAGE(th) + STACK_GUARD_SIZE)
#else

#define CONTROL_STACK_HARD_GUARD_PAGE(th) \
    (((os_vm_address_t)(th->control_stack_end)) - STACK_GUARD_SIZE)
#define CONTROL_STACK_GUARD_PAGE(th) \
    (CONTROL_STACK_HARD_GUARD_PAGE(th) - STACK_GUARD_SIZE)
#define CONTROL_STACK_RETURN_GUARD_PAGE(th) \
    (CONTROL_STACK_GUARD_PAGE(th) - STACK_GUARD_SIZE)

#endif

#ifndef ALIEN_STACK_GROWS_UPWARD

#define ALIEN_STACK_HARD_GUARD_PAGE(th)         \
    ((os_vm_address_t)(th->alien_stack_start))
#define ALIEN_STACK_GUARD_PAGE(th) \
    (ALIEN_STACK_HARD_GUARD_PAGE(th) + os_vm_page_size)
#define ALIEN_STACK_RETURN_GUARD_PAGE(th) \
    (ALIEN_STACK_GUARD_PAGE(th) + os_vm_page_size)

#else

#define ALIEN_STACK_HARD_GUARD_PAGE(th)                            \
    (((os_vm_address_t)th->alien_stack_start) + ALIEN_STACK_SIZE - \
     os_vm_page_size)
#define ALIEN_STACK_GUARD_PAGE(th) \
    (ALIEN_STACK_HARD_GUARD_PAGE(th) - os_vm_page_size)
#define ALIEN_STACK_RETURN_GUARD_PAGE(th) \
    (ALIEN_STACK_GUARD_PAGE(th) - os_vm_page_size)

#endif

#define BINDING_STACK_HARD_GUARD_PAGE(th)                              \
    (((os_vm_address_t)th->binding_stack_start) + BINDING_STACK_SIZE - \
     os_vm_page_size)
#define BINDING_STACK_GUARD_PAGE(th) \
    (BINDING_STACK_HARD_GUARD_PAGE(th) - os_vm_page_size)
#define BINDING_STACK_RETURN_GUARD_PAGE(th) \
    (BINDING_STACK_GUARD_PAGE(th) - os_vm_page_size)

extern bool allocate_hardwired_spaces(bool);

extern void
protect_control_stack_hard_guard_page(int protect_p, struct thread *thread);
extern void
protect_control_stack_guard_page(int protect_p, struct thread *thread);
extern void
protect_control_stack_return_guard_page(int protect_p, struct thread *thread);
extern void
protect_binding_stack_hard_guard_page(int protect_p, struct thread *thread);
extern void
protect_binding_stack_guard_page(int protect_p, struct thread *thread);
extern void
protect_binding_stack_return_guard_page(int protect_p, struct thread *thread);
extern void
protect_alien_stack_hard_guard_page(int protect_p, struct thread *thread);
extern void
protect_alien_stack_guard_page(int protect_p, struct thread *thread);
extern void
protect_alien_stack_return_guard_page(int protect_p, struct thread *thread);
#endif

/* note for anyone trying to port an architecture's support files
 * from CMU CL to SBCL:
 *
 * CMU CL had architecture-dependent header files included here to
 * define memory map data:
 *   #ifdef LISP_FEATURE_X86
 *   #include "x86-validate.h"
 *   #endif
 * and so forth. In SBCL, the memory map data are defined at the Lisp
 * level (compiler/{arch}/parms.lisp) and stuffed into the sbcl.h file
 * created by GENESIS, so there's no longer a need for an
 * architecture-dependent header file of memory map data.
 */

#endif
