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

#ifndef LISP_FEATURE_GENCGC
/* FIXME: genesis/constants.h also defines this with a constant value */
#define DYNAMIC_SPACE_START current_dynamic_space
#endif

#define BINDING_STACK_SIZE (1024*1024)   /* chosen at random */
#define ALIEN_STACK_SIZE (1024*1024)     /* chosen at random */

/* eventually choosable per-thread: */
#define DEFAULT_CONTROL_STACK_SIZE (2*1024*1024)

/* constants derived from the fundamental constants in passed by GENESIS */
#ifdef LISP_FEATURE_GENCGC
#define DEFAULT_DYNAMIC_SPACE_SIZE (DYNAMIC_SPACE_END - DYNAMIC_SPACE_START)
#else
#define DEFAULT_DYNAMIC_SPACE_SIZE (DYNAMIC_0_SPACE_END - DYNAMIC_0_SPACE_START)
#endif
#define READ_ONLY_SPACE_SIZE (READ_ONLY_SPACE_END - READ_ONLY_SPACE_START)
#define STATIC_SPACE_SIZE (STATIC_SPACE_END - STATIC_SPACE_START)
#ifdef LISP_FEATURE_LINKAGE_TABLE
#define LINKAGE_TABLE_SPACE_SIZE \
    (LINKAGE_TABLE_SPACE_END - LINKAGE_TABLE_SPACE_START)
#endif

#if !defined(LANGUAGE_ASSEMBLY)
#include "thread.h"

#ifdef LISP_FEATURE_STACK_GROWS_DOWNWARD_NOT_UPWARD

#define CONTROL_STACK_HARD_GUARD_PAGE(th) \
    ((os_vm_address_t)(th->control_stack_start))
#define CONTROL_STACK_GUARD_PAGE(th) \
    (CONTROL_STACK_HARD_GUARD_PAGE(th) + os_vm_page_size)
#define CONTROL_STACK_RETURN_GUARD_PAGE(th) \
    (CONTROL_STACK_GUARD_PAGE(th) + os_vm_page_size)
#else

#define CONTROL_STACK_HARD_GUARD_PAGE(th) \
    (((os_vm_address_t)(th->control_stack_end)) - os_vm_page_size)
#define CONTROL_STACK_GUARD_PAGE(th) \
    (CONTROL_STACK_HARD_GUARD_PAGE(th) - os_vm_page_size)
#define CONTROL_STACK_RETURN_GUARD_PAGE(th) \
    (CONTROL_STACK_GUARD_PAGE(th) - os_vm_page_size)

#endif

#ifdef ALIEN_STACK_GROWS_DOWNWARD

#define ALIEN_STACK_HARD_GUARD_PAGE(th)         \
    ((os_vm_address_t)(th->alien_stack_start))
#define ALIEN_STACK_GUARD_PAGE(th) \
    (ALIEN_STACK_HARD_GUARD_PAGE(th) + os_vm_page_size)
#define ALIEN_STACK_RETURN_GUARD_PAGE(th) \
    (ALIEN_STACK_GUARD_PAGE(th) + os_vm_page_size)

#elif defined(ALIEN_STACK_GROWS_UPWARD)

#define ALIEN_STACK_HARD_GUARD_PAGE(th)                            \
    (((os_vm_address_t)th->alien_stack_start) + ALIEN_STACK_SIZE - \
     os_vm_page_size)
#define ALIEN_STACK_GUARD_PAGE(th) \
    (ALIEN_STACK_HARD_GUARD_PAGE(th) - os_vm_page_size)
#define ALIEN_STACK_RETURN_GUARD_PAGE(th) \
    (ALIEN_STACK_GUARD_PAGE(th) - os_vm_page_size)

#else
#error ALIEN_STACK_GROWS_DOWNWARD or ALIEN_STACK_GROWS_UPWARD has to be defined
#endif

#define BINDING_STACK_HARD_GUARD_PAGE(th)                              \
    (((os_vm_address_t)th->binding_stack_start) + BINDING_STACK_SIZE - \
     os_vm_page_size)
#define BINDING_STACK_GUARD_PAGE(th) \
    (BINDING_STACK_HARD_GUARD_PAGE(th) - os_vm_page_size)
#define BINDING_STACK_RETURN_GUARD_PAGE(th) \
    (BINDING_STACK_GUARD_PAGE(th) - os_vm_page_size)

extern void validate(void);

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
extern os_vm_address_t undefined_alien_address;
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
 * level (compiler/target/parms.lisp) and stuffed into the sbcl.h file
 * created by GENESIS, so there's no longer a need for an
 * architecture-dependent header file of memory map data.
 */

#endif
