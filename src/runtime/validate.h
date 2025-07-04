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

/* constants derived from the fundamental constants in passed by GENESIS */
#define READ_ONLY_SPACE_SIZE (READ_ONLY_SPACE_END - READ_ONLY_SPACE_START)
#define NIL_SYMBOL_SLOTS_START ((lispobj*)(STATIC_SPACE_START + NIL_SYMBOL_SLOTS_OFFSET))
#define NIL_SYMBOL_SLOTS_END (ALIGN_UP(SYMBOL_SIZE,2)+NIL_SYMBOL_SLOTS_START)
#define STATIC_SPACE_OBJECTS_START (STATIC_SPACE_START + STATIC_SPACE_OBJECTS_OFFSET)
#define STATIC_SPACE_END (STATIC_SPACE_START + STATIC_SPACE_SIZE)

#ifdef LISP_FEATURE_X86_64
#define T_SYMBOL_SLOTS_START ((lispobj*)(LISP_T - OTHER_POINTER_LOWTAG))
#define T_SYMBOL_SLOTS_END (T_SYMBOL_SLOTS_START+SYMBOL_SIZE)
#endif

#ifdef LISP_FEATURE_DARWIN_JIT
#define STATIC_CODE_SPACE_SIZE (STATIC_CODE_SPACE_END - STATIC_CODE_SPACE_START)
#endif

#define ALIEN_LINKAGE_SPACE_END (ALIEN_LINKAGE_SPACE_START + ALIEN_LINKAGE_SPACE_SIZE)
#ifdef LISP_FEATURE_LINKAGE_SPACE
#define LISP_LINKAGE_SPACE_SIZE (1<<(N_LINKAGE_INDEX_BITS+WORD_SHIFT))
#endif

#if !defined(__ASSEMBLER__)
#include <stdbool.h>
#include "thread.h"

#if defined(LISP_FEATURE_WIN32)

#define CONTROL_STACK_HARD_GUARD_PAGE(th) \
    ((os_vm_address_t)(th->control_stack_start))
#define CONTROL_STACK_GUARD_PAGE(th) \
    (CONTROL_STACK_HARD_GUARD_PAGE(th) + win32_page_size + win32_stack_guarantee)

#elif defined(LISP_FEATURE_STACK_GROWS_DOWNWARD_NOT_UPWARD)

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

#ifdef LISP_FEATURE_OS_PROVIDES_DLOPEN
extern void ensure_undefined_alien(void);
#else
#define ensure_undefined_alien() {}
#endif
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
 * level (compiler/{arch}/parms.lisp) and stuffed into the sbcl.h file
 * created by GENESIS, so there's no longer a need for an
 * architecture-dependent header file of memory map data.
 */

#endif
