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

#ifndef __ASSEMBLER__
# include <sys/types.h>
# include <unistd.h>
# include "runtime.h"
# include "os.h"
#endif

#include "sbcl.h"

#ifndef __ASSEMBLER__

extern sword_t next_free_page;
#define dynamic_space_highwatermark() (next_free_page*GENCGC_PAGE_BYTES+DYNAMIC_SPACE_START)

#ifdef LISP_FEATURE_SB_THREAD

#ifdef LISP_FEATURE_ARM64
#define foreign_function_call_active_p(thread) \
    (thread->control_stack_pointer)
#else
#define foreign_function_call_active_p(thread) \
    (thread->foreign_function_call_active)
#endif

#else
extern int foreign_function_call_active;
#define foreign_function_call_active_p(thread) \
    foreign_function_call_active
#endif

extern os_vm_size_t dynamic_space_size;
extern os_vm_size_t thread_control_stack_size;

#ifndef LISP_FEATURE_DARWIN_JIT
extern uword_t READ_ONLY_SPACE_START, READ_ONLY_SPACE_END;
#endif
#ifdef LISP_FEATURE_CHENEYGC
extern uword_t DYNAMIC_0_SPACE_START, DYNAMIC_1_SPACE_START;
#else
extern uword_t DYNAMIC_SPACE_START;
#endif
#ifdef LISP_FEATURE_IMMOBILE_SPACE
extern uword_t FIXEDOBJ_SPACE_START, TEXT_SPACE_START;
extern uword_t immobile_space_lower_bound, immobile_space_max_offset;
extern uword_t immobile_range_1_max_offset, immobile_range_2_min_offset;
extern unsigned int text_space_size;
#endif
extern uword_t asm_routines_start, asm_routines_end;
extern int gc_card_table_nbits;

static inline lispobj points_to_asm_code_p(uword_t ptr) {
    return asm_routines_start <= ptr && ptr < asm_routines_end;
}

extern boolean alloc_profiling;
extern os_vm_address_t alloc_profile_buffer;
extern lispobj alloc_profile_data; // Lisp SIMPLE-VECTOR

extern lispobj arena_chain;

#ifdef LISP_FEATURE_WIN32
#define ENVIRON _environ
#else
#define ENVIRON environ
#endif
extern char **ENVIRON;

#if !defined(LISP_FEATURE_SB_THREAD)
extern lispobj *current_control_stack_pointer;
#endif
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || !defined(LISP_FEATURE_SB_THREAD)
extern lispobj *current_control_frame_pointer;
#endif
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64) && !defined(LISP_FEATURE_SB_THREAD)
extern lispobj *current_binding_stack_pointer;
#endif

extern lispobj *read_only_space_free_pointer;
extern lispobj *static_space_free_pointer;

static inline boolean readonly_space_p(lispobj ptr) {
    return ptr >= READ_ONLY_SPACE_START && (lispobj*)ptr < read_only_space_free_pointer;
}

#ifdef LISP_FEATURE_DARWIN_JIT
extern lispobj *static_code_space_free_pointer;
#endif

#ifdef LISP_FEATURE_IMMOBILE_SPACE
extern lispobj *text_space_highwatermark;
extern lispobj *fixedobj_free_pointer;
extern lispobj ALIEN_LINKAGE_TABLE_SPACE_START;
#endif
extern os_vm_address_t anon_dynamic_space_start;
extern lispobj* tlsf_mem_start; // meaningful only if immobile space

# ifndef LISP_FEATURE_GENCGC
extern lispobj *current_auto_gc_trigger;
# endif

#ifdef LISP_FEATURE_CHENEYGC
extern lispobj *current_dynamic_space;
#endif

extern lispobj lisp_package_vector;

extern void globals_init(void);

#else /* __ASSEMBLER__ */

# ifdef LISP_FEATURE_MIPS
#   define EXTERN(name) .globl name
# endif
/**/
# ifdef LISP_FEATURE_SPARC
#  ifdef SVR4
#   define EXTERN(name) .global name
#  else
#   define EXTERN(name) .global _ ## name
#  endif
# endif
/**/
# if defined(LISP_FEATURE_PPC) || defined(LISP_FEATURE_PPC64)
#  ifdef LISP_FEATURE_DARWIN
#   define EXTERN(name) .globl _ ## name
#  else
#   define EXTERN(name) .globl name
#  endif
# endif
/**/
# if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
#  define EXTERN(name) .global name
# endif
/**/
# if defined(LISP_FEATURE_ARM) || defined(LISP_FEATURE_ARM64)
#   define EXTERN(name) .global name
# endif

# if defined(LISP_FEATURE_RISCV)
#   define EXTERN(name) .globl name
# endif

#ifndef LISP_FEATURE_SB_THREAD
EXTERN(foreign_function_call_active)
#endif

#if !defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
EXTERN(current_control_stack_pointer)
#endif
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || !defined(LISP_FEATURE_SB_THREAD)
EXTERN(current_control_frame_pointer)
#endif
# if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64) && !defined(LISP_FEATURE_SB_THREAD)
EXTERN(current_binding_stack_pointer)
# endif

#endif /* __ASSEMBLER__ */

#endif /* _INCLUDED_GLOBALS_H_ */
