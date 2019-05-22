/*
 * macros for manipulating pseudo-atomic flags (per thread)
 */

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

#ifndef PSEUDO_ATOMIC_H
#define PSEUDO_ATOMIC_H

#include "interr.h"

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)

#define set_alloc_pointer(value) dynamic_space_free_pointer = (lispobj*)(value)
#define get_alloc_pointer() (dynamic_space_free_pointer)

#if defined(LISP_FEATURE_X86)
#define LISPOBJ_ASM_SUFFIX "l"
#elif defined(LISP_FEATURE_X86_64)
#define LISPOBJ_ASM_SUFFIX "q"
#endif

#ifdef LISP_FEATURE_SB_THREAD
# define pa_bits thread->pseudo_atomic_bits
#else
# define pa_bits SYMBOL(PSEUDO_ATOMIC_BITS)->value
#endif

static inline int
get_pseudo_atomic_atomic(struct thread __attribute__((unused)) *thread)
{
    return (pa_bits & ~1) != 0;
}

static inline void
set_pseudo_atomic_atomic(struct thread __attribute__((unused)) *thread)
{
    if (pa_bits) lose("set_pseudo_atomic_atomic: bits=%"OBJ_FMTX, pa_bits);
    __asm__ volatile ("or" LISPOBJ_ASM_SUFFIX " $~1, %0" : "+m" (pa_bits));
}

static inline void
clear_pseudo_atomic_atomic(struct thread __attribute__((unused)) *thread)
{
    __asm__ volatile ("and" LISPOBJ_ASM_SUFFIX " $1, %0" : "+m" (pa_bits));
}

static inline int
get_pseudo_atomic_interrupted(struct thread __attribute__((unused)) *thread)
{
    return pa_bits & 1;
}

static inline void
set_pseudo_atomic_interrupted(struct thread *thread)
{
    if (!get_pseudo_atomic_atomic(thread))
        lose("set_pseudo_atomic_interrupted not in pseudo atomic");
    __asm__ volatile ("or" LISPOBJ_ASM_SUFFIX " $1, %0" : "+m" (pa_bits));
}

static inline void
clear_pseudo_atomic_interrupted(struct thread *thread)
{
    if (get_pseudo_atomic_atomic(thread))
        lose("clear_pseudo_atomic_interrupted in pseudo atomic");
    __asm__ volatile ("and" LISPOBJ_ASM_SUFFIX " $~1, %0" : "+m" (pa_bits));
}
#undef pa_bits
#undef LISPOBJ_ASM_SUFFIX

#elif (defined LISP_FEATURE_ARM || defined LISP_FEATURE_ARM64 || defined LISP_FEATURE_RISCV) && !defined LISP_FEATURE_SB_THREAD
static inline int
get_pseudo_atomic_atomic(struct thread *thread)
{
    return SymbolValue(PSEUDO_ATOMIC_ATOMIC, thread) != NIL;
}

static inline void
set_pseudo_atomic_atomic(struct thread *thread)
{
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, PSEUDO_ATOMIC_ATOMIC, thread);
}

static inline void
clear_pseudo_atomic_atomic(struct thread *thread)
{
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, NIL, thread);
}

static inline int
get_pseudo_atomic_interrupted(struct thread *thread)
{
    return SymbolValue(PSEUDO_ATOMIC_INTERRUPTED, thread) != 0;
}

static inline void
set_pseudo_atomic_interrupted(struct thread *thread)
{
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, (lispobj)do_pending_interrupt, thread);
}

static inline void
clear_pseudo_atomic_interrupted(struct thread *thread)
{
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, 0, 0);
}

#elif defined(LISP_FEATURE_GENCGC)

/* FIXME: Are these async signal safe? Compiler reordering? */

#if !defined LISP_FEATURE_ARM && !defined LISP_FEATURE_ARM64 && !defined LISP_FEATURE_RISCV
#define set_alloc_pointer(value) \
    (dynamic_space_free_pointer = \
     ((lispobj *) \
      ((value) | (((uword_t)dynamic_space_free_pointer) & LOWTAG_MASK))))

#define get_alloc_pointer()                                     \
    ((uword_t) dynamic_space_free_pointer & ~LOWTAG_MASK)
#endif

#ifdef LISP_FEATURE_SB_THREAD
#define get_pseudo_atomic_atomic(thread) \
    ((thread)->pseudo_atomic_bits & flag_PseudoAtomic)
#define set_pseudo_atomic_atomic(thread) \
    ((thread)->pseudo_atomic_bits |= flag_PseudoAtomic)
#define clear_pseudo_atomic_atomic(thread) \
    ((thread)->pseudo_atomic_bits &= ~flag_PseudoAtomic)
#define get_pseudo_atomic_interrupted(thread) \
    ((thread)->pseudo_atomic_bits & flag_PseudoAtomicInterrupted)
#define set_pseudo_atomic_interrupted(thread) \
    ((thread)->pseudo_atomic_bits |= flag_PseudoAtomicInterrupted)
#define clear_pseudo_atomic_interrupted(thread) \
    ((thread)->pseudo_atomic_bits &= ~flag_PseudoAtomicInterrupted)
#else
#define get_pseudo_atomic_atomic(thread)                                \
    ((uword_t)dynamic_space_free_pointer & flag_PseudoAtomic)
#define set_pseudo_atomic_atomic(thread)                                \
    (dynamic_space_free_pointer                                         \
     = (lispobj*) ((uword_t)dynamic_space_free_pointer | flag_PseudoAtomic))
#define clear_pseudo_atomic_atomic(thread)                              \
    (dynamic_space_free_pointer                                         \
     = (lispobj*) ((uword_t) dynamic_space_free_pointer & ~flag_PseudoAtomic))
#define get_pseudo_atomic_interrupted(thread)                           \
    ((uword_t) dynamic_space_free_pointer & flag_PseudoAtomicInterrupted)
#define clear_pseudo_atomic_interrupted(thread)                         \
    (dynamic_space_free_pointer                                         \
     = (lispobj*) ((uword_t) dynamic_space_free_pointer & ~flag_PseudoAtomicInterrupted))
#define set_pseudo_atomic_interrupted(thread)                           \
    (dynamic_space_free_pointer                                         \
     = (lispobj*) ((uword_t) dynamic_space_free_pointer | flag_PseudoAtomicInterrupted))
#endif

#else /* CHENEYGC */

#define set_alloc_pointer(value) dynamic_space_free_pointer = (lispobj*)(value)
#define get_alloc_pointer() (dynamic_space_free_pointer)

#endif /* defined(LISP_FEATURE_GENCGC) */

#if defined LISP_FEATURE_ARM || defined LISP_FEATURE_ARM64 || defined LISP_FEATURE_RISCV
#define set_alloc_pointer(value)                \
    SetSymbolValue(ALLOCATION_POINTER, value, 0)
#define get_alloc_pointer()                     \
    SymbolValue(ALLOCATION_POINTER, 0)
#endif
#endif /* PSEUDO_ATOMIC_H */
