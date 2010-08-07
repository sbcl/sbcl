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

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)

#define set_alloc_pointer(value)                \
    SetSymbolValue(ALLOCATION_POINTER, value, 0)
#define get_alloc_pointer()                     \
    SymbolValue(ALLOCATION_POINTER, 0)

#if defined(LISP_FEATURE_X86)
#define LISPOBJ_ASM_SUFFIX "l"
#elif defined(LISP_FEATURE_X86_64)
#define LISPOBJ_ASM_SUFFIX "q"
#endif

static inline int
get_pseudo_atomic_atomic(struct thread *thread)
{
    return SymbolValue(PSEUDO_ATOMIC_BITS, thread) & (~1);
}

static inline void
set_pseudo_atomic_atomic(struct thread *thread)
{
    lispobj *p = SymbolValueAddress(PSEUDO_ATOMIC_BITS, thread);
    if (*p)
        lose("set_pseudo_atomic_atomic: pseudo atomic bits is %d.", *p);
    __asm__ __volatile__
        ("or" LISPOBJ_ASM_SUFFIX " %0,%1"
         :
         : "g" (~1), "m" (*p)
         : "memory");
}

static inline void
clear_pseudo_atomic_atomic(struct thread *thread)
{
    lispobj *p = SymbolValueAddress(PSEUDO_ATOMIC_BITS, thread);
    __asm__ __volatile__
        ("and" LISPOBJ_ASM_SUFFIX " %0,%1"
         :
         : "g" (1), "m" (*p)
         : "memory");
}

static inline int
get_pseudo_atomic_interrupted(struct thread *thread)
{
    return SymbolValue(PSEUDO_ATOMIC_BITS, thread) & 1;
}

static inline void
set_pseudo_atomic_interrupted(struct thread *thread)
{
    if (!get_pseudo_atomic_atomic(thread))
        lose("set_pseudo_atomic_interrupted not in pseudo atomic");
    lispobj *p = SymbolValueAddress(PSEUDO_ATOMIC_BITS, thread);
    __asm__ __volatile__
        ("or" LISPOBJ_ASM_SUFFIX " %0,%1"
         :
         : "g" (1), "m" (*p)
         : "memory");
}

static inline void
clear_pseudo_atomic_interrupted(struct thread *thread)
{
    if (get_pseudo_atomic_atomic(thread))
        lose("clear_pseudo_atomic_interrupted in pseudo atomic");
    lispobj *p = SymbolValueAddress(PSEUDO_ATOMIC_BITS, thread);
    __asm__ __volatile__
        ("and" LISPOBJ_ASM_SUFFIX " %0,%1"
         :
         : "g" (~1), "m" (*p)
         : "memory");
}

#undef LISPOBJ_SUFFIX

#elif defined(LISP_FEATURE_GENCGC)

/* FIXME: Are these async signal safe? Compiler reordering? */

#define set_alloc_pointer(value) \
    (dynamic_space_free_pointer = \
     ((lispobj *) \
      ((value) | (((unsigned long)dynamic_space_free_pointer) & LOWTAG_MASK))))

#define get_alloc_pointer()                                     \
    ((unsigned long) dynamic_space_free_pointer & ~LOWTAG_MASK)

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
    ((unsigned long)dynamic_space_free_pointer & flag_PseudoAtomic)
#define set_pseudo_atomic_atomic(thread)                                \
    (dynamic_space_free_pointer                                         \
     = (lispobj*) ((unsigned long)dynamic_space_free_pointer | flag_PseudoAtomic))
#define clear_pseudo_atomic_atomic(thread)                              \
    (dynamic_space_free_pointer                                         \
     = (lispobj*) ((unsigned long) dynamic_space_free_pointer & ~flag_PseudoAtomic))
#define get_pseudo_atomic_interrupted(thread)                           \
    ((unsigned long) dynamic_space_free_pointer & flag_PseudoAtomicInterrupted)
#define clear_pseudo_atomic_interrupted(thread)                         \
    (dynamic_space_free_pointer                                         \
     = (lispobj*) ((unsigned long) dynamic_space_free_pointer & ~flag_PseudoAtomicInterrupted))
#define set_pseudo_atomic_interrupted(thread)                           \
    (dynamic_space_free_pointer                                         \
     = (lispobj*) ((unsigned long) dynamic_space_free_pointer | flag_PseudoAtomicInterrupted))
#endif

#endif

#endif /* PSEUDO_ATOMIC_H */

