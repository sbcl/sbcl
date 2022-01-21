/* -*- Mode: C -*- */

#define set_alloc_pointer(value) dynamic_space_free_pointer = (lispobj*)(value)
#define get_alloc_pointer() (dynamic_space_free_pointer)

#include "interr.h" // for lose()
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
    // mask out the 'interrupted' bit before testing
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
