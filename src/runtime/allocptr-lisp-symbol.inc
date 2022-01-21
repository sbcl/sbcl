/* -*- Mode: C -*- */

// ARM, AARCH64, and RISCV use a static-space lisp symbol for ALLOCATION_POINTER
#define set_alloc_pointer(value) SetSymbolValue(ALLOCATION_POINTER, value, 0)
#define get_alloc_pointer() (lispobj*)SymbolValue(ALLOCATION_POINTER, 0)

#ifdef LISP_FEATURE_SB_THREAD
#include "pseudo-atomic.h"
#else
// unithread ARM, AARCH64, and RISCV use a static symbol for PSEUDO_ATOMIC_foo
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
    #ifndef DO_PENDING_INTERRUPT
    // RISCV defines do_pending_interrupt as a lisp asm routine the address of which
    // is stored in a static lisp symbol and which in C is obtained via #define.
    extern void do_pending_interrupt();
    #endif
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, (lispobj)do_pending_interrupt, thread);
}

static inline void
clear_pseudo_atomic_interrupted(struct thread *thread)
{
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, 0, 0);
}
#endif
