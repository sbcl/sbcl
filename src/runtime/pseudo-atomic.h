/*
 * macros for getting/setting the pseudo-atomic flags (per thread, if applicable)
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

#include "genesis/thread.h"
#include "genesis/static-symbols.h"
#include "genesis/symbol.h"

#if defined LISP_FEATURE_SPARC || defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64

/* These architectures make no distinction between +/- sb-thread.
 * They always use per-thread bit and never static symbols for the PA bits.
 * The PA-Atomic field is at byte 0 of the thread slot regardless of endianness.
 * The PA-Interrupted field is at byte 2 regardless of endian-ness.
 * This holds for either word size.
 * These values can be anything - they just have to be compatible between C and Lisp.
 * Lisp uses NULL-TN to set pseudo-atomic and THREAD-TN to clear it. */
# define get_pseudo_atomic_atomic(th) (th)->pseudo_atomic_bits[0] != 0
# define clear_pseudo_atomic_atomic(th) (th)->pseudo_atomic_bits[0] = 0
// A 2-byte field allows for 'lhz' followed by 'twi'
# define get_pseudo_atomic_interrupted(th) (th)->pseudo_atomic_bits[2] != 0
// make it look like a fixnum in case we only have a descriptor register
// at our disposal when loading the flag
# define set_pseudo_atomic_interrupted(th) (th)->pseudo_atomic_bits[2] = 8
# define clear_pseudo_atomic_interrupted(th) (th)->pseudo_atomic_bits[2] = 0

#elif defined LISP_FEATURE_ARM || defined LISP_FEATURE_ARM64 \
  || defined LISP_FEATURE_MIPS || defined LISP_FEATURE_RISCV
#include "thread.h" // for SymbolValue

/* These architectures use a thread slot if #+sb-thread,
 * or else two static symbols if #-sb-thread.
 * The complication is that if using a thread slot, the the values
 * are stored in two bits, but otherwise the value is the entire word. */

#ifdef LISP_FEATURE_SB_THREAD

#define get_pseudo_atomic_atomic(thread) \
    ((thread)->pseudo_atomic_bits & flag_PseudoAtomic)
#define clear_pseudo_atomic_atomic(thread) \
    ((thread)->pseudo_atomic_bits &= ~flag_PseudoAtomic)
#define get_pseudo_atomic_interrupted(thread) \
    ((thread)->pseudo_atomic_bits & flag_PseudoAtomicInterrupted)
#define set_pseudo_atomic_interrupted(thread) \
    ((thread)->pseudo_atomic_bits |= flag_PseudoAtomicInterrupted)
#define clear_pseudo_atomic_interrupted(thread) \
    ((thread)->pseudo_atomic_bits &= ~flag_PseudoAtomicInterrupted)

#else

static inline int
get_pseudo_atomic_atomic(struct thread *thread)
{
    return SymbolValue(PSEUDO_ATOMIC_ATOMIC, thread) != NIL;
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
#endif // LISP_FEATURE_SB_THREAD

#elif defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64

/* x86 uses either a thread slot, or a single static symbol holding
 * the same value as the thread slot would hold.
 * The encoding of the values is strange - the entire word is nonzero
 * when pseudo-atomic, and the lowest bit should be 0.
 * If interrupted, the low bit becomes 1. This seems a little bogus because
 * symbol->value at that point can have "illegal" bits (non-descriptor).
 * I guess the reason it's allowed is GC can't ever see the bad value.
 * But it sure seems like it's asking for trouble */

#if defined LISP_FEATURE_X86 && !defined LISP_FEATURE_SB_THREAD
# define pa_bits SYMBOL(PSEUDO_ATOMIC_BITS)->value
#else
# define pa_bits thread->pseudo_atomic_bits
#endif

#include "interr.h" // for lose()

static inline int
get_pseudo_atomic_atomic(struct thread __attribute__((unused)) *thread)
{
    // mask out the 'interrupted' bit before testing
    return (pa_bits & ~1) != 0;
}

static inline void
clear_pseudo_atomic_atomic(struct thread __attribute__((unused)) *thread)
{
    __sync_fetch_and_and(&(pa_bits), 1); // leave the "interrupted" bit intact
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
    __sync_fetch_and_or(&(pa_bits), 1);
}

static inline void
clear_pseudo_atomic_interrupted(struct thread *thread)
{
    if (get_pseudo_atomic_atomic(thread))
        lose("clear_pseudo_atomic_interrupted in pseudo atomic");
    __sync_fetch_and_and(&(pa_bits), ~(uword_t)1);
}
#undef pa_bits

#else

#error "architecture needs definition of pseudo-atomic bits"

#endif

#endif /* PSEUDO_ATOMIC_H */
