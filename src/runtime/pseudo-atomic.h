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

#endif
