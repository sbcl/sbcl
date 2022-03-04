/*
 * macros for getting/setting the end of dynamic space
 * and manipulating pseudo-atomic flags (per thread, if applicable)
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

#ifndef GETALLOCPTR_H
#define GETALLOCPTR_H

# define set_alloc_pointer(value) dynamic_space_free_pointer = (lispobj*)(value)
# define get_alloc_pointer() (dynamic_space_free_pointer)

#if defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64

# include "allocptr-x86.inc"

#elif defined LISP_FEATURE_SPARC || defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64

// PPC always uses the canonical variant of of PA flag manipulation:
// per-thread bits, no static symbols, and no messing with low bits of dynamic_space_free_pointer.
// The PA-Atomic field is at byte 0 of the thread slot regardless of endianness.
// The PA-Interrupted field is at byte 2 regardless of endian-ness.
// This holds for either word size.
// These values can be anything - they just have to be compatible between C and Lisp.
// Lisp uses NULL-TN to set pseudo-atomic and THREAD-TN to clear it.
# define get_pseudo_atomic_atomic(th) (th)->pseudo_atomic_bits[0] != 0
# define set_pseudo_atomic_atomic(th) (th)->pseudo_atomic_bits[0] = 1
# define clear_pseudo_atomic_atomic(th) (th)->pseudo_atomic_bits[0] = 0
// A 2-byte field allows for 'lhz' followed by 'twi'
# define get_pseudo_atomic_interrupted(th) (th)->pseudo_atomic_bits[2] != 0
// make it look like a fixnum in case we only have a descriptor register
// at our disposal when loading the flag
# define set_pseudo_atomic_interrupted(th) (th)->pseudo_atomic_bits[2] = 8
# define clear_pseudo_atomic_interrupted(th) (th)->pseudo_atomic_bits[2] = 0

#elif defined LISP_FEATURE_ARM || defined LISP_FEATURE_ARM64 \
  || defined LISP_FEATURE_MIPS || defined LISP_FEATURE_RISCV

# include "allocptr-lisp-symbol.inc"

#endif

#endif /* GETALLOCPTR_H */
