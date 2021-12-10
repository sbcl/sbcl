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

#ifndef _SBCL_UNALIGNED_H_
#define _SBCL_UNALIGNED_H_

// For CPUs that can do unaligned memory operations, the C compiler
// is generally smart enough to not actually do a memcpy()
static inline uint32_t UNALIGNED_LOAD32(void* p) {
    uint32_t val;
    memcpy(&val, p, 4);
    return val;
}
// 'volatile' works around a spurious GCC warning
static inline void UNALIGNED_STORE32(void* volatile p, uint32_t val) {
    memcpy(p, &val, 4);
}
#ifdef LISP_FEATURE_64_BIT
static inline void UNALIGNED_STORE64(void* p, uint64_t val) {
    memcpy(p, &val, 8);
}
#endif

#endif /* _SBCL_UNALIGNED_H_ */
