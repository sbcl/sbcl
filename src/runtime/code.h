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

#ifndef _CODE_H_
#define _CODE_H_

#include "genesis/code.h"
#include "gc-internal.h" // for gc_asser()

/// Internal use only (within this header)
static inline sword_t code_boxed_nwords_(lispobj header)
{
#ifdef LISP_FEATURE_64_BIT
    return header >> 32;
#else
    /* Mask out bits reserved for GC */
    return HeaderValue(header & ~((1 << 31) | (1 << 30)));
#endif
}

/// Internal use only (within this header)
static inline unsigned int
code_unboxed_nbytes_(lispobj n) // given n = code->code_size
{
    // Cast out high 32 bits of code_size if lispobj is 64 bits.
    return fixnum_value((uint32_t)n);
}

/// Internal use only (within this header)
static inline unsigned int
code_unboxed_nwords_(lispobj n)
{
    // Return ceiling |N / N_WORD_BYTES|
    return (code_unboxed_nbytes_(n) + (N_WORD_BYTES-1)) >> WORD_SHIFT;
}

//// Interfaces

// Return signed int in case something tries to compute the number of boxed
// words excluding the header word itself using "code_boxed_nwords(header) - 1",a
// which, for a filler needs to come out as negative, not a huge positive.
static inline sword_t code_header_words(struct code* c)
{
    return code_boxed_nwords_(c->header);
}

static inline int code_total_nwords(struct code* c) {
   return ALIGN_UP(code_boxed_nwords_(c->header) + code_unboxed_nwords_(c->code_size),
                   2);
}

static inline char* code_text_start(struct code* code) {
    return (char*)code + N_WORD_BYTES * code_header_words(code);
}

/* Code component trailer words:
 *
 *      | fun_offset | fun_offset | .... | N-entries |
 *                                       ^           total size
 *                 fun_table pointer ---/
 *
 * The fun_table pointer is aligned at a 4-byte boundary.
 */
static inline unsigned int*
code_fun_table(struct code* code) {
  return (unsigned int*)
      (code_text_start(code) + code_unboxed_nbytes_(code->code_size) - sizeof (uint16_t));
}
static inline unsigned short
code_n_funs(struct code* code) {
    // immobile space filler objects appear to be code but have no simple-funs.
    // Should probably consider changing the widetag to FILLER_WIDETAG.
    return code_header_words(code) ? *((unsigned short*)code_fun_table(code)) >> 4 : 0;
}

/// The length in bytes of the unboxed portion excluding the simple-fun offset table.
static inline int code_text_size(struct code* c) {
    return code_unboxed_nbytes_(c->code_size)
        - (code_n_funs(c) * sizeof (uint32_t)) - sizeof (uint16_t);
}
// Iterate over the native pointers to each function in 'code_var'
// offsets are stored as the number of bytes into the instructions
// portion of the code object at which the simple-fun object resides.
// We use bytes, not words, because that's what the COMPUTE-FUN vop expects.
#define for_each_simple_fun(index_var,fun_var,code_var,assertp,guts)  \
  { int _nfuns_ = code_n_funs(code_var);                              \
    if (_nfuns_ > 0) {                                                \
      char *_insts_ = code_text_start(code_var);                      \
      int index_var = 0;                                              \
      unsigned int* _offsets_ = code_fun_table(code_var) - 1;         \
      do {                                                            \
       struct simple_fun* fun_var                                     \
           = (struct simple_fun*)(_insts_ + _offsets_[-index_var]);   \
       if (assertp)                                                   \
         gc_assert(widetag_of(&fun_var->header)==SIMPLE_FUN_WIDETAG); \
       guts ;                                                         \
      } while (++index_var < _nfuns_);                                \
  }}

/// Return true if 'pointer' is plausibly a pointer into the text of
/// the code object at 'start_addr'.
///
/// We don't pedantically check that it is within bounds, nor that it is not
/// pointing into either a simple-fun header or the trailing simple-fun table
/// or a piece of unboxed data preceding the first simple-fun.
/// Arguably we should at least check that it is below code_text_size though.
static inline int instruction_ptr_p(char *pointer, lispobj *start_addr)
{
    return widetag_of(start_addr) == CODE_HEADER_WIDETAG &&
           pointer >= code_text_start((struct code*)start_addr);
}

#endif
