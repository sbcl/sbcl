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
#include "gc-assert.h"

static inline int code_total_nwords(struct code* c) {
#ifdef LISP_FEATURE_64_BIT
    return c->header >> 32;
#else
    /* Mask out bits reserved for GC */
    return (c->header >> N_WIDETAG_BITS) & 0x3FFFFF;
#endif
}

// Return signed int in case something tries to compute the number of boxed
// words excluding the header word itself using "code_header_words(header) - 1",
// which, for a filler needs to come out as negative, not a huge positive.
static inline sword_t code_boxed_len(struct code* c)
{
    return (c->boxed_size & 0xFFFFFFFF); // byte count
}
static inline sword_t code_header_words(struct code* c)
{
    return code_boxed_len(c) >> WORD_SHIFT; // word count
}

/* Code component trailer words:
 *
 *            fun table pointer v
 * ... | offset | offset | .... |   N-funs  |  datalen   |
 *                                 (uint16)    (uint16)
 *     | -----------       code trailer     -------------|
 *
 *     ^ end of instructions              end of object  ^
 *
 * The fun_table pointer is aligned at a 4-byte boundary.
 * The final uint16 is the length of the code trailer, including the bytes
 * comprising that length field itself, and the preceding count (n-funs) field.
 * Thus the minimum trailer size should be 4 for any normal code object.
 * Immobile trampolines are a special case - they indicate 0 trailer length,
 * but the final 2 bytes are present since they have to be read.
 */
static inline unsigned int* code_fun_table(struct code* code) {
    return (unsigned int*)((char*)code + N_WORD_BYTES*code_total_nwords(code) - 4);
}
static inline unsigned short code_trailer_len(struct code* code) {
    // Do not attempt to read the trailer len from a code page filler object.
    // Fillers are recognizable by boxed_size == 0.
    return code->boxed_size ?
      *(unsigned short*)((char*)code + N_WORD_BYTES*code_total_nwords(code) - 2) : 0;
}
static inline unsigned short code_n_funs(struct code* code) {
    // Do not attempt to read the fun table size from a code object with no trailer.
    // If there is a nonzero trailer length, assume it is at least enough to store
    // the length of the fun table.
    return !code_trailer_len(code) ? 0 : *(unsigned short*)code_fun_table(code) >> 4;
}

static inline char* code_text_start(struct code* code) {
    return (char*)code + code_boxed_len(code);
}
/// The length in bytes of the unboxed portion excluding the trailer.
static inline int code_text_size(struct code* c) {
    return N_WORD_BYTES * code_total_nwords(c) - code_boxed_len(c) - code_trailer_len(c);
}
/// Return the text start, unless the boxed size has not yet been assigned.
/// In the latter case, the text start would seem to be the object address,
/// and reading a word there as if it were the jump table size would be wrong.
static inline lispobj* code_jumptable_start(struct code* code) {
    return code->boxed_size ? (lispobj*)code_text_start(code) : 0;
}
static inline unsigned int jumptable_count(lispobj* table) {
    // extract low 14 bits regardless of machine word size
    return table ? *table & 0x3FFF : 0;
}
static inline unsigned int code_serialno(struct code* code) {
    lispobj* table = code_jumptable_start(code);
#ifdef LISP_FEATURE_64_BIT
    return table ? *table >> 32 : 0; // high 4 bits are the serialno
#else
    return table ? *table >> 14 : 0; // else it only gets 18 (= 32 - 14) bits
#endif
}

static inline unsigned int code_n_named_calls(struct code* code) {
#ifdef LISP_FEATURE_64_BIT
    return code->boxed_size >> 32;
#else
    return 0;
#endif
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

/// Maximum number of word backwards from a simple-fun
/// to its containing code component - corresponds to ~128MB.
/// The limit exists so that we can store the layout pointer
/// in the header of any callable object if N_WORD_BITS = 64.
/// This is not technically a restriction on the code size.
#define FUN_HEADER_NWORDS_MASK 0xFFFFFF

static inline lispobj* FUNCTION(lispobj obj) {
  return (lispobj*)(obj - FUN_POINTER_LOWTAG);
}

static inline lispobj* fun_code_header(lispobj* fun) {
    return fun - (HeaderValue(*fun) & FUN_HEADER_NWORDS_MASK);
}
static inline lispobj fun_code_tagged(lispobj* fun) {
    return make_lispobj(fun_code_header(fun), OTHER_POINTER_LOWTAG);
}

#ifdef RETURN_PC_WIDETAG
#define embedded_obj_p(tag) (tag==RETURN_PC_WIDETAG || tag==SIMPLE_FUN_WIDETAG)
#else
#define embedded_obj_p(tag) (tag==SIMPLE_FUN_WIDETAG)
#endif
/* Convert from a lispobj with lowtag bits to the starting address
 * of the heap object. */
static inline lispobj *
base_pointer(lispobj ptr)
{
    lispobj *obj = native_pointer(ptr);
    int widetag = widetag_of(obj);
    return embedded_obj_p(widetag) ? fun_code_header(obj) : obj;
}

#if defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64 || defined LISP_FEATURE_ARM64
# define fun_self_from_baseptr(simple_fun) (lispobj)simple_fun->insts
# define fun_self_from_taggedptr(funptr) \
    funptr - FUN_POINTER_LOWTAG + 2*N_WORD_BYTES
# define fun_taggedptr_from_self(self) \
    self - 2*N_WORD_BYTES + FUN_POINTER_LOWTAG
#else
# define fun_self_from_baseptr(simple_fun) \
    make_lispobj(simple_fun,FUN_POINTER_LOWTAG)
# define fun_self_from_taggedptr(funptr) funptr
# define fun_taggedptr_from_self(self) self
#endif

#define simplefun_is_wrapped(fun) \
  fun->self != fun_self_from_baseptr(fun) && fun->self != 0
#define CODE_IS_TRACED 0x01

#endif
