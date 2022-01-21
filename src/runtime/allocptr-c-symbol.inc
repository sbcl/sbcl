/* -*- Mode: C -*- */

#ifdef LISP_FEATURE_CHENEYGC
#define set_alloc_pointer(value) dynamic_space_free_pointer = (lispobj*)(value)
#define get_alloc_pointer() (dynamic_space_free_pointer)
#endif

#ifdef LISP_FEATURE_GENCGC
#define set_alloc_pointer(value) \
    (dynamic_space_free_pointer = \
     ((lispobj *) \
      ((value) | (((uword_t)dynamic_space_free_pointer) & LOWTAG_MASK))))

#define get_alloc_pointer()                                     \
    ((uword_t) dynamic_space_free_pointer & ~LOWTAG_MASK)
#ifdef LISP_FEATURE_SB_THREAD
#include "pseudo-atomic.h"
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
#endif
