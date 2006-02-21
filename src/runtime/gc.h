/*
 * garbage collection
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

#ifndef _GC_H_
#define _GC_H_
typedef signed long page_index_t;
typedef signed char generation_index_t;

extern void gc_init(void);
extern void gc_initialize_pointers(void);
extern void collect_garbage(generation_index_t last_gen);
extern void gc_init_tables(void);


#include "os.h"

extern void set_auto_gc_trigger(os_vm_size_t usage);
extern void clear_auto_gc_trigger(void);

extern int maybe_gc_pending;

#include "fixnump.h"

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)

#define set_alloc_pointer(value)                \
    SetSymbolValue(ALLOCATION_POINTER, value, 0)
#define get_alloc_pointer()                     \
    SymbolValue(ALLOCATION_POINTER, 0)
#define get_binding_stack_pointer(thread)       \
    SymbolValue(BINDING_STACK_POINTER, thread)
#define get_pseudo_atomic_atomic(thread)        \
    SymbolValue(PSEUDO_ATOMIC_ATOMIC, thread)
#define set_pseudo_atomic_atomic(thread)                                \
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(1), thread);
#define clear_pseudo_atomic_atomic(thread)                      \
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(0), thread);
#define get_pseudo_atomic_interrupted(thread)                   \
    fixnum_value(SymbolValue(PSEUDO_ATOMIC_INTERRUPTED, thread))
#define clear_pseudo_atomic_interrupted(thread)                         \
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(0), thread)
#define set_pseudo_atomic_interrupted(thread)                           \
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(1), thread)

#elif defined(LISP_FEATURE_PPC) && defined(LISP_FEATURE_GENCGC)

#define set_alloc_pointer(value) \
    (dynamic_space_free_pointer =                                       \
     (value) | (((unsigned long)dynamic_space_free_pointer) & LOWTAG_MASK))

#define get_alloc_pointer()                                     \
    ((unsigned long) dynamic_space_free_pointer & ~LOWTAG_MASK)
#define get_binding_stack_pointer(thread)       \
    (current_binding_stack_pointer)
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

#endif /* _GC_H_ */
