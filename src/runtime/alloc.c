/*
 * C half of code-component allocator for Lisp with gencgc.
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

#include "sbcl.h"
#include "alloc.h"
#include "thread.h"
#include "pseudo-atomic.h"
#include "genesis/code.h"

#ifdef LISP_FEATURE_GENCGC
#ifdef LISP_FEATURE_SB_THREAD
/* This lock is used to protect non-thread-local allocation. */
static pthread_mutex_t allocation_lock = PTHREAD_MUTEX_INITIALIZER;
#endif
lispobj alloc_code_object (unsigned boxed, unsigned unboxed)
{
    /* It used to be that even on gencgc builds the
     * ALLOCATE-CODE-OBJECT VOP did all this initialization within
     * pseudo atomic. Here, we rely on gc being inhibited. */
    if (read_TLS(GC_INHIBIT, arch_os_get_current_thread()) == NIL)
        lose("alloc_code_object called with GC enabled.");

    struct code * code;
    struct thread __attribute__((unused)) *th = arch_os_get_current_thread();
    /* boxed is the number of constants; add other slots, align it to
     * two words, so that the code start is aligned. */
    int boxedwords = ALIGN_UP(offsetof(struct code, constants)/sizeof(lispobj)+boxed, 2);

    /* Unboxed is the size of instructions in bytes. It will be stored
     * as is in the code_size slot, but it needs to be allocated with
     * double-word alignment. */
    unsigned unboxed_aligned = ALIGN_UP(unboxed, 2*N_WORD_BYTES);

    /* Since alloc_code_object is run under WITHOUT-GCING it doesn't
     * actaully need to be pseudo-atomic, this is just to appease the
     * assertions in general_alloc() */
    set_pseudo_atomic_atomic(th);

    /* Allocations of code are all serialized. We might also acquire
     * free_pages_lock depending on availability of space in the region */
    int result = thread_mutex_lock(&allocation_lock);
    gc_assert(!result);
    code = (struct code *)
      lisp_alloc(boxedwords*N_WORD_BYTES + unboxed_aligned,
                 CODE_PAGE_ALLOCATED,
                 &gc_alloc_region[CODE_PAGE_ALLOCATED-1],
                 arch_os_get_current_thread());
    result = thread_mutex_unlock(&allocation_lock);
    gc_assert(!result);

    clear_pseudo_atomic_atomic(th);

    code->header = (boxedwords << N_WIDETAG_BITS) | CODE_HEADER_WIDETAG;
    code->code_size = make_fixnum(unboxed);
    code->debug_info = NIL;
    return make_lispobj(code, OTHER_POINTER_LOWTAG);
}
#endif
