/*
 * allocation routines for C code.  For allocation done by Lisp look
 * instead at src/compiler/target/alloc.lisp and .../macros.lisp
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
lispobj alloc_code_object (unsigned boxed, unsigned unboxed) {
    struct code * code;
    struct thread *th = arch_os_get_current_thread();
    /* boxed is the number of constants, add other slots, align it to
     * two words, so that the code start is aligned, and convert it to
     * bytes. */
    boxed = (boxed + 1 +
             (offsetof(struct code, constants) >>
              WORD_SHIFT)) << WORD_SHIFT;
    boxed &= ~LOWTAG_MASK;

    /* Unboxed is the size of instructions in bytes. It will be stored
     * as is in the code_size slot, but it needs to be allocated with
     * double-word alignment. */
    unsigned unboxed_aligned = (unboxed + LOWTAG_MASK) & ~LOWTAG_MASK;

    /* Since alloc_code_object is run under WITHOUT-GCING it doesn't
     * actaully need to be pseudo-atomic, this is just to appease the
     * assertions in general_alloc() */
    set_pseudo_atomic_atomic(th);
    code = (struct code *)general_alloc(boxed + unboxed_aligned, CODE_PAGE_FLAG);
    clear_pseudo_atomic_atomic(th);

    /* It used to be that even on gencgc builds the
     * ALLOCATE-CODE-OBJECT VOP did all this initialization within
     * pseudo atomic. Here, we rely on gc being inhibited. */
    if (SymbolValue(GC_INHIBIT, arch_os_get_current_thread()) == NIL)
        lose("alloc_code_object called with GC enabled.");
    boxed = boxed << (N_WIDETAG_BITS - WORD_SHIFT);
    code->header = boxed | CODE_HEADER_WIDETAG;
    code->code_size = make_fixnum(unboxed);
    code->entry_points = NIL;
    code->debug_info = NIL;
    return make_lispobj(code, OTHER_POINTER_LOWTAG);
}
#endif
