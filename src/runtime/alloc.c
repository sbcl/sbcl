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

#include <stdio.h>
#include <string.h>

#include "sbcl.h"
#include "runtime.h"
#include "os.h"
#include "alloc.h"
#include "globals.h"
#include "gc.h"
#include "thread.h"
#include "pseudo-atomic.h"
#include "genesis/vector.h"
#include "genesis/cons.h"
#include "genesis/bignum.h"
#include "genesis/sap.h"
#include "genesis/code.h"

#define ALIGNED_SIZE(n) ((n) + LOWTAG_MASK) & ~LOWTAG_MASK

#ifdef LISP_FEATURE_GENCGC
static lispobj *
pa_alloc(int bytes, int page_type_flag)
{
    lispobj *result;
    struct thread *th = arch_os_get_current_thread();

    /* SIG_STOP_FOR_GC must be unblocked: else two threads racing here
     * may deadlock: one will wait on the GC lock, and the other
     * cannot stop the first one... */
    check_gc_signals_unblocked_or_lose(0);

    /* FIXME: OOAO violation: see arch_pseudo_* */
    set_pseudo_atomic_atomic(th);
    result = general_alloc(bytes, page_type_flag);
#if 0
    /* See how the runtime deals with GC being triggerred. */
    if ((SymbolValue(GC_PENDING,th) == NIL) &&
        (SymbolValue(GC_INHIBIT,th) == NIL) &&
        (random() < RAND_MAX/100)) {
        SetSymbolValue(GC_PENDING,T,th);
        set_pseudo_atomic_interrupted(th);
        maybe_save_gc_mask_and_block_deferrables(NULL);
    }
#endif
    clear_pseudo_atomic_atomic(th);

    if (get_pseudo_atomic_interrupted(th)) {
        /* WARNING KLUDGE FIXME: pa_alloc() is not pseudo-atomic on
         * anything but x86[-64]. maybe_defer_handler doesn't defer
         * interrupts if foreign_function_call_active
         *
         * If the C stack is not scavenged during GC, result needs to
         * be protected against not being referred to by any roots, so
         * we push it onto the lisp control stack, and read it back
         * off after any potential GC has finished */
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
#ifdef LISP_FEATURE_STACK_GROWS_DOWNWARD_NOT_UPWARD
#error "!C_STACK_IS_CONTROL_STACK and STACK_GROWS_DOWNWARD_NOT_UPWARD is not supported"
#endif
        *access_control_stack_pointer(th) = (lispobj) result;
        access_control_stack_pointer(th) += 1;
#endif
        do_pending_interrupt();
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        access_control_stack_pointer(th) -= 1;
        result = (lispobj *) *access_control_stack_pointer(th);
#endif
    }
    return result;
}
#else
static lispobj *
pa_alloc(int bytes, int page_type_flag)
{
    lispobj *result;

    /* This is not pseudo atomic at all, but is called only from
     * interrupt safe places like interrupt handlers. MG -
     * 2005-08-09 */
    check_deferrables_blocked_or_lose(0);

    result = dynamic_space_free_pointer;

    /* Align up to next dual word boundary. */
    bytes = ALIGNED_SIZE(bytes);

    dynamic_space_free_pointer = (lispobj *)((char *)result + bytes);

    if (current_auto_gc_trigger
        && dynamic_space_free_pointer > current_auto_gc_trigger) {
        clear_auto_gc_trigger();
        set_auto_gc_trigger((char *)dynamic_space_free_pointer
                            - (char *)current_dynamic_space);
    }
    return result;
}
#endif

static lispobj *
alloc_unboxed(int type, int words)
{
    lispobj *result;

    result = pa_alloc(ALIGNED_SIZE((1 + words) * sizeof(lispobj)),
                      UNBOXED_PAGE_FLAG);
    *result = (lispobj) (words << N_WIDETAG_BITS) | type;
    return result;
}

static lispobj
alloc_vector(int type, int length, int size, int page_type_flag)
{
    struct vector *result;

    result = (struct vector *)
        pa_alloc(ALIGNED_SIZE((2 + (length*size + 31) / 32) * sizeof(lispobj)),
                 page_type_flag);

    result->header = type;
    result->length = make_fixnum(length);

    return make_lispobj(result,OTHER_POINTER_LOWTAG);
}

lispobj
alloc_cons(lispobj car, lispobj cdr)
{
    struct cons *ptr =
        (struct cons *)pa_alloc(ALIGNED_SIZE(sizeof(struct cons)),
                                BOXED_PAGE_FLAG);

    ptr->car = car;
    ptr->cdr = cdr;

    return make_lispobj(ptr, LIST_POINTER_LOWTAG);
}

lispobj
alloc_number(long n)
{
    struct bignum *ptr;

    if (-0x20000000 < n && n < 0x20000000)
        return make_fixnum(n);
    else {
        ptr = (struct bignum *)alloc_unboxed(BIGNUM_WIDETAG, 1);

        ptr->digits[0] = n;

        return make_lispobj(ptr, OTHER_POINTER_LOWTAG);
    }
}

lispobj
alloc_base_string(char *str)
{
    int len = strlen(str);
    lispobj result = alloc_vector(SIMPLE_BASE_STRING_WIDETAG, len+1, 8,
                                  UNBOXED_PAGE_FLAG);
    struct vector *vec = (struct vector *)native_pointer(result);

    vec->length = make_fixnum(len);
    strcpy((char *)vec->data, str);

    return result;
}

lispobj
alloc_sap(void *ptr)
{
    struct sap *sap;
    sap=(struct sap *)
        alloc_unboxed((int)SAP_WIDETAG, sizeof(struct sap)/sizeof(lispobj) -1);
    sap->pointer = ptr;
    return make_lispobj(sap,OTHER_POINTER_LOWTAG);
}

lispobj
alloc_code_object (unsigned boxed, unsigned unboxed) {
    struct code * code;
    /* Coming in, boxed is the number of boxed words requested.
     * Converting it to a fixnum makes it measured in bytes. It's also
     * rounded up to double word along the way. */
    boxed = make_fixnum(boxed + 1 +
                        (offsetof(struct code, trace_table_offset) >>
                         WORD_SHIFT));
    boxed &= ~LOWTAG_MASK;

    /* Unboxed is in bytes, round it up to double word boundary. Now
     * it's also a fixnum containing the number of unboxed words. */
    unboxed += LOWTAG_MASK;
    unboxed &= ~LOWTAG_MASK;

    code = (struct code *)pa_alloc(boxed + unboxed, CODE_PAGE_FLAG);

    /* It used to be that even on gencgc builds the
     * ALLOCATE-CODE-OBJECT VOP did all this initialization within
     * pseudo atomic. Here, we rely on gc being inhibited. */
    if (SymbolValue(GC_INHIBIT, arch_os_get_current_thread()) == NIL)
        lose("alloc_code_object called with GC enabled.");
    boxed = boxed << (N_WIDETAG_BITS - WORD_SHIFT);
    code->header = boxed | CODE_HEADER_WIDETAG;
    code->code_size = unboxed;
    code->entry_points = NIL;
    code->debug_info = NIL;
    return make_lispobj(code, OTHER_POINTER_LOWTAG);
}
