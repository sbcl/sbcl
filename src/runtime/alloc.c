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
#include "genesis/vector.h"
#include "genesis/cons.h"
#include "genesis/bignum.h"
#include "genesis/sap.h"

#define ALIGNED_SIZE(n) ((n) + LOWTAG_MASK) & ~LOWTAG_MASK

#ifdef LISP_FEATURE_GENCGC
extern lispobj *alloc(long bytes);
#endif

static lispobj *
pa_alloc(int bytes)
{
    lispobj *result;
#ifdef LISP_FEATURE_GENCGC
    struct thread *th = arch_os_get_current_thread();

    /* FIXME: OOAO violation: see arch_pseudo_* */
    clear_pseudo_atomic_interrupted(th);
    set_pseudo_atomic_atomic(th);
    result = alloc(bytes);
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
        *current_control_stack_pointer = (lispobj) result;
        current_control_stack_pointer += 1;
#endif
        do_pending_interrupt();
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        current_control_stack_pointer -= 1;
        result = (lispobj *) *current_control_stack_pointer;
#endif
    }
#else
    /* FIXME: this is not pseudo atomic at all, but is called only from
     * interrupt safe places like interrupt handlers. MG - 2005-08-09 */
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
#endif
    return result;
}


lispobj *
alloc_unboxed(int type, int words)
{
    lispobj *result;

    result = pa_alloc(ALIGNED_SIZE((1 + words) * sizeof(lispobj)));
    *result = (lispobj) (words << N_WIDETAG_BITS) | type;
    return result;
}

static lispobj
alloc_vector(int type, int length, int size)
{
    struct vector *result;

    result = (struct vector *)
      pa_alloc(ALIGNED_SIZE((2 + (length*size + 31) / 32) * sizeof(lispobj)));

    result->header = type;
    result->length = make_fixnum(length);

    return make_lispobj(result,OTHER_POINTER_LOWTAG);
}

lispobj
alloc_cons(lispobj car, lispobj cdr)
{
    struct cons *ptr =
        (struct cons *)pa_alloc(ALIGNED_SIZE(sizeof(struct cons)));

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
    lispobj result = alloc_vector(SIMPLE_BASE_STRING_WIDETAG, len+1, 8);
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
