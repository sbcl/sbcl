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

#include "runtime.h"
#include "os.h"
#include "sbcl.h"
#include "alloc.h"
#include "globals.h"
#include "gc.h"
#include "thread.h"
#include "genesis/vector.h"
#include "genesis/cons.h"
#include "genesis/bignum.h"
#include "genesis/sap.h"

#define GET_FREE_POINTER() dynamic_space_free_pointer
#define SET_FREE_POINTER(new_value) \
    (dynamic_space_free_pointer = (new_value))
#define GET_GC_TRIGGER() current_auto_gc_trigger
#define SET_GC_TRIGGER(new_value) \
    clear_auto_gc_trigger(); set_auto_gc_trigger(new_value);

#define ALIGNED_SIZE(n) (n+LOWTAG_MASK) & ~LOWTAG_MASK

#if defined LISP_FEATURE_GENCGC
extern lispobj *alloc(int bytes);
lispobj *
pa_alloc(int bytes) 
{
    lispobj *result=0;
    struct thread *th=arch_os_get_current_thread();
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(0),th);
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(1),th);
    result=alloc(bytes);
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(0),th);
    if (SymbolValue(PSEUDO_ATOMIC_INTERRUPTED,th)) 
	/* even if we gc at this point, the new allocation will be
	 * protected from being moved, because result is on the c stack
	 * and points to it */
	do_pending_interrupt(); 
    return result; 
}

#else
static lispobj *
pa_alloc(int bytes)
{
    char *result;

    /* Round to dual word boundary. */
    bytes = (bytes + LOWTAG_MASK) & ~LOWTAG_MASK;

    result = (char *)GET_FREE_POINTER();

    SET_FREE_POINTER((lispobj *)(result + bytes));

    if (GET_GC_TRIGGER() && GET_FREE_POINTER() > GET_GC_TRIGGER()) {
	SET_GC_TRIGGER((char *)GET_FREE_POINTER()
		       - (char *)current_dynamic_space);
    }
    return (lispobj *) result;
}
#endif


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
    struct cons *ptr = (struct cons *)pa_alloc(ALIGNED_SIZE(sizeof(struct cons)));

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
