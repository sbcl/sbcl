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

#include "os.h" /* for os_context_t */

extern lispobj *from_space;
extern lispobj *from_space_free_pointer;
#define compacting_p() (1) /* always */

extern lispobj *new_space;
extern lispobj *new_space_free_pointer;


/* predicates */
/* #if defined(DEBUG_SPACE_PREDICATES) */
#if 0
boolean
from_space_p(lispobj object)
{
    lispobj *ptr;

    /* this can be called for untagged pointers as well as for
       descriptors, so this assertion's not applicable
       gc_assert(is_lisp_pointer(object));
    */
    ptr = native_pointer(object);

    return ((from_space <= ptr) &&
            (ptr < from_space_free_pointer));
}

boolean
new_space_p(lispobj object)
{
    lispobj *ptr;

    /*    gc_assert(is_lisp_pointer(object)); */

    ptr = native_pointer(object);

    return ((new_space <= ptr) &&
            (ptr < new_space_free_pointer));
}

#else

#define from_space_p(ptr) \
        ((from_space <= ((lispobj *) ((uintptr_t) ptr))) && \
         (((lispobj *) ((uintptr_t) ptr))< from_space_free_pointer))

#define new_space_p(ptr) \
        ((new_space <= ((lispobj *) ((uintptr_t) ptr))) && \
         (((lispobj *) ((uintptr_t) ptr)) < new_space_free_pointer))

#endif

extern boolean cheneygc_handle_wp_violation(os_context_t*, void*);
