extern lispobj *from_space;
extern lispobj *from_space_free_pointer;

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
    ptr = (lispobj *) native_pointer(object);

    return ((from_space <= ptr) &&
            (ptr < from_space_free_pointer));
}

boolean
new_space_p(lispobj object)
{
    lispobj *ptr;

    /*    gc_assert(is_lisp_pointer(object)); */

    ptr = (lispobj *) native_pointer(object);

    return ((new_space <= ptr) &&
            (ptr < new_space_free_pointer));
}

#else

#define from_space_p(ptr) \
        ((from_space <= ((lispobj *) ((pointer_sized_uint_t) ptr))) && \
         (((lispobj *) ((pointer_sized_uint_t) ptr))< from_space_free_pointer))

#define new_space_p(ptr) \
        ((new_space <= ((lispobj *) ((pointer_sized_uint_t) ptr))) && \
         (((lispobj *) ((pointer_sized_uint_t) ptr)) < new_space_free_pointer))

#endif

