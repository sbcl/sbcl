/* predicates */
#if defined(DEBUG_SPACE_PREDICATES)

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

    gc_assert(is_lisp_pointer(object));

    ptr = (lispobj *) native_pointer(object);
		
    return ((new_space <= ptr) &&
	    (ptr < new_space_free_pointer));
}	    

#else

#define from_space_p(ptr) \
	((from_space <= ((lispobj *) ptr)) && \
	 (((lispobj *) ptr) < from_space_free_pointer))

#define new_space_p(ptr) \
	((new_space <= ((lispobj *) ptr)) && \
	 (((lispobj *) ptr) < new_space_free_pointer))

#endif

static inline lispobj *
gc_quick_alloc(bytes) {
    lispobj *new=new_space_free_pointer;
    new_space_free_pointer+=(bytes/4);
    return new;
}

static inline lispobj  copy_large_unboxed_object(object, nwords) {
    return copy_object(object,nwords);
}
static inline lispobj  copy_unboxed_object(object, nwords) {
    return copy_object(object,nwords);
}
static inline lispobj  copy_large_object(object, nwords) {
    return copy_object(object,nwords);
}
