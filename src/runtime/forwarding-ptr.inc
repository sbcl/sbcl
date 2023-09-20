#define FORWARDING_HEADER 1

inline static bool
forwarding_pointer_p(lispobj *pointer) {
#if defined LISP_FEATURE_64_BIT && defined LISP_FEATURE_LITTLE_ENDIAN
    /* Read exactly 1 byte. The upper bytes can store the original object size.
     * With other architectures we will need to pick a byte distinct from any widetag
     * and any pointer in the low N-LOWTAG-BITS */
    return *(char*)pointer == 1;
#else
    return (*pointer == FORWARDING_HEADER);
#endif
}

static inline lispobj
forwarding_pointer_value(lispobj *pointer) {
    return pointer[1];
}
static inline void set_forwarding_pointer(lispobj *addr, lispobj newspace_copy) {
  // The object at 'addr' might already have been forwarded,
  // but that's ok. Such occurs primarily when dealing with
  // code components, because code can be forwarded by scavenging any
  // pointer to a function that resides within the code.
  // Testing whether the object had been forwarded would just slow
  // things down, so we blindly stomp on whatever was there.
  // Unfortunately this also implies we can't assert
  // that we're operating on a not-yet-forwarded object here.
#ifdef LISP_FEATURE_GENERATIONAL
  //gc_dcheck(from_space_p(addr)); // inclusion order problem, too bad
    addr[0] = FORWARDING_HEADER;
    addr[1] = newspace_copy;
#else
    addr[0] = newspace_copy;
#endif
}
static inline void set_forwarding_pointer_resized(lispobj *addr, lispobj newspace_copy,
                                                  __attribute__((unused)) int old_nwords)
{
#if defined LISP_FEATURE_64_BIT && defined LISP_FEATURE_LITTLE_ENDIAN
    addr[0] = FORWARDING_HEADER | (old_nwords<<N_WIDETAG_BITS);
    addr[1] = newspace_copy;
#else
    set_forwarding_pointer(addr, newspace_copy);
#endif
}

/// Return the newspace copy of 'ptr', which MUST be a possibly-tagged pointer
/// into a managed space. 'ptr' must not be nullptr but can be NIL.
static inline lispobj follow_fp(lispobj ptr)
{
  return forwarding_pointer_p(native_pointer(ptr))
      ? forwarding_pointer_value(native_pointer(ptr)) : ptr;
}
/// This is often more convenient than follow_fp.
static inline lispobj barrier_load(lispobj* slot)
{
    lispobj val = *slot;
    return is_lisp_pointer(val) ? follow_fp(val) : val;
}
