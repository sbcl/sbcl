/*
 * C-level stuff to implement Lisp-level PURIFY
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
#include <sys/types.h>
#include <stdlib.h>
#include <strings.h>
#include <errno.h>

#include "sbcl.h"
#include "runtime.h"
#include "os.h"
#include "globals.h"
#include "validate.h"
#include "interrupt.h"
#include "purify.h"
#include "interr.h"
#include "gc.h"
#include "gc-internal.h"
#include "thread.h"
#include "genesis/primitive-objects.h"
#include "genesis/static-symbols.h"
#include "genesis/layout.h"
#include "genesis/hash-table.h"
#include "gencgc.h"

/* We don't ever do purification with GENCGC as of 1.0.5.*. There was
 * a lot of hairy and fragile ifdeffage in here to support purify on
 * x86oids, which has now been removed. So this code can't even be
 * compiled with GENCGC any more.  -- JES, 2007-04-30.
 */
#ifndef LISP_FEATURE_GENCGC

#define PRINTNOISE

static lispobj *dynamic_space_purify_pointer;


/* These hold the original end of the read_only and static spaces so
 * we can tell what are forwarding pointers. */

static lispobj *read_only_end, *static_end;

static lispobj *read_only_free, *static_free;

static lispobj *pscav(lispobj *addr, long nwords, boolean constant);

#define LATERBLOCKSIZE 1020
#define LATERMAXCOUNT 10

static struct
later {
    struct later *next;
    union {
        lispobj *ptr;
        long count;
    } u[LATERBLOCKSIZE];
} *later_blocks = NULL;
static long later_count = 0;

#if N_WORD_BITS == 32
 #define SIMPLE_ARRAY_WORD_WIDETAG SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG
#elif N_WORD_BITS == 64
 #define SIMPLE_ARRAY_WORD_WIDETAG SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG
#endif


static boolean
forwarding_pointer_p(lispobj obj)
{
    lispobj *ptr = native_pointer(obj);

    return ((static_end <= ptr && ptr <= static_free) ||
            (read_only_end <= ptr && ptr <= read_only_free));
}

static boolean
dynamic_pointer_p(lispobj ptr)
{
    return (ptr >= (lispobj)current_dynamic_space
            &&
            ptr < (lispobj)dynamic_space_purify_pointer);
}

static inline lispobj *
newspace_alloc(long nwords, int constantp)
{
    lispobj *ret;
    nwords=CEILING(nwords,2);
    if(constantp) {
        if(read_only_free + nwords >= (lispobj *)READ_ONLY_SPACE_END) {
            lose("Ran out of read-only space while purifying!\n");
        }
        ret=read_only_free;
        read_only_free+=nwords;
    } else {
        if(static_free + nwords >= (lispobj *)STATIC_SPACE_END) {
            lose("Ran out of static space while purifying!\n");
        }
        ret=static_free;
        static_free+=nwords;
    }
    return ret;
}


static void
pscav_later(lispobj *where, long count)
{
    struct later *new;

    if (count > LATERMAXCOUNT) {
        while (count > LATERMAXCOUNT) {
            pscav_later(where, LATERMAXCOUNT);
            count -= LATERMAXCOUNT;
            where += LATERMAXCOUNT;
        }
    }
    else {
        if (later_blocks == NULL || later_count == LATERBLOCKSIZE ||
            (later_count == LATERBLOCKSIZE-1 && count > 1)) {
            new  = (struct later *)malloc(sizeof(struct later));
            new->next = later_blocks;
            if (later_blocks && later_count < LATERBLOCKSIZE)
                later_blocks->u[later_count].ptr = NULL;
            later_blocks = new;
            later_count = 0;
        }

        if (count != 1)
            later_blocks->u[later_count++].count = count;
        later_blocks->u[later_count++].ptr = where;
    }
}

static lispobj
ptrans_boxed(lispobj thing, lispobj header, boolean constant)
{
    long nwords;
    lispobj result, *new, *old;

    nwords = CEILING(1 + HeaderValue(header), 2);

    /* Allocate it */
    old = (lispobj *)native_pointer(thing);
    new = newspace_alloc(nwords,constant);

    /* Copy it. */
    bcopy(old, new, nwords * sizeof(lispobj));

    /* Deposit forwarding pointer. */
    result = make_lispobj(new, lowtag_of(thing));
    *old = result;

    /* Scavenge it. */
    pscav(new, nwords, constant);

    return result;
}

/* We need to look at the layout to see whether it is a pure structure
 * class, and only then can we transport as constant. If it is pure,
 * we can ALWAYS transport as a constant. */
static lispobj
ptrans_instance(lispobj thing, lispobj header, boolean /* ignored */ constant)
{
    struct layout *layout =
      (struct layout *) native_pointer(((struct instance *)native_pointer(thing))->slots[0]);
    lispobj pure = layout->pure;

    switch (pure) {
    case T:
        return (ptrans_boxed(thing, header, 1));
    case NIL:
        return (ptrans_boxed(thing, header, 0));
    case 0:
        {
            /* Substructure: special case for the COMPACT-INFO-ENVs,
             * where the instance may have a point to the dynamic
             * space placed into it (e.g. the cache-name slot), but
             * the lists and arrays at the time of a purify can be
             * moved to the RO space. */
            long nwords;
            lispobj result, *new, *old;

            nwords = CEILING(1 + HeaderValue(header), 2);

            /* Allocate it */
            old = (lispobj *)native_pointer(thing);
            new = newspace_alloc(nwords, 0); /*  inconstant */

            /* Copy it. */
            bcopy(old, new, nwords * sizeof(lispobj));

            /* Deposit forwarding pointer. */
            result = make_lispobj(new, lowtag_of(thing));
            *old = result;

            /* Scavenge it. */
            pscav(new, nwords, 1);

            return result;
        }
    default:
        gc_abort();
        return NIL; /* dummy value: return something ... */
    }
}

static lispobj
ptrans_fdefn(lispobj thing, lispobj header)
{
    long nwords;
    lispobj result, *new, *old, oldfn;
    struct fdefn *fdefn;

    nwords = CEILING(1 + HeaderValue(header), 2);

    /* Allocate it */
    old = (lispobj *)native_pointer(thing);
    new = newspace_alloc(nwords, 0);    /* inconstant */

    /* Copy it. */
    bcopy(old, new, nwords * sizeof(lispobj));

    /* Deposit forwarding pointer. */
    result = make_lispobj(new, lowtag_of(thing));
    *old = result;

    /* Scavenge the function. */
    fdefn = (struct fdefn *)new;
    oldfn = fdefn->fun;
    pscav(&fdefn->fun, 1, 0);
    if ((char *)oldfn + FUN_RAW_ADDR_OFFSET == fdefn->raw_addr)
        fdefn->raw_addr = (char *)fdefn->fun + FUN_RAW_ADDR_OFFSET;

    return result;
}

static lispobj
ptrans_unboxed(lispobj thing, lispobj header)
{
    long nwords;
    lispobj result, *new, *old;

    nwords = CEILING(1 + HeaderValue(header), 2);

    /* Allocate it */
    old = (lispobj *)native_pointer(thing);
    new = newspace_alloc(nwords,1);     /* always constant */

    /* copy it. */
    bcopy(old, new, nwords * sizeof(lispobj));

    /* Deposit forwarding pointer. */
    result = make_lispobj(new , lowtag_of(thing));
    *old = result;

    return result;
}

static lispobj
ptrans_vector(lispobj thing, long bits, long extra,
              boolean boxed, boolean constant)
{
    struct vector *vector;
    long nwords;
    lispobj result, *new;
    long length;

    vector = (struct vector *)native_pointer(thing);
    length = fixnum_value(vector->length)+extra;
    // Argh, handle simple-vector-nil separately.
    if (bits == 0) {
      nwords = 2;
    } else {
      nwords = CEILING(NWORDS(length, bits) + 2, 2);
    }

    new=newspace_alloc(nwords, (constant || !boxed));
    bcopy(vector, new, nwords * sizeof(lispobj));

    result = make_lispobj(new, lowtag_of(thing));
    vector->header = result;

    if (boxed)
        pscav(new, nwords, constant);

    return result;
}

static lispobj
ptrans_code(lispobj thing)
{
    struct code *code, *new;
    long nwords;
    lispobj func, result;

    code = (struct code *)native_pointer(thing);
    nwords = CEILING(HeaderValue(code->header) + fixnum_value(code->code_size),
                     2);

    new = (struct code *)newspace_alloc(nwords,1); /* constant */

    bcopy(code, new, nwords * sizeof(lispobj));

    result = make_lispobj(new, OTHER_POINTER_LOWTAG);

    /* Stick in a forwarding pointer for the code object. */
    *(lispobj *)code = result;

    /* Put in forwarding pointers for all the functions. */
    for (func = code->entry_points;
         func != NIL;
         func = ((struct simple_fun *)native_pointer(func))->next) {

        gc_assert(lowtag_of(func) == FUN_POINTER_LOWTAG);

        *(lispobj *)native_pointer(func) = result + (func - thing);
    }

    /* Arrange to scavenge the debug info later. */
    pscav_later(&new->debug_info, 1);

    /* FIXME: why would this be a fixnum? */
    /* "why" is a hard word, but apparently for compiled functions the
       trace_table_offset contains the length of the instructions, as
       a fixnum.  See CODE-INST-AREA-LENGTH in
       src/compiler/target-disassem.lisp.  -- CSR, 2004-01-08 */
    if (!(fixnump(new->trace_table_offset)))
#if 0
        pscav(&new->trace_table_offset, 1, 0);
#else
        new->trace_table_offset = NIL; /* limit lifetime */
#endif

    /* Scavenge the constants. */
    pscav(new->constants, HeaderValue(new->header)-5, 1);

    /* Scavenge all the functions. */
    pscav(&new->entry_points, 1, 1);
    for (func = new->entry_points;
         func != NIL;
         func = ((struct simple_fun *)native_pointer(func))->next) {
        gc_assert(lowtag_of(func) == FUN_POINTER_LOWTAG);
        gc_assert(!dynamic_pointer_p(func));

        pscav(&((struct simple_fun *)native_pointer(func))->self, 2, 1);
        pscav_later(&((struct simple_fun *)native_pointer(func))->name, 4);
    }

    return result;
}

static lispobj
ptrans_func(lispobj thing, lispobj header)
{
    long nwords;
    lispobj code, *new, *old, result;
    struct simple_fun *function;

    /* Thing can either be a function header, a closure function
     * header, a closure, or a funcallable-instance. If it's a closure
     * or a funcallable-instance, we do the same as ptrans_boxed.
     * Otherwise we have to do something strange, 'cause it is buried
     * inside a code object. */

    if (widetag_of(header) == SIMPLE_FUN_HEADER_WIDETAG) {

        /* We can only end up here if the code object has not been
         * scavenged, because if it had been scavenged, forwarding pointers
         * would have been left behind for all the entry points. */

        function = (struct simple_fun *)native_pointer(thing);
        code =
            make_lispobj
            ((native_pointer(thing) -
              (HeaderValue(function->header))), OTHER_POINTER_LOWTAG);

        /* This will cause the function's header to be replaced with a
         * forwarding pointer. */

        ptrans_code(code);

        /* So we can just return that. */
        return function->header;
    }
    else {
        /* It's some kind of closure-like thing. */
        nwords = CEILING(1 + HeaderValue(header), 2);
        old = (lispobj *)native_pointer(thing);

        /* Allocate the new one.  FINs *must* not go in read_only
         * space.  Closures can; they never change */

        new = newspace_alloc
            (nwords,(widetag_of(header)!=FUNCALLABLE_INSTANCE_HEADER_WIDETAG));

        /* Copy it. */
        bcopy(old, new, nwords * sizeof(lispobj));

        /* Deposit forwarding pointer. */
        result = make_lispobj(new, lowtag_of(thing));
        *old = result;

        /* Scavenge it. */
        pscav(new, nwords, 0);

        return result;
    }
}

static lispobj
ptrans_returnpc(lispobj thing, lispobj header)
{
    lispobj code, new;

    /* Find the corresponding code object. */
    code = thing - HeaderValue(header)*sizeof(lispobj);

    /* Make sure it's been transported. */
    new = *(lispobj *)native_pointer(code);
    if (!forwarding_pointer_p(new))
        new = ptrans_code(code);

    /* Maintain the offset: */
    return new + (thing - code);
}

#define WORDS_PER_CONS CEILING(sizeof(struct cons) / sizeof(lispobj), 2)

static lispobj
ptrans_list(lispobj thing, boolean constant)
{
    struct cons *old, *new, *orig;
    long length;

    orig = (struct cons *) newspace_alloc(0,constant);
    length = 0;

    do {
        /* Allocate a new cons cell. */
        old = (struct cons *)native_pointer(thing);
        new = (struct cons *) newspace_alloc(WORDS_PER_CONS,constant);

        /* Copy the cons cell and keep a pointer to the cdr. */
        new->car = old->car;
        thing = new->cdr = old->cdr;

        /* Set up the forwarding pointer. */
        *(lispobj *)old = make_lispobj(new, LIST_POINTER_LOWTAG);

        /* And count this cell. */
        length++;
    } while (lowtag_of(thing) == LIST_POINTER_LOWTAG &&
             dynamic_pointer_p(thing) &&
             !(forwarding_pointer_p(*(lispobj *)native_pointer(thing))));

    /* Scavenge the list we just copied. */
    pscav((lispobj *)orig, length * WORDS_PER_CONS, constant);

    return make_lispobj(orig, LIST_POINTER_LOWTAG);
}

static lispobj
ptrans_otherptr(lispobj thing, lispobj header, boolean constant)
{
    switch (widetag_of(header)) {
        /* FIXME: this needs a reindent */
      case BIGNUM_WIDETAG:
      case SINGLE_FLOAT_WIDETAG:
      case DOUBLE_FLOAT_WIDETAG:
#ifdef LONG_FLOAT_WIDETAG
      case LONG_FLOAT_WIDETAG:
#endif
#ifdef COMPLEX_SINGLE_FLOAT_WIDETAG
      case COMPLEX_SINGLE_FLOAT_WIDETAG:
#endif
#ifdef COMPLEX_DOUBLE_FLOAT_WIDETAG
      case COMPLEX_DOUBLE_FLOAT_WIDETAG:
#endif
#ifdef COMPLEX_LONG_FLOAT_WIDETAG
      case COMPLEX_LONG_FLOAT_WIDETAG:
#endif
      case SAP_WIDETAG:
          return ptrans_unboxed(thing, header);
#ifdef LUTEX_WIDETAG
      case LUTEX_WIDETAG:
          gencgc_unregister_lutex((struct lutex *) native_pointer(thing));
          return ptrans_unboxed(thing, header);
#endif

      case RATIO_WIDETAG:
      case COMPLEX_WIDETAG:
      case SIMPLE_ARRAY_WIDETAG:
      case COMPLEX_BASE_STRING_WIDETAG:
#ifdef COMPLEX_CHARACTER_STRING_WIDETAG
    case COMPLEX_CHARACTER_STRING_WIDETAG:
#endif
      case COMPLEX_BIT_VECTOR_WIDETAG:
      case COMPLEX_VECTOR_NIL_WIDETAG:
      case COMPLEX_VECTOR_WIDETAG:
      case COMPLEX_ARRAY_WIDETAG:
        return ptrans_boxed(thing, header, constant);

      case VALUE_CELL_HEADER_WIDETAG:
      case WEAK_POINTER_WIDETAG:
        return ptrans_boxed(thing, header, 0);

      case SYMBOL_HEADER_WIDETAG:
        return ptrans_boxed(thing, header, 0);

      case SIMPLE_ARRAY_NIL_WIDETAG:
        return ptrans_vector(thing, 0, 0, 0, constant);

      case SIMPLE_BASE_STRING_WIDETAG:
        return ptrans_vector(thing, 8, 1, 0, constant);

#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
    case SIMPLE_CHARACTER_STRING_WIDETAG:
        return ptrans_vector(thing, 32, 1, 0, constant);
#endif

      case SIMPLE_BIT_VECTOR_WIDETAG:
        return ptrans_vector(thing, 1, 0, 0, constant);

      case SIMPLE_VECTOR_WIDETAG:
        return ptrans_vector(thing, N_WORD_BITS, 0, 1, constant);

      case SIMPLE_ARRAY_UNSIGNED_BYTE_2_WIDETAG:
        return ptrans_vector(thing, 2, 0, 0, constant);

      case SIMPLE_ARRAY_UNSIGNED_BYTE_4_WIDETAG:
        return ptrans_vector(thing, 4, 0, 0, constant);

      case SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG:
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
      case SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG:
      case SIMPLE_ARRAY_UNSIGNED_BYTE_7_WIDETAG:
#endif
        return ptrans_vector(thing, 8, 0, 0, constant);

      case SIMPLE_ARRAY_UNSIGNED_BYTE_16_WIDETAG:
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
      case SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG:
      case SIMPLE_ARRAY_UNSIGNED_BYTE_15_WIDETAG:
#endif
        return ptrans_vector(thing, 16, 0, 0, constant);

      case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG
      case SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG:
      case SIMPLE_ARRAY_UNSIGNED_BYTE_29_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
      case SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG:
      case SIMPLE_ARRAY_UNSIGNED_BYTE_31_WIDETAG:
#endif
        return ptrans_vector(thing, 32, 0, 0, constant);

#if N_WORD_BITS == 64
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_60_WIDETAG
      case SIMPLE_ARRAY_UNSIGNED_BYTE_60_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG
      case SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG
      case SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG
      case SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG
      case SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG:
#endif
        return ptrans_vector(thing, 64, 0, 0, constant);
#endif

      case SIMPLE_ARRAY_SINGLE_FLOAT_WIDETAG:
        return ptrans_vector(thing, 32, 0, 0, constant);

      case SIMPLE_ARRAY_DOUBLE_FLOAT_WIDETAG:
        return ptrans_vector(thing, 64, 0, 0, constant);

#ifdef SIMPLE_ARRAY_LONG_FLOAT_WIDETAG
      case SIMPLE_ARRAY_LONG_FLOAT_WIDETAG:
#ifdef LISP_FEATURE_SPARC
        return ptrans_vector(thing, 128, 0, 0, constant);
#endif
#endif

#ifdef SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG
      case SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG:
        return ptrans_vector(thing, 64, 0, 0, constant);
#endif

#ifdef SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG
      case SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG:
        return ptrans_vector(thing, 128, 0, 0, constant);
#endif

#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
      case SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG:
#ifdef LISP_FEATURE_SPARC
        return ptrans_vector(thing, 256, 0, 0, constant);
#endif
#endif

      case CODE_HEADER_WIDETAG:
        return ptrans_code(thing);

      case RETURN_PC_HEADER_WIDETAG:
        return ptrans_returnpc(thing, header);

      case FDEFN_WIDETAG:
        return ptrans_fdefn(thing, header);

      default:
        fprintf(stderr, "Invalid widetag: %d\n", widetag_of(header));
        /* Should only come across other pointers to the above stuff. */
        gc_abort();
        return NIL;
    }
}

static long
pscav_fdefn(struct fdefn *fdefn)
{
    boolean fix_func;

    fix_func = ((char *)(fdefn->fun+FUN_RAW_ADDR_OFFSET) == fdefn->raw_addr);
    pscav(&fdefn->name, 1, 1);
    pscav(&fdefn->fun, 1, 0);
    if (fix_func)
        fdefn->raw_addr = (char *)(fdefn->fun + FUN_RAW_ADDR_OFFSET);
    return sizeof(struct fdefn) / sizeof(lispobj);
}

static lispobj *
pscav(lispobj *addr, long nwords, boolean constant)
{
    lispobj thing, *thingp, header;
    long count = 0; /* (0 = dummy init value to stop GCC warning) */
    struct vector *vector;

    while (nwords > 0) {
        thing = *addr;
        if (is_lisp_pointer(thing)) {
            /* It's a pointer. Is it something we might have to move? */
            if (dynamic_pointer_p(thing)) {
                /* Maybe. Have we already moved it? */
                thingp = (lispobj *)native_pointer(thing);
                header = *thingp;
                if (is_lisp_pointer(header) && forwarding_pointer_p(header))
                    /* Yep, so just copy the forwarding pointer. */
                    thing = header;
                else {
                    /* Nope, copy the object. */
                    switch (lowtag_of(thing)) {
                      case FUN_POINTER_LOWTAG:
                        thing = ptrans_func(thing, header);
                        break;

                      case LIST_POINTER_LOWTAG:
                        thing = ptrans_list(thing, constant);
                        break;

                      case INSTANCE_POINTER_LOWTAG:
                        thing = ptrans_instance(thing, header, constant);
                        break;

                      case OTHER_POINTER_LOWTAG:
                        thing = ptrans_otherptr(thing, header, constant);
                        break;

                      default:
                        /* It was a pointer, but not one of them? */
                        gc_abort();
                    }
                }
                *addr = thing;
            }
            count = 1;
        }
#if N_WORD_BITS == 64
        else if (widetag_of(thing) == SINGLE_FLOAT_WIDETAG) {
            count = 1;
        }
#endif
        else if (thing & FIXNUM_TAG_MASK) {
            /* It's an other immediate. Maybe the header for an unboxed */
            /* object. */
            switch (widetag_of(thing)) {
              case BIGNUM_WIDETAG:
              case SINGLE_FLOAT_WIDETAG:
              case DOUBLE_FLOAT_WIDETAG:
#ifdef LONG_FLOAT_WIDETAG
              case LONG_FLOAT_WIDETAG:
#endif
              case SAP_WIDETAG:
                /* It's an unboxed simple object. */
                count = CEILING(HeaderValue(thing)+1, 2);
                break;

              case SIMPLE_VECTOR_WIDETAG:
                  if (HeaderValue(thing) == subtype_VectorValidHashing) {
                    struct hash_table *hash_table =
                        (struct hash_table *)native_pointer(addr[2]);
                    hash_table->needs_rehash_p = T;
                  }
                count = 2;
                break;

              case SIMPLE_ARRAY_NIL_WIDETAG:
                count = 2;
                break;

              case SIMPLE_BASE_STRING_WIDETAG:
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length)+1,8)+2,2);
                break;

#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
            case SIMPLE_CHARACTER_STRING_WIDETAG:
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length)+1,32)+2,2);
                break;
#endif

              case SIMPLE_BIT_VECTOR_WIDETAG:
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),1)+2,2);
                break;

              case SIMPLE_ARRAY_UNSIGNED_BYTE_2_WIDETAG:
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),2)+2,2);
                break;

              case SIMPLE_ARRAY_UNSIGNED_BYTE_4_WIDETAG:
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),4)+2,2);
                break;

              case SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG:
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
              case SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG:
              case SIMPLE_ARRAY_UNSIGNED_BYTE_7_WIDETAG:
#endif
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),8)+2,2);
                break;

              case SIMPLE_ARRAY_UNSIGNED_BYTE_16_WIDETAG:
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
              case SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG:
              case SIMPLE_ARRAY_UNSIGNED_BYTE_15_WIDETAG:
#endif
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),16)+2,2);
                break;

              case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG
              case SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG:
              case SIMPLE_ARRAY_UNSIGNED_BYTE_29_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
              case SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG:
              case SIMPLE_ARRAY_UNSIGNED_BYTE_31_WIDETAG:
#endif
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),32)+2,2);
                break;

#if N_WORD_BITS == 64
              case SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG:
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG
              case SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG:
              case SIMPLE_ARRAY_UNSIGNED_BYTE_60_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG
              case SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG:
              case SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG:
#endif
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),64)+2,2);
                break;
#endif

              case SIMPLE_ARRAY_SINGLE_FLOAT_WIDETAG:
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length), 32) + 2,
                                2);
                break;

              case SIMPLE_ARRAY_DOUBLE_FLOAT_WIDETAG:
#ifdef SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG
              case SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG:
#endif
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length), 64) + 2,
                                2);
                break;

#ifdef SIMPLE_ARRAY_LONG_FLOAT_WIDETAG
              case SIMPLE_ARRAY_LONG_FLOAT_WIDETAG:
                vector = (struct vector *)addr;
#ifdef LISP_FEATURE_SPARC
                count = fixnum_value(vector->length)*4+2;
#endif
                break;
#endif

#ifdef SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG
              case SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG:
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length), 128) + 2,
                                2);
                break;
#endif

#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
              case SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG:
                vector = (struct vector *)addr;
#ifdef LISP_FEATURE_SPARC
                count = fixnum_value(vector->length)*8+2;
#endif
                break;
#endif

              case CODE_HEADER_WIDETAG:
                gc_abort(); /* no code headers in static space */
                break;

              case SIMPLE_FUN_HEADER_WIDETAG:
              case RETURN_PC_HEADER_WIDETAG:
                /* We should never hit any of these, 'cause they occur
                 * buried in the middle of code objects. */
                gc_abort();
                break;

              case WEAK_POINTER_WIDETAG:
                /* Weak pointers get preserved during purify, 'cause I
                 * don't feel like figuring out how to break them. */
                pscav(addr+1, 2, constant);
                count = 4;
                break;

              case FDEFN_WIDETAG:
                /* We have to handle fdefn objects specially, so we
                 * can fix up the raw function address. */
                count = pscav_fdefn((struct fdefn *)addr);
                break;

              case INSTANCE_HEADER_WIDETAG:
                {
                    struct instance *instance = (struct instance *) addr;
                    struct layout *layout
                        = (struct layout *) native_pointer(instance->slots[0]);
                    long nuntagged = fixnum_value(layout->n_untagged_slots);
                    long nslots = HeaderValue(*addr);
                    pscav(addr + 1, nslots - nuntagged, constant);
                    count = CEILING(1 + nslots, 2);
                }
                break;

              default:
                count = 1;
                break;
            }
        }
        else {
            /* It's a fixnum. */
            count = 1;
        }

        addr += count;
        nwords -= count;
    }

    return addr;
}

int
purify(lispobj static_roots, lispobj read_only_roots)
{
    lispobj *clean;
    long count, i;
    struct later *laters, *next;
    struct thread *thread;

    if(all_threads->next) {
        /* FIXME: there should be _some_ sensible error reporting
         * convention.  See following comment too */
        fprintf(stderr,"Can't purify when more than one thread exists\n");
        fflush(stderr);
        return 0;
    }

#ifdef PRINTNOISE
    printf("[doing purification:");
    fflush(stdout);
#endif

    for_each_thread(thread)
        if (fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX,thread)) != 0) {
        /* FIXME: 1. What does this mean? 2. It shouldn't be reporting
         * its error simply by a. printing a string b. to stdout instead
         * of stderr. */
        printf(" Ack! Can't purify interrupt contexts. ");
        fflush(stdout);
        return 0;
    }

    dynamic_space_purify_pointer = dynamic_space_free_pointer;

    read_only_end = read_only_free =
        (lispobj *)SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0);
    static_end = static_free =
        (lispobj *)SymbolValue(STATIC_SPACE_FREE_POINTER,0);

#ifdef PRINTNOISE
    printf(" roots");
    fflush(stdout);
#endif

    pscav(&static_roots, 1, 0);
    pscav(&read_only_roots, 1, 1);

#ifdef PRINTNOISE
    printf(" handlers");
    fflush(stdout);
#endif
    pscav((lispobj *) interrupt_handlers,
          sizeof(interrupt_handlers) / sizeof(lispobj),
          0);

#ifdef PRINTNOISE
    printf(" stack");
    fflush(stdout);
#endif
    pscav((lispobj *)all_threads->control_stack_start,
          access_control_stack_pointer(all_threads) -
          all_threads->control_stack_start,
          0);

#ifdef PRINTNOISE
    printf(" bindings");
    fflush(stdout);
#endif

    pscav( (lispobj *)all_threads->binding_stack_start,
           (lispobj *)get_binding_stack_pointer(all_threads) -
           all_threads->binding_stack_start,
          0);

    /* The original CMU CL code had scavenge-read-only-space code
     * controlled by the Lisp-level variable
     * *SCAVENGE-READ-ONLY-SPACE*. It was disabled by default, and it
     * wasn't documented under what circumstances it was useful or
     * safe to turn it on, so it's been turned off in SBCL. If you
     * want/need this functionality, and can test and document it,
     * please submit a patch. */
#if 0
    if (SymbolValue(SCAVENGE_READ_ONLY_SPACE) != UNBOUND_MARKER_WIDETAG
        && SymbolValue(SCAVENGE_READ_ONLY_SPACE) != NIL) {
      unsigned  read_only_space_size =
          (lispobj *)SymbolValue(READ_ONLY_SPACE_FREE_POINTER) -
          (lispobj *)READ_ONLY_SPACE_START;
      fprintf(stderr,
              "scavenging read only space: %d bytes\n",
              read_only_space_size * sizeof(lispobj));
      pscav( (lispobj *)READ_ONLY_SPACE_START, read_only_space_size, 0);
    }
#endif

#ifdef PRINTNOISE
    printf(" static");
    fflush(stdout);
#endif
    clean = (lispobj *)STATIC_SPACE_START;
    do {
        while (clean != static_free)
            clean = pscav(clean, static_free - clean, 0);
        laters = later_blocks;
        count = later_count;
        later_blocks = NULL;
        later_count = 0;
        while (laters != NULL) {
            for (i = 0; i < count; i++) {
                if (laters->u[i].count == 0) {
                    ;
                } else if (laters->u[i].count <= LATERMAXCOUNT) {
                    pscav(laters->u[i+1].ptr, laters->u[i].count, 1);
                    i++;
                } else {
                    pscav(laters->u[i].ptr, 1, 1);
                }
            }
            next = laters->next;
            free(laters);
            laters = next;
            count = LATERBLOCKSIZE;
        }
    } while (clean != static_free || later_blocks != NULL);

#ifdef PRINTNOISE
    printf(" cleanup");
    fflush(stdout);
#endif
#ifdef LISP_FEATURE_HPUX
    clear_auto_gc_trigger(); /* restore mmap as it was given by os */
#endif

    os_zero((os_vm_address_t) current_dynamic_space,
            (os_vm_size_t) dynamic_space_size);

    /* Zero the stack. */
    os_zero((os_vm_address_t) access_control_stack_pointer(all_threads),
            (os_vm_size_t)
            ((all_threads->control_stack_end -
              access_control_stack_pointer(all_threads)) * sizeof(lispobj)));

    /* It helps to update the heap free pointers so that free_heap can
     * verify after it's done. */
    SetSymbolValue(READ_ONLY_SPACE_FREE_POINTER, (lispobj)read_only_free,0);
    SetSymbolValue(STATIC_SPACE_FREE_POINTER, (lispobj)static_free,0);

    dynamic_space_free_pointer = current_dynamic_space;
    set_auto_gc_trigger(bytes_consed_between_gcs);

    /* Blast away instruction cache */
    os_flush_icache((os_vm_address_t)READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE);
    os_flush_icache((os_vm_address_t)STATIC_SPACE_START, STATIC_SPACE_SIZE);

#ifdef PRINTNOISE
    printf(" done]\n");
    fflush(stdout);
#endif
    return 0;
}
#else /* LISP_FEATURE_GENCGC */
int
purify(lispobj static_roots, lispobj read_only_roots)
{
    lose("purify called for GENCGC. This should not happen.");
}
#endif /* LISP_FEATURE_GENCGC */
