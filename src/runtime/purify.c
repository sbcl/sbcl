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
#include "fixnump.h"
#include "gc.h"
#include "gc-internal.h"
#include "thread.h"
#include "genesis/primitive-objects.h"
#include "genesis/static-symbols.h"

#define PRINTNOISE

#if defined(LISP_FEATURE_GENCGC)
/* this is another artifact of the poor integration between gencgc and
 * the rest of the runtime: on cheney gc there is a global
 * dynamic_space_free_pointer which is valid whenever foreign function
 * call is active, but in gencgc there's no such variable and we have
 * to keep our own
 */
static lispobj *dynamic_space_free_pointer;
#endif
extern unsigned long bytes_consed_between_gcs;

#define gc_abort() \
  lose("GC invariant lost, file \"%s\", line %d", __FILE__, __LINE__)

#if 1
#define gc_assert(ex) do { \
	if (!(ex)) gc_abort(); \
} while (0)
#else
#define gc_assert(ex)
#endif


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

/* FIXME: Shouldn't this be defined in sbcl.h?  See also notes in
 * cheneygc.c */

#ifdef LISP_FEATURE_SPARC
#define FUN_RAW_ADDR_OFFSET 0
#else
#define FUN_RAW_ADDR_OFFSET (6*sizeof(lispobj) - FUN_POINTER_LOWTAG)
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
#ifndef LISP_FEATURE_GENCGC
    return (ptr >= (lispobj)current_dynamic_space
	    &&
	    ptr < (lispobj)dynamic_space_free_pointer);
#else
    /* Be more conservative, and remember, this is a maybe. */
    return (ptr >= (lispobj)DYNAMIC_SPACE_START
	    &&
	    ptr < (lispobj)dynamic_space_free_pointer);
#endif
}

static inline lispobj *
newspace_alloc(long nwords, int constantp) 
{
    lispobj *ret;
    nwords=CEILING(nwords,2);
    if(constantp) {
	ret=read_only_free;
	read_only_free+=nwords;
    } else {
	ret=static_free;
	static_free+=nwords;
    }
    return ret;
}



#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)

#ifdef LISP_FEATURE_GENCGC
/*
 * enhanced x86/GENCGC stack scavenging by Douglas Crosher
 *
 * Scavenging the stack on the i386 is problematic due to conservative
 * roots and raw return addresses. Here it is handled in two passes:
 * the first pass runs before any objects are moved and tries to
 * identify valid pointers and return address on the stack, the second
 * pass scavenges these.
 */

static unsigned pointer_filter_verbose = 0;

/* FIXME: This is substantially the same code as
 * possibly_valid_dynamic_space_pointer in gencgc.c.  The only
 * relevant difference seems to be that the gencgc code also checks
 * for raw pointers into Code objects, whereas in purify these are
 * checked separately in setup_i386_stack_scav - they go onto
 * valid_stack_ra_locations instead of just valid_stack_locations */

static int
valid_dynamic_space_pointer(lispobj *pointer, lispobj *start_addr)
{
    /* If it's not a return address then it needs to be a valid Lisp
     * pointer. */
    if (!is_lisp_pointer((lispobj)pointer))
	return 0;

    /* Check that the object pointed to is consistent with the pointer
     * low tag. */
    switch (lowtag_of((lispobj)pointer)) {
    case FUN_POINTER_LOWTAG:
	/* Start_addr should be the enclosing code object, or a closure
	 * header. */
	switch (widetag_of(*start_addr)) {
	case CODE_HEADER_WIDETAG:
	    /* This case is probably caught above. */
	    break;
	case CLOSURE_HEADER_WIDETAG:
	case FUNCALLABLE_INSTANCE_HEADER_WIDETAG:
	    if ((long)pointer != ((long)start_addr+FUN_POINTER_LOWTAG)) {
		if (pointer_filter_verbose) {
		    fprintf(stderr,"*Wf2: %x %x %x\n", 
			    (unsigned long) pointer, 
			    (unsigned long) start_addr, *start_addr);
		}
		return 0;
	    }
	    break;
	default:
	    if (pointer_filter_verbose) {
		fprintf(stderr,"*Wf3: %x %x %x\n", (unsigned long) pointer, 
			(unsigned long) start_addr, *start_addr);
	    }
	    return 0;
	}
	break;
    case LIST_POINTER_LOWTAG:
	if ((long)pointer != ((long)start_addr+LIST_POINTER_LOWTAG)) {
	    if (pointer_filter_verbose)
		fprintf(stderr,"*Wl1: %x %x %x\n", (unsigned long) pointer, 
			(unsigned long) start_addr, *start_addr);
	    return 0;
	}
	/* Is it plausible cons? */
	if ((is_lisp_pointer(start_addr[0])
	    || ((start_addr[0] & FIXNUM_TAG_MASK) == 0) /* fixnum */
	    || (widetag_of(start_addr[0]) == CHARACTER_WIDETAG)
#if N_WORD_BITS == 64
	    || (widetag_of(start_addr[0]) == SINGLE_FLOAT_WIDETAG)
#endif
	    || (widetag_of(start_addr[0]) == UNBOUND_MARKER_WIDETAG))
	   && (is_lisp_pointer(start_addr[1])
	       || ((start_addr[1] & FIXNUM_TAG_MASK) == 0) /* fixnum */
	       || (widetag_of(start_addr[1]) == CHARACTER_WIDETAG)
#if N_WORD_BITS == 64
	       || (widetag_of(start_addr[1]) == SINGLE_FLOAT_WIDETAG)
#endif
	       || (widetag_of(start_addr[1]) == UNBOUND_MARKER_WIDETAG))) {
	    break;
	} else {
	    if (pointer_filter_verbose) {
		fprintf(stderr,"*Wl2: %x %x %x\n", (unsigned long) pointer, 
			(unsigned long) start_addr, *start_addr);
	    }
	    return 0;
	}
    case INSTANCE_POINTER_LOWTAG:
	if ((long)pointer != ((long)start_addr+INSTANCE_POINTER_LOWTAG)) {
	    if (pointer_filter_verbose) {
		fprintf(stderr,"*Wi1: %x %x %x\n", (unsigned long) pointer, 
			(unsigned long) start_addr, *start_addr);
	    }
	    return 0;
	}
	if (widetag_of(start_addr[0]) != INSTANCE_HEADER_WIDETAG) {
	    if (pointer_filter_verbose) {
		fprintf(stderr,"*Wi2: %x %x %x\n", (unsigned long) pointer, 
			(unsigned long) start_addr, *start_addr);
	    }
	    return 0;
	}
	break;
    case OTHER_POINTER_LOWTAG:
	if ((long)pointer != ((long)start_addr+OTHER_POINTER_LOWTAG)) {
	    if (pointer_filter_verbose) {
		fprintf(stderr,"*Wo1: %x %x %x\n", (unsigned long) pointer, 
			(unsigned long) start_addr, *start_addr);
	    }
	    return 0;
	}
	/* Is it plausible? Not a cons. XXX should check the headers. */
	if (is_lisp_pointer(start_addr[0]) || ((start_addr[0] & FIXNUM_TAG_MASK) == 0)) {
	    if (pointer_filter_verbose) {
		fprintf(stderr,"*Wo2: %x %x %x\n", (unsigned long) pointer, 
			(unsigned long) start_addr, *start_addr);
	    }
	    return 0;
	}
	switch (widetag_of(start_addr[0])) {
	case UNBOUND_MARKER_WIDETAG:
	case CHARACTER_WIDETAG:
#if N_WORD_BITS == 64
	case SINGLE_FLOAT_WIDETAG:
#endif
	    if (pointer_filter_verbose) {
		fprintf(stderr,"*Wo3: %x %x %x\n", (unsigned long) pointer, 
			(unsigned long) start_addr, *start_addr);
	    }
	    return 0;

	    /* only pointed to by function pointers? */
	case CLOSURE_HEADER_WIDETAG:
	case FUNCALLABLE_INSTANCE_HEADER_WIDETAG:
	    if (pointer_filter_verbose) {
		fprintf(stderr,"*Wo4: %x %x %x\n", (unsigned long) pointer, 
			(unsigned long) start_addr, *start_addr);
	    }
	    return 0;

	case INSTANCE_HEADER_WIDETAG:
	    if (pointer_filter_verbose) {
		fprintf(stderr,"*Wo5: %x %x %x\n", (unsigned long) pointer, 
			(unsigned long) start_addr, *start_addr);
	    }
	    return 0;

	    /* the valid other immediate pointer objects */
	case SIMPLE_VECTOR_WIDETAG:
	case RATIO_WIDETAG:
	case COMPLEX_WIDETAG:
#ifdef COMPLEX_SINGLE_FLOAT_WIDETAG
	case COMPLEX_SINGLE_FLOAT_WIDETAG:
#endif
#ifdef COMPLEX_DOUBLE_FLOAT_WIDETAG
	case COMPLEX_DOUBLE_FLOAT_WIDETAG:
#endif
#ifdef COMPLEX_LONG_FLOAT_WIDETAG
	case COMPLEX_LONG_FLOAT_WIDETAG:
#endif
	case SIMPLE_ARRAY_WIDETAG:
	case COMPLEX_BASE_STRING_WIDETAG:
#ifdef COMPLEX_CHARACTER_STRING_WIDETAG
	case COMPLEX_CHARACTER_STRING_WIDETAG:
#endif
	case COMPLEX_VECTOR_NIL_WIDETAG:
	case COMPLEX_BIT_VECTOR_WIDETAG:
	case COMPLEX_VECTOR_WIDETAG:
	case COMPLEX_ARRAY_WIDETAG:
	case VALUE_CELL_HEADER_WIDETAG:
	case SYMBOL_HEADER_WIDETAG:
	case FDEFN_WIDETAG:
	case CODE_HEADER_WIDETAG:
	case BIGNUM_WIDETAG:
#if N_WORD_BITS != 64
	case SINGLE_FLOAT_WIDETAG:
#endif
	case DOUBLE_FLOAT_WIDETAG:
#ifdef LONG_FLOAT_WIDETAG
	case LONG_FLOAT_WIDETAG:
#endif
	case SIMPLE_ARRAY_NIL_WIDETAG:
	case SIMPLE_BASE_STRING_WIDETAG:
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
	case SIMPLE_CHARACTER_STRING_WIDETAG:
#endif
	case SIMPLE_BIT_VECTOR_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_2_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_4_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_7_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_15_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_16_WIDETAG:
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_29_WIDETAG
	case SIMPLE_ARRAY_UNSIGNED_BYTE_29_WIDETAG:
#endif
	case SIMPLE_ARRAY_UNSIGNED_BYTE_31_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_60_WIDETAG
		case SIMPLE_ARRAY_UNSIGNED_BYTE_60_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG
		case SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG
		case SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
	case SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
	case SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG
	case SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
	case SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG
		case SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG
		case SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG:
#endif
	case SIMPLE_ARRAY_SINGLE_FLOAT_WIDETAG:
	case SIMPLE_ARRAY_DOUBLE_FLOAT_WIDETAG:
#ifdef SIMPLE_ARRAY_LONG_FLOAT_WIDETAG
	case SIMPLE_ARRAY_LONG_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG
	case SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG
	case SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
	case SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG:
#endif
	case SAP_WIDETAG:
	case WEAK_POINTER_WIDETAG:
	    break;

	default:
	    if (pointer_filter_verbose) {
		fprintf(stderr,"*Wo6: %x %x %x\n", (unsigned long) pointer, 
			(unsigned long) start_addr, *start_addr);
	    }
	    return 0;
	}
	break;
    default:
	if (pointer_filter_verbose) {
	    fprintf(stderr,"*W?: %x %x %x\n", (unsigned long) pointer, 
		    (unsigned long) start_addr, *start_addr);
	}
	return 0;
    }

    /* looks good */
    return 1;
}

#define MAX_STACK_POINTERS 256
lispobj *valid_stack_locations[MAX_STACK_POINTERS];
unsigned long num_valid_stack_locations;

#define MAX_STACK_RETURN_ADDRESSES 128
lispobj *valid_stack_ra_locations[MAX_STACK_RETURN_ADDRESSES];
lispobj *valid_stack_ra_code_objects[MAX_STACK_RETURN_ADDRESSES];
unsigned long num_valid_stack_ra_locations;

/* Identify valid stack slots. */
static void
setup_i386_stack_scav(lispobj *lowaddr, lispobj *base)
{
    lispobj *sp = lowaddr;
    num_valid_stack_locations = 0;
    num_valid_stack_ra_locations = 0;
    for (sp = lowaddr; sp < base; sp++) {
	lispobj thing = *sp;
	/* Find the object start address */
	lispobj *start_addr = search_dynamic_space((void *)thing);
	if (start_addr) {
	    /* We need to allow raw pointers into Code objects for
	     * return addresses. This will also pick up pointers to
	     * functions in code objects. */
	    if (widetag_of(*start_addr) == CODE_HEADER_WIDETAG) {
		/* FIXME asserting here is a really dumb thing to do.
		 * If we've overflowed some arbitrary static limit, we
		 * should just refuse to purify, instead of killing
		 * the whole lisp session
		 */
		gc_assert(num_valid_stack_ra_locations <
			  MAX_STACK_RETURN_ADDRESSES);
		valid_stack_ra_locations[num_valid_stack_ra_locations] = sp;
		valid_stack_ra_code_objects[num_valid_stack_ra_locations++] =
		    (lispobj *)((long)start_addr + OTHER_POINTER_LOWTAG);
	    } else {
		if (valid_dynamic_space_pointer((void *)thing, start_addr)) {
		    gc_assert(num_valid_stack_locations < MAX_STACK_POINTERS);
		    valid_stack_locations[num_valid_stack_locations++] = sp;
		}
	    }
	}
    }
    if (pointer_filter_verbose) {
	fprintf(stderr, "number of valid stack pointers = %d\n",
		num_valid_stack_locations);
	fprintf(stderr, "number of stack return addresses = %d\n",
		num_valid_stack_ra_locations);
    }
}

static void
pscav_i386_stack(void)
{
    long i;

    for (i = 0; i < num_valid_stack_locations; i++)
	pscav(valid_stack_locations[i], 1, 0);

    for (i = 0; i < num_valid_stack_ra_locations; i++) {
	lispobj code_obj = (lispobj)valid_stack_ra_code_objects[i];
	pscav(&code_obj, 1, 0);
	if (pointer_filter_verbose) {
	    fprintf(stderr,"*C moved RA %x to %x; for code object %x to %x\n",
		    *valid_stack_ra_locations[i],
		    (long)(*valid_stack_ra_locations[i])
		    - ((long)valid_stack_ra_code_objects[i] - (long)code_obj),
		    (unsigned long) valid_stack_ra_code_objects[i], code_obj);
	}
	*valid_stack_ra_locations[i] =
	    ((long)(*valid_stack_ra_locations[i])
	     - ((long)valid_stack_ra_code_objects[i] - (long)code_obj));
    }
}
#endif
#endif


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
    lispobj layout = ((struct instance *)native_pointer(thing))->slots[0];
    lispobj pure = ((struct instance *)native_pointer(layout))->slots[15];

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
    new = newspace_alloc(nwords, 0);	/* inconstant */

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
    new = newspace_alloc(nwords,1);	/* always constant */
    
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

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
static void
apply_code_fixups_during_purify(struct code *old_code, struct code *new_code)
{
    long nheader_words, ncode_words, nwords;
    void  *constants_start_addr, *constants_end_addr;
    void  *code_start_addr, *code_end_addr;
    lispobj fixups = NIL;
    unsigned  displacement = (unsigned)new_code - (unsigned)old_code;
    struct vector *fixups_vector;

    ncode_words = fixnum_value(new_code->code_size);
    nheader_words = HeaderValue(*(lispobj *)new_code);
    nwords = ncode_words + nheader_words;

    constants_start_addr = (void *)new_code + 5 * N_WORD_BYTES;
    constants_end_addr = (void *)new_code + nheader_words*N_WORD_BYTES;
    code_start_addr = (void *)new_code + nheader_words*N_WORD_BYTES;
    code_end_addr = (void *)new_code + nwords*N_WORD_BYTES;

    /* The first constant should be a pointer to the fixups for this
     * code objects. Check. */
    fixups = new_code->constants[0];

    /* It will be 0 or the unbound-marker if there are no fixups, and
     * will be an other-pointer to a vector if it is valid. */
    if ((fixups==0) ||
	(fixups==UNBOUND_MARKER_WIDETAG) ||
	!is_lisp_pointer(fixups)) {
#ifdef LISP_FEATURE_GENCGC
	/* Check for a possible errors. */
	sniff_code_object(new_code,displacement);
#endif
	return;
    }

    fixups_vector = (struct vector *)native_pointer(fixups);

    /* Could be pointing to a forwarding pointer. */
    if (is_lisp_pointer(fixups) && (dynamic_pointer_p(fixups))
	&& forwarding_pointer_p(*(lispobj *)fixups_vector)) {
	/* If so then follow it. */
	fixups_vector =
	    (struct vector *)native_pointer(*(lispobj *)fixups_vector);
    }

    if (widetag_of(fixups_vector->header) == SIMPLE_ARRAY_WORD_WIDETAG) {
	/* We got the fixups for the code block. Now work through the
	 * vector, and apply a fixup at each address. */
	long length = fixnum_value(fixups_vector->length);
	long i;
	for (i=0; i<length; i++) {
	    unsigned offset = fixups_vector->data[i];
	    /* Now check the current value of offset. */
	    unsigned old_value =
		*(unsigned *)((unsigned)code_start_addr + offset);

	    /* If it's within the old_code object then it must be an
	     * absolute fixup (relative ones are not saved) */
	    if ((old_value>=(unsigned)old_code)
		&& (old_value<((unsigned)old_code + nwords * N_WORD_BYTES)))
		/* So add the dispacement. */
		*(unsigned *)((unsigned)code_start_addr + offset) = old_value
		    + displacement;
	    else
		/* It is outside the old code object so it must be a relative
		 * fixup (absolute fixups are not saved). So subtract the
		 * displacement. */
		*(unsigned *)((unsigned)code_start_addr + offset) = old_value
		    - displacement;
	}
    }

    /* No longer need the fixups. */
    new_code->constants[0] = 0;

#ifdef LISP_FEATURE_GENCGC
    /* Check for possible errors. */
    sniff_code_object(new_code,displacement);
#endif
}
#endif

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

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    apply_code_fixups_during_purify(code,new);
#endif

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

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
	/* Temporarily convert the self pointer to a real function pointer. */
	((struct simple_fun *)native_pointer(func))->self
	    -= FUN_RAW_ADDR_OFFSET;
#endif
        pscav(&((struct simple_fun *)native_pointer(func))->self, 2, 1);
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
	((struct simple_fun *)native_pointer(func))->self
	    += FUN_RAW_ADDR_OFFSET;
#endif
        pscav_later(&((struct simple_fun *)native_pointer(func))->name, 3);
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
#ifdef LISP_FEATURE_X86
        return ptrans_vector(thing, 96, 0, 0, constant);
#endif
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
#ifdef LISP_FEATURE_X86
        return ptrans_vector(thing, 192, 0, 0, constant);
#endif
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

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
/* now putting code objects in static space */
static long
pscav_code(struct code*code)
{
    long nwords;
    lispobj func;
    nwords = CEILING(HeaderValue(code->header) + fixnum_value(code->code_size),
		     2);

    /* Arrange to scavenge the debug info later. */
    pscav_later(&code->debug_info, 1);

    /* Scavenge the constants. */
    pscav(code->constants, HeaderValue(code->header)-5, 1);

    /* Scavenge all the functions. */
    pscav(&code->entry_points, 1, 1);
    for (func = code->entry_points;
         func != NIL;
         func = ((struct simple_fun *)native_pointer(func))->next) {
        gc_assert(lowtag_of(func) == FUN_POINTER_LOWTAG);
        gc_assert(!dynamic_pointer_p(func));

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
	/* Temporarily convert the self pointer to a real function
	 * pointer. */
	((struct simple_fun *)native_pointer(func))->self
	    -= FUN_RAW_ADDR_OFFSET;
#endif
        pscav(&((struct simple_fun *)native_pointer(func))->self, 2, 1);
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
	((struct simple_fun *)native_pointer(func))->self
	    += FUN_RAW_ADDR_OFFSET;
#endif
        pscav_later(&((struct simple_fun *)native_pointer(func))->name, 3);
    }

    return CEILING(nwords,2);
}
#endif

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
                    *addr = (subtype_VectorMustRehash << N_WIDETAG_BITS) |
                        SIMPLE_VECTOR_WIDETAG;
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
#ifdef LISP_FEATURE_X86
                count = fixnum_value(vector->length)*3+2;
#endif
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
#ifdef LISP_FEATURE_X86
                count = fixnum_value(vector->length)*6+2;
#endif
#ifdef LISP_FEATURE_SPARC
                count = fixnum_value(vector->length)*8+2;
#endif
                break;
#endif

              case CODE_HEADER_WIDETAG:
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
                gc_abort(); /* no code headers in static space */
#else
		count = pscav_code((struct code*)addr);
#endif
                break;

              case SIMPLE_FUN_HEADER_WIDETAG:
              case RETURN_PC_HEADER_WIDETAG:
                /* We should never hit any of these, 'cause they occur
                 * buried in the middle of code objects. */
                gc_abort();
		break;

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
	      case CLOSURE_HEADER_WIDETAG:
	      case FUNCALLABLE_INSTANCE_HEADER_WIDETAG:
		/* The function self pointer needs special care on the
		 * x86 because it is the real entry point. */
		{
		  lispobj fun = ((struct closure *)addr)->fun
		    - FUN_RAW_ADDR_OFFSET;
		  pscav(&fun, 1, constant);
		  ((struct closure *)addr)->fun = fun + FUN_RAW_ADDR_OFFSET;
		}
		count = 2;
		break;
#endif

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
#ifdef LISP_FEATURE_GENCGC
    gc_alloc_update_all_page_tables();
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

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    dynamic_space_free_pointer =
      (lispobj*)SymbolValue(ALLOCATION_POINTER,0);
#endif

    read_only_end = read_only_free =
        (lispobj *)SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0);
    static_end = static_free =
        (lispobj *)SymbolValue(STATIC_SPACE_FREE_POINTER,0);

#ifdef PRINTNOISE
    printf(" roots");
    fflush(stdout);
#endif

#if defined(LISP_FEATURE_GENCGC) && (defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
    /* note this expects only one thread to be active.  We'd have to 
     * stop all the others in the same way as GC does if we wanted 
     * PURIFY to work when >1 thread exists */
    setup_i386_stack_scav(((&static_roots)-2),
			  ((void *)all_threads->control_stack_end));
#endif

    pscav(&static_roots, 1, 0);
    pscav(&read_only_roots, 1, 1);

#ifdef PRINTNOISE
    printf(" handlers");
    fflush(stdout);
#endif
    pscav((lispobj *) all_threads->interrupt_data->interrupt_handlers,
          sizeof(all_threads->interrupt_data->interrupt_handlers)
	  / sizeof(lispobj),
          0);

#ifdef PRINTNOISE
    printf(" stack");
    fflush(stdout);
#endif
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
    pscav((lispobj *)all_threads->control_stack_start,
	  current_control_stack_pointer - 
	  all_threads->control_stack_start,
	  0);
#else
#ifdef LISP_FEATURE_GENCGC
    pscav_i386_stack();
#endif
#endif

#ifdef PRINTNOISE
    printf(" bindings");
    fflush(stdout);
#endif
#if !(defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
    pscav( (lispobj *)all_threads->binding_stack_start,
	  (lispobj *)current_binding_stack_pointer -
	   all_threads->binding_stack_start,
	  0);
#else
    for_each_thread(thread) {
	pscav( (lispobj *)thread->binding_stack_start,
	       (lispobj *)SymbolValue(BINDING_STACK_POINTER,thread) -
	       (lispobj *)thread->binding_stack_start,
	  0);
	pscav( (lispobj *) (thread+1),
	       fixnum_value(SymbolValue(FREE_TLS_INDEX,0)) -
	       (sizeof (struct thread))/(sizeof (lispobj)),
	  0);
    }


#endif

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

    os_zero((os_vm_address_t) current_dynamic_space,
            (os_vm_size_t) DYNAMIC_SPACE_SIZE);

    /* Zero the stack. Note that the stack is also zeroed by SUB-GC
     * calling SCRUB-CONTROL-STACK - this zeros the stack on the x86. */
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
    os_zero((os_vm_address_t) current_control_stack_pointer,
            (os_vm_size_t)
	    ((all_threads->control_stack_end -
	      current_control_stack_pointer) * sizeof(lispobj)));
#endif

    /* It helps to update the heap free pointers so that free_heap can
     * verify after it's done. */
    SetSymbolValue(READ_ONLY_SPACE_FREE_POINTER, (lispobj)read_only_free,0);
    SetSymbolValue(STATIC_SPACE_FREE_POINTER, (lispobj)static_free,0);

#if !defined(ALLOCATION_POINTER)
    dynamic_space_free_pointer = current_dynamic_space;
    set_auto_gc_trigger(bytes_consed_between_gcs);
#else
#if defined LISP_FEATURE_GENCGC
    gc_free_heap();
#else
#error unsupported case /* in CMU CL, was "ibmrt using GC" */
#endif
#endif

    /* Blast away instruction cache */
    os_flush_icache((os_vm_address_t)READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE);
    os_flush_icache((os_vm_address_t)STATIC_SPACE_START, STATIC_SPACE_SIZE);

#ifdef PRINTNOISE
    printf(" done]\n");
    fflush(stdout);
#endif
    return 0;
}
