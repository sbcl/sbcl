/*
 * Garbage Collection common functions for scavenging, moving and sizing 
 * objects.  These are for use with both GC (stop & copy GC) and GENCGC
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

/*
 * GENerational Conservative Garbage Collector for SBCL x86
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

/*
 * For a review of garbage collection techniques (e.g. generational
 * GC) and terminology (e.g. "scavenging") see Paul R. Wilson,
 * "Uniprocessor Garbage Collection Techniques". As of 20000618, this
 * had been accepted for _ACM Computing Surveys_ and was available
 * as a PostScript preprint through
 *   <http://www.cs.utexas.edu/users/oops/papers.html>
 * as
 *   <ftp://ftp.cs.utexas.edu/pub/garbage/bigsurv.ps>.
 */

#include <stdio.h>
#include <signal.h>
#include "runtime.h"
#include "sbcl.h"
#include "os.h"
#include "interr.h"
#include "globals.h"
#include "interrupt.h"
#include "validate.h"
#include "lispregs.h"
#include "arch.h"
#include "gc.h"
#include "gc-internal.h"

#ifdef LISP_FEATURE_SPARC
#define LONG_FLOAT_SIZE 4
#else
#ifdef LISP_FEATURE_X86
#define LONG_FLOAT_SIZE 3
#endif
#endif

inline static boolean 
forwarding_pointer_p(lispobj *pointer) {
    lispobj first_word=*pointer;			     
#ifdef LISP_FEATURE_GENCGC
    return (first_word == 0x01);
#else
    return (is_lisp_pointer(first_word) && new_space_p(first_word))
#endif
}

static inline lispobj 
forwarding_pointer_value(lispobj *pointer) {
#ifdef LISP_FEATURE_GENCGC
    return pointer[1];
#else
    return pointer[0];
#endif
}
static inline lispobj
set_forwarding_pointer(lispobj * pointer, lispobj newspace_copy) {
#ifdef LISP_FEATURE_GENCGC
    pointer[0]=0x01;
    pointer[1]=newspace_copy;
#else
    pointer[0]=newspace_copy;
#endif
    return newspace_copy;
}

int (*scavtab[256])(lispobj *where, lispobj object);
lispobj (*transother[256])(lispobj object);
int (*sizetab[256])(lispobj *where);

/*
 * copying objects
 */

/* to copy a boxed object */
lispobj
copy_object(lispobj object, int nwords)
{
    int tag;
    lispobj *new;
    lispobj *source, *dest;

    gc_assert(is_lisp_pointer(object));
    gc_assert(from_space_p(object));
    gc_assert((nwords & 0x01) == 0);

    /* Get tag of object. */
    tag = lowtag_of(object);

    /* Allocate space. */
    new = gc_general_alloc(nwords*4,ALLOC_BOXED,ALLOC_QUICK);

    dest = new;
    source = (lispobj *) native_pointer(object);

    /* Copy the object. */
    while (nwords > 0) {
	dest[0] = source[0];
	dest[1] = source[1];
	dest += 2;
	source += 2;
	nwords -= 2;
    }

    /* Return Lisp pointer of new object. */
    return ((lispobj) new) | tag;
}


/* FIXME: Most calls end up going to some trouble to compute an
 * 'n_words' value for this function. The system might be a little
 * simpler if this function used an 'end' parameter instead. */

void
scavenge(lispobj *start, long n_words)
{
    lispobj *end = start + n_words;
    lispobj *object_ptr;
    int n_words_scavenged;
    
    for (object_ptr = start;
	 object_ptr < end;
	 object_ptr += n_words_scavenged) {

	lispobj object = *object_ptr;
	
	gc_assert(!forwarding_pointer_p(object_ptr));

	if (is_lisp_pointer(object)) {
	    if (from_space_p(object)) {
		/* It currently points to old space. Check for a
		 * forwarding pointer. */
		lispobj *ptr = (lispobj *)native_pointer(object);
		if (forwarding_pointer_p(ptr)) {
		    /* Yes, there's a forwarding pointer. */
		    *object_ptr = forwarding_pointer_value(ptr);
		    n_words_scavenged = 1;
		} else {
		    /* Scavenge that pointer. */
		    n_words_scavenged =
			(scavtab[widetag_of(object)])(object_ptr, object);
		}
	    } else {
		/* It points somewhere other than oldspace. Leave it
		 * alone. */
		n_words_scavenged = 1;
	    }
	}
#ifndef LISP_FEATURE_GENCGC
	/* this workaround is probably not necessary for gencgc; at least, the
	 * behaviour it describes has never been reported */
	else if (nwords==1) {
	    /* there are some situations where an
	       other-immediate may end up in a descriptor
	       register.  I'm not sure whether this is
	       supposed to happen, but if it does then we
	       don't want to (a) barf or (b) scavenge over the
	       data-block, because there isn't one.  So, if
	       we're checking a single word and it's anything
	       other than a pointer, just hush it up */

	    words_scavenged=1;
	    if ((scavtab[type]==scav_lose) ||
		(((scavtab[type])(start,object))>1)) {
		fprintf(stderr,"warning: attempted to scavenge non-descriptor value %x at %p.  If you can\nreproduce this warning, send a test case to sbcl-devel@lists.sourceforge.net\n",
			object,start);
	    }
	}
#endif
	else if ((object & 3) == 0) {
	    /* It's a fixnum: really easy.. */
	    n_words_scavenged = 1;
	} else {
	    /* It's some sort of header object or another. */
	    n_words_scavenged =
		(scavtab[widetag_of(object)])(object_ptr, object);
	}
    }
    gc_assert(object_ptr == end);
}

static lispobj trans_fun_header(lispobj object); /* forward decls */
static lispobj trans_boxed(lispobj object);

static int
scav_fun_pointer(lispobj *where, lispobj object)
{
    lispobj *first_pointer;
    lispobj copy;

    gc_assert(is_lisp_pointer(object));

    /* Object is a pointer into from_space - not a FP. */
    first_pointer = (lispobj *) native_pointer(object);

    /* must transport object -- object may point to either a function
     * header, a closure function header, or to a closure header. */

    switch (widetag_of(*first_pointer)) {
    case SIMPLE_FUN_HEADER_WIDETAG:
    case CLOSURE_FUN_HEADER_WIDETAG:
	copy = trans_fun_header(object);
	break;
    default:
	copy = trans_boxed(object);
	break;
    }

    if (copy != object) {
	/* Set forwarding pointer */
	set_forwarding_pointer(first_pointer,copy);
    }

    gc_assert(is_lisp_pointer(copy));
    gc_assert(!from_space_p(copy));

    *where = copy;

    return 1;
}


static struct code *
trans_code(struct code *code)
{
    struct code *new_code;
    lispobj first, l_code, l_new_code;
    int nheader_words, ncode_words, nwords;
    unsigned long displacement;
    lispobj fheaderl, *prev_pointer;

    /* if object has already been transported, just return pointer */
    first = code->header;
    if (forwarding_pointer_p((lispobj *)code)) {
#ifdef DEBUG_CODE_GC
	printf("Was already transported\n");
#endif
	return (struct code *) forwarding_pointer_value((lispobj *)code);
    }
	
    gc_assert(widetag_of(first) == CODE_HEADER_WIDETAG);

    /* prepare to transport the code vector */
    l_code = (lispobj) LOW_WORD(code) | OTHER_POINTER_LOWTAG;

    ncode_words = fixnum_value(code->code_size);
    nheader_words = HeaderValue(code->header);
    nwords = ncode_words + nheader_words;
    nwords = CEILING(nwords, 2);

    l_new_code = copy_object(l_code, nwords);
    new_code = (struct code *) native_pointer(l_new_code);

#if defined(DEBUG_CODE_GC)
    printf("Old code object at 0x%08x, new code object at 0x%08x.\n",
	   (unsigned long) code, (unsigned long) new_code);
    printf("Code object is %d words long.\n", nwords);
#endif

#ifdef LISP_FEATURE_GENCGC
    if (new_code == code)
	return new_code;
#endif

    displacement = l_new_code - l_code;

    /* set forwarding pointer */
    set_forwarding_pointer((lispobj *)code, l_new_code);
	
    /* set forwarding pointers for all the function headers in the */
    /* code object.  also fix all self pointers */

    fheaderl = code->entry_points;
    prev_pointer = &new_code->entry_points;

    while (fheaderl != NIL) {
	struct simple_fun *fheaderp, *nfheaderp;
	lispobj nfheaderl;
		
	fheaderp = (struct simple_fun *) native_pointer(fheaderl);
	gc_assert(widetag_of(fheaderp->header) == SIMPLE_FUN_HEADER_WIDETAG);

	/* Calculate the new function pointer and the new */
	/* function header. */
	nfheaderl = fheaderl + displacement;
	nfheaderp = (struct simple_fun *) native_pointer(nfheaderl);

#ifdef DEBUG_CODE_GC
	printf("fheaderp->header (at %x) <- %x\n",
	       &(fheaderp->header) , nfheaderl);
#endif
	set_forwarding_pointer((lispobj *)fheaderp, nfheaderl);
		
	/* fix self pointer. */
	nfheaderp->self =
#ifdef LISP_FEATURE_GENCGC	/* GENCGC?  Maybe x86 is better conditional  */
	    FUN_RAW_ADDR_OFFSET +
#endif
	    nfheaderl; 
	
	*prev_pointer = nfheaderl;

	fheaderl = fheaderp->next;
	prev_pointer = &nfheaderp->next;
    }
    os_flush_icache((os_vm_address_t) (((int *)new_code) + nheader_words),
		    ncode_words * sizeof(int));
#ifdef LISP_FEATURE_GENCGC
    gencgc_apply_code_fixups(code, new_code);
#endif
    return new_code;
}

static int
scav_code_header(lispobj *where, lispobj object)
{
    struct code *code;
    int n_header_words, n_code_words, n_words;
    lispobj entry_point;	/* tagged pointer to entry point */
    struct simple_fun *function_ptr; /* untagged pointer to entry point */

    code = (struct code *) where;
    n_code_words = fixnum_value(code->code_size);
    n_header_words = HeaderValue(object);
    n_words = n_code_words + n_header_words;
    n_words = CEILING(n_words, 2);

    /* Scavenge the boxed section of the code data block. */
    scavenge(where + 1, n_header_words - 1);

    /* Scavenge the boxed section of each function object in the
     * code data block. */
    for (entry_point = code->entry_points;
	 entry_point != NIL;
	 entry_point = function_ptr->next) {

	gc_assert(is_lisp_pointer(entry_point));

	function_ptr = (struct simple_fun *) native_pointer(entry_point);
	gc_assert(widetag_of(function_ptr->header)==SIMPLE_FUN_HEADER_WIDETAG);

	scavenge(&function_ptr->name, 1);
	scavenge(&function_ptr->arglist, 1);
	scavenge(&function_ptr->type, 1);
    }
	
    return n_words;
}

static lispobj
trans_code_header(lispobj object)
{
    struct code *ncode;

    ncode = trans_code((struct code *) native_pointer(object));
    return (lispobj) LOW_WORD(ncode) | OTHER_POINTER_LOWTAG;
}


static int
size_code_header(lispobj *where)
{
    struct code *code;
    int nheader_words, ncode_words, nwords;

    code = (struct code *) where;
	
    ncode_words = fixnum_value(code->code_size);
    nheader_words = HeaderValue(code->header);
    nwords = ncode_words + nheader_words;
    nwords = CEILING(nwords, 2);

    return nwords;
}

static int
scav_return_pc_header(lispobj *where, lispobj object)
{
    lose("attempted to scavenge a return PC header where=0x%08x object=0x%08x",
	 (unsigned long) where,
	 (unsigned long) object);
    return 0; /* bogus return value to satisfy static type checking */
}

static lispobj
trans_return_pc_header(lispobj object)
{
    struct simple_fun *return_pc;
    unsigned long offset;
    struct code *code, *ncode;

    return_pc = (struct simple_fun *) native_pointer(object);
    offset = HeaderValue(return_pc->header)  * 4 ;

    /* Transport the whole code object */
    code = (struct code *) ((unsigned long) return_pc - offset);
    ncode = trans_code(code);

    return ((lispobj) LOW_WORD(ncode) + offset) | OTHER_POINTER_LOWTAG;
}

/* On the 386, closures hold a pointer to the raw address instead of the
 * function object, so we can use CALL [$FDEFN+const] to invoke
 * the function without loading it into a register. Given that code
 * objects don't move, we don't need to update anything, but we do
 * have to figure out that the function is still live. */

#ifdef LISP_FEATURE_X86
static int
scav_closure_header(lispobj *where, lispobj object)
{
    struct closure *closure;
    lispobj fun;

    closure = (struct closure *)where;
    fun = closure->fun - FUN_RAW_ADDR_OFFSET;
    scavenge(&fun, 1);
#ifdef LISP_FEATURE_GENCGC
    /* The function may have moved so update the raw address. But
     * don't write unnecessarily. */
    if (closure->fun != fun + FUN_RAW_ADDR_OFFSET)
	closure->fun = fun + FUN_RAW_ADDR_OFFSET;
#endif
    return 2;
}
#endif

static int
scav_fun_header(lispobj *where, lispobj object)
{
    lose("attempted to scavenge a function header where=0x%08x object=0x%08x",
	 (unsigned long) where,
	 (unsigned long) object);
    return 0; /* bogus return value to satisfy static type checking */
}

static lispobj
trans_fun_header(lispobj object)
{
    struct simple_fun *fheader;
    unsigned long offset;
    struct code *code, *ncode;
	
    fheader = (struct simple_fun *) native_pointer(object);
    offset = HeaderValue(fheader->header) * 4;

    /* Transport the whole code object */
    code = (struct code *) ((unsigned long) fheader - offset);
    ncode = trans_code(code);

    return ((lispobj) LOW_WORD(ncode) + offset) | FUN_POINTER_LOWTAG;
}


/*
 * instances
 */

static int
scav_instance_pointer(lispobj *where, lispobj object)
{
    lispobj copy, *first_pointer;

    /* Object is a pointer into from space - not a FP. */
    copy = trans_boxed(object);

#ifdef LISP_FEATURE_GENCGC
    gc_assert(copy != object);
#endif

    first_pointer = (lispobj *) native_pointer(object);
    set_forwarding_pointer(first_pointer,copy);
    *where = copy;

    return 1;
}


/*
 * lists and conses
 */

static lispobj trans_list(lispobj object);

static int
scav_list_pointer(lispobj *where, lispobj object)
{
    lispobj first, *first_pointer;

    gc_assert(is_lisp_pointer(object));

    /* Object is a pointer into from space - not FP. */
    first_pointer = (lispobj *) native_pointer(object);

    first = trans_list(object);
    gc_assert(first != object);

    /* Set forwarding pointer */
    set_forwarding_pointer(first_pointer, first);

    gc_assert(is_lisp_pointer(first));
    gc_assert(!from_space_p(first));

    *where = first;
    return 1;
}


static lispobj
trans_list(lispobj object)
{
    lispobj new_list_pointer;
    struct cons *cons, *new_cons;
    lispobj cdr;

    cons = (struct cons *) native_pointer(object);

    /* Copy 'object'. */
    new_cons = (struct cons *) 
	gc_general_alloc(sizeof(struct cons),ALLOC_BOXED,ALLOC_QUICK);
    new_cons->car = cons->car;
    new_cons->cdr = cons->cdr; /* updated later */
    new_list_pointer = (lispobj)new_cons | lowtag_of(object);

    /* Grab the cdr: set_forwarding_pointer will clobber it in GENCGC  */
    cdr = cons->cdr;

    set_forwarding_pointer((lispobj *)cons, new_list_pointer);

    /* Try to linearize the list in the cdr direction to help reduce
     * paging. */
    while (1) {
	lispobj  new_cdr;
	struct cons *cdr_cons, *new_cdr_cons;
	
	if(lowtag_of(cdr) != LIST_POINTER_LOWTAG ||
	   !from_space_p(cdr) ||
	   forwarding_pointer_p((lispobj *)native_pointer(cdr)))
	    break;
	
	cdr_cons = (struct cons *) native_pointer(cdr);

	/* Copy 'cdr'. */
	new_cdr_cons = (struct cons*)
	    gc_general_alloc(sizeof(struct cons),ALLOC_BOXED,ALLOC_QUICK);
	new_cdr_cons->car = cdr_cons->car;
	new_cdr_cons->cdr = cdr_cons->cdr;
	new_cdr = (lispobj)new_cdr_cons | lowtag_of(cdr);

	/* Grab the cdr before it is clobbered. */
	cdr = cdr_cons->cdr;
	set_forwarding_pointer((lispobj *)cdr_cons, new_cdr);

	/* Update the cdr of the last cons copied into new space to
	 * keep the newspace scavenge from having to do it. */
	new_cons->cdr = new_cdr;

	new_cons = new_cdr_cons;
    }

    return new_list_pointer;
}


/*
 * scavenging and transporting other pointers
 */

static int
scav_other_pointer(lispobj *where, lispobj object)
{
    lispobj first, *first_pointer;

    gc_assert(is_lisp_pointer(object));

    /* Object is a pointer into from space - not FP. */
    first_pointer = (lispobj *) native_pointer(object);
    first = (transother[widetag_of(*first_pointer)])(object);

    if (first != object) {
	set_forwarding_pointer(first_pointer, first);
#ifdef LISP_FEATURE_GENCGC
	*where = first;
#endif
    }
#ifndef LISP_FEATURE_GENCGC
    *where = first;
#endif
    gc_assert(is_lisp_pointer(first));
    gc_assert(!from_space_p(first));

    return 1;
}

/*
 * immediate, boxed, and unboxed objects
 */

static int
size_pointer(lispobj *where)
{
    return 1;
}

static int
scav_immediate(lispobj *where, lispobj object)
{
    return 1;
}

static lispobj
trans_immediate(lispobj object)
{
    lose("trying to transport an immediate");
    return NIL; /* bogus return value to satisfy static type checking */
}

static int
size_immediate(lispobj *where)
{
    return 1;
}


static int
scav_boxed(lispobj *where, lispobj object)
{
    return 1;
}

static lispobj
trans_boxed(lispobj object)
{
    lispobj header;
    unsigned long length;

    gc_assert(is_lisp_pointer(object));

    header = *((lispobj *) native_pointer(object));
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return copy_object(object, length);
}


static int
size_boxed(lispobj *where)
{
    lispobj header;
    unsigned long length;

    header = *where;
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return length;
}

/* Note: on the sparc we don't have to do anything special for fdefns, */
/* 'cause the raw-addr has a function lowtag. */
#ifndef LISP_FEATURE_SPARC
static int
scav_fdefn(lispobj *where, lispobj object)
{
    struct fdefn *fdefn;

    fdefn = (struct fdefn *)where;

    /* FSHOW((stderr, "scav_fdefn, function = %p, raw_addr = %p\n", 
       fdefn->fun, fdefn->raw_addr)); */

    if ((char *)(fdefn->fun + FUN_RAW_ADDR_OFFSET) 
	== (char *)((unsigned long)(fdefn->raw_addr))) {
	scavenge(where + 1, sizeof(struct fdefn)/sizeof(lispobj) - 1);

	/* Don't write unnecessarily. */
	if (fdefn->raw_addr != (char *)(fdefn->fun + FUN_RAW_ADDR_OFFSET))
	    fdefn->raw_addr = (char *)(fdefn->fun + FUN_RAW_ADDR_OFFSET);
	/* gc.c has more casts here, which may be relevant or alternatively
	   may be compiler warning defeaters.  try 
        fdefn->raw_addr =
	    (u32)  ((char *) LOW_WORD(fdefn->fun)) + FUN_RAW_ADDR_OFFSET;
	*/
	return sizeof(struct fdefn) / sizeof(lispobj);
    } else {
	return 1;
    }
}
#endif

static int
scav_unboxed(lispobj *where, lispobj object)
{
    unsigned long length;

    length = HeaderValue(object) + 1;
    length = CEILING(length, 2);

    return length;
}

static lispobj
trans_unboxed(lispobj object)
{
    lispobj header;
    unsigned long length;


    gc_assert(is_lisp_pointer(object));

    header = *((lispobj *) native_pointer(object));
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return copy_unboxed_object(object, length);
}

static int
size_unboxed(lispobj *where)
{
    lispobj header;
    unsigned long length;

    header = *where;
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return length;
}

static int
/* vector-like objects */

#define NWORDS(x,y) (CEILING((x),(y)) / (y))

scav_string(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    /* NOTE: Strings contain one more byte of data than the length */
    /* slot indicates. */

    vector = (struct vector *) where;
    length = fixnum_value(vector->length) + 1;
    nwords = CEILING(NWORDS(length, 4) + 2, 2);

    return nwords;
}
static lispobj
trans_string(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    /* NOTE: A string contains one more byte of data (a terminating
     * '\0' to help when interfacing with C functions) than indicated
     * by the length slot. */

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length) + 1;
    nwords = CEILING(NWORDS(length, 4) + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_string(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    /* NOTE: A string contains one more byte of data (a terminating
     * '\0' to help when interfacing with C functions) than indicated
     * by the length slot. */

    vector = (struct vector *) where;
    length = fixnum_value(vector->length) + 1;
    nwords = CEILING(NWORDS(length, 4) + 2, 2);

    return nwords;
}

static lispobj
trans_vector(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);

    length = fixnum_value(vector->length);
    nwords = CEILING(length + 2, 2);

    return copy_large_object(object, nwords);
}

static int
size_vector(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length + 2, 2);

    return nwords;
}

static int
scav_vector_bit(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 32) + 2, 2);

    return nwords;
}

static lispobj
trans_vector_bit(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 32) + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_vector_bit(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 32) + 2, 2);

    return nwords;
}

static int
scav_vector_unsigned_byte_2(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 16) + 2, 2);

    return nwords;
}

static lispobj
trans_vector_unsigned_byte_2(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 16) + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_vector_unsigned_byte_2(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 16) + 2, 2);

    return nwords;
}

static int
scav_vector_unsigned_byte_4(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 8) + 2, 2);

    return nwords;
}

static lispobj
trans_vector_unsigned_byte_4(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 8) + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}
static int
size_vector_unsigned_byte_4(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 8) + 2, 2);

    return nwords;
}


static int
scav_vector_unsigned_byte_8(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 4) + 2, 2);

    return nwords;
}

/*********************/



static lispobj
trans_vector_unsigned_byte_8(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 4) + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_vector_unsigned_byte_8(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 4) + 2, 2);

    return nwords;
}


static int
scav_vector_unsigned_byte_16(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 2) + 2, 2);

    return nwords;
}

static lispobj
trans_vector_unsigned_byte_16(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 2) + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_vector_unsigned_byte_16(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(NWORDS(length, 2) + 2, 2);

    return nwords;
}

static int
scav_vector_unsigned_byte_32(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length + 2, 2);

    return nwords;
}

static lispobj
trans_vector_unsigned_byte_32(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(length + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_vector_unsigned_byte_32(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length + 2, 2);

    return nwords;
}

static int
scav_vector_single_float(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length + 2, 2);

    return nwords;
}

static lispobj
trans_vector_single_float(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(length + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_vector_single_float(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length + 2, 2);

    return nwords;
}

static int
scav_vector_double_float(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length * 2 + 2, 2);

    return nwords;
}

static lispobj
trans_vector_double_float(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(length * 2 + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_vector_double_float(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length * 2 + 2, 2);

    return nwords;
}

#ifdef SIMPLE_ARRAY_LONG_FLOAT_WIDETAG
static int
scav_vector_long_float(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length * 
		     LONG_FLOAT_SIZE
		     + 2, 2);
    return nwords;
}

static lispobj
trans_vector_long_float(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(length * LONG_FLOAT_SIZE + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_vector_long_float(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length * LONG_FLOAT_SIZE + 2, 2);

    return nwords;
}
#endif


#ifdef SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG
static int
scav_vector_complex_single_float(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length * 2 + 2, 2);

    return nwords;
}

static lispobj
trans_vector_complex_single_float(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(length * 2 + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_vector_complex_single_float(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length * 2 + 2, 2);

    return nwords;
}
#endif

#ifdef SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG
static int
scav_vector_complex_double_float(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length * 4 + 2, 2);

    return nwords;
}

static lispobj
trans_vector_complex_double_float(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(length * 4 + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_vector_complex_double_float(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length * 4 + 2, 2);

    return nwords;
}
#endif


#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
static int
scav_vector_complex_long_float(lispobj *where, lispobj object)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length * (2* LONG_FLOAT_SIZE) + 2, 2);

    return nwords;
}

static lispobj
trans_vector_complex_long_float(lispobj object)
{
    struct vector *vector;
    int length, nwords;

    gc_assert(is_lisp_pointer(object));

    vector = (struct vector *) native_pointer(object);
    length = fixnum_value(vector->length);
    nwords = CEILING(length * (2*LONG_FLOAT_SIZE) + 2, 2);

    return copy_large_unboxed_object(object, nwords);
}

static int
size_vector_complex_long_float(lispobj *where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length * (2*LONG_FLOAT_SIZE) + 2, 2);

    return nwords;
}
#endif

#define WEAK_POINTER_NWORDS \
	CEILING((sizeof(struct weak_pointer) / sizeof(lispobj)), 2)

static lispobj
trans_weak_pointer(lispobj object)
{
    lispobj copy;
#ifndef LISP_FEATURE_GENCGC
    struct weak_pointer *wp;
#endif
    gc_assert(is_lisp_pointer(object));

#if defined(DEBUG_WEAK)
    printf("Transporting weak pointer from 0x%08x\n", object);
#endif

    /* Need to remember where all the weak pointers are that have */
    /* been transported so they can be fixed up in a post-GC pass. */

    copy = copy_object(object, WEAK_POINTER_NWORDS);
#ifndef LISP_FEATURE_GENCGC
    wp = (struct weak_pointer *) native_pointer(copy);
	

    /* Push the weak pointer onto the list of weak pointers. */
    wp->next = LOW_WORD(weak_pointers);
    weak_pointers = wp;
#endif
    return copy;
}

static int
size_weak_pointer(lispobj *where)
{
    return WEAK_POINTER_NWORDS;
}


void scan_weak_pointers(void)
{
    struct weak_pointer *wp;
    for (wp = weak_pointers; wp != NULL; wp = wp->next) {
	lispobj value = wp->value;
	lispobj *first_pointer;

	if (!(is_lisp_pointer(value) && from_space_p(value)))
	    continue;

	/* Now, we need to check whether the object has been forwarded. If
	 * it has been, the weak pointer is still good and needs to be
	 * updated. Otherwise, the weak pointer needs to be nil'ed
	 * out. */

	first_pointer = (lispobj *)native_pointer(value);
	
	if (forwarding_pointer_p(first_pointer)) {
	    wp->value=forwarding_pointer_value(first_pointer);
	} else {
	    /* Break it. */
	    wp->value = NIL;
	    wp->broken = T;
	}
    }
}



/*
 * initialization
 */

static int
scav_lose(lispobj *where, lispobj object)
{
    lose("no scavenge function for object 0x%08x (widetag 0x%x)",
	 (unsigned long)object,
	 widetag_of(*(lispobj*)native_pointer(object)));
    return 0; /* bogus return value to satisfy static type checking */
}

static lispobj
trans_lose(lispobj object)
{
    lose("no transport function for object 0x%08x (widetag 0x%x)",
	 (unsigned long)object,
	 widetag_of(*(lispobj*)native_pointer(object)));
    return NIL; /* bogus return value to satisfy static type checking */
}

static int
size_lose(lispobj *where)
{
    lose("no size function for object at 0x%08x (widetag 0x%x)",
	 (unsigned long)where,
	 widetag_of(where));
    return 1; /* bogus return value to satisfy static type checking */
}


/*
 * initialization
 */

void
gc_init_tables(void)
{
    int i;

    /* Set default value in all slots of scavenge table.  FIXME
     * replace this gnarly sizeof with something based on
     * N_WIDETAG_BITS */
    for (i = 0; i < ((sizeof scavtab)/(sizeof scavtab[0])); i++) { 
	scavtab[i] = scav_lose;
    }

    /* For each type which can be selected by the lowtag alone, set
     * multiple entries in our widetag scavenge table (one for each
     * possible value of the high bits).
     */

    for (i = 0; i < (1<<(N_WIDETAG_BITS-N_LOWTAG_BITS)); i++) {
	scavtab[EVEN_FIXNUM_LOWTAG|(i<<N_LOWTAG_BITS)] = scav_immediate;
	scavtab[FUN_POINTER_LOWTAG|(i<<N_LOWTAG_BITS)] = scav_fun_pointer;
	/* skipping OTHER_IMMEDIATE_0_LOWTAG */
	scavtab[LIST_POINTER_LOWTAG|(i<<N_LOWTAG_BITS)] = scav_list_pointer;
	scavtab[ODD_FIXNUM_LOWTAG|(i<<N_LOWTAG_BITS)] = scav_immediate;
	scavtab[INSTANCE_POINTER_LOWTAG|(i<<N_LOWTAG_BITS)] = scav_instance_pointer;
	/* skipping OTHER_IMMEDIATE_1_LOWTAG */
	scavtab[OTHER_POINTER_LOWTAG|(i<<N_LOWTAG_BITS)] = scav_other_pointer;
    }

    /* Other-pointer types (those selected by all eight bits of the
     * tag) get one entry each in the scavenge table. */
    scavtab[BIGNUM_WIDETAG] = scav_unboxed;
    scavtab[RATIO_WIDETAG] = scav_boxed;
    scavtab[SINGLE_FLOAT_WIDETAG] = scav_unboxed;
    scavtab[DOUBLE_FLOAT_WIDETAG] = scav_unboxed;
#ifdef LONG_FLOAT_WIDETAG
    scavtab[LONG_FLOAT_WIDETAG] = scav_unboxed;
#endif
    scavtab[COMPLEX_WIDETAG] = scav_boxed;
#ifdef COMPLEX_SINGLE_FLOAT_WIDETAG
    scavtab[COMPLEX_SINGLE_FLOAT_WIDETAG] = scav_unboxed;
#endif
#ifdef COMPLEX_DOUBLE_FLOAT_WIDETAG
    scavtab[COMPLEX_DOUBLE_FLOAT_WIDETAG] = scav_unboxed;
#endif
#ifdef COMPLEX_LONG_FLOAT_WIDETAG
    scavtab[COMPLEX_LONG_FLOAT_WIDETAG] = scav_unboxed;
#endif
    scavtab[SIMPLE_ARRAY_WIDETAG] = scav_boxed;
    scavtab[SIMPLE_STRING_WIDETAG] = scav_string;
    scavtab[SIMPLE_BIT_VECTOR_WIDETAG] = scav_vector_bit;
    scavtab[SIMPLE_ARRAY_UNSIGNED_BYTE_2_WIDETAG] =
	scav_vector_unsigned_byte_2;
    scavtab[SIMPLE_ARRAY_UNSIGNED_BYTE_4_WIDETAG] =
	scav_vector_unsigned_byte_4;
    scavtab[SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG] =
	scav_vector_unsigned_byte_8;
    scavtab[SIMPLE_ARRAY_UNSIGNED_BYTE_16_WIDETAG] =
	scav_vector_unsigned_byte_16;
    scavtab[SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG] =
	scav_vector_unsigned_byte_32;
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
    scavtab[SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG] = scav_vector_unsigned_byte_8;
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
    scavtab[SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG] =
	scav_vector_unsigned_byte_16;
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG
    scavtab[SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG] =
	scav_vector_unsigned_byte_32;
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
    scavtab[SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG] =
	scav_vector_unsigned_byte_32;
#endif
    scavtab[SIMPLE_ARRAY_SINGLE_FLOAT_WIDETAG] = scav_vector_single_float;
    scavtab[SIMPLE_ARRAY_DOUBLE_FLOAT_WIDETAG] = scav_vector_double_float;
#ifdef SIMPLE_ARRAY_LONG_FLOAT_WIDETAG
    scavtab[SIMPLE_ARRAY_LONG_FLOAT_WIDETAG] = scav_vector_long_float;
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG
    scavtab[SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG] =
	scav_vector_complex_single_float;
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG
    scavtab[SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG] =
	scav_vector_complex_double_float;
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
    scavtab[SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG] =
	scav_vector_complex_long_float;
#endif
    scavtab[COMPLEX_STRING_WIDETAG] = scav_boxed;
    scavtab[COMPLEX_BIT_VECTOR_WIDETAG] = scav_boxed;
    scavtab[COMPLEX_VECTOR_WIDETAG] = scav_boxed;
    scavtab[COMPLEX_ARRAY_WIDETAG] = scav_boxed;
    scavtab[CODE_HEADER_WIDETAG] = scav_code_header;
#ifndef LISP_FEATURE_GENCGC	/* FIXME ..._X86 ? */
    scavtab[SIMPLE_FUN_HEADER_WIDETAG] = scav_fun_header;
    scavtab[CLOSURE_FUN_HEADER_WIDETAG] = scav_fun_header;
    scavtab[RETURN_PC_HEADER_WIDETAG] = scav_return_pc_header;
#endif
#ifdef LISP_FEATURE_X86
    scavtab[CLOSURE_HEADER_WIDETAG] = scav_closure_header;
    scavtab[FUNCALLABLE_INSTANCE_HEADER_WIDETAG] = scav_closure_header;
#else
    scavtab[CLOSURE_HEADER_WIDETAG] = scav_boxed;
    scavtab[FUNCALLABLE_INSTANCE_HEADER_WIDETAG] = scav_boxed;
#endif
    scavtab[VALUE_CELL_HEADER_WIDETAG] = scav_boxed;
    scavtab[SYMBOL_HEADER_WIDETAG] = scav_boxed;
    scavtab[BASE_CHAR_WIDETAG] = scav_immediate;
    scavtab[SAP_WIDETAG] = scav_unboxed;
    scavtab[UNBOUND_MARKER_WIDETAG] = scav_immediate;
    scavtab[INSTANCE_HEADER_WIDETAG] = scav_boxed;
#ifdef LISP_FEATURE_SPARC
    scavtab[FDEFN_WIDETAG] = scav_boxed;
#else
    scavtab[FDEFN_WIDETAG] = scav_fdefn;
#endif

    /* transport other table, initialized same way as scavtab */
    for (i = 0; i < ((sizeof transother)/(sizeof transother[0])); i++)
	transother[i] = trans_lose;
    transother[BIGNUM_WIDETAG] = trans_unboxed;
    transother[RATIO_WIDETAG] = trans_boxed;
    transother[SINGLE_FLOAT_WIDETAG] = trans_unboxed;
    transother[DOUBLE_FLOAT_WIDETAG] = trans_unboxed;
#ifdef LONG_FLOAT_WIDETAG
    transother[LONG_FLOAT_WIDETAG] = trans_unboxed;
#endif
    transother[COMPLEX_WIDETAG] = trans_boxed;
#ifdef COMPLEX_SINGLE_FLOAT_WIDETAG
    transother[COMPLEX_SINGLE_FLOAT_WIDETAG] = trans_unboxed;
#endif
#ifdef COMPLEX_DOUBLE_FLOAT_WIDETAG
    transother[COMPLEX_DOUBLE_FLOAT_WIDETAG] = trans_unboxed;
#endif
#ifdef COMPLEX_LONG_FLOAT_WIDETAG
    transother[COMPLEX_LONG_FLOAT_WIDETAG] = trans_unboxed;
#endif
    transother[SIMPLE_ARRAY_WIDETAG] = trans_boxed; /* but not GENCGC */
    transother[SIMPLE_STRING_WIDETAG] = trans_string;
    transother[SIMPLE_BIT_VECTOR_WIDETAG] = trans_vector_bit;
    transother[SIMPLE_VECTOR_WIDETAG] = trans_vector;
    transother[SIMPLE_ARRAY_UNSIGNED_BYTE_2_WIDETAG] =
	trans_vector_unsigned_byte_2;
    transother[SIMPLE_ARRAY_UNSIGNED_BYTE_4_WIDETAG] =
	trans_vector_unsigned_byte_4;
    transother[SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG] =
	trans_vector_unsigned_byte_8;
    transother[SIMPLE_ARRAY_UNSIGNED_BYTE_16_WIDETAG] =
	trans_vector_unsigned_byte_16;
    transother[SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG] =
	trans_vector_unsigned_byte_32;
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
    transother[SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG] =
	trans_vector_unsigned_byte_8;
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
    transother[SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG] =
	trans_vector_unsigned_byte_16;
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG
    transother[SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG] =
	trans_vector_unsigned_byte_32;
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
    transother[SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG] =
	trans_vector_unsigned_byte_32;
#endif
    transother[SIMPLE_ARRAY_SINGLE_FLOAT_WIDETAG] =
	trans_vector_single_float;
    transother[SIMPLE_ARRAY_DOUBLE_FLOAT_WIDETAG] =
	trans_vector_double_float;
#ifdef SIMPLE_ARRAY_LONG_FLOAT_WIDETAG
    transother[SIMPLE_ARRAY_LONG_FLOAT_WIDETAG] =
	trans_vector_long_float;
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG
    transother[SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG] =
	trans_vector_complex_single_float;
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG
    transother[SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG] =
	trans_vector_complex_double_float;
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
    transother[SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG] =
	trans_vector_complex_long_float;
#endif
    transother[COMPLEX_STRING_WIDETAG] = trans_boxed;
    transother[COMPLEX_BIT_VECTOR_WIDETAG] = trans_boxed;
    transother[COMPLEX_VECTOR_WIDETAG] = trans_boxed;
    transother[COMPLEX_ARRAY_WIDETAG] = trans_boxed;
    transother[CODE_HEADER_WIDETAG] = trans_code_header;
    transother[SIMPLE_FUN_HEADER_WIDETAG] = trans_fun_header;
    transother[CLOSURE_FUN_HEADER_WIDETAG] = trans_fun_header;
    transother[RETURN_PC_HEADER_WIDETAG] = trans_return_pc_header;
    transother[CLOSURE_HEADER_WIDETAG] = trans_boxed;
    transother[FUNCALLABLE_INSTANCE_HEADER_WIDETAG] = trans_boxed;
    transother[VALUE_CELL_HEADER_WIDETAG] = trans_boxed;
    transother[SYMBOL_HEADER_WIDETAG] = trans_boxed;
    transother[BASE_CHAR_WIDETAG] = trans_immediate;
    transother[SAP_WIDETAG] = trans_unboxed;
    transother[UNBOUND_MARKER_WIDETAG] = trans_immediate;
    transother[WEAK_POINTER_WIDETAG] = trans_weak_pointer;
    transother[INSTANCE_HEADER_WIDETAG] = trans_boxed;
    transother[FDEFN_WIDETAG] = trans_boxed;

    /* size table, initialized the same way as scavtab */
    for (i = 0; i < ((sizeof sizetab)/(sizeof sizetab[0])); i++)
	sizetab[i] = size_lose;
    for (i = 0; i < (1<<(N_WIDETAG_BITS-N_LOWTAG_BITS)); i++) {
	sizetab[EVEN_FIXNUM_LOWTAG|(i<<3)] = size_immediate;
	sizetab[FUN_POINTER_LOWTAG|(i<<3)] = size_pointer;
	/* skipping OTHER_IMMEDIATE_0_LOWTAG */
	sizetab[LIST_POINTER_LOWTAG|(i<<3)] = size_pointer;
	sizetab[ODD_FIXNUM_LOWTAG|(i<<3)] = size_immediate;
	sizetab[INSTANCE_POINTER_LOWTAG|(i<<3)] = size_pointer;
	/* skipping OTHER_IMMEDIATE_1_LOWTAG */
	sizetab[OTHER_POINTER_LOWTAG|(i<<3)] = size_pointer;
    }
    sizetab[BIGNUM_WIDETAG] = size_unboxed;
    sizetab[RATIO_WIDETAG] = size_boxed;
    sizetab[SINGLE_FLOAT_WIDETAG] = size_unboxed;
    sizetab[DOUBLE_FLOAT_WIDETAG] = size_unboxed;
#ifdef LONG_FLOAT_WIDETAG
    sizetab[LONG_FLOAT_WIDETAG] = size_unboxed;
#endif
    sizetab[COMPLEX_WIDETAG] = size_boxed;
#ifdef COMPLEX_SINGLE_FLOAT_WIDETAG
    sizetab[COMPLEX_SINGLE_FLOAT_WIDETAG] = size_unboxed;
#endif
#ifdef COMPLEX_DOUBLE_FLOAT_WIDETAG
    sizetab[COMPLEX_DOUBLE_FLOAT_WIDETAG] = size_unboxed;
#endif
#ifdef COMPLEX_LONG_FLOAT_WIDETAG
    sizetab[COMPLEX_LONG_FLOAT_WIDETAG] = size_unboxed;
#endif
    sizetab[SIMPLE_ARRAY_WIDETAG] = size_boxed;
    sizetab[SIMPLE_STRING_WIDETAG] = size_string;
    sizetab[SIMPLE_BIT_VECTOR_WIDETAG] = size_vector_bit;
    sizetab[SIMPLE_VECTOR_WIDETAG] = size_vector;
    sizetab[SIMPLE_ARRAY_UNSIGNED_BYTE_2_WIDETAG] =
	size_vector_unsigned_byte_2;
    sizetab[SIMPLE_ARRAY_UNSIGNED_BYTE_4_WIDETAG] =
	size_vector_unsigned_byte_4;
    sizetab[SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG] =
	size_vector_unsigned_byte_8;
    sizetab[SIMPLE_ARRAY_UNSIGNED_BYTE_16_WIDETAG] =
	size_vector_unsigned_byte_16;
    sizetab[SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG] =
	size_vector_unsigned_byte_32;
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
    sizetab[SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG] = size_vector_unsigned_byte_8;
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
    sizetab[SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG] =
	size_vector_unsigned_byte_16;
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG
    sizetab[SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG] =
	size_vector_unsigned_byte_32;
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
    sizetab[SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG] =
	size_vector_unsigned_byte_32;
#endif
    sizetab[SIMPLE_ARRAY_SINGLE_FLOAT_WIDETAG] = size_vector_single_float;
    sizetab[SIMPLE_ARRAY_DOUBLE_FLOAT_WIDETAG] = size_vector_double_float;
#ifdef SIMPLE_ARRAY_LONG_FLOAT_WIDETAG
    sizetab[SIMPLE_ARRAY_LONG_FLOAT_WIDETAG] = size_vector_long_float;
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG
    sizetab[SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG] =
	size_vector_complex_single_float;
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG
    sizetab[SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG] =
	size_vector_complex_double_float;
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
    sizetab[SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG] =
	size_vector_complex_long_float;
#endif
    sizetab[COMPLEX_STRING_WIDETAG] = size_boxed;
    sizetab[COMPLEX_BIT_VECTOR_WIDETAG] = size_boxed;
    sizetab[COMPLEX_VECTOR_WIDETAG] = size_boxed;
    sizetab[COMPLEX_ARRAY_WIDETAG] = size_boxed;
    sizetab[CODE_HEADER_WIDETAG] = size_code_header;
#if 0
    /* We shouldn't see these, so just lose if it happens. */
    sizetab[SIMPLE_FUN_HEADER_WIDETAG] = size_function_header;
    sizetab[CLOSURE_FUN_HEADER_WIDETAG] = size_function_header;
    sizetab[RETURN_PC_HEADER_WIDETAG] = size_return_pc_header;
#endif
    sizetab[CLOSURE_HEADER_WIDETAG] = size_boxed;
    sizetab[FUNCALLABLE_INSTANCE_HEADER_WIDETAG] = size_boxed;
    sizetab[VALUE_CELL_HEADER_WIDETAG] = size_boxed;
    sizetab[SYMBOL_HEADER_WIDETAG] = size_boxed;
    sizetab[BASE_CHAR_WIDETAG] = size_immediate;
    sizetab[SAP_WIDETAG] = size_unboxed;
    sizetab[UNBOUND_MARKER_WIDETAG] = size_immediate;
    sizetab[WEAK_POINTER_WIDETAG] = size_weak_pointer;
    sizetab[INSTANCE_HEADER_WIDETAG] = size_boxed;
    sizetab[FDEFN_WIDETAG] = size_boxed;
}
