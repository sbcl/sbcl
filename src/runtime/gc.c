/*
 * stop and copy GC based on Cheney's algorithm
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
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include "runtime.h"
#include "sbcl.h"
#include "os.h"
#include "gc.h"
#include "globals.h"
#include "interrupt.h"
#include "validate.h"
#include "lispregs.h"
#include "interr.h"

/* So you need to debug? */
#if 0
#define PRINTNOISE
#define DEBUG_SPACE_PREDICATES
#define DEBUG_SCAVENGE_VERBOSE
#define DEBUG_COPY_VERBOSE
#define DEBUG_CODE_GC
#endif

static lispobj *from_space;
static lispobj *from_space_free_pointer;

static lispobj *new_space;
static lispobj *new_space_free_pointer;

static int (*scavtab[256])(lispobj *where, lispobj object);
static lispobj (*transother[256])(lispobj object);
static int (*sizetab[256])(lispobj *where);

static struct weak_pointer *weak_pointers;

static void scavenge(lispobj *start, u32 nwords);
static void scavenge_newspace(void);
static void scavenge_interrupt_contexts(void);
static void scan_weak_pointers(void);
static int scav_lose(lispobj *where, lispobj object);

#define gc_abort() lose("GC invariant lost!  File \"%s\", line %d\n", \
			__FILE__, __LINE__)

#if 1
#define gc_assert(ex) do { \
	if (!(ex)) gc_abort(); \
} while (0)
#else
#define gc_assert(ex)
#endif

#define CEILING(x,y) (((x) + ((y) - 1)) & (~((y) - 1)))


/* predicates */

#if defined(DEBUG_SPACE_PREDICATES)

boolean
from_space_p(lispobj object)
{
	lispobj *ptr;

	/* this can be called for untagged pointers as well as for 
	   descriptors, so this assertion's not applicable
	   gc_assert(Pointerp(object));
	*/
	ptr = (lispobj *) PTR(object);

	return ((from_space <= ptr) &&
		(ptr < from_space_free_pointer));
}	    

boolean
new_space_p(lispobj object)
{
	lispobj *ptr;

	gc_assert(Pointerp(object));

	ptr = (lispobj *) PTR(object);
		
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


/* copying objects */

static lispobj
copy_object(lispobj object, int nwords)
{
	int tag;
	lispobj *new;
	lispobj *source, *dest;

	gc_assert(Pointerp(object));
	gc_assert(from_space_p(object));
	gc_assert((nwords & 0x01) == 0);

	/* get tag of object */
	tag = LowtagOf(object);

	/* allocate space */
	new = new_space_free_pointer;
	new_space_free_pointer += nwords;

	dest = new;
	source = (lispobj *) PTR(object);

#ifdef DEBUG_COPY_VERBOSE
	fprintf(stderr,"Copying %d words from %p to %p\n", nwords,source,new);
#endif

	/* copy the object */
	while (nwords > 0) {
            dest[0] = source[0];
            dest[1] = source[1];
            dest += 2;
            source += 2;
            nwords -= 2;
	}
	/* return lisp pointer of new object */
	return (lispobj)(LOW_WORD(new) | tag);
}


/* collecting garbage */

#ifdef PRINTNOISE
static double
tv_diff(struct timeval *x, struct timeval *y)
{
    return (((double) x->tv_sec + (double) x->tv_usec * 1.0e-6) -
	    ((double) y->tv_sec + (double) y->tv_usec * 1.0e-6));
}
#endif

#define BYTES_ZERO_BEFORE_END (1<<12)

#ifdef alpha
#define U32 u32
#else
#define U32 unsigned long
#endif
static void
zero_stack(void)
{
    U32 *ptr = (U32 *)current_control_stack_pointer;
 search:
    do {
	if (*ptr)
	    goto fill;
	ptr++;
    } while (((unsigned long)ptr) & (BYTES_ZERO_BEFORE_END-1));
    return;
 fill:
    do {
	*ptr++ = 0;
    } while (((unsigned long)ptr) & (BYTES_ZERO_BEFORE_END-1));

    goto search;
}
#undef U32


/* Note: The generic GC interface we're implementing passes us a
 * last_generation argument. That's meaningless for us, since we're
 * not a generational GC. So we ignore it. */
void
collect_garbage(unsigned ignore)
{
#ifdef PRINTNOISE
struct timeval start_tv, stop_tv;
	struct rusage start_rusage, stop_rusage;
	double real_time, system_time, user_time;
	double percent_retained, gc_rate;
	unsigned long size_discarded;
	unsigned long size_retained;
#endif
	lispobj *current_static_space_free_pointer;
	unsigned long static_space_size; 
	unsigned long control_stack_size, binding_stack_size; 
	sigset_t tmp, old;

#ifdef PRINTNOISE
	printf("[Collecting garbage ... \n");
	
	getrusage(RUSAGE_SELF, &start_rusage);
	gettimeofday(&start_tv, (struct timezone *) 0);
#endif
	
	sigemptyset(&tmp);
	sigaddset_blockable(&tmp);
	sigprocmask(SIG_BLOCK, &tmp, &old);

	current_static_space_free_pointer =
	    (lispobj *) ((unsigned long)
			 SymbolValue(STATIC_SPACE_FREE_POINTER));


	/* Set up from space and new space pointers. */

	from_space = current_dynamic_space;
#ifndef ibmrt
	from_space_free_pointer = dynamic_space_free_pointer;
#else
	from_space_free_pointer = (lispobj *)SymbolValue(ALLOCATION_POINTER);
#endif

#ifdef PRINTNOISE
	fprintf(stderr,"from_space = %lx\n",
		(unsigned long) current_dynamic_space);
#endif
	if (current_dynamic_space == (lispobj *) DYNAMIC_0_SPACE_START)
	    new_space = (lispobj *)DYNAMIC_1_SPACE_START;
	else if (current_dynamic_space == (lispobj *) DYNAMIC_1_SPACE_START)
	    new_space = (lispobj *) DYNAMIC_0_SPACE_START;
	else {
	    lose("GC lossage.  Current dynamic space is bogus!\n");
	}
	new_space_free_pointer = new_space;


	/* Initialize the weak pointer list. */
	weak_pointers = (struct weak_pointer *) NULL;


	/* Scavenge all of the roots. */
#ifdef PRINTNOISE
	printf("Scavenging interrupt contexts ...\n");
#endif
	scavenge_interrupt_contexts();

#ifdef PRINTNOISE
	printf("Scavenging interrupt handlers (%d bytes) ...\n",
	       (int)sizeof(interrupt_handlers));
#endif
	scavenge((lispobj *) interrupt_handlers,
		 sizeof(interrupt_handlers) / sizeof(lispobj));
	
	/* _size quantities are in units of sizeof(lispobj) - i.e. 4 */
	control_stack_size = 
	    current_control_stack_pointer-
	    (lispobj *)CONTROL_STACK_START;
#ifdef PRINTNOISE
	printf("Scavenging the control stack at %p (%ld words) ...\n",
	       ((lispobj *)CONTROL_STACK_START), 
	       control_stack_size);
#endif
	scavenge(((lispobj *)CONTROL_STACK_START), control_stack_size);
		 

#ifdef ibmrt
	binding_stack_size =
	    (lispobj *)SymbolValue(BINDING_STACK_POINTER) - binding_stack;
#else
	binding_stack_size = 
	  current_binding_stack_pointer - 
	    (lispobj *)BINDING_STACK_START;
#endif
#ifdef PRINTNOISE
	printf("Scavenging the binding stack %x - %x (%d words) ...\n",
	       BINDING_STACK_START,current_binding_stack_pointer,
	       (int)(binding_stack_size));
#endif
	scavenge(((lispobj *)BINDING_STACK_START), binding_stack_size);
		 
	static_space_size = 
	    current_static_space_free_pointer - (lispobj *) STATIC_SPACE_START;
#ifdef PRINTNOISE
	printf("Scavenging static space %x - %x (%d words) ...\n",
	       STATIC_SPACE_START,current_static_space_free_pointer,
	       (int)(static_space_size));
#endif
	scavenge(((lispobj *)STATIC_SPACE_START), static_space_size);

	/* Scavenge newspace. */
#ifdef PRINTNOISE
	printf("Scavenging new space (%d bytes) ...\n",
	       (int)((new_space_free_pointer - new_space) * sizeof(lispobj)));
#endif
	scavenge_newspace();


#if defined(DEBUG_PRINT_GARBAGE)
	print_garbage(from_space, from_space_free_pointer);
#endif

	/* Scan the weak pointers. */
#ifdef PRINTNOISE
	printf("Scanning weak pointers ...\n");
#endif
	scan_weak_pointers();


	/* Flip spaces. */
#ifdef PRINTNOISE
	printf("Flipping spaces ...\n");
#endif

	os_zero((os_vm_address_t) current_dynamic_space,
		(os_vm_size_t) DYNAMIC_SPACE_SIZE);

	current_dynamic_space = new_space;
#ifndef ibmrt
	dynamic_space_free_pointer = new_space_free_pointer;
#else
	SetSymbolValue(ALLOCATION_POINTER, (lispobj)new_space_free_pointer);
#endif

#ifdef PRINTNOISE
	size_discarded = (from_space_free_pointer - from_space) * sizeof(lispobj);
	size_retained = (new_space_free_pointer - new_space) * sizeof(lispobj);
#endif

	/* Zero stack. */
#ifdef PRINTNOISE
	printf("Zeroing empty part of control stack ...\n");
#endif
	zero_stack();

	sigprocmask(SIG_SETMASK, &old, 0);


#ifdef PRINTNOISE
	gettimeofday(&stop_tv, (struct timezone *) 0);
	getrusage(RUSAGE_SELF, &stop_rusage);

	printf("done.]\n");
	
	percent_retained = (((float) size_retained) /
			     ((float) size_discarded)) * 100.0;

	printf("Total of %ld bytes out of %ld bytes retained (%3.2f%%).\n",
	       size_retained, size_discarded, percent_retained);

	real_time = tv_diff(&stop_tv, &start_tv);
	user_time = tv_diff(&stop_rusage.ru_utime, &start_rusage.ru_utime);
	system_time = tv_diff(&stop_rusage.ru_stime, &start_rusage.ru_stime);

#if 0
	printf("Statistics:\n");
	printf("%10.2f sec of real time\n", real_time);
	printf("%10.2f sec of user time,\n", user_time);
	printf("%10.2f sec of system time.\n", system_time);
#else
        printf("Statistics: %10.2fs real, %10.2fs user, %10.2fs system.\n",
               real_time, user_time, system_time);
#endif        

	gc_rate = ((float) size_retained / (float) (1<<20)) / real_time;

	printf("%10.2f M bytes/sec collected.\n", gc_rate);
#endif
}


/* scavenging */

#define DIRECT_SCAV 0

static void
scavenge(lispobj *start, u32 nwords)
{
	while (nwords > 0) {
		lispobj object;
		int type, words_scavenged;

		object = *start;
		type = TypeOf(object);

#if defined(DEBUG_SCAVENGE_VERBOSE)
		fprintf(stderr,"Scavenging object at 0x%08x, object = 0x%08x, type = %d\n",
		       (unsigned long) start, (unsigned long) object, type);
#endif

#if DIRECT_SCAV
		words_scavenged = (scavtab[type])(start, object);
#else
                if (Pointerp(object)) {
		    /* It be a pointer. */
		    if (from_space_p(object)) {
                        /* It currently points to old space.  Check for a */
                        /* forwarding pointer. */
                        lispobj first_word;

                        first_word = *((lispobj *)PTR(object));
                        if (Pointerp(first_word) && new_space_p(first_word)) {
                            /* Yep, there be a forwarding pointer. */
                            *start = first_word;
                            words_scavenged = 1;
                        }
                        else {
                            /* Scavenge that pointer. */
                            words_scavenged = (scavtab[type])(start, object);
                        }
                    }
                    else {
                        /* It points somewhere other than oldspace.  Leave */
                        /* it alone. */
                        words_scavenged = 1;
                    }
                }
		else if(nwords==1) {
		    /* there are some situations where an
		       other-immediate may end up in a descriptor
		       register.  I'm not sure whether this is
		       supposed to happen, but if it does then we
		       don't want to (a) barf or (b) scavenge over the
		       data-block, because there isn't one.  So, if
		       we're checking a single word and it's anything
		       other than a pointer, just hush it up */

		    words_scavenged=1;
		    if((scavtab[type]==scav_lose) ||
		       (((scavtab[type])(start,object))>1)) {
			fprintf(stderr,"warning: attempted to scavenge non-descriptor value %x at %p.  If you can\nreproduce this warning, send a test case to sbcl-devel@lists.sourceforge.net\n",
				object,start);
		    }
		}
                else if ((object & 3) == 0) {
                    /* It's a fixnum.  Real easy. */
                    words_scavenged = 1;
                }
                else {
                    /* It's some random header object. */
                    words_scavenged = (scavtab[type])(start, object);

                }
#endif
		start += words_scavenged;
		nwords -= words_scavenged;
	}
	gc_assert(nwords == 0);
}

static void
scavenge_newspace(void)
{
    lispobj *here, *next;

    here = new_space;
    while (here < new_space_free_pointer) {
	/*	printf("here=%lx, new_space_free_pointer=%lx\n",
		here,new_space_free_pointer); */
	next = new_space_free_pointer;
	scavenge(here, next - here);
	here = next;
    }
    /* printf("done with newspace\n"); */
}

/* scavenging interrupt contexts */

static int boxed_registers[] = BOXED_REGISTERS;

static void
scavenge_interrupt_context(os_context_t *context)
{
	int i;
#ifdef reg_LIP
	unsigned long lip;
	unsigned long lip_offset;
	int lip_register_pair;
#endif
	unsigned long pc_code_offset;
#ifdef SC_NPC
	unsigned long npc_code_offset;
#endif

	/* Find the LIP's register pair and calculate its offset */
	/* before we scavenge the context. */
#ifdef reg_LIP
	lip = *os_context_register_addr(context, reg_LIP);
	/*  0x7FFFFFFF or 0x7FFFFFFFFFFFFFFF ? */
	lip_offset = 0x7FFFFFFF;
	lip_register_pair = -1;
	for (i = 0; i < (sizeof(boxed_registers) / sizeof(int)); i++) {
		unsigned long reg;
		long offset;
		int index;

		index = boxed_registers[i];
		reg = *os_context_register_addr(context, index);
		/* would be using PTR if not for integer length issues */
		if ((reg & ~((1L<<lowtag_Bits)-1)) <= lip) {
			offset = lip - reg;
			if (offset < lip_offset) {
				lip_offset = offset;
				lip_register_pair = index;
			}
		}
	}
#endif reg_LIP

	/* Compute the PC's offset from the start of the CODE */
	/* register. */
	pc_code_offset = *os_context_pc_addr(context) - 
	    *os_context_register_addr(context, reg_CODE);
#ifdef SC_NPC
	npc_code_offset = SC_NPC(context) - SC_REG(context, reg_CODE);
#endif SC_NPC
	       
	/* Scanvenge all boxed registers in the context. */
	for (i = 0; i < (sizeof(boxed_registers) / sizeof(int)); i++) {
		int index;
	        lispobj foo;
		
		index = boxed_registers[i];
                foo = *os_context_register_addr(context,index);
                scavenge((lispobj *) &foo, 1);
                *os_context_register_addr(context,index) = foo;

		/* this is unlikely to work as intended on bigendian
		 * 64 bit platforms */

		scavenge((lispobj *)
			 os_context_register_addr(context, index), 1);
	}

#ifdef reg_LIP
	/* Fix the LIP */
	*os_context_register_addr(context, reg_LIP) =
	    *os_context_register_addr(context, lip_register_pair) + lip_offset;
#endif reg_LIP
	
	/* Fix the PC if it was in from space */
	if (from_space_p(*os_context_pc_addr(context)))
	    *os_context_pc_addr(context) = 
		*os_context_register_addr(context, reg_CODE) + pc_code_offset;
#ifdef SC_NPC
	if (from_space_p(SC_NPC(context)))
		SC_NPC(context) = SC_REG(context, reg_CODE) + npc_code_offset;
#endif SC_NPC
}

void scavenge_interrupt_contexts(void)
{
    int i, index;
    os_context_t *context;

    index = fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX));

    for (i = 0; i < index; i++) {
	context = lisp_interrupt_contexts[i];
	scavenge_interrupt_context(context); 
    }
}


/* debugging code */

void
print_garbage(lispobj *from_space, lispobj *from_space_free_pointer)
{
	lispobj *start;
	int total_words_not_copied;

	printf("Scanning from space ...\n");

	total_words_not_copied = 0;
	start = from_space;
	while (start < from_space_free_pointer) {
		lispobj object;
		int forwardp, type, nwords;
		lispobj header;

		object = *start;
		forwardp = Pointerp(object) && new_space_p(object);

		if (forwardp) {
			int tag;
			lispobj *pointer;

			tag = LowtagOf(object);

			switch (tag) {
			case type_ListPointer:
				nwords = 2;
				break;
			case type_InstancePointer:
				printf("Don't know about instances yet!\n");
				nwords = 1;
				break;
			case type_FunctionPointer:
				nwords = 1;
				break;
			case type_OtherPointer:
				pointer = (lispobj *) PTR(object);
				header = *pointer;
				type = TypeOf(header);
				nwords = (sizetab[type])(pointer);
			}
		} else {
			type = TypeOf(object);
			nwords = (sizetab[type])(start);
			total_words_not_copied += nwords;
			printf("%4d words not copied at 0x%16lx; ",
			       nwords, (unsigned long) start);
			printf("Header word is 0x%08x\n", 
			       (unsigned int) object);
		}
		start += nwords;
	}
	printf("%d total words not copied.\n", total_words_not_copied);
}


/* code and code-related objects */

#define RAW_ADDR_OFFSET (6*sizeof(lispobj) - type_FunctionPointer)

static lispobj trans_function_header(lispobj object);
static lispobj trans_boxed(lispobj object);

#if DIRECT_SCAV
static int
scav_function_pointer(lispobj *where, lispobj object)
{
	gc_assert(Pointerp(object));

	if (from_space_p(object)) {
		lispobj first, *first_pointer;

		/* object is a pointer into from space.  check to see */
		/* if it has been forwarded */
		first_pointer = (lispobj *) PTR(object);
		first = *first_pointer;
		
		if (!(Pointerp(first) && new_space_p(first))) {
			int type;
			lispobj copy;

			/* must transport object -- object may point */
			/* to either a function header, a closure */
			/* function header, or to a closure header. */
			
			type = TypeOf(first);
			switch (type) {
			  case type_FunctionHeader:
			  case type_ClosureFunctionHeader:
			    copy = trans_function_header(object);
			    break;
			  default:
			    copy = trans_boxed(object);
			    break;
			}

			first = *first_pointer = copy;
		}

		gc_assert(Pointerp(first));
		gc_assert(!from_space_p(first));

		*where = first;
	}
	return 1;
}
#else
static int
scav_function_pointer(lispobj *where, lispobj object)
{
  lispobj  *first_pointer;
  lispobj copy;
  lispobj first;
  int type;

  gc_assert(Pointerp(object));
      
  /* object is a pointer into from space. Not a FP */
  first_pointer = (lispobj *) PTR(object);
  first = *first_pointer;
		
  /* must transport object -- object may point */
  /* to either a function header, a closure */
  /* function header, or to a closure header. */
  
  type = TypeOf(first);
  switch (type) {
  case type_FunctionHeader:
  case type_ClosureFunctionHeader:
    copy = trans_function_header(object);
    break;
  default:
    copy = trans_boxed(object);
    break;
  }
  
  first = *first_pointer = copy;

  gc_assert(Pointerp(first));
  gc_assert(!from_space_p(first));

  *where = first;
  return 1;
}
#endif

static struct code *
trans_code(struct code *code)
{
	struct code *new_code;
	lispobj first, l_code, l_new_code;
	int nheader_words, ncode_words, nwords;
	unsigned long displacement;
	lispobj fheaderl, *prev_pointer;

#if defined(DEBUG_CODE_GC)
	printf("\nTransporting code object located at 0x%08x.\n",
	       (unsigned long) code);
#endif

	/* if object has already been transported, just return pointer */
	first = code->header;
	if (Pointerp(first) && new_space_p(first)) {
#ifdef DEBUG_CODE_GC
	    printf("Was already transported\n");
#endif
	    return (struct code *) PTR(first);
	}
	
	gc_assert(TypeOf(first) == type_CodeHeader);

	/* prepare to transport the code vector */
	l_code = (lispobj) LOW_WORD(code) | type_OtherPointer;

	ncode_words = fixnum_value(code->code_size);
	nheader_words = HeaderValue(code->header);
	nwords = ncode_words + nheader_words;
	nwords = CEILING(nwords, 2);

	l_new_code = copy_object(l_code, nwords);
	new_code = (struct code *) PTR(l_new_code);

	displacement = l_new_code - l_code;

#if defined(DEBUG_CODE_GC)
	printf("Old code object at 0x%08x, new code object at 0x%08x.\n",
	       (unsigned long) code, (unsigned long) new_code);
	printf("Code object is %d words long.\n", nwords);
#endif

	/* set forwarding pointer */
	code->header = l_new_code;
	
	/* set forwarding pointers for all the function headers in the */
	/* code object.  also fix all self pointers */

	fheaderl = code->entry_points;
	prev_pointer = &new_code->entry_points;

	while (fheaderl != NIL) {
		struct function *fheaderp, *nfheaderp;
		lispobj nfheaderl;
		
		fheaderp = (struct function *) PTR(fheaderl);
		gc_assert(TypeOf(fheaderp->header) == type_FunctionHeader);

		/* calcuate the new function pointer and the new */
		/* function header */
		nfheaderl = fheaderl + displacement;
		nfheaderp = (struct function *) PTR(nfheaderl);

		/* set forwarding pointer */
#ifdef DEBUG_CODE_GC
		printf("fheaderp->header (at %x) <- %x\n",
		       &(fheaderp->header) , nfheaderl);
#endif
		fheaderp->header = nfheaderl;
		
		/* fix self pointer */
		nfheaderp->self = nfheaderl;

		*prev_pointer = nfheaderl;

		fheaderl = fheaderp->next;
		prev_pointer = &nfheaderp->next;
	}

#ifndef MACH
        os_flush_icache((os_vm_address_t) (((int *)new_code) + nheader_words),
		        ncode_words * sizeof(int));
#endif
	return new_code;
}

static int
scav_code_header(lispobj *where, lispobj object)
{
	struct code *code;
	int nheader_words, ncode_words, nwords;
	lispobj fheaderl;
	struct function *fheaderp;

	code = (struct code *) where;
	ncode_words = fixnum_value(code->code_size);
	nheader_words = HeaderValue(object);
	nwords = ncode_words + nheader_words;
	nwords = CEILING(nwords, 2);

#if defined(DEBUG_CODE_GC)
	printf("\nScavening code object at 0x%08x.\n",
	       (unsigned long) where);
	printf("Code object is %d words long.\n", nwords);
	printf("Scavenging boxed section of code data block (%d words).\n",
	       nheader_words - 1);
#endif

	/* Scavenge the boxed section of the code data block */
	scavenge(where + 1, nheader_words - 1);

	/* Scavenge the boxed section of each function object in the */
	/* code data block */
	fheaderl = code->entry_points;
	while (fheaderl != NIL) {
		fheaderp = (struct function *) PTR(fheaderl);
		gc_assert(TypeOf(fheaderp->header) == type_FunctionHeader);
		
#if defined(DEBUG_CODE_GC)
		printf("Scavenging boxed section of entry point located at 0x%08x.\n",
		       (unsigned long) PTR(fheaderl));
#endif
		scavenge(&fheaderp->name, 1);
		scavenge(&fheaderp->arglist, 1);
		scavenge(&fheaderp->type, 1);
		
		fheaderl = fheaderp->next;
	}
	
	return nwords;
}

static lispobj
trans_code_header(lispobj object)
{
	struct code *ncode;

	ncode = trans_code((struct code *) PTR(object));
	return (lispobj) LOW_WORD(ncode) | type_OtherPointer;
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
    fprintf(stderr, "GC lossage.  Should not be scavenging a ");
    fprintf(stderr, "Return PC Header.\n");
    fprintf(stderr, "where = 0x%p, object = 0x%x", where, object);
    lose(NULL);
    return 0;
}

static lispobj
trans_return_pc_header(lispobj object)
{
	struct function *return_pc;
	unsigned long offset;
	struct code *code, *ncode;
	lispobj ret;
	return_pc = (struct function *) PTR(object);
	offset = HeaderValue(return_pc->header)  * 4 ;

	/* Transport the whole code object */
	code = (struct code *) ((unsigned long) return_pc - offset);
#ifdef DEBUG_CODE_GC
	printf("trans_return_pc_header object=%x, code=%lx\n",object,code);
#endif
	ncode = trans_code(code);
	if(object==0x304748d7) {
	    /* ldb_monitor(); */
	}
	ret= ((lispobj) LOW_WORD(ncode) + offset) | type_OtherPointer;
#ifdef DEBUG_CODE_GC
	printf("trans_return_pc_header returning %x\n",ret);
#endif
	return ret;
}

/* On the 386, closures hold a pointer to the raw address instead of
 * the function object, so we can use CALL [$FDEFN+const] to invoke
 * the function without loading it into a register. Given that code
 * objects don't move, we don't need to update anything, but we do
 * have to figure out that the function is still live. */
#ifdef __i386__
static
scav_closure_header(where, object)
lispobj *where, object;
{
	struct closure *closure;
	lispobj fun;

	closure = (struct closure *)where;
	fun = closure->function - RAW_ADDR_OFFSET;
	scavenge(&fun, 1);

	return 2;
}
#endif

static int
scav_function_header(lispobj *where, lispobj object)
{
    fprintf(stderr, "GC lossage.  Should not be scavenging a ");
    fprintf(stderr, "Function Header.\n");
    fprintf(stderr, "where = 0x%p, object = 0x%08x",
	    where, (unsigned int) object);
    lose(NULL);
    return 0;
}

static lispobj
trans_function_header(lispobj object)
{
	struct function *fheader;
	unsigned long offset;
	struct code *code, *ncode;
	
	fheader = (struct function *) PTR(object);
	offset = HeaderValue(fheader->header) * 4;

	/* Transport the whole code object */
	code = (struct code *) ((unsigned long) fheader - offset);
	ncode = trans_code(code);

	return ((lispobj) LOW_WORD(ncode) + offset) | type_FunctionPointer;
}



/* instances */

#if DIRECT_SCAV
static int
scav_instance_pointer(lispobj *where, lispobj object)
{
    if (from_space_p(object)) {
	lispobj first, *first_pointer;

	/* object is a pointer into from space.  check to see */
	/* if it has been forwarded */
	first_pointer = (lispobj *) PTR(object);
	first = *first_pointer;
		
	if (!(Pointerp(first) && new_space_p(first)))
	    first = *first_pointer = trans_boxed(object);
	*where = first;
    }
    return 1;
}
#else
static int
scav_instance_pointer(lispobj *where, lispobj object)
{
  lispobj  *first_pointer;
  
  /* object is a pointer into from space.  Not a FP */
  first_pointer = (lispobj *) PTR(object);
  
  *where = *first_pointer = trans_boxed(object);
  return 1;
}
#endif


/* lists and conses */

static lispobj trans_list(lispobj object);

#if DIRECT_SCAV
static int
scav_list_pointer(lispobj *where, lispobj object)
{
	gc_assert(Pointerp(object));

	if (from_space_p(object)) {
		lispobj first, *first_pointer;

		/* object is a pointer into from space.  check to see */
		/* if it has been forwarded */
		first_pointer = (lispobj *) PTR(object);
		first = *first_pointer;
		
		if (!(Pointerp(first) && new_space_p(first)))
			first = *first_pointer = trans_list(object);

		gc_assert(Pointerp(first));
		gc_assert(!from_space_p(first));
	
		*where = first;
	}
	return 1;
}
#else
static int
scav_list_pointer(lispobj *where, lispobj object)
{
  lispobj first, *first_pointer;

  gc_assert(Pointerp(object));

  /* object is a pointer into from space.  Not a FP. */
  first_pointer = (lispobj *) PTR(object);
  
  first = *first_pointer = trans_list(object);
  
  gc_assert(Pointerp(first));
  gc_assert(!from_space_p(first));
  
  *where = first;
  return 1;
}
#endif

static lispobj
trans_list(lispobj object)
{
	lispobj new_list_pointer;
	struct cons *cons, *new_cons;
	
	cons = (struct cons *) PTR(object);

	/* ### Don't use copy_object here. */
	new_list_pointer = copy_object(object, 2);
	new_cons = (struct cons *) PTR(new_list_pointer);

	/* Set forwarding pointer. */
	cons->car = new_list_pointer;
	
	/* Try to linearize the list in the cdr direction to help reduce */
	/* paging. */

	while (1) {
		lispobj cdr, new_cdr, first;
		struct cons *cdr_cons, *new_cdr_cons;

		cdr = cons->cdr;

                if (LowtagOf(cdr) != type_ListPointer ||
                    !from_space_p(cdr) ||
                    (Pointerp(first = *(lispobj *)PTR(cdr)) &&
                     new_space_p(first)))
                	break;

		cdr_cons = (struct cons *) PTR(cdr);

		/* ### Don't use copy_object here */
		new_cdr = copy_object(cdr, 2);
		new_cdr_cons = (struct cons *) PTR(new_cdr);

		/* Set forwarding pointer */
		cdr_cons->car = new_cdr;

		/* Update the cdr of the last cons copied into new */
		/* space to keep the newspace scavenge from having to */
		/* do it. */
		new_cons->cdr = new_cdr;
		
		cons = cdr_cons;
		new_cons = new_cdr_cons;
	}

	return new_list_pointer;
}


/* scavenging and transporting other pointers */

#if DIRECT_SCAV
static int
scav_other_pointer(lispobj *where, lispobj object)
{
	gc_assert(Pointerp(object));

	if (from_space_p(object)) {
		lispobj first, *first_pointer;

		/* object is a pointer into from space.  check to see */
		/* if it has been forwarded */
		first_pointer = (lispobj *) PTR(object);
		first = *first_pointer;
		
		if (!(Pointerp(first) && new_space_p(first)))
			first = *first_pointer = 
				(transother[TypeOf(first)])(object);

		gc_assert(Pointerp(first));
		gc_assert(!from_space_p(first));

		*where = first;
	}
	return 1;
}
#else
static int
scav_other_pointer(lispobj *where, lispobj object)
{
  lispobj first, *first_pointer;

  gc_assert(Pointerp(object));

  /* Object is a pointer into from space - not a FP */
  first_pointer = (lispobj *) PTR(object);
  first = *first_pointer = (transother[TypeOf(*first_pointer)])(object);

  gc_assert(Pointerp(first));
  gc_assert(!from_space_p(first));

  *where = first;
  return 1;
}
#endif


/* immediate, boxed, and unboxed objects */

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
    fprintf(stderr, "GC lossage.  Trying to transport an immediate!?\n");
    lose(NULL);
    return NIL;
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

	gc_assert(Pointerp(object));

	header = *((lispobj *) PTR(object));
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
#ifndef sparc
static int
scav_fdefn(lispobj *where, lispobj object)
{
    struct fdefn *fdefn;

    fdefn = (struct fdefn *)where;
    
    if ((char *)(fdefn->function + RAW_ADDR_OFFSET) 
	== (char *)((unsigned long)(fdefn->raw_addr))) {
        scavenge(where + 1, sizeof(struct fdefn)/sizeof(lispobj) - 1);
        fdefn->raw_addr = (u32)  ((char *) LOW_WORD(fdefn->function)) + RAW_ADDR_OFFSET;
        return sizeof(struct fdefn) / sizeof(lispobj);
    }
    else
        return 1;
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


	gc_assert(Pointerp(object));

	header = *((lispobj *) PTR(object));
	length = HeaderValue(header) + 1;
	length = CEILING(length, 2);

	return copy_object(object, length);
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


/* vector-like objects */

#define NWORDS(x,y) (CEILING((x),(y)) / (y))

static int
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

	gc_assert(Pointerp(object));

	/* NOTE: Strings contain one more byte of data than the length */
	/* slot indicates. */

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length) + 1;
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return copy_object(object, nwords);
}

static int
size_string(lispobj *where)
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

static int
scav_vector(lispobj *where, lispobj object)
{
    if (HeaderValue(object) == subtype_VectorValidHashing)
        *where = (subtype_VectorMustRehash<<type_Bits) | type_SimpleVector;

    return 1;
}


static lispobj
trans_vector(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);

	length = fixnum_value(vector->length);
	nwords = CEILING(length + 2, 2);

	return copy_object(object, nwords);
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

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 32) + 2, 2);

	return copy_object(object, nwords);
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

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 16) + 2, 2);

	return copy_object(object, nwords);
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

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 8) + 2, 2);

	return copy_object(object, nwords);
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

static lispobj
trans_vector_unsigned_byte_8(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return copy_object(object, nwords);
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

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 2) + 2, 2);

	return copy_object(object, nwords);
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

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length + 2, 2);

	return copy_object(object, nwords);
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

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length + 2, 2);

	return copy_object(object, nwords);
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

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 2 + 2, 2);

	return copy_object(object, nwords);
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


#ifdef type_SimpleArrayLongFloat
static int
scav_vector_long_float(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
#ifdef sparc
	nwords = CEILING(length * 4 + 2, 2);
#endif

	return nwords;
}

static lispobj
trans_vector_long_float(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
#ifdef sparc
	nwords = CEILING(length * 4 + 2, 2);
#endif

	return copy_object(object, nwords);
}

static int
size_vector_long_float(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
#ifdef sparc
	nwords = CEILING(length * 4 + 2, 2);
#endif

	return nwords;
}
#endif


#ifdef type_SimpleArrayComplexSingleFloat
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

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 2 + 2, 2);

	return copy_object(object, nwords);
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

#ifdef type_SimpleArrayComplexDoubleFloat
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

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 4 + 2, 2);

	return copy_object(object, nwords);
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

#ifdef type_SimpleArrayComplexLongFloat
static int
scav_vector_complex_long_float(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
#ifdef sparc
	nwords = CEILING(length * 8 + 2, 2);
#endif

	return nwords;
}

static lispobj
trans_vector_complex_long_float(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
#ifdef sparc
	nwords = CEILING(length * 8 + 2, 2);
#endif

	return copy_object(object, nwords);
}

static int
size_vector_complex_long_float(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
#ifdef sparc
	nwords = CEILING(length * 8 + 2, 2);
#endif

	return nwords;
}
#endif


/* weak pointers */

#define WEAK_POINTER_NWORDS \
	CEILING((sizeof(struct weak_pointer) / sizeof(lispobj)), 2)

static int
scav_weak_pointer(lispobj *where, lispobj object)
{
	/* Do not let GC scavenge the value slot of the weak pointer */
	/* (that is why it is a weak pointer).  Note:  we could use */
	/* the scav_unboxed method here. */

	return WEAK_POINTER_NWORDS;
}

static lispobj
trans_weak_pointer(lispobj object)
{
	lispobj copy;
	struct weak_pointer *wp;

	gc_assert(Pointerp(object));

#if defined(DEBUG_WEAK)
	printf("Transporting weak pointer from 0x%08x\n", object);
#endif

	/* Need to remember where all the weak pointers are that have */
	/* been transported so they can be fixed up in a post-GC pass. */

	copy = copy_object(object, WEAK_POINTER_NWORDS);
	wp = (struct weak_pointer *) PTR(copy);
	

	/* Push the weak pointer onto the list of weak pointers. */
	wp->next = LOW_WORD(weak_pointers);
	weak_pointers = wp;

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

	for (wp = weak_pointers; wp != (struct weak_pointer *) NULL;
	     wp = (struct weak_pointer *)((unsigned long)wp->next)) {
		lispobj value;
		lispobj first, *first_pointer;

		value = wp->value;

#if defined(DEBUG_WEAK)
		printf("Weak pointer at 0x%p\n",  wp);
		printf("Value: 0x%08x\n", (unsigned int) value);
#endif		

		if (!(Pointerp(value) && from_space_p(value)))
			continue;

		/* Now, we need to check if the object has been */
		/* forwarded.  If it has been, the weak pointer is */
		/* still good and needs to be updated.  Otherwise, the */
		/* weak pointer needs to be nil'ed out. */

		first_pointer = (lispobj *) PTR(value);
		first = *first_pointer;
		
#if defined(DEBUG_WEAK)
		printf("First: 0x%08x\n", (unsigned long) first);
#endif		

		if (Pointerp(first) && new_space_p(first))
			wp->value = first;
		else {
			wp->value = NIL;
			wp->broken = T;
		}
	}
}



/* initialization */

static int
scav_lose(lispobj *where, lispobj object)
{
    fprintf(stderr, "GC lossage.  No scavenge function for object 0x%08x (at 0x%016lx)\n",
	    (unsigned int) object, (unsigned long)where);
    lose(NULL);
    return 0;
}

static lispobj
trans_lose(lispobj object)
{
    fprintf(stderr, "GC lossage.  No transport function for object 0x%08x\n",
	    (unsigned int)object);
    lose(NULL);
    return NIL;
}

static int
size_lose(lispobj *where)
{
	fprintf(stderr, "Size lossage.  No size function for object at 0x%p\n",
		where);
	fprintf(stderr, "First word of object: 0x%08x\n",
		(u32) *where);
	return 1;
}

/* KLUDGE: SBCL already has two GC implementations, and if someday the
 * precise generational GC is revived, it might have three. It would
 * be nice to share the scavtab[] data set up here, and perhaps other
 * things too, between all of them, rather than trying to maintain
 * multiple copies. -- WHN 2001-05-09 */
void
gc_init(void)
{
	int i;

	/* scavenge table */
	for (i = 0; i < 256; i++)
	    scavtab[i] = scav_lose; 
	/* scavtab[i] = scav_immediate; */

	for (i = 0; i < 32; i++) {
		scavtab[type_EvenFixnum|(i<<3)] = scav_immediate;
		scavtab[type_FunctionPointer|(i<<3)] = scav_function_pointer;
		/* OtherImmediate0 */
		scavtab[type_ListPointer|(i<<3)] = scav_list_pointer;
		scavtab[type_OddFixnum|(i<<3)] = scav_immediate;
		scavtab[type_InstancePointer|(i<<3)] = scav_instance_pointer;
		/* OtherImmediate1 */
		scavtab[type_OtherPointer|(i<<3)] = scav_other_pointer;
	}

	scavtab[type_Bignum] = scav_unboxed;
	scavtab[type_Ratio] = scav_boxed;
	scavtab[type_SingleFloat] = scav_unboxed;
	scavtab[type_DoubleFloat] = scav_unboxed;
#ifdef type_LongFloat
	scavtab[type_LongFloat] = scav_unboxed;
#endif
	scavtab[type_Complex] = scav_boxed;
#ifdef type_ComplexSingleFloat
	scavtab[type_ComplexSingleFloat] = scav_unboxed;
#endif
#ifdef type_ComplexDoubleFloat
	scavtab[type_ComplexDoubleFloat] = scav_unboxed;
#endif
#ifdef type_ComplexLongFloat
	scavtab[type_ComplexLongFloat] = scav_unboxed;
#endif
	scavtab[type_SimpleArray] = scav_boxed;
	scavtab[type_SimpleString] = scav_string;
	scavtab[type_SimpleBitVector] = scav_vector_bit;
	scavtab[type_SimpleVector] = scav_vector;
	scavtab[type_SimpleArrayUnsignedByte2] = scav_vector_unsigned_byte_2;
	scavtab[type_SimpleArrayUnsignedByte4] = scav_vector_unsigned_byte_4;
	scavtab[type_SimpleArrayUnsignedByte8] = scav_vector_unsigned_byte_8;
	scavtab[type_SimpleArrayUnsignedByte16] = scav_vector_unsigned_byte_16;
	scavtab[type_SimpleArrayUnsignedByte32] = scav_vector_unsigned_byte_32;
#ifdef type_SimpleArraySignedByte8
	scavtab[type_SimpleArraySignedByte8] = scav_vector_unsigned_byte_8;
#endif
#ifdef type_SimpleArraySignedByte16
	scavtab[type_SimpleArraySignedByte16] = scav_vector_unsigned_byte_16;
#endif
#ifdef type_SimpleArraySignedByte30
	scavtab[type_SimpleArraySignedByte30] = scav_vector_unsigned_byte_32;
#endif
#ifdef type_SimpleArraySignedByte32
	scavtab[type_SimpleArraySignedByte32] = scav_vector_unsigned_byte_32;
#endif
	scavtab[type_SimpleArraySingleFloat] = scav_vector_single_float;
	scavtab[type_SimpleArrayDoubleFloat] = scav_vector_double_float;
#ifdef type_SimpleArrayLongFloat
	scavtab[type_SimpleArrayLongFloat] = scav_vector_long_float;
#endif
#ifdef type_SimpleArrayComplexSingleFloat
	scavtab[type_SimpleArrayComplexSingleFloat] = scav_vector_complex_single_float;
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
	scavtab[type_SimpleArrayComplexDoubleFloat] = scav_vector_complex_double_float;
#endif
#ifdef type_SimpleArrayComplexLongFloat
	scavtab[type_SimpleArrayComplexLongFloat] = scav_vector_complex_long_float;
#endif
	scavtab[type_ComplexString] = scav_boxed;
	scavtab[type_ComplexBitVector] = scav_boxed;
	scavtab[type_ComplexVector] = scav_boxed;
	scavtab[type_ComplexArray] = scav_boxed;
	scavtab[type_CodeHeader] = scav_code_header;
	scavtab[type_FunctionHeader] = scav_function_header;
	scavtab[type_ClosureFunctionHeader] = scav_function_header;
	scavtab[type_ReturnPcHeader] = scav_return_pc_header;
#ifdef __i386__
	scavtab[type_ClosureHeader] = scav_closure_header;
	scavtab[type_FuncallableInstanceHeader] = scav_closure_header;
	scavtab[type_ByteCodeFunction] = scav_closure_header;
	scavtab[type_ByteCodeClosure] = scav_closure_header;
	/*	scavtab[type_DylanFunctionHeader] = scav_closure_header; */
#else
	scavtab[type_ClosureHeader] = scav_boxed;
	scavtab[type_FuncallableInstanceHeader] = scav_boxed;
	scavtab[type_ByteCodeFunction] = scav_boxed;
	scavtab[type_ByteCodeClosure] = scav_boxed;
	/* scavtab[type_DylanFunctionHeader] = scav_boxed; */
#endif
	scavtab[type_ValueCellHeader] = scav_boxed;
        scavtab[type_SymbolHeader] = scav_boxed;
	scavtab[type_BaseChar] = scav_immediate;
	scavtab[type_Sap] = scav_unboxed;
	scavtab[type_UnboundMarker] = scav_immediate;
	scavtab[type_WeakPointer] = scav_weak_pointer;
        scavtab[type_InstanceHeader] = scav_boxed;
#ifndef sparc
        scavtab[type_Fdefn] = scav_fdefn;
#else
        scavtab[type_Fdefn] = scav_boxed;
#endif

	/* Transport Other Table */
	for (i = 0; i < 256; i++)
		transother[i] = trans_lose;

	transother[type_Bignum] = trans_unboxed;
	transother[type_Ratio] = trans_boxed;
	transother[type_SingleFloat] = trans_unboxed;
	transother[type_DoubleFloat] = trans_unboxed;
#ifdef type_LongFloat
	transother[type_LongFloat] = trans_unboxed;
#endif
	transother[type_Complex] = trans_boxed;
#ifdef type_ComplexSingleFloat
	transother[type_ComplexSingleFloat] = trans_unboxed;
#endif
#ifdef type_ComplexDoubleFloat
	transother[type_ComplexDoubleFloat] = trans_unboxed;
#endif
#ifdef type_ComplexLongFloat
	transother[type_ComplexLongFloat] = trans_unboxed;
#endif
	transother[type_SimpleArray] = trans_boxed;
	transother[type_SimpleString] = trans_string;
	transother[type_SimpleBitVector] = trans_vector_bit;
	transother[type_SimpleVector] = trans_vector;
	transother[type_SimpleArrayUnsignedByte2] = trans_vector_unsigned_byte_2;
	transother[type_SimpleArrayUnsignedByte4] = trans_vector_unsigned_byte_4;
	transother[type_SimpleArrayUnsignedByte8] = trans_vector_unsigned_byte_8;
	transother[type_SimpleArrayUnsignedByte16] = trans_vector_unsigned_byte_16;
	transother[type_SimpleArrayUnsignedByte32] = trans_vector_unsigned_byte_32;
#ifdef type_SimpleArraySignedByte8
	transother[type_SimpleArraySignedByte8] = trans_vector_unsigned_byte_8;
#endif
#ifdef type_SimpleArraySignedByte16
	transother[type_SimpleArraySignedByte16] = trans_vector_unsigned_byte_16;
#endif
#ifdef type_SimpleArraySignedByte30
	transother[type_SimpleArraySignedByte30] = trans_vector_unsigned_byte_32;
#endif
#ifdef type_SimpleArraySignedByte32
	transother[type_SimpleArraySignedByte32] = trans_vector_unsigned_byte_32;
#endif
	transother[type_SimpleArraySingleFloat] = trans_vector_single_float;
	transother[type_SimpleArrayDoubleFloat] = trans_vector_double_float;
#ifdef type_SimpleArrayLongFloat
	transother[type_SimpleArrayLongFloat] = trans_vector_long_float;
#endif
#ifdef type_SimpleArrayComplexSingleFloat
	transother[type_SimpleArrayComplexSingleFloat] = trans_vector_complex_single_float;
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
	transother[type_SimpleArrayComplexDoubleFloat] = trans_vector_complex_double_float;
#endif
#ifdef type_SimpleArrayComplexLongFloat
	transother[type_SimpleArrayComplexLongFloat] = trans_vector_complex_long_float;
#endif
	transother[type_ComplexString] = trans_boxed;
	transother[type_ComplexBitVector] = trans_boxed;
	transother[type_ComplexVector] = trans_boxed;
	transother[type_ComplexArray] = trans_boxed;
	transother[type_CodeHeader] = trans_code_header;
	transother[type_FunctionHeader] = trans_function_header;
	transother[type_ClosureFunctionHeader] = trans_function_header;
	transother[type_ReturnPcHeader] = trans_return_pc_header;
	transother[type_ClosureHeader] = trans_boxed;
	transother[type_FuncallableInstanceHeader] = trans_boxed;
	transother[type_ByteCodeFunction] = trans_boxed;
	transother[type_ByteCodeClosure] = trans_boxed;
	transother[type_ValueCellHeader] = trans_boxed;
	transother[type_SymbolHeader] = trans_boxed;
	transother[type_BaseChar] = trans_immediate;
	transother[type_Sap] = trans_unboxed;
	transother[type_UnboundMarker] = trans_immediate;
	transother[type_WeakPointer] = trans_weak_pointer;
        transother[type_InstanceHeader] = trans_boxed;
	transother[type_Fdefn] = trans_boxed;

	/* Size table */

	for (i = 0; i < 256; i++)
		sizetab[i] = size_lose;

	for (i = 0; i < 32; i++) {
		sizetab[type_EvenFixnum|(i<<3)] = size_immediate;
		sizetab[type_FunctionPointer|(i<<3)] = size_pointer;
		/* OtherImmediate0 */
		sizetab[type_ListPointer|(i<<3)] = size_pointer;
		sizetab[type_OddFixnum|(i<<3)] = size_immediate;
		sizetab[type_InstancePointer|(i<<3)] = size_pointer;
		/* OtherImmediate1 */
		sizetab[type_OtherPointer|(i<<3)] = size_pointer;
	}

	sizetab[type_Bignum] = size_unboxed;
	sizetab[type_Ratio] = size_boxed;
	sizetab[type_SingleFloat] = size_unboxed;
	sizetab[type_DoubleFloat] = size_unboxed;
#ifdef type_LongFloat
	sizetab[type_LongFloat] = size_unboxed;
#endif
	sizetab[type_Complex] = size_boxed;
#ifdef type_ComplexSingleFloat
	sizetab[type_ComplexSingleFloat] = size_unboxed;
#endif
#ifdef type_ComplexDoubleFloat
	sizetab[type_ComplexDoubleFloat] = size_unboxed;
#endif
#ifdef type_ComplexLongFloat
	sizetab[type_ComplexLongFloat] = size_unboxed;
#endif
	sizetab[type_SimpleArray] = size_boxed;
	sizetab[type_SimpleString] = size_string;
	sizetab[type_SimpleBitVector] = size_vector_bit;
	sizetab[type_SimpleVector] = size_vector;
	sizetab[type_SimpleArrayUnsignedByte2] = size_vector_unsigned_byte_2;
	sizetab[type_SimpleArrayUnsignedByte4] = size_vector_unsigned_byte_4;
	sizetab[type_SimpleArrayUnsignedByte8] = size_vector_unsigned_byte_8;
	sizetab[type_SimpleArrayUnsignedByte16] = size_vector_unsigned_byte_16;
	sizetab[type_SimpleArrayUnsignedByte32] = size_vector_unsigned_byte_32;
#ifdef type_SimpleArraySignedByte8
	sizetab[type_SimpleArraySignedByte8] = size_vector_unsigned_byte_8;
#endif
#ifdef type_SimpleArraySignedByte16
	sizetab[type_SimpleArraySignedByte16] = size_vector_unsigned_byte_16;
#endif
#ifdef type_SimpleArraySignedByte30
	sizetab[type_SimpleArraySignedByte30] = size_vector_unsigned_byte_32;
#endif
#ifdef type_SimpleArraySignedByte32
	sizetab[type_SimpleArraySignedByte32] = size_vector_unsigned_byte_32;
#endif
	sizetab[type_SimpleArraySingleFloat] = size_vector_single_float;
	sizetab[type_SimpleArrayDoubleFloat] = size_vector_double_float;
#ifdef type_SimpleArrayLongFloat
	sizetab[type_SimpleArrayLongFloat] = size_vector_long_float;
#endif
#ifdef type_SimpleArrayComplexSingleFloat
	sizetab[type_SimpleArrayComplexSingleFloat] = size_vector_complex_single_float;
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
	sizetab[type_SimpleArrayComplexDoubleFloat] = size_vector_complex_double_float;
#endif
#ifdef type_SimpleArrayComplexLongFloat
	sizetab[type_SimpleArrayComplexLongFloat] = size_vector_complex_long_float;
#endif
	sizetab[type_ComplexString] = size_boxed;
	sizetab[type_ComplexBitVector] = size_boxed;
	sizetab[type_ComplexVector] = size_boxed;
	sizetab[type_ComplexArray] = size_boxed;
	sizetab[type_CodeHeader] = size_code_header;
#if 0
	/* Shouldn't see these so just lose if it happens */
	sizetab[type_FunctionHeader] = size_function_header;
	sizetab[type_ClosureFunctionHeader] = size_function_header;
	sizetab[type_ReturnPcHeader] = size_return_pc_header;
#endif
	sizetab[type_ClosureHeader] = size_boxed;
	sizetab[type_FuncallableInstanceHeader] = size_boxed;
	sizetab[type_ValueCellHeader] = size_boxed;
	sizetab[type_SymbolHeader] = size_boxed;
	sizetab[type_BaseChar] = size_immediate;
	sizetab[type_Sap] = size_unboxed;
	sizetab[type_UnboundMarker] = size_immediate;
	sizetab[type_WeakPointer] = size_weak_pointer;
        sizetab[type_InstanceHeader] = size_boxed;
	sizetab[type_Fdefn] = size_boxed;
}

/* noise to manipulate the gc trigger stuff */

#ifndef ibmrt

void set_auto_gc_trigger(os_vm_size_t dynamic_usage)
{
    os_vm_address_t addr=(os_vm_address_t)current_dynamic_space +
	dynamic_usage;
    long length =
	DYNAMIC_SPACE_SIZE + (os_vm_address_t)current_dynamic_space - addr;

    if(addr < (os_vm_address_t)dynamic_space_free_pointer) {
	fprintf(stderr,
	   "set_auto_gc_trigger: tried to set gc trigger too low! (%d < %p)\n",
		(unsigned int)dynamic_usage,
		(os_vm_address_t)dynamic_space_free_pointer
		- (os_vm_address_t)current_dynamic_space);
	return;
    }
    else if (length < 0) {
	fprintf(stderr,
		"set_auto_gc_trigger: tried to set gc trigger too high! (%p)\n",
		dynamic_usage);
	return;
    }

    addr=os_round_up_to_page(addr);
    length=os_trunc_size_to_page(length);

#if defined(SUNOS) || defined(SOLARIS)
    os_invalidate(addr,length);
#else
    os_protect(addr, length, 0);
#endif

    current_auto_gc_trigger = (lispobj *)addr;
}

void clear_auto_gc_trigger(void)
{
    if(current_auto_gc_trigger!=NULL){
#if defined(SUNOS) || defined(SOLARIS)/* don't want to force whole space into swapping mode... */
	os_vm_address_t addr=(os_vm_address_t)current_auto_gc_trigger;
	os_vm_size_t length=
	    DYNAMIC_SPACE_SIZE + (os_vm_address_t)current_dynamic_space - addr;

	os_validate(addr,length);
#else
	os_protect((os_vm_address_t)current_dynamic_space,
		   DYNAMIC_SPACE_SIZE,
		   OS_VM_PROT_ALL);
#endif

	current_auto_gc_trigger = NULL;
    }
}

#endif
