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
#include "gc-internal.h"
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

lispobj *from_space;
lispobj *from_space_free_pointer;

lispobj *new_space;
lispobj *new_space_free_pointer;

static void scavenge_newspace(void);
static void scavenge_interrupt_contexts(void);


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

/* FIXME do we need this?  Doesn't it duplicate lisp code in 
 * scrub-control-stack? */

static void
zero_stack(void)
{
    u32 *ptr = (u32 *)current_control_stack_pointer;
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


void *
gc_general_alloc(int bytes, int unboxed_p, int quick_p) {
    lispobj *new=new_space_free_pointer;
    new_space_free_pointer+=(bytes/4);
    return new;
}

lispobj  copy_large_unboxed_object(lispobj object, int nwords) {
    return copy_object(object,nwords);
}
lispobj  copy_unboxed_object(lispobj object, int nwords) {
    return copy_object(object,nwords);
}
lispobj  copy_large_object(lispobj object, int nwords) {
    return copy_object(object,nwords);
}

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
    from_space_free_pointer = dynamic_space_free_pointer;

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
		 

    binding_stack_size = 
	current_binding_stack_pointer - 
	(lispobj *)BINDING_STACK_START;
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
    dynamic_space_free_pointer = new_space_free_pointer;

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
    /* os_flush_icache((os_vm_address_t) 0, sizeof(unsigned long)); */
    /* Maybe FIXME: it's possible that we could significantly reduce 
     * RSS by zeroing the from_space or madvise(MADV_DONTNEED) or 
     * similar os-dependent tricks here */
}


/* scavenging */

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
#ifdef ARCH_HAS_LINK_REGISTER
    unsigned long lr_code_offset;
#endif
#ifdef ARCH_HAS_NPC_REGISTER
    unsigned long npc_code_offset;
#endif
#ifdef DEBUG_SCAVENGE_VERBOSE
    fprintf(stderr, "Scavenging interrupt context at 0x%x\n",context);
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
	if ((reg & ~((1L<<N_LOWTAG_BITS)-1)) <= lip) {
	    offset = lip - reg;
	    if (offset < lip_offset) {
		lip_offset = offset;
		lip_register_pair = index;
	    }
	}
    }
#endif /* reg_LIP */

    /* Compute the PC's offset from the start of the CODE */
    /* register. */
    pc_code_offset =
	*os_context_pc_addr(context) - 
	*os_context_register_addr(context, reg_CODE);
#ifdef ARCH_HAS_NPC_REGISTER
    npc_code_offset =
	*os_context_npc_addr(context) - 
	*os_context_register_addr(context, reg_CODE);
#endif 
#ifdef ARCH_HAS_LINK_REGISTER
    lr_code_offset =
	*os_context_lr_addr(context) - 
	*os_context_register_addr(context, reg_CODE);
#endif
	       
    /* Scavenge all boxed registers in the context. */
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
#endif /* reg_LIP */
	
    /* Fix the PC if it was in from space */
    if (from_space_p(*os_context_pc_addr(context)))
	*os_context_pc_addr(context) = 
	    *os_context_register_addr(context, reg_CODE) + pc_code_offset;
#ifdef ARCH_HAS_LINK_REGISTER
    /* Fix the LR ditto; important if we're being called from 
     * an assembly routine that expects to return using blr, otherwise
     * harmless */
    if (from_space_p(*os_context_lr_addr(context)))
	*os_context_lr_addr(context) = 
	    *os_context_register_addr(context, reg_CODE) + lr_code_offset;
#endif

#ifdef ARCH_HAS_NPC_REGISTER
    if (from_space_p(*os_context_npc_addr(context)))
	*os_context_npc_addr(context) = 
	    *os_context_register_addr(context, reg_CODE) + npc_code_offset;
#endif
}

void scavenge_interrupt_contexts(void)
{
    int i, index;
    os_context_t *context;

    index = fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX));

#ifdef DEBUG_SCAVENGE_VERBOSE
    fprintf(stderr, "%d interrupt contexts to scan\n",index);
#endif
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
	forwardp = is_lisp_pointer(object) && new_space_p(object);

	if (forwardp) {
	    int tag;
	    lispobj *pointer;

	    tag = lowtag_of(object);

	    switch (tag) {
	    case LIST_POINTER_LOWTAG:
		nwords = 2;
		break;
	    case INSTANCE_POINTER_LOWTAG:
		printf("Don't know about instances yet!\n");
		nwords = 1;
		break;
	    case FUN_POINTER_LOWTAG:
		nwords = 1;
		break;
	    case OTHER_POINTER_LOWTAG:
		pointer = (lispobj *) native_pointer(object);
		header = *pointer;
		type = widetag_of(header);
		nwords = (sizetab[type])(pointer);
		break;
	    default: nwords=1;	/* shut yer whinging, gcc */
	    }
	} else {
	    type = widetag_of(object);
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

/* FIXME (1) this could probably be defined using something like
 *  sizeof(lispobj)*floor(sizeof(struct simple_fun)/sizeof(lispobj))
 *    -  FUN_POINTER_LOWTAG
 * as I'm reasonably sure that simple_fun->code must always be the 
 * last slot in the object 

 * FIXME (2) it also appears in purify.c, and it has a different value
 * for SPARC users in that bit
 */

#define FUN_RAW_ADDR_OFFSET (6*sizeof(lispobj) - FUN_POINTER_LOWTAG)

/* Note: on the sparc we don't have to do anything special for fdefns, */
/* 'cause the raw-addr has a function lowtag. */
#ifndef LISP_FEATURE_SPARC
static int
scav_fdefn(lispobj *where, lispobj object)
{
    struct fdefn *fdefn;

    fdefn = (struct fdefn *)where;
    
    if ((char *)(fdefn->fun + FUN_RAW_ADDR_OFFSET) 
	== (char *)((unsigned long)(fdefn->raw_addr))) {
        scavenge(where + 1, sizeof(struct fdefn)/sizeof(lispobj) - 1);
        fdefn->raw_addr =
	    (u32)  ((char *) LOW_WORD(fdefn->fun)) + FUN_RAW_ADDR_OFFSET;
        return sizeof(struct fdefn) / sizeof(lispobj);
    }
    else
        return 1;
}
#endif



/* vector-like objects */

/* #define NWORDS(x,y) (CEILING((x),(y)) / (y)) */

static int
scav_vector(lispobj *where, lispobj object)
{
    if (HeaderValue(object) == subtype_VectorValidHashing) {
        *where =
	    (subtype_VectorMustRehash<<N_WIDETAG_BITS) | SIMPLE_VECTOR_WIDETAG;
    }

    return 1;
}


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


/* initialization.  if gc_init can be moved to after core load, we could
 * combine these two functions */

void
gc_init(void)
{
    gc_init_tables();
    scavtab[SIMPLE_VECTOR_WIDETAG] = scav_vector;
    scavtab[WEAK_POINTER_WIDETAG] = scav_weak_pointer;
}

void
gc_initialize_pointers(void)
{
    current_dynamic_space = DYNAMIC_0_SPACE_START;
}




/* noise to manipulate the gc trigger stuff */

void set_auto_gc_trigger(os_vm_size_t dynamic_usage)
{
    os_vm_address_t addr=(os_vm_address_t)current_dynamic_space 
	+ dynamic_usage;
	
    long length = DYNAMIC_SPACE_SIZE - dynamic_usage;

    if (addr < (os_vm_address_t)dynamic_space_free_pointer) {
	fprintf(stderr,
	   "set_auto_gc_trigger: tried to set gc trigger too low! (%d < %p)\n",
		(unsigned int)dynamic_usage,
		(os_vm_address_t)dynamic_space_free_pointer
		- (os_vm_address_t)current_dynamic_space);
	lose("lost");
    }
    else if (length < 0) {
	fprintf(stderr,
		"set_auto_gc_trigger: tried to set gc trigger too high! (%p)\n",
		dynamic_usage);
	lose("lost");
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
    if (current_auto_gc_trigger!=NULL){
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
