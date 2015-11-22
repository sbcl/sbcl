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
#include "sbcl.h"
#include "runtime.h"
#include "os.h"
#include "gc.h"
#include "gc-internal.h"
#include "globals.h"
#include "interrupt.h"
#include "validate.h"
#include "lispregs.h"
#include "interr.h"
#include "genesis/static-symbols.h"
#include "genesis/primitive-objects.h"
#include "thread.h"
#include "arch.h"

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


/* collecting garbage */

#ifdef PRINTNOISE
static double
tv_diff(struct timeval *x, struct timeval *y)
{
    return (((double) x->tv_sec + (double) x->tv_usec * 1.0e-6) -
            ((double) y->tv_sec + (double) y->tv_usec * 1.0e-6));
}
#endif

void *
gc_general_alloc(word_t bytes, int page_type_flag, int quick_p) {
    lispobj *new=new_space_free_pointer;
    new_space_free_pointer+=(bytes/N_WORD_BYTES);
    return new;
}

lispobj  copy_large_unboxed_object(lispobj object, sword_t nwords) {
    return copy_object(object,nwords);
}
lispobj  copy_unboxed_object(lispobj object, sword_t nwords) {
    return copy_object(object,nwords);
}
lispobj  copy_large_object(lispobj object, sword_t nwords) {
    return copy_object(object,nwords);
}

/* Note: The generic GC interface we're implementing passes us a
 * last_generation argument. That's meaningless for us, since we're
 * not a generational GC. So we ignore it. */
void
collect_garbage(generation_index_t ignore)
{
#ifdef PRINTNOISE
    struct timeval start_tv, stop_tv;
    struct rusage start_rusage, stop_rusage;
    double real_time, system_time, user_time;
    double percent_retained, gc_rate;
    unsigned long size_discarded;
#endif
    unsigned long size_retained;
    lispobj *current_static_space_free_pointer;
    unsigned long static_space_size;
    unsigned long control_stack_size, binding_stack_size;
    sigset_t tmp, old;
    struct thread *th=arch_os_get_current_thread();

#ifdef PRINTNOISE
    printf("[Collecting garbage ... \n");

    getrusage(RUSAGE_SELF, &start_rusage);
    gettimeofday(&start_tv, (struct timezone *) 0);
#endif

    /* it's possible that signals are blocked already if this was called
     * from a signal handler (e.g. with the sigsegv gc_trigger stuff) */
    block_blockable_signals(0, &old);

    current_static_space_free_pointer =
        (lispobj *) ((unsigned long)
                     SymbolValue(STATIC_SPACE_FREE_POINTER,0));


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
    scavenge_interrupt_contexts(th);

#ifdef PRINTNOISE
    printf("Scavenging interrupt handlers (%d bytes) ...\n",
           (int)sizeof(interrupt_handlers));
#endif
    scavenge((lispobj *) interrupt_handlers,
             sizeof(interrupt_handlers) / sizeof(lispobj));

#ifdef PRINTNOISE
    printf("Scavenging the control stack ...\n");
#endif
    scavenge_control_stack(th);


    binding_stack_size =
        (lispobj *)get_binding_stack_pointer(th) -
        (lispobj *)th->binding_stack_start;
#ifdef PRINTNOISE
    printf("Scavenging the binding stack %x - %x (%d words) ...\n",
           th->binding_stack_start,get_binding_stack_pointer(th),
           (int)(binding_stack_size));
#endif
    scavenge(((lispobj *)th->binding_stack_start), binding_stack_size);

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
    printf("Scanning weak hash tables ...\n");
#endif
    scan_weak_hash_tables();

    /* Scan the weak pointers. */
#ifdef PRINTNOISE
    printf("Scanning weak pointers ...\n");
#endif
    scan_weak_pointers();

    /* Flip spaces. */
#ifdef PRINTNOISE
    printf("Flipping spaces ...\n");
#endif

    /* Maybe FIXME: it's possible that we could significantly reduce
     * RSS by zeroing the from_space or madvise(MADV_DONTNEED) or
     * similar os-dependent tricks here */
#ifdef LISP_FEATURE_HPUX
    /* hpux cant handle unmapping areas that are not 100% mapped */
    clear_auto_gc_trigger();
#endif
    os_zero((os_vm_address_t) from_space,
            (os_vm_size_t) dynamic_space_size);

    current_dynamic_space = new_space;
    dynamic_space_free_pointer = new_space_free_pointer;

#ifdef PRINTNOISE
    size_discarded = (from_space_free_pointer - from_space) * sizeof(lispobj);
#endif
    size_retained = (new_space_free_pointer - new_space) * sizeof(lispobj);

    os_flush_icache((os_vm_address_t)new_space, size_retained);

    /* Zero stack. */
#ifdef PRINTNOISE
    printf("Zeroing empty part of control stack ...\n");
#endif
    scrub_control_stack();
    set_auto_gc_trigger(size_retained+bytes_consed_between_gcs);
    thread_sigmask(SIG_SETMASK, &old, 0);


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

    printf("Statistics: %10.2fs real, %10.2fs user, %10.2fs system.\n",
           real_time, user_time, system_time);

    gc_rate = ((float) size_retained / (float) (1<<20)) / real_time;

    printf("%10.2f M bytes/sec collected.\n", gc_rate);
#endif
}


/* scavenging */

static void
scavenge_newspace(void)
{
    lispobj *here, *next;

    here = new_space;
    while (here < new_space_free_pointer) {
        /*      printf("here=%lx, new_space_free_pointer=%lx\n",
                here,new_space_free_pointer); */
        next = new_space_free_pointer;
        scavenge(here, next - here);
        scav_weak_hash_tables();
        here = next;
    }
    /* printf("done with newspace\n"); */
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
            default: nwords=1;  /* shut yer whinging, gcc */
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


/* weak pointers */

#define WEAK_POINTER_NWORDS \
        CEILING((sizeof(struct weak_pointer) / sizeof(lispobj)), 2)

static long
scav_weak_pointer(lispobj *where, lispobj object)
{
    /* Do not let GC scavenge the value slot of the weak pointer */
    /* (that is why it is a weak pointer).  Note:  we could use */
    /* the scav_unboxed method here. */

    return WEAK_POINTER_NWORDS;
}

lispobj *
search_read_only_space(void *pointer)
{
    lispobj* start = (lispobj*)READ_ONLY_SPACE_START;
    lispobj* end = (lispobj*)SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0);
    if ((pointer < (void *)start) || (pointer >= (void *)end))
        return NULL;
    return (gc_search_space(start,
                            (((lispobj *)pointer)+2)-start,
                            (lispobj *)pointer));
}

lispobj *
search_static_space(void *pointer)
{
    lispobj* start = (lispobj*)STATIC_SPACE_START;
    lispobj* end = (lispobj*)SymbolValue(STATIC_SPACE_FREE_POINTER,0);
    if ((pointer < (void *)start) || (pointer >= (void *)end))
        return NULL;
    return (gc_search_space(start,
                            (((lispobj *)pointer)+2)-start,
                            (lispobj *)pointer));
}

lispobj *
search_dynamic_space(void *pointer)
{
    lispobj *start = (lispobj *) current_dynamic_space;
    lispobj *end = (lispobj *) dynamic_space_free_pointer;
    if ((pointer < (void *)start) || (pointer >= (void *)end))
        return NULL;
    return (gc_search_space(start,
                            (((lispobj *)pointer)+2)-start,
                            (lispobj *)pointer));
}

/* initialization.  if gc_init can be moved to after core load, we could
 * combine these two functions */

void
gc_init(void)
{
    gc_init_tables();
    scavtab[WEAK_POINTER_WIDETAG] = scav_weak_pointer;
}

void
gc_initialize_pointers(void)
{
    /* FIXME: We do nothing here.  We (briefly) misguidedly attempted
       to set current_dynamic_space to DYNAMIC_0_SPACE_START here,
       forgetting that (a) actually it could be the other and (b) it's
       set in coreparse.c anyway.  There's a FIXME note left here to
       note that current_dynamic_space is a violation of OAOO: we can
       tell which dynamic space we're currently in by looking at
       dynamic_space_free_pointer.  -- CSR, 2002-08-09 */
}




/* noise to manipulate the gc trigger stuff */

/* Functions that substantially change the dynamic space free pointer
 * (collect_garbage, purify) are responsible also for resetting the
 * auto_gc_trigger */
void set_auto_gc_trigger(os_vm_size_t dynamic_usage)
{
    os_vm_address_t addr;
    os_vm_size_t length;

    addr = os_round_up_to_page((os_vm_address_t)current_dynamic_space
                               + dynamic_usage);
    if (addr < (os_vm_address_t)dynamic_space_free_pointer)
        lose("set_auto_gc_trigger: tried to set gc trigger too low! (%ld < 0x%08lx)\n",
             (unsigned long)dynamic_usage,
             (unsigned long)((os_vm_address_t)dynamic_space_free_pointer
                             - (os_vm_address_t)current_dynamic_space));

    if (dynamic_usage > dynamic_space_size)
        lose("set_auto_gc_trigger: tried to set gc trigger too high! (0x%08lx)\n",
             (unsigned long)dynamic_usage);
    length = os_trunc_size_to_page(dynamic_space_size - dynamic_usage);

#if defined(SUNOS) || defined(SOLARIS) || defined(LISP_FEATURE_HPUX)
    os_invalidate(addr, length);
#else
    os_protect(addr, length, 0);
#endif

    current_auto_gc_trigger = (lispobj *)addr;
}

void clear_auto_gc_trigger(void)
{
    os_vm_address_t addr;
    os_vm_size_t length;

    if (current_auto_gc_trigger == NULL)
        return;

    addr = (os_vm_address_t)current_auto_gc_trigger;
    length = dynamic_space_size + (os_vm_address_t)current_dynamic_space - addr;

#if defined(SUNOS) || defined(SOLARIS) || defined(LISP_FEATURE_HPUX)
    /* don't want to force whole space into swapping mode... */
    os_validate(addr, length);
#else
    os_protect(addr, length, OS_VM_PROT_ALL);
#endif

    current_auto_gc_trigger = NULL;
}

static boolean
gc_trigger_hit(void *addr)
{
    if (current_auto_gc_trigger == NULL)
        return 0;
    else{
        return (addr >= (void *)current_auto_gc_trigger &&
                addr <((void *)current_dynamic_space + dynamic_space_size));
    }
}

boolean
cheneygc_handle_wp_violation(os_context_t *context, void *addr)
{
    if(!foreign_function_call_active && gc_trigger_hit(addr)){
        struct thread *thread=arch_os_get_current_thread();
        clear_auto_gc_trigger();
        /* Don't flood the system with interrupts if the need to gc is
         * already noted. This can happen for example when SUB-GC
         * allocates or after a gc triggered in a WITHOUT-GCING. */
        if (SymbolValue(GC_PENDING,thread) == NIL) {
            if (SymbolValue(GC_INHIBIT,thread) == NIL) {
                if (arch_pseudo_atomic_atomic(context)) {
                    /* set things up so that GC happens when we finish
                     * the PA section */
                    SetSymbolValue(GC_PENDING,T,thread);
                    arch_set_pseudo_atomic_interrupted(context);
                    maybe_save_gc_mask_and_block_deferrables
                        (os_context_sigmask_addr(context));
                } else {
                    maybe_gc(context);
                }
            } else {
                SetSymbolValue(GC_PENDING,T,thread);
            }
        }
        return 1;
    }
    return 0;
}
