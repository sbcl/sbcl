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
#include <stdlib.h>
#include "sbcl.h"
#include "runtime.h"
#include "os.h"
#include "gc.h"
#include "gc-internal.h"
#include "gc-private.h"
#include "globals.h"
#include "interrupt.h"
#include "validate.h"
#include "lispregs.h"
#include "interr.h"
#include "genesis/static-symbols.h"
#include "genesis/primitive-objects.h"
#include "thread.h"
#include "arch.h"
#include "code.h"
#include "private-cons.inc"
#include "getallocptr.h"
#include "genesis/gc-tables.h" // for leaf_obj_widetag_p

/* So you need to debug? */
#if 0
#define PRINTNOISE
#define DEBUG_SPACE_PREDICATES
#define DEBUG_SCAVENGE_VERBOSE
#define DEBUG_COPY_VERBOSE
#endif

lispobj *from_space;
lispobj *from_space_free_pointer;

lispobj *new_space;
lispobj *new_space_free_pointer;

/* This does nothing. It's only to satisfy a reference from gc-common. */
char gc_coalesce_string_literals = 0;

boolean gc_active_p = 0;

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
gc_general_alloc(__attribute__((unused)) void* ignore,
                 sword_t bytes,
                 __attribute__((unused)) int page_type) {
    lispobj *new=new_space_free_pointer;
    new_space_free_pointer+=(bytes/N_WORD_BYTES);
    return new;
}

lispobj  copy_unboxed_object(lispobj object, sword_t nwords) {
    return gc_copy_object(object, nwords, 0, 0);
}
lispobj  copy_possibly_large_object(lispobj object, sword_t nwords,
                                    __attribute__((unused)) void* region,
                                    __attribute__((unused)) int page_type) {
    return gc_copy_object(object, nwords, 0, 0);
}

/*
 * This flag is needed for compatibility with gencgc.
 * In theory, it says to splat a nonzero byte pattern over newly allocated
 * memory before giving the block to Lisp, to verify that Lisp is able to deal
 * with non-pre-zeroed memory.
 * In practice, that's not how cheneygc works.
 */
char gc_allocate_dirty = 0;

#if 0
static void verify_range(int purified, lispobj *base, lispobj *end);
void show_spaces()
{
    fprintf(stderr, " R/O     = %10p:%10p\n", (void*)READ_ONLY_SPACE_START, read_only_space_free_pointer);
    fprintf(stderr, " static  = %10p:%10p\n", (void*)STATIC_SPACE_START, static_space_free_pointer);
    fprintf(stderr, " dynamic = %10p:%10p\n", current_dynamic_space, dynamic_space_free_pointer);
    struct thread* th = all_threads;
    fprintf(stderr, " stack   = %10p:%10p\n",
            th->control_stack_start,
            access_control_stack_pointer(th));
}
void verify_heap(int purified)
{
    show_spaces();
    verify_range(purified, (lispobj*)READ_ONLY_SPACE_START, read_only_space_free_pointer);
    verify_range(purified, (lispobj*)STATIC_SPACE_START, static_space_free_pointer);
    if (!purified)
    verify_range(0, current_dynamic_space, dynamic_space_free_pointer);
}
#endif

/* Note: The generic GC interface we're implementing passes us a
 * last_generation argument. That's meaningless for us, since we're
 * not a generational GC. So we ignore it. */
int n_gcs;
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
    sigset_t old;
    struct thread *th=get_sb_vm_thread();

#ifdef PRINTNOISE
    printf("[Collecting garbage ... \n");

    getrusage(RUSAGE_SELF, &start_rusage);
    gettimeofday(&start_tv, (struct timezone *) 0);
#endif

    gc_active_p = 1;

    /* it's possible that signals are blocked already if this was called
     * from a signal handler (e.g. with the sigsegv gc_trigger stuff) */
    block_blockable_signals(&old);

    current_static_space_free_pointer = static_space_free_pointer;

    /* Set up from space and new space pointers. */

    // ++n_gcs; fprintf(stderr, "[%d] pre-verify:\n", n_gcs); verify_heap(0);
    from_space = current_dynamic_space;
    from_space_free_pointer = get_alloc_pointer();

#ifdef PRINTNOISE
    fprintf(stderr,"from_space = %lx\n",
            (unsigned long) current_dynamic_space);
#endif
    if (current_dynamic_space == (lispobj *) DYNAMIC_0_SPACE_START)
        new_space = (lispobj *)DYNAMIC_1_SPACE_START;
    else if (current_dynamic_space == (lispobj *) DYNAMIC_1_SPACE_START)
        new_space = (lispobj *) DYNAMIC_0_SPACE_START;
    else {
        lose("GC lossage.  Current dynamic space is bogus!");
    }
    new_space_free_pointer = new_space;

    /* Scavenge all of the roots. */
#ifdef PRINTNOISE
    printf("Scavenging interrupt contexts ...\n");
#endif
    scavenge_interrupt_contexts(th);

#ifdef PRINTNOISE
    printf("Scavenging interrupt handlers ...\n");
#endif
    scavenge(lisp_sig_handlers, NSIG);

#ifdef PRINTNOISE
    printf("Scavenging the control stack ...\n");
#endif
    scavenge_control_stack(th);

    scav_binding_stack((lispobj*)th->binding_stack_start,
                       (lispobj*)get_binding_stack_pointer(th),
                       0);

    heap_scavenge(((lispobj *)STATIC_SPACE_START),
                  current_static_space_free_pointer);
    scavenge(&lisp_package_vector, 1);

    /* Scavenge newspace. */
#ifdef PRINTNOISE
    printf("Scavenging new space (%d bytes) ...\n",
           (int)((new_space_free_pointer - new_space) * sizeof(lispobj)));
#endif
    scavenge_newspace();


#if defined(DEBUG_PRINT_GARBAGE)
    print_garbage(from_space, from_space_free_pointer);
#endif

    scan_binding_stack();

    smash_weak_pointers();
    gc_dispose_private_pages();
    cull_weak_hash_tables(weak_ht_alivep_funs);

    /* Flip spaces. */
#ifdef PRINTNOISE
    printf("Flipping spaces ...\n");
#endif

    /* Maybe FIXME: it's possible that we could significantly reduce
     * RSS by zeroing the from_space or madvise(MADV_DONTNEED) or
     * similar os-dependent tricks here */
    os_zero((os_vm_address_t) from_space,
            (os_vm_size_t) dynamic_space_size);

    current_dynamic_space = new_space;
    set_alloc_pointer((lispobj)new_space_free_pointer);

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

    gc_active_p = 0;
    // fprintf(stderr, "post-verify:\n"); verify_heap(0);

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

    do {
        /*      printf("here=%lx, new_space_free_pointer=%lx\n",
                here,new_space_free_pointer); */
        next = new_space_free_pointer;
        heap_scavenge(here, next);
        here = next;
    } while (new_space_free_pointer > here ||
             (test_weak_triggers(0, 0) && new_space_free_pointer > here));
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
                pointer = native_pointer(object);
                header = *pointer;
                type = header_widetag(header);
                nwords = (sizetab[type])(pointer);
                break;
            default: nwords=1;  /* shut yer whinging, gcc */
            }
        } else {
            type = header_widetag(object);
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

lispobj *
search_dynamic_space(void *pointer)
{
    lispobj *start = (lispobj *) current_dynamic_space;
    lispobj *end = get_alloc_pointer();
    if ((pointer < (void *)start) || (pointer >= (void *)end))
        return NULL;
    return gc_search_space(start, pointer);
}

/* initialization.  if gc_init can be moved to after core load, we could
 * combine these two functions */

void
gc_init(void)
{
    gc_common_init();
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
    if (addr < (os_vm_address_t)get_alloc_pointer())
        lose("set_auto_gc_trigger: tried to set gc trigger too low! (%ld < 0x%08lx)",
             (unsigned long)dynamic_usage,
             (unsigned long)((os_vm_address_t)get_alloc_pointer()
                             - (os_vm_address_t)current_dynamic_space));

    if (dynamic_usage > dynamic_space_size)
        lose("set_auto_gc_trigger: tried to set gc trigger too high! (0x%08lx)",
             (unsigned long)dynamic_usage);
    length = os_trunc_size_to_page(dynamic_space_size - dynamic_usage);

    // range has to fall entirely within either semispace
    uword_t semispace_0_end = DYNAMIC_0_SPACE_START + dynamic_space_size;
    uword_t semispace_1_end = DYNAMIC_1_SPACE_START + dynamic_space_size;
    uword_t end = (uword_t)addr + length - 1;
    if (((uword_t)addr >= DYNAMIC_0_SPACE_START && end < semispace_0_end) ||
        ((uword_t)addr >= DYNAMIC_1_SPACE_START && end < semispace_1_end)) {
#if defined(SUNOS) || defined(SOLARIS)
        os_invalidate(addr, length);
#else
        os_protect(addr, length, 0);
#endif
    } else {
        lose("auto_gc_trigger can't protect %p..%p (not owned)\n",
             (void*)addr, (char*)end-1);
    }
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

#if defined(SUNOS) || defined(SOLARIS)
    /* don't want to force whole space into swapping mode... */
    os_validate(NOT_MOVABLE, addr, length);
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
                (char*)addr <((char *)current_dynamic_space + dynamic_space_size));
    }
}

boolean
cheneygc_handle_wp_violation(os_context_t *context, void *addr)
{
    if(!foreign_function_call_active && gc_trigger_hit(addr)){
        struct thread *thread=get_sb_vm_thread();
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

void gc_show_pte(lispobj obj)
{
    printf("unimplemented\n");
}

sword_t scav_code_header(lispobj *where, lispobj header)
{
    struct code *code = (struct code *) where;
    sword_t n_header_words = code_header_words(code);

    /* Scavenge the boxed section of the code data block. */
    scavenge(where + 2, n_header_words - 2);
    /* And scavenge any 'self' pointers pointing outside of the object */
    for_each_simple_fun(i, fun, code, 1, {
        if (simplefun_is_wrapped(fun)) {
            lispobj target_fun = fun_taggedptr_from_self(fun->self);
            lispobj new = target_fun;
            scavenge(&new, 1);
            if (new != target_fun) fun->self = fun_self_from_taggedptr(new);
        }
    })

    return code_total_nwords(code);
}

#if 0
static boolean in_stack_range_p(lispobj ptr)
{
    struct thread* th = all_threads;
    return (ptr >= (uword_t)th->control_stack_start &&
            ptr < (uword_t)access_control_stack_pointer(th));
}

static boolean valid_space_p(int purified, lispobj ptr)
{
    if ((ptr >= READ_ONLY_SPACE_START && ptr < (uword_t)read_only_space_free_pointer) ||
        (ptr >= STATIC_SPACE_START && ptr < (uword_t)static_space_free_pointer) ||
        (!purified &&
         ((ptr >= (uword_t)current_dynamic_space && ptr < (uword_t)dynamic_space_free_pointer) ||
          in_stack_range_p(ptr))))
        return 1;
    return 0;
}

void check_ptr(int purified, lispobj* where, lispobj ptr)
{
    if (!is_lisp_pointer(ptr)) return;

    if (!valid_space_p(purified, ptr)) {
        fprintf(stderr, "Sus' pointer %p @ %p outside expected ranges\n",
                (void*)ptr, where);
        return;
    }

    // must be properly tagged
    if (lowtag_of(ptr) == LIST_POINTER_LOWTAG) {
        gc_assert(is_cons_half(CONS(ptr)->car));
        gc_assert(is_cons_half(CONS(ptr)->cdr));
    } else {
        int widetag = widetag_of(native_pointer(ptr));
        if (LOWTAG_FOR_WIDETAG(widetag) != lowtag_of(ptr) && !in_stack_range_p(ptr))
            lose("Widetag/lowtag mismatch on %p", (void*)ptr);
    }
}

#define CHECK_PTR(x,y) check_ptr(purified,x,y)

static void
verify_range(int purified, lispobj *base, lispobj *end)
{
    lispobj* where = base;
    int len, i;
    lispobj layout;
    for ( ; where < end ; where += OBJECT_SIZE(*where, where) ) {
        if (is_cons_half(*where)) {
          CHECK_PTR(where+0, where[0]);
          CHECK_PTR(where+1, where[1]);
        } else switch (widetag_of(where)) {
          case INSTANCE_WIDETAG:
            layout = instance_layout(where);
            if (layout) {
                gc_assert(layoutp(layout));
                len = instance_length(*where);
                struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
                for (i=0; i<len; ++i)
                    if (bitmap_logbitp(i, bitmap)) CHECK_PTR(where+i+1, where[1+i]);
            }
            break;
          case CODE_HEADER_WIDETAG:
            len = code_header_words((struct code*)where);
            for(i=2; i<len; ++i) CHECK_PTR(where+i, where[i]);
            break;
          case FDEFN_WIDETAG:
            CHECK_PTR(where+1, where[1]);
            CHECK_PTR(where+2, where[2]);
            CHECK_PTR(where+3, fdefn_callee_lispobj((struct fdefn*)where));
            break;
          case CLOSURE_WIDETAG:
          case FUNCALLABLE_INSTANCE_WIDETAG:
            // Scan the closure's trampoline word.
            CHECK_PTR(where+1, fun_taggedptr_from_self(where[1]));
            len = SHORT_BOXED_NWORDS(*where);
            for(i=2; i<=len; ++i) CHECK_PTR(where+i, where[i]);
            break;
          default:
            if (!leaf_obj_widetag_p(widetag_of(where))) {
                int size = sizetab[widetag_of(where)](where);
                for(i=1; i<size; ++i) CHECK_PTR(where+i, where[i]);
            }
          }
    }
}

void dump_space_to_file(lispobj* where, lispobj* limit, char* pathname)
{
    FILE *f;
    f = fopen(pathname, "w");
    while (where < limit) {
        fprintf(f, "%p: %x\n", where, *where);
        where += OBJECT_SIZE(*where, where);
    }
    fprintf(f, "--\n");
    fclose(f);
}
#endif
