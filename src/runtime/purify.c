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
#include "gc-private.h"
#include "thread.h"
#include "genesis/gc-tables.h"
#include "genesis/primitive-objects.h"
#include "genesis/static-symbols.h"
#include "genesis/layout.h"
#include "genesis/defstruct-description.h"
#include "genesis/hash-table.h"
#include "code.h"
#include "getallocptr.h"

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

/* These are private to purify, not to be confused with the external symbols
 * named 'read_only_space_free_pointer', respectively 'static_space' */
static lispobj *read_only_free, *static_free;

static lispobj *pscav(lispobj *addr, long nwords, boolean constant);

#define LATERBLOCKSIZE 1020

static struct
later {
    struct later *next;
    // slightly denser packing vs a struct of a lispobj* and an int
    lispobj *ptr[LATERBLOCKSIZE];
    int count[LATERBLOCKSIZE];
} *later_blocks = NULL;
static long later_count = 0;


static boolean forwarded_p(lispobj obj)
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
    gc_assert((nwords & 1) == 0);
    if(constantp) {
        if(read_only_free + nwords >= (lispobj *)READ_ONLY_SPACE_END) {
            lose("Ran out of read-only space while purifying!");
        }
        ret=read_only_free;
        read_only_free+=nwords;
    } else {
        if(static_free + nwords >= (lispobj *)STATIC_SPACE_END) {
            lose("Ran out of static space while purifying!");
        }
        ret=static_free;
        static_free+=nwords;
    }
    return ret;
}


/* Enqueue <where,count> into later_blocks */
static void
pscav_later(lispobj *where, long count)
{
    struct later *new;

    if (later_blocks == NULL || later_count == LATERBLOCKSIZE) {
        new  = (struct later *)malloc(sizeof(struct later));
        new->next = later_blocks;
        later_blocks = new;
        later_count = 0;
    }

    later_blocks->count[later_count] = count;
    later_blocks->ptr[later_count] = where;
    ++later_count;
}

//FILE *xlog;
//#define XLOG(old,new) fprintf(xlog, "%x > %x\n", old, (uword_t)new)
#define XLOG(dumm1,dummy2)

static lispobj
ptrans_boxed(lispobj thing, lispobj header, boolean constant)
{
    /* Allocate it */
    lispobj *old = native_pointer(thing);
    long nwords = sizetab[header_widetag(header)](old);
    lispobj *new = newspace_alloc(nwords,constant);
    XLOG(thing, new);

    /* Copy it. */
    memcpy(new, old, nwords * sizeof(lispobj));

    /* Deposit forwarding pointer. */
    lispobj result = make_lispobj(new, lowtag_of(thing));
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
    constant = 0;
    lispobj info = LAYOUT(instance_layout(native_pointer(thing)))->_info;
    if (instancep(info)) {
        lispobj pure = ((struct defstruct_description*)native_pointer(info))->pure;
        if (pure != NIL && pure != T) {
            gc_abort();
            return NIL; /* dummy value: return something ... */
        }
        constant = (pure == T);
    }
    return ptrans_boxed(thing, header, constant);
}

static lispobj
ptrans_fdefn(lispobj thing, lispobj header)
{
    /* Allocate it */
    lispobj *old = native_pointer(thing);
    long nwords = sizetab[header_widetag(header)](old);
    lispobj *new = newspace_alloc(nwords, 0);    /* inconstant */
    XLOG(thing, new);

    /* Copy it. */
    memcpy(new, old, nwords * sizeof(lispobj));

    /* Deposit forwarding pointer. */
    lispobj result = make_lispobj(new, lowtag_of(thing));
    *old = result;

    /* Scavenge the function. */
    struct fdefn *fdefn = (struct fdefn *)new;
    lispobj oldfn = fdefn->fun;
    pscav(&fdefn->fun, 1, 0);
    if ((char *)oldfn + FUN_RAW_ADDR_OFFSET == fdefn->raw_addr)
        fdefn->raw_addr = (char *)fdefn->fun + FUN_RAW_ADDR_OFFSET;

    return result;
}

static lispobj
ptrans_unboxed(lispobj thing, lispobj header)
{
    /* Allocate it */
    lispobj *old = native_pointer(thing);
    long nwords = sizetab[header_widetag(header)](old);
    lispobj *new = newspace_alloc(nwords, 1);     /* always constant */
    XLOG(thing, new);

    /* copy it. */
    memcpy(new, old, nwords * sizeof(lispobj));

    /* Deposit forwarding pointer. */
    lispobj result = make_lispobj(new, lowtag_of(thing));
    *old = result;

    return result;
}

static lispobj
ptrans_vector(lispobj thing, boolean boxed, boolean constant)
{
    struct vector *vector = VECTOR(thing);
    long nwords = sizetab[header_widetag(vector->header)]((lispobj*)vector);

    lispobj *new = newspace_alloc(nwords, (constant || !boxed));
    XLOG(thing, new);
    memcpy(new, vector, nwords * sizeof(lispobj));

    lispobj result = make_lispobj(new, lowtag_of(thing));
    vector->header = result;

    if (boxed)
        pscav(new, nwords, constant);

    return result;
}

static lispobj
ptrans_code(lispobj thing)
{
    struct code *code = (struct code *)native_pointer(thing);
    long nwords = code_total_nwords(code);

    struct code *new = (struct code *)newspace_alloc(nwords,1); /* constant */
    XLOG(thing, new);

    memcpy(new, code, nwords * sizeof(lispobj));

    lispobj result = make_lispobj(new, OTHER_POINTER_LOWTAG);
    uword_t displacement = result - thing;

#if defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64
    // Fixup absolute jump tables. These aren't recorded in code->fixups
    // because we don't need to denote an arbitrary set of places in the code.
    // The count alone suffices. A GC immediately after creating the code
    // could cause us to observe some 0 words here. Those should be ignored.
    lispobj* jump_table = code_jumptable_start(new);
    int count = jumptable_count(jump_table);
    int i;
    for (i = 1; i < count; ++i)
        if (jump_table[i]) jump_table[i] += displacement;
#endif
    /* Put in forwarding pointers for all the functions. */
    for_each_simple_fun(i, newfunc, new, 1, {
        lispobj* old = (lispobj*)((char*)newfunc - displacement);
        *old = make_lispobj(newfunc, FUN_POINTER_LOWTAG);
        pscav(&newfunc->self, 1, 1); // and fix the self-pointer now
    });

    int n_funs = code_n_funs(code);
    /* Stick in a forwarding pointer for the code object. */
    /* This smashes the header, so do it only after reading n_funs */
    *(lispobj *)code = result;

    int n_later_words = 1 + n_funs * CODE_SLOTS_PER_SIMPLE_FUN;
    /* Scavenge the constants excluding the ones that will be done later. */
    lispobj* from = &new->debug_info + n_later_words;
    lispobj* end = (lispobj*)new + code_header_words(new);
    pscav(from, end - from, 1);
    /* Arrange to scavenge the debug info and simple-fun metadata later. */
    pscav_later(&new->debug_info, n_later_words);
    return result;
}

static lispobj
ptrans_func(lispobj thing, lispobj header)
{
    /* Thing can either be a function header,
     * a closure, or a funcallable-instance. If it's a closure
     * or a funcallable-instance, we do the same as ptrans_boxed.
     * Otherwise we have to do something strange, 'cause it is buried
     * inside a code object. */

    if (header_widetag(header) == SIMPLE_FUN_WIDETAG) {

        /* We can only end up here if the code object has not been
         * scavenged, because if it had been scavenged, forwarding pointers
         * would have been left behind for all the entry points. */

        struct simple_fun *function = (struct simple_fun *)native_pointer(thing);
        lispobj code = fun_code_tagged((lispobj*)function);

        /* This will cause the function's header to be replaced with a
         * forwarding pointer. */

        ptrans_code(code);

        /* So we can just return that. */
        return function->header;
    } else {
        /* It's some kind of closure-like thing. */
        lispobj *old = native_pointer(thing);
        long nwords = sizetab[header_widetag(header)](old);

        /* Allocate the new one.  FINs *must* not go in read_only
         * space.  Closures can; they never change */

        lispobj *new = newspace_alloc
            (nwords,(header_widetag(header)!=FUNCALLABLE_INSTANCE_WIDETAG));
        XLOG(thing, new);

        /* Copy it. */
        memcpy(new, old, nwords * sizeof(lispobj));

        /* Deposit forwarding pointer. */
        lispobj result = make_lispobj(new, lowtag_of(thing));
        *old = result;

        /* Scavenge it. */
        pscav(new, nwords, 0);

        return result;
    }
}

static lispobj
ptrans_returnpc(lispobj thing, lispobj header)
{
    /* Find the corresponding code object. */
    lispobj code = thing - HeaderValue(header)*sizeof(lispobj);

    /* Make sure it's been transported. */
    lispobj new = *native_pointer(code);
    if (!forwarded_p(new))
        new = ptrans_code(code);

    /* Maintain the offset: */
    return new + (thing - code);
}

#define WORDS_PER_CONS ALIGN_UP(sizeof(struct cons) / sizeof(lispobj), 2)

static lispobj
ptrans_list(lispobj thing, boolean constant)
{
    struct cons *old, *new, *orig;
    long length;

    orig = (struct cons *) newspace_alloc(0,constant);
    //fprintf(xlog, "%x > %x", thing, (uword_t)orig);
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
    } while (listp(thing) &&
             dynamic_pointer_p(thing) &&
             !(forwarded_p(*native_pointer(thing))));

    /* Scavenge the list we just copied. */
    //fprintf(xlog, "*%d\n", (int)length);
    pscav((lispobj *)orig, length * WORDS_PER_CONS, constant);

    return make_lispobj(orig, LIST_POINTER_LOWTAG);
}

static lispobj
ptrans_otherptr(lispobj thing, lispobj header, boolean constant)
{
    int widetag = header_widetag(header);
    switch (widetag) {
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
      case COMPLEX_VECTOR_WIDETAG:
      case COMPLEX_ARRAY_WIDETAG:
        return ptrans_boxed(thing, header, constant);

      case VALUE_CELL_WIDETAG:
      case WEAK_POINTER_WIDETAG:
        return ptrans_boxed(thing, header, 0);

      case SYMBOL_WIDETAG:
        return ptrans_boxed(thing, header, 0);

      case SIMPLE_VECTOR_WIDETAG:
        return ptrans_vector(thing, 1, constant);

      case CODE_HEADER_WIDETAG:
        return ptrans_code(thing);

      case RETURN_PC_WIDETAG:
        return ptrans_returnpc(thing, header);

      case FDEFN_WIDETAG:
        return ptrans_fdefn(thing, header);

      default:
        if (other_immediate_lowtag_p(widetag) &&
            specialized_vector_widetag_p(widetag))
            return ptrans_vector(thing, 0, constant);
        fprintf(stderr, "Invalid widetag: %d\n", header_widetag(header));
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

    while (nwords > 0) {
        thing = *addr;
        int widetag = header_widetag(thing);
        if (is_lisp_pointer(thing)) {
            /* It's a pointer. Is it something we might have to move? */
            if (dynamic_pointer_p(thing)) {
                /* Maybe. Have we already moved it? */
                thingp = native_pointer(thing);
                header = *thingp;
                if (is_lisp_pointer(header) && forwarded_p(header))
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
        else if (widetag == SINGLE_FLOAT_WIDETAG) {
            count = 1;
        }
#endif
        else if (thing & FIXNUM_TAG_MASK) {
            /* It's an other immediate. Maybe the header for an unboxed */
            /* object. */
            switch (widetag) {
              case BIGNUM_WIDETAG:
              case SINGLE_FLOAT_WIDETAG:
              case DOUBLE_FLOAT_WIDETAG:
#ifdef LONG_FLOAT_WIDETAG
              case LONG_FLOAT_WIDETAG:
#endif
              case SAP_WIDETAG:
                /* It's an unboxed simple object. */
                count = ALIGN_UP(HeaderValue(thing)+1, 2);
                break;

              case SIMPLE_VECTOR_WIDETAG:
                // addr[0] : header
                //     [1] : vector length
                //     [2] : element[0] = high-water mark
                //     [3] : element[1] = rehash bit
                if (vector_flagp(thing, VectorAddrHashing))
                    addr[3] = make_fixnum(1); // just flag it for rehash
                count = 2;
                break;

              case CODE_HEADER_WIDETAG:
                gc_abort(); /* no code headers in static space */
                break;

              case SIMPLE_FUN_WIDETAG:
              case RETURN_PC_WIDETAG:
                /* We should never hit any of these, 'cause they occur
                 * buried in the middle of code objects. */
                gc_abort();
                break;

              case WEAK_POINTER_WIDETAG:
                /* Weak pointers get preserved during purify, 'cause I
                 * don't feel like figuring out how to break them. */
                pscav(addr+1, 2, constant);
                count = WEAK_POINTER_NWORDS;
                break;

              case FDEFN_WIDETAG:
                /* We have to handle fdefn objects specially, so we
                 * can fix up the raw function address. */
                count = pscav_fdefn((struct fdefn *)addr);
                break;

              case INSTANCE_WIDETAG:
                {
                lispobj layout = instance_layout(addr);
                lispobj layout_header = *native_pointer(layout);
                if (is_lisp_pointer(layout_header)) {
                    gc_assert(forwarded_p(layout_header));
                    layout = layout_header;
                }
                if (!layoutp(layout)) lose("Bad layout in instance: %p %x", addr, layout);
                struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
                gc_assert(bitmap.nwords >= 1);
                long nslots = instance_length(*addr);
                int index;
                for (index = 0; index < nslots ; index++)
                    // logically treat index 0 (layout) as a tagged slot
                    if (index == 0 || bitmap_logbitp(index, bitmap))
                        pscav((addr+1) + index, 1, constant);
                count = 1 + (nslots | 1);
                }
                break;

              default:
                if (other_immediate_lowtag_p(widetag) &&
                    specialized_vector_widetag_p(widetag))
                    count = sizetab[header_widetag(thing)](addr);
                else
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

extern void dump_space_to_file(lispobj* where, lispobj* limit, char* pathname);
int
purify(lispobj static_roots, lispobj read_only_roots)
{
    lispobj *clean;
    long count, i;
    struct thread *thread;

    if(all_threads->next) {
        /* FIXME: there should be _some_ sensible error reporting
         * convention.  See following comment too */
        fprintf(stderr,"Can't purify when more than one thread exists\n");
        fflush(stderr);
        return 0;
    }
    // verify_heap(0); fprintf(stderr, "pre-verify passed\n");

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

    dynamic_space_purify_pointer = get_alloc_pointer();

    read_only_end = read_only_free = read_only_space_free_pointer;
    static_end = static_free = static_space_free_pointer;

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
    pscav(lisp_sig_handlers, NSIG, 0);
    pscav(&lisp_package_vector, 1, 0);

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

#ifdef PRINTNOISE
    printf(" static");
    fflush(stdout);
#endif
    clean = (lispobj *)STATIC_SPACE_START;
    do {
        while (clean != static_free)
            clean = pscav(clean, static_free - clean, 0);
        struct later* laters = later_blocks;
        count = later_count;
        later_blocks = NULL;
        later_count = 0;
        while (laters != NULL) {
            for (i = 0; i < count; i++)
                pscav(laters->ptr[i], laters->count[i], 1);
            struct later* next = laters->next;
            free(laters);
            laters = next;
            count = LATERBLOCKSIZE;
        }
    } while (clean != static_free || later_blocks != NULL);

#ifdef PRINTNOISE
    printf(" cleanup");
    fflush(stdout);
#endif

    os_zero((os_vm_address_t) current_dynamic_space, dynamic_space_size);

    /* Zero the stack. */
    os_zero((os_vm_address_t) access_control_stack_pointer(all_threads),
            (os_vm_size_t)
            ((all_threads->control_stack_end -
              access_control_stack_pointer(all_threads)) * sizeof(lispobj)));

    /* It helps to update the heap free pointers so that free_heap can
     * verify after it's done. */
    read_only_space_free_pointer = read_only_free;
    static_space_free_pointer = static_free;

    set_alloc_pointer((lispobj)current_dynamic_space);
    set_auto_gc_trigger(bytes_consed_between_gcs);

    /* Blast away instruction cache */
    os_flush_icache((os_vm_address_t)READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE);
    os_flush_icache((os_vm_address_t)STATIC_SPACE_START, STATIC_SPACE_SIZE);

#ifdef PRINTNOISE
    // verify_heap(1);
    printf(" done]\n");
    fflush(stdout);
#endif
    //fclose(xlog);
    return 0;
}
#else /* LISP_FEATURE_GENCGC */
int
purify(lispobj __attribute__((unused)) static_roots,
       lispobj __attribute__((unused)) read_only_roots)
{
    lose("purify called for GENCGC. This should not happen.");
}
#endif /* LISP_FEATURE_GENCGC */
