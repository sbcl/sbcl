/*
 * garbage collection
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

#ifndef _GC_H_
#define _GC_H_

#include "genesis/sbcl.h"
#include "gc-assert.h"
#include "gc-typedefs.h"
#include "globals.h"
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#if defined LISP_FEATURE_SB_THREAD && !defined LISP_FEATURE_SB_SAFEPOINT
# define THREADS_USING_GCSIGNAL 1
#endif

#if defined LISP_FEATURE_GENERATIONAL && !defined LISP_FEATURE_C_STACK_IS_CONTROL_STACK
# define GENCGC_IS_PRECISE 1
#else
# define GENCGC_IS_PRECISE 0
#endif

extern void gc_init(void);
extern void collect_garbage(generation_index_t last_gen);

extern void set_auto_gc_trigger(os_vm_size_t usage);
extern void clear_auto_gc_trigger(void);

extern bool maybe_gc(os_context_t *context);
void gc_heap_exhausted_error_or_lose (sword_t available, sword_t requested) never_returns;

extern bool gc_active_p;
extern int sb_sprof_enabled;

extern os_vm_size_t bytes_consed_between_gcs;

// flags passed to verify_heap().
// The low 4 bits supply the generation number and 'raise' flag
#define VERIFY_VERBOSE    (1<<4)
#define VERIFY_PRE_GC     (1<<5)
#define VERIFY_POST_GC    (1<<6)
/* AGGRESSIVE = always call valid_tagged_pointer_p() on pointers. */
#define VERIFY_AGGRESSIVE (1<<7)
#define VERIFY_TAGS       (1<<8)
/* QUICK = skip most tests. This is intended for use when GC is believed
 * to be correct per se (i.e. not for debugging GC), and so the verify
 * pass executes more quickly */
#define VERIFY_QUICK      (1<<9)
/* FINAL = warn about pointers from heap space to non-heap space.
 * Such pointers would normally be ignored and do not get flagged as failure.
 * This can be used in conjunction with QUICK, AGGRESSIVE, or neither. */
#define VERIFY_FINAL      (1<<10)
#define VERIFY_DONT_LOSE  (1<<11)

/* VERIFYING_foo indicates internal state, not a caller's option */
/* GENERATIONAL implies formatted objects, but there are ranges of objects
 * that are not generational (static space)
 * so there are no page protection checks performed for pointers from objects
 * in such ranges */
#define VERIFYING_GENERATIONAL (1<<12)
/* UNFORMATTED implies that this is not a range of objects
 * but rather a range of pointers such as a binding stack, TLS,
 * lisp signal handler array, or other similar array */
#define VERIFYING_UNFORMATTED (1<<13)
#define VERIFY_PRINT_HEADER_ON_FAILURE (1<<14)

extern generation_index_t verify_gens;
#define MAX_ERR_OBJS 5
struct verify_state {
    lispobj* object_addr;
    lispobj object_header;
    uword_t flags;
    generation_index_t object_gen;
    generation_index_t min_pointee_gen;
#ifdef LISP_FEATURE_MARK_REGION_GC
    _Atomic(int) nerrors;
#else
    int nerrors;
#endif
    lispobj err_objs[5];
};
void hexdump_spaces(struct verify_state*, char *reason, char *pathname);
int verify_heap(lispobj*, int flags);
int hexdump_and_verify_heap(lispobj*, int flags);

page_index_t gc_find_freeish_pages(page_index_t *restart_page_ptr, sword_t nbytes,
                                   int page_type, generation_index_t gen);

extern void tlsf_dump_pool(void*, void*, char *pathname);

extern generation_index_t gencgc_oldest_gen_to_gc;
extern page_index_t gencgc_alloc_start_page;
extern bool conservative_stack;
extern lispobj lisp_init_function;
char *page_card_mark_string(page_index_t page, char *result);
/* These functions for looking up lisp objects given a pointer are for use
 * from the C runtime, but should not be directly called from Lisp.
 * The Lisp-callable ones may take additional steps, like ensuring mutual-exclusivity
 * with GC, which are at best not needed or at worst incorrect (such as attempting
 * recursive lock acquisition on a non-recursive lock) if called from C */
extern int valid_tagged_pointer_p(lispobj);
extern lispobj *component_ptr_from_pc(char *pc);

#define linkage_cell_taggedptr(index) fun_taggedptr_from_self(linkage_space[index])
extern void sweep_linkage_space();

extern page_index_t page_table_pages;
/* Find the page index within the page_table for the given
 * address. Return -1 on failure. */
static inline page_index_t find_page_index(void *addr)
{
    if (addr >= (void*)DYNAMIC_SPACE_START) {
        // Do not directly assign this computation to a variable of type 'page_index_t'
        // because an excessively high address could chop high bits off, making the
        // result look in range by accident.
        uword_t index = ((uintptr_t)addr -
                         (uintptr_t)DYNAMIC_SPACE_START) / GENCGC_PAGE_BYTES;
        if (index < (uword_t)page_table_pages)
            return index;
    }
    return (-1);
}

/* Calculate the start address for the given page number. */
static inline char *page_address(page_index_t page_num)
{
    return (void*)(DYNAMIC_SPACE_START + (page_num * GENCGC_PAGE_BYTES));
}

#define PAGE_INDEX_FMT "d"
#include "immobile-space.h" // provides dummy stubs if #-immobile-space
#ifdef LISP_FEATURE_MARK_REGION_GC
#include "pmrgc-impl.h"
#include "mark-region.h"
#elif defined LISP_FEATURE_GENCGC
#include "gencgc-impl.h"
#else
#error "GC selector not defined"
#endif

#if (defined LISP_FEATURE_DARWIN || defined LISP_FEATURE_LINUX) \
  && defined LISP_FEATURE_SB_THREAD
#define MEASURE_STOP_THE_WORLD_PAUSE
#endif

#ifdef LISP_FEATURE_X86_64
#define GC_SAFEPOINT_PAGE_ADDR (void*)(gc_card_mark-BACKEND_PAGE_BYTES)
#define GC_SAFEPOINT_TRAP_ADDR (void*)(gc_card_mark-8)
#endif
void remap_free_pages(page_index_t,page_index_t);
void page_remap_as_type(int,void*,sword_t);
extern lispobj *dynamic_space_code_from_pc(char *pc);

#ifdef TRACE_MMAP_SYSCALLS
extern void set_page_type_impl(struct page*, int newval);
#define set_page_type(pte, newval) set_page_type_impl(&(pte), newval)
#else
#define set_page_type(pte, newval) pte.type = newval
#endif
extern void prepare_pages(bool commit, page_index_t start, page_index_t end,
                          int page_type, generation_index_t);

/* One byte per page indicating data vs code, but only if the OS generally
 * prohibits memory from being writable + executable. There's probably a way to
 * squeeze a bit into the 'type' field of the page table, but it's clearer to
 * have this externally so that page type 0 remains as "free" */
#ifdef LISP_FEATURE_DARWIN_JIT
extern _Atomic(char) *page_execp;
static inline void set_page_executable(page_index_t i, bool val) { page_execp[i] = val; }
#endif

extern void remset_union(lispobj);
extern lispobj remset_transfer_list;
void remember_all_permgen();
extern int permgen_remset_count;
extern lispobj permgen_remset[];

#endif /* _GC_H_ */
