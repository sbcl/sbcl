/* Test that layout pointer in a compact instance header
 * isn't overlooked by scan_boxed_root_cards_spanning()
 * or update_writeprotection() */

#include <stdio.h>
#include "genesis/sbcl.h"
#include "gc.h"
#include "genesis/instance.h"
#include "thread.h"

extern struct generation generations[];
extern os_vm_size_t bytes_allocated;

// Create a 2-word instance and a 6-word funcallable instance.
// Ensure they are on different GC cards.
static void make_instances(int page_type, generation_index_t gen, lispobj result[2])
{
    page_index_t page = 0, last;
    // Passing SINGLE_OBJECT_FLAG avoids combining with a previous page
    last = gc_find_freeish_pages(&page, GENCGC_PAGE_BYTES,
                                 SINGLE_OBJECT_FLAG | page_type, gen);
    gc_assert(last == page);
    lispobj* where = (void*)page_address(page);
    page_table[page].type = page_type;
    page_table[page].gen = 1;
    gc_assert(page_table[page].scan_start_offset_ == 0);
    page_table[page].words_used_ = (2 * GENCGC_CARD_BYTES) >> WORD_SHIFT;
    page_table[page].need_zerofill = 1;
    bytes_allocated += 2 * GENCGC_CARD_BYTES;
    generations[gen].bytes_allocated += 2 * GENCGC_CARD_BYTES;

    // Create the ordinary instance, total length 2 words
    lispobj instance = make_lispobj(where, INSTANCE_POINTER_LOWTAG);
    where[0] = (1 << INSTANCE_LENGTH_SHIFT) | INSTANCE_WIDETAG;
    where[1] = 0;
    where += 2;

    // Fill to the end of the card
    int filler_nwords = (GENCGC_CARD_BYTES >> WORD_SHIFT) - 2;
    *where = make_filler_header(filler_nwords);

    // Assert that no tagged pointer can point to the filler
    // fprintf(stderr, "Filler @ %p: %"OBJ_FMTX"\n", where, *where);
    int lowtag;
    for (lowtag=1; lowtag<=LOWTAG_MASK; ++lowtag) {
        lispobj addr = make_lispobj(where, lowtag);
        gc_assert(!plausible_tag_p(addr));
    }

    where += filler_nwords;

    // Create the funcallable instance, total length 6 words
    lispobj funinstance = make_lispobj(where, FUN_POINTER_LOWTAG);
    where[0] = (1 << N_WIDETAG_BITS) | FUNCALLABLE_INSTANCE_WIDETAG;
    where[1] = where[2] = where[3] = where[4] = where[5] = 0;
    where += 6;

    // Fill to the end of the card
    filler_nwords = (GENCGC_CARD_BYTES >> WORD_SHIFT) - 6;
    *where = make_filler_header(filler_nwords);
    where += filler_nwords;

    result[0] = instance;
    result[1] = funinstance;
}

static void perform_gc(lispobj* stackptr)
{
    extern void close_current_thread_tlab();
    extern void garbage_collect_generation(generation_index_t, int, void*);

    gc_active_p = 1;
    gc_close_collector_regions(0); // TODO: should be THREAD_PAGE_FLAG
    close_current_thread_tlab();

    /* We have to clobber the dynamic space codeblob tree because ordinarily
     * it gets smashed at the start of collect_garbage(), and since this test
     * bypasses collect_garbage(), the GC might have problems later.
     * (Not exactly sure how, but "header not OK for code page" was triggering)
     * So, handily, since there are no concurrency issues in this test,
     * it doesn't matter that the GC can't utilize the tree. It falls back
     * to scanning code pages linearly in preserve_pointer which is fine. */
    SYMBOL(DYNSPACE_CODEBLOB_TREE)->value = NIL;

    verify_heap(stackptr, VERIFY_PRE_GC);
    garbage_collect_generation(0, 0, stackptr);
    gc_active_p = 0;
}

void run_cardmark_test(int page_type,
                       lispobj young_layout, lispobj old_layout,
                       lispobj* stackptr)
{
    lispobj instances[2];
    make_instances(page_type, 1, instances);
    lispobj instance = instances[0], funinstance = instances[1];
    fprintf(stderr, "allocated @ %p and %p\n", (void*)instance, (void*)funinstance);
    long instance_card = addr_to_card_index((void*)instance);
    long funinstance_card = addr_to_card_index((void*)funinstance);

    // Assert something about the card indices
    fprintf(stderr, "page = %d, cards = %d and %d\n",
            (int)find_page_index((void*)instance),
            (int)instance_card, (int)funinstance_card);
    gc_assert(funinstance_card == instance_card+1);

    perform_gc(stackptr);
    // the cards containing the instances should be unmarked
    gc_assert(gc_card_mark[instance_card] == CARD_UNMARKED);
    gc_assert(gc_card_mark[funinstance_card] == CARD_UNMARKED);
    instance_layout(native_pointer(instance)) = old_layout;
    instance_layout(native_pointer(funinstance)) = old_layout;
    gc_card_mark[instance_card] = CARD_MARKED;
    gc_card_mark[funinstance_card] = CARD_MARKED;
    perform_gc(stackptr);
    // should have gotten unmarked
    gc_assert(gc_card_mark[instance_card] == CARD_UNMARKED);
    gc_assert(gc_card_mark[funinstance_card] == CARD_UNMARKED);

    // Test 1: regular instance
    instance_layout(native_pointer(instance)) = young_layout;
    gc_card_mark[instance_card] = CARD_MARKED;
    gc_card_mark[funinstance_card] = CARD_MARKED; // "spuriously" marked
    perform_gc(stackptr);
    // should stay marked
    gc_assert(gc_card_mark[instance_card] == CARD_MARKED);
    // BOXED pages can undirty at the card granularity, but MIXED pages can't
    if (page_type == PAGE_TYPE_BOXED)
        gc_assert(gc_card_mark[funinstance_card] == CARD_UNMARKED);

    instance_layout(native_pointer(instance)) = old_layout;
    perform_gc(stackptr);
    // both cards are clean
    gc_assert(gc_card_mark[instance_card] == CARD_UNMARKED);
    gc_assert(gc_card_mark[funinstance_card] == CARD_UNMARKED);

    // Test 2: funinstance
    instance_layout(native_pointer(funinstance)) = young_layout;
    gc_card_mark[funinstance_card] = CARD_MARKED;
    gc_card_mark[instance_card] = CARD_MARKED; // "spuriously" marked
    perform_gc(stackptr);
    // should stay marked
    gc_assert(gc_card_mark[funinstance_card] == CARD_MARKED);
    // BOXED pages can undirty at the card granularity, but MIXED pages can't
    if (page_type == PAGE_TYPE_BOXED)
        gc_assert(gc_card_mark[instance_card] == CARD_UNMARKED);

    funinstance_layout(native_pointer(funinstance)) = old_layout;
    perform_gc(stackptr);
    // both cards are clean
    gc_assert(gc_card_mark[instance_card] == CARD_UNMARKED);
    gc_assert(gc_card_mark[funinstance_card] == CARD_UNMARKED);
    printf("PASS\n");
}

int compact_instance_layout_pointer_test(lispobj arg)
{
    // Capture the approximate stack pointer on entry
    lispobj* stackptr = &arg;
    struct vector* layouts = (void*)native_pointer(arg);
    lispobj young_layout = layouts->data[0];
    lispobj old_layout = layouts->data[1];
    gc_assert(immobile_obj_generation(native_pointer(young_layout))==0);
    // Don't want sticky marks messing up the test.
    // It suffices to check that there are no register contexts.
    struct thread* self = get_sb_vm_thread();
    gc_assert(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX, self) == 0);
    printf("PAGE_TYPE_MIXED:\n"); fflush(stdout);
    run_cardmark_test(PAGE_TYPE_MIXED, young_layout, old_layout, stackptr);
    printf("PAGE_TYPE_BOXED:\n"); fflush(stdout);
    run_cardmark_test(PAGE_TYPE_BOXED, young_layout, old_layout, stackptr);
    return 0;
}
