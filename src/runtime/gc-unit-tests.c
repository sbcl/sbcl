#include "gencgc.c"

#define MAX_PAGES_FOR_TEST 20

/* Testing approach for adjust_obj_ptes():
 * - Allocate a large object that is a smidgen smaller or larger
 *   than an integral number of pages (for varying values of "smidgen").
 * - Capture the page table entries so we know what they would
 *   contain if the object were initially allocated as such.
 * - Clean up the page tables (as though the allocation never happened).
 * - Allocate an arbitrarily sized larger object, then shrink it down
 *   to the reference size, asserting that:
 *   - the page table entries look exactly as if the object
 *     had been created at the smaller size.
 *   - trailing pages are empty.
 *   - the number of bytes freed is correct.
 */

void test_adjust_obj_ptes()
{
    void shrink_obj_test(int ending_size, int created_type,
                         struct page *expected_result);
    unsigned char card_table[1];

    // Mock out the dynamic space. Always allocate one extra page in
    // the page table as a sentinel.
    struct page expected_result[1+MAX_PAGES_FOR_TEST];
    page_table_pages = MAX_PAGES_FOR_TEST;
    posix_memalign((void**)&DYNAMIC_SPACE_START, GENCGC_PAGE_BYTES,
                   MAX_PAGES_FOR_TEST * GENCGC_PAGE_BYTES);
    gc_card_table_nbits = 1;
    gc_card_table_mask = 1;
    gc_card_mark = card_table;

    struct alloc_region test_region;
    int npages, fuzz;
    // For varying object sizes of "Npages +/- fuzz", create the
    // reference object. Fuzz is quantized to the size of one cons cell,
    // i.e. 2*N_WORD_BYTES, the smallest allocatable thing.
    for (npages = 1 ; npages <= 8; ++npages)
        for (fuzz = -3; fuzz <= 3; ++fuzz) {
            int request = npages * GENCGC_PAGE_BYTES + (N_WORD_BYTES*2)*fuzz;
            // Mock out initial state: region is freshly initialized, linear
            // scan for free space from start of heap,
            // and pick the generation.
            gc_init_region(&test_region);
            RESET_ALLOC_START_PAGES();
            test_region.last_page = -1;
            gc_alloc_generation = SCRATCH_GENERATION;

            // Wipe out the page table and the allocation counts,
            // then create the reference object.
            page_table = calloc(1+page_table_pages, sizeof(struct page));
            generation_index_t gen;
            for (gen=0; gen < NUM_GENERATIONS; ++gen)
              generations[gen].bytes_allocated = 0;
            bytes_allocated = 0;
            void *result = gc_alloc_large(request, PAGE_TYPE_UNBOXED, &test_region, 0);

            // Assert some things about the reference object.
            gc_assert(result == (void*)DYNAMIC_SPACE_START);
            gc_assert((int)bytes_allocated == request);
            gc_assert((int)generations[gc_alloc_generation].bytes_allocated == request);

            // Capture the exact state of each page: kind, bytes used, etc.
            memcpy(expected_result, page_table,
                   page_table_pages * sizeof (struct page));

            // Delete mock page table, then run the shrinkage test two ways:
            // (1) object is "moved" [sic] from boxed to unboxed page,
            // (2) object was initially on unboxed page, stays on unboxed page.
            free(page_table);
            shrink_obj_test(request, PAGE_TYPE_MIXED, expected_result);
            shrink_obj_test(request, PAGE_TYPE_UNBOXED, expected_result);
        }
}

void shrink_obj_test(int ending_size, int initial_type,
                     struct page *expected_result)
{
    int npages, fuzz, page;
    struct alloc_region test_region;

    // For various sizes at least as large as 'ending size', create an
    // object at that size, then shrink the object to 'ending_size' and
    // assert that the pages look as if it was initially allocated
    // at the desired size.
    for (npages = 1 ; npages <= 10; ++npages)
        for (fuzz = -4; fuzz <= 4; ++fuzz) {
            int initial_size = npages * GENCGC_PAGE_BYTES + (N_WORD_BYTES*2)*fuzz;
            // Test only makes sense where the original size exceeds
            // or is equal to the ending size.
            if (initial_size >= ending_size) {
                gc_init_region(&test_region);
                RESET_ALLOC_START_PAGES();
                test_region.last_page = -1;

                // Start with a fresh page table
                page_table = calloc(1+page_table_pages, sizeof(struct page));
                from_space = gc_alloc_generation = 2;
                void *result = gc_alloc_large(initial_size, initial_type, &test_region, 0);
                // We're in trouble if pages other than expected were gotten
                gc_assert(result == (void*)DYNAMIC_SPACE_START);

                // Execute the function under test: move the object FROM generation 2
                // TO the SCRATCH_GENERATION, and change its page type from whatever
                // 'initial_type' was to UNBOXED.
                sword_t freed = adjust_obj_ptes(find_page_index(result),
                                                ending_size/N_WORD_BYTES,
                                                SCRATCH_GENERATION,
                                                SINGLE_OBJECT_FLAG | PAGE_TYPE_UNBOXED);

                // After changing the size, all pages should have the correct
                // number of bytes used, and the bytes freed should be as expected.
                gc_assert(freed == (initial_size - ending_size));
                for (page=0; page<MAX_PAGES_FOR_TEST; ++page) {
                    gc_assert(page_table[page].words_used_ ==
                              expected_result[page].words_used_);
                    gc_assert(page_table[page].scan_start_offset_ ==
                              expected_result[page].scan_start_offset_);
                    gc_assert(page_table[page].type ==
                              expected_result[page].type);
                    // generation is only relevant for in-use pages
                    if (!page_free_p(page))
                        gc_assert(page_table[page].gen ==
                                  expected_result[page].gen);
                }
            }
        }
}

void run_gencgc_tests()
{
    // Assert that widetags do not satisfy is_lisp_pointer
    gc_assert(!is_lisp_pointer(CHARACTER_WIDETAG));
    gc_assert(!is_lisp_pointer(SIMPLE_VECTOR_WIDETAG));
    // Assert that INSTANCE_WIDETAG is 1 bit different from FUNCALLABLE_INSTANCE
    gc_assert((INSTANCE_WIDETAG | (1<<FUNINSTANCE_SELECTOR_BIT_NUMBER))
              == FUNCALLABLE_INSTANCE_WIDETAG);
    gc_assert(instanceoid_widetag_p(INSTANCE_WIDETAG));
    gc_assert(instanceoid_widetag_p(FUNCALLABLE_INSTANCE_WIDETAG));
    test_adjust_obj_ptes();
}

int main()
{
    void run_gencgc_tests();
    printf(";;; Running GC tests\n");
    run_gencgc_tests();
    printf(";;; Success\n");
}
