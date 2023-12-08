/* -*- Mode: C -*- */

/* GC private-use list allocator.
 *
 * A private-use list is a list of private-uses conses,
 * which are just like ordinary conses except that the cdrs
 * do not have a pointer lowtag and the last cdr is 0, not NIL.
 *
 * With gencgc we steal a GC card having generation = 0 so that it
 * won't look like a root page. Optionally, malloc / free can be used
 * for testing a fallback algorithm that works with cheneygc.
 * The fallback is susceptible to deadlock with multiple threads
 * and therefore not suitable for production use.
 * (if a foreign call entails malloc, and malloc potentially acquires a
 * mutex, and the Lisp thread making the foreign call is stopped for GC,
 * then GC can't proceed)
 *
 * Logically this code is common to both GCs, but it requires some
 * knowledge of gencgc's structures. You might think it should be split
 * into two wholly separate implementation files, but the single file
 * facilitates exercising the malloc/free-based implementation
 * with either GC.
 */

#define PRIVATE_CONS_DEBUG 0

#ifdef LISP_FEATURE_CHENEYGC

static struct cons* private_cons_chain;
static int private_cons_n_avail;

#define CHUNKSIZE 4096
static struct cons* private_cons_impl()
{
    if (!private_cons_n_avail) {
        struct cons* new = malloc(CHUNKSIZE);
        private_cons_n_avail = (CHUNKSIZE / sizeof (struct cons)) - 1;
        /* Treating 'new' as an array of 'private_cons_n_avail' conses,
         * we allocate backwards toward the 0th element.
         * Element 0 itself holds the list of chunks to subsequently free. */
        new->car = 0; // unused
        new->cdr = (lispobj)private_cons_chain;
        private_cons_chain = new;
        if (PRIVATE_CONS_DEBUG)
            fprintf(stderr, "%d conses @ %p\n", (1+private_cons_n_avail), new);
    }
    return private_cons_chain + private_cons_n_avail--;
}

static void release_pages_impl()
{
    struct cons* list = private_cons_chain, *next;
    for ( ; list ; list = next ) {
        if (PRIVATE_CONS_DEBUG) fprintf(stderr, "Freeing %p\n", list);
        next = (struct cons*)list->cdr;
        free(list);
    }
    private_cons_chain = NULL;
    private_cons_n_avail = 0;
}

#else

static page_index_t private_cons_page_chain = -1;
#define GC_PRIVATE_CONS_GENERATION 6

static struct cons* private_cons_impl()
{
    page_index_t page = private_cons_page_chain;
    page_bytes_t bytes_used;
    struct cons* cons;

    if (page >= 0 && (bytes_used = page_bytes_used(page)) < GENCGC_PAGE_BYTES) {
        cons = (struct cons*)(page_address(page) + bytes_used);
    } else {
#ifdef LISP_FEATURE_MARK_REGION_GC
        struct allocator_state alloc_start = get_alloc_start_page(PAGE_TYPE_UNBOXED);
        page = try_allocate_large(GENCGC_PAGE_BYTES, PAGE_TYPE_UNBOXED, 0,
                                  &alloc_start, page_table_pages, NULL);
        set_alloc_start_page(PAGE_TYPE_UNBOXED, alloc_start);
        /* TODO: Should figure out what will trigger "heap exhausted" errors.
         * Probably not this though. */
        if (page == -1) lose("Ran out of pages for GC-private conses");
#else
        page = get_alloc_start_page(PAGE_TYPE_UNBOXED);
        page_index_t last_page __attribute__((unused)) =
            gc_find_freeish_pages(&page, GENCGC_PAGE_BYTES,
                                  SINGLE_OBJECT_FLAG | PAGE_TYPE_UNBOXED,
                                  GC_PRIVATE_CONS_GENERATION);
        // See question about last_page in gc_alloc_large
        set_alloc_start_page(PAGE_TYPE_UNBOXED, page);
        gc_assert(last_page == page);
#endif
        page_table[page].gen = GC_PRIVATE_CONS_GENERATION;
        set_page_type(page_table[page], PAGE_TYPE_UNBOXED);
        // UNBOXED pages do not generally required zero-fill.
        prepare_pages(1, page, page, PAGE_TYPE_UNBOXED, -1);
        struct cons* page_header = (struct cons*)page_address(page);
        if (PRIVATE_CONS_DEBUG)
            fprintf(stderr, "GC-private page @ %p\n", page_header);
        page_index_t tail = private_cons_page_chain;
        page_header->car = 0; // unused
        page_header->cdr = (lispobj)(tail >= 0 ? page_address(tail) : 0);
        private_cons_page_chain = page;
        bytes_used = 2*N_WORD_BYTES; // one cons (so far)
        cons = page_header + 1;
    }
    set_page_bytes_used(page, bytes_used + 2*N_WORD_BYTES);
    return cons;
}

static void release_pages_impl()
{
    struct cons *list, *next;
    if (private_cons_page_chain >= 0) {
          for (list = (struct cons*)page_address(private_cons_page_chain) ;
               list ; list = next) {
            page_index_t index = find_page_index(list);
            next = (struct cons*)list->cdr; // read prior to decommitting (if we do that)
            if (PRIVATE_CONS_DEBUG)
                fprintf(stderr, "Freeing GC-private page @ %p (index %ld)\n",
                        list, (long)index);
            set_page_need_to_zero(index, 1);
            set_page_bytes_used(index, 0);
            reset_page_flags(index);
        }
        private_cons_page_chain = -1;
    }
}

#endif

static struct cons* private_cons_recycle_list;

uword_t gc_private_cons(uword_t car, uword_t cdr)
{
    struct cons* cons = private_cons_recycle_list;
    if (cons)
        private_cons_recycle_list = (struct cons*)cons->cdr;
    else
        cons = private_cons_impl();
    cons->car = car;
    cons->cdr = cdr;
#if PRIVATE_CONS_DEBUG
    if (cdr) fprintf(stderr, "private_cons(%p,%p)=%p\n", (void*)car, (void*)cdr, cons);
#endif
    return (uword_t)cons;
}

/* Push all the conses in 'list' onto the recycle list. */
void gc_private_free(struct cons* list)
{
#if PRIVATE_CONS_DEBUG
    { int n = 0; struct cons* tail = list; while(tail) ++n, tail = (void*)tail->cdr;
      if (n>1) fprintf(stderr, "private_free(%p){%d}\n", list, n); }
#endif
    struct cons* head = list;
    while (list->cdr)
        list = (struct cons*)list->cdr;
    list->cdr = (lispobj)private_cons_recycle_list;
    private_cons_recycle_list = head;
}

/* Give back all the memory used by private cons cells
 * to either the GC allocator or the malloc implementation. */
void gc_dispose_private_pages()
{
    private_cons_recycle_list = 0;
    release_pages_impl();
}
