/* -*- Mode: C -*- */

static int
is_in_stack_space(lispobj ptr)
{
    /* For space verification: Pointers can be valid if they point
     * to a thread stack space.  This would be faster if the thread
     * structures had page-table entries as if they were part of
     * the heap space. */
    /* Actually, no, how would that be faster?
     * If you have to examine thread structures, you have to examine
     * them all. This demands something like a binary search tree */
    struct thread *th;
    for_each_thread(th) {
        if ((th->control_stack_start <= (lispobj *)ptr) &&
            (th->control_stack_end >= (lispobj *)ptr)) {
            return 1;
        }
    }
    return 0;
}
static int is_in_static_space(void* ptr) {
    return (uword_t)ptr >= STATIC_SPACE_START && (lispobj*)ptr < static_space_free_pointer;
}

static boolean card_markedp(void* addr)
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (immobile_space_p((lispobj)addr))
        return !immobile_card_protected_p(addr);
#endif
    return gc_card_mark[addr_to_card_index(addr)] != CARD_UNMARKED;
}

char *page_card_mark_string(page_index_t page, char *result)
{
    long card = addr_to_card_index(page_address(page));
    if (cardseq_all_marked_nonsticky(card))
        result[0] = '*', result[1] = 0;
    else if (!cardseq_any_marked(card))
        result[0] = '-', result[1] = 0;
    else {
        int i;
        for(i=0; i<CARDS_PER_PAGE; ++i)
        switch (gc_card_mark[card+i] & MARK_BYTE_MASK) {
        case CARD_MARKED: result[i] = '*'; break;
        case CARD_UNMARKED: result[i] = '-'; break;
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        case STICKY_MARK: result[i] = 'S'; break;
#else
        case WP_CLEARED_AND_MARKED: result[i] = 'd'; break; // "d" is for dirty
#endif
        default: result[i] = '?'; break; // illegal value
        }
        result[CARDS_PER_PAGE] = 0;
    }
    return result;
}

#define PRINT_HEADER_ON_FAILURE 2048

// Check a single pointer. Return 1 if we should stop verifying due to too many errors.
// (Otherwise continue showing errors until then)
// NOTE: This function can produces false failure indications,
// usually related to dynamic space pointing to the stack of a
// dead thread, but there may be other reasons as well.
static void note_failure(lispobj thing, lispobj *where, struct verify_state *state,
                         char *str)
{
    if (state->flags & PRINT_HEADER_ON_FAILURE) {
        if (state->flags & VERIFY_PRE_GC) fprintf(stderr, "pre-GC failure\n");
        if (state->flags & VERIFY_POST_GC) fprintf(stderr, "post-GC failure\n");
        state->flags &= ~PRINT_HEADER_ON_FAILURE;
    }
    if (state->object_addr) {
        lispobj obj = compute_lispobj(state->object_addr);
        page_index_t pg = find_page_index(state->object_addr);
        fprintf(stderr, "Ptr %p @ %"OBJ_FMTX" (lispobj %"OBJ_FMTX",pg%d) sees %s\n",
                (void*)thing, (uword_t)where, obj, (int)pg, str);
        // Record this in state->err_objs if possible
        int i;
        for(i=0; i<MAX_ERR_OBJS; ++i)
            if (!state->err_objs[i]) {
                state->err_objs[i] = (uword_t)state->object_addr;
                break;
            }
    } else {
        fprintf(stderr, "Ptr %p @ %"OBJ_FMTX" sees %s\n", (void*)thing, (uword_t)where, str);
    }
}

static int
verify_pointer(lispobj thing, lispobj *where, struct verify_state *state)
{
    extern int valid_lisp_pointer_p(lispobj);

    /* Strict containment: no pointer from a heap space may point
     * to anything outside of a heap space. */
    // boolean strict_containment = state->flags & VERIFY_FINAL;

#define FAIL_IF(cond, why) \
    if (cond) { if (++state->nerrors > 25) return 1; note_failure(thing,where,state,why); }

    if (!is_lisp_pointer(thing)) {
        FAIL_IF(!is_lisp_immediate(thing), "strange non-pointer");
        return 0;
    }
    // if (strict_containment && !gc_managed_heap_space_p(thing)) GC_WARN("non-Lisp memory");
    page_index_t source_page_index = find_page_index(where);
    page_index_t target_page_index = find_page_index((void*)thing);
    if (!(target_page_index >= 0 || immobile_space_p(thing))) return 0; // can't do much with it
    if ((state->flags & VERIFY_TAGS) && target_page_index >= 0) {
        if (listp(thing)) {
            FAIL_IF(!(is_cons_half(CONS(thing)->car) && is_cons_half(CONS(thing)->cdr)),
                    "non-cons");
        } else {
            FAIL_IF(LOWTAG_FOR_WIDETAG(widetag_of(native_pointer(thing))) != lowtag_of(thing),
                    "incompatible widetag");
        }
    }
    generation_index_t to_gen =
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        points_to_asm_code_p((uword_t)thing)?
        gc_gen_of(make_lispobj((void*)asm_routines_start,OTHER_POINTER_LOWTAG),0):
#endif
        gc_gen_of(thing, ARTIFICIALLY_HIGH_GEN);
    if (to_gen < state->min_pointee_gen) state->min_pointee_gen = to_gen;
    if (state->flags & VERIFY_QUICK) return 0;
    if (target_page_index >= 0) {
        // If it's within the dynamic space it should point to a used page.
        FAIL_IF(page_free_p(target_page_index), "free page");
        FAIL_IF(!(page_table[target_page_index].type & OPEN_REGION_PAGE_FLAG)
                && (thing & (GENCGC_PAGE_BYTES-1)) >= page_bytes_used(target_page_index),
                "unallocated space");
    } else {
        // The object pointed to must not have been discarded as garbage.
        FAIL_IF(!other_immediate_lowtag_p(*native_pointer(thing)), "trashed object");
    }
    // Must not point to a forwarding pointer
    FAIL_IF(*native_pointer(thing) == FORWARDING_HEADER, "forwarding ptr");
    // Forbid pointers from R/O space into a GCed space
    FAIL_IF((READ_ONLY_SPACE_START <= (uword_t)where && where < read_only_space_free_pointer),
            "dynamic space from RO space");
    // Card marking invariant check, but only if the source of pointer is a heap object
    if (header_widetag(state->object_header) == CODE_HEADER_WIDETAG
        && ! is_in_static_space(state->object_addr)
        && to_gen < state->object_gen) {
        // two things must be true:
        // 1. the card containing the code must be marked
        FAIL_IF(!card_markedp(state->object_addr), "younger obj from WP'd code header page");
        // 2. the object header must be marked as written
        if (!header_rememberedp(state->object_header))
            lose("code @ %p (g%d). word @ %p -> %"OBJ_FMTX" (g%d)",
                 state->object_addr, state->object_gen, where, thing, to_gen);
    } else if ((state->flags & VERIFYING_GENERATIONAL) && to_gen < state->object_gen
               && source_page_index >= 0) {
        /* The WP criteria are:
         *  - CONS marks the exact card since it can't span cards
         *  - SIMPLE-VECTOR marks the card containing the cell with the old->young pointer.
         *  - Everything else marks the object header -OR- the card with the pointer.
         *    (either/or because Lisp marks the header card,
         *     but the collector marks the cell's card.) */
        int marked = card_markedp(where)
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        || (state->object_header
            && header_widetag(state->object_header) != SIMPLE_VECTOR_WIDETAG
            && card_markedp(state->object_addr))
#endif
        ;
        FAIL_IF(!marked, "younger obj from WP page");
    }
    int valid;
    if (state->flags & VERIFY_AGGRESSIVE) // Extreme paranoia mode
        valid = valid_lisp_pointer_p(thing);
    else {
        /* Efficiently decide whether 'thing' is plausible.
         * This MUST NOT use properly_tagged_descriptor_p() which
         * assumes a known good object base address, and would
         * "dangerously" scan a code component for embedded funs. */
        valid = plausible_tag_p(thing);
    }
    /* If 'thing' points to a stack, we can only hope that the stack
     * frame is ok, or the object at 'where' is unreachable. */
    FAIL_IF(!valid && !is_in_stack_space(thing), "junk");
    return 0;
}
#define CHECK(pointer, where) if (verify_pointer(pointer, where, state)) return 1

/* Return 0 if good, 1 if bad */
static int verify_headered_object(lispobj* object, sword_t nwords,
                                  struct verify_state *state)
{
    long i;
    int widetag = widetag_of(object);
    if (instanceoid_widetag_p(widetag)) {
        lispobj layout = layout_of(object);
        if (layout) {
            CHECK(layout, object);
            struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
            if (widetag_of(object) == FUNCALLABLE_INSTANCE_WIDETAG) {
#ifndef LISP_FEATURE_COMPACT_INSTANCE_HEADER
                gc_assert(bitmap.bits[0] == (sword_t)-4);
#endif
            }
            if (lockfree_list_node_layout_p(LAYOUT(layout))) {
                struct list_node* node = (void*)object;
                lispobj next = node->_node_next;
                if (fixnump(next) && next)
                  CHECK(next | INSTANCE_POINTER_LOWTAG, &node->_node_next);
            }
            for (i=0; i<(nwords-1); ++i)
                if (bitmap_logbitp(i, bitmap)) CHECK(object[1+i], object+1+i);
        }
        return 0;
    }
    if (widetag == CODE_HEADER_WIDETAG) {
        struct code *code = (struct code *)object;
        gc_assert(fixnump(object[1])); // boxed size, needed for code_header_words()
        sword_t nheader_words = code_header_words(code);
        /* Verify the boxed section of the code data block */
        state->min_pointee_gen = ARTIFICIALLY_HIGH_GEN;
#ifdef LISP_FEATURE_UNTAGGED_FDEFNS
        {
        lispobj* pfdefn = code->constants + code_n_funs(code) * CODE_SLOTS_PER_SIMPLE_FUN;
        lispobj* end = pfdefn + code_n_named_calls(code);
        for ( ; pfdefn < end ; ++pfdefn)
            if (*pfdefn) CHECK(*pfdefn | OTHER_POINTER_LOWTAG, pfdefn);
        }
#endif
        for (i=2; i <nheader_words; ++i) CHECK(object[i], object+i);
        // Check the SIMPLE-FUN headers
        for_each_simple_fun(i, fheaderp, code, 1, {
#if defined LISP_FEATURE_COMPACT_INSTANCE_HEADER
            lispobj __attribute__((unused)) layout = funinstance_layout((lispobj*)fheaderp);
            gc_assert(!layout || layout == LAYOUT_OF_FUNCTION);
#elif defined LISP_FEATURE_64_BIT
            gc_assert((fheaderp->header >> 32) == 0);
#endif
        });
#if 0 // this looks redundant. It's checked with each pointer, no?
        boolean rememberedp = header_rememberedp(code->header);
        /* The remembered set invariant is that an object is marked "written"
         * if and only if either it points to a younger object or is pointed
         * to by a register or stack. (The pointed-to case assumes that the
         * very next instruction on return from GC would store an old->young
         * pointer into that object). Non-compacting GC does not have the
         * "only if" part of that, nor does pre-GC verification because we
         * don't test the generation of the newval when storing into code. */
        if (is_in_static_space(object)) { }
        else if (compacting_p() && (state->flags & VERIFY_POST_GC) ?
            (state->min_pointee_gen < state->object_gen) != rememberedp :
            (state->min_pointee_gen < state->object_gen) && !rememberedp)
            lose("object @ %p is gen%d min_pointee=gen%d %s",
                 (void*)state->tagged_object, state->object_gen, state->min_pointee_gen,
                 rememberedp ? "written" : "not written");
#endif
        return 0;
    }
    if (widetag == SYMBOL_WIDETAG) {
        struct symbol* s = (void*)object;
        CHECK(s->value, &s->value);
        CHECK(s->fdefn, &s->fdefn);
        CHECK(s->info, &s->info);
        CHECK(decode_symbol_name(s->name), &s->name);
        return 0;
    }
    if (widetag == FDEFN_WIDETAG) {
        struct fdefn* f = (void*)object;
        CHECK(f->name, &f->name);
        CHECK(f->fun, &f->fun);
        CHECK(decode_fdefn_rawfun(f), (lispobj*)&f->raw_addr);
        return 0;
    }
    for (i=1; i<nwords; ++i) CHECK(object[i], object+i);
    return 0;
}

static __attribute__((unused)) boolean acceptable_filler_cons_p(lispobj* where)
{
    if (where[0] == 0 && where[1] == 0) return 1;
    // These "conses" can result from bignum multiplication-
    // trailing insigificant sign bits which get chopped.
    if (where[0] == (uword_t)-1 && where[1] == (uword_t)-1) return 1;
    if (where[0] == (uword_t)-1 && where[1] == 0) return 1;
    return 0;
}
static int verify_range(lispobj* start, lispobj* end, struct verify_state* state)
{
    lispobj* where = start;
    if (state->flags & VERIFYING_GENERATIONAL && find_page_index(start)>=0) {
        page_index_t page = find_page_index(start);
        if (page_table[page].type == PAGE_TYPE_CONS)
            gc_assert(page_words_used(page) <=  MAX_CONSES_PER_PAGE*CONS_SIZE);
    }
    if ((state->flags & VERIFYING_UNFORMATTED)) {
        while (where < end) {
            if (*where != NO_TLS_VALUE_MARKER) {
                int result = verify_pointer(*where, where, state);
                if (result) return result;
            }
            ++where;
        }
        return 0;
    }
    while (where < end) {
        int widetag = is_header(*where) ? header_widetag(*where) : LIST_POINTER_LOWTAG;
        /* Technically we should wait until after performing the widetag validity
         * tests before calling the sizer.  Otherwise the lossage message would
         * not be as good as it could be. I guess that failure doesn't happen much */
        sword_t nwords = object_size(where);
        state->object_addr = where;
        state->object_header = is_cons_half(*where) ? 0 : *where;
        if (state->flags & VERIFYING_GENERATIONAL) {
            page_index_t pg = find_page_index(where);
            state->object_gen = pg >= 0 ? page_table[pg].gen :
              gc_gen_of((lispobj)where, ARTIFICIALLY_HIGH_GEN);
#ifdef LISP_FEATURE_PPC64
            // Cons fillers (two words of all 1s) cause failure of
            // the default verification logic, so brute-force skip them
            // regardless of whether the page type is PAGE_TYPE_CONS.
            if (*where == (uword_t)-1 && where[1] == (uword_t)-1) {
                where +=2;
                continue;
            }
#endif
            if (widetag != FILLER_WIDETAG && pg >= 0) {
                    // Assert proper page type
                    if (state->object_header) // is not a cons
                        gc_assert(page_table[pg].type != PAGE_TYPE_CONS);
#ifdef LISP_FEATURE_USE_CONS_REGION
                    else if (page_table[pg].type != PAGE_TYPE_CONS) {
                      if (is_cons_half(where[0]))
                          gc_assert(acceptable_filler_cons_p(where));
                    }
#endif
                    if (widetag == CODE_HEADER_WIDETAG) {
                        if (!is_code(page_table[pg].type))
                            lose("object @ %p is code on non-code page", where);
                    } else if (widetag == FUNCALLABLE_INSTANCE_WIDETAG) {
                        // where these reside depends on the architecture
                    } else {
                        if (is_code(page_table[pg].type))
                            lose("object @ %p is non-code on code page", where);
                    }
            }
        }
        if (!state->object_header) {
            if (verify_pointer(where[0], where+0, state) ||
                verify_pointer(where[1], where+1, state)) break;
        } else if (widetag == FILLER_WIDETAG) { // do nothing
        } else if (!(other_immediate_lowtag_p(widetag) && LOWTAG_FOR_WIDETAG(widetag))) {
            lose("Unhandled widetag %"OBJ_FMTX" @ %p", *where, where);
        } else if (leaf_obj_widetag_p(widetag)) {
#ifdef LISP_FEATURE_UBSAN
            if (specialized_vector_widetag_p(widetag)) {
                if (is_lisp_pointer(object[1])) {
                    struct vector* bits = (void*)native_pointer(object[1]);
                    if (header_widetag(bits->header) != SIMPLE_BIT_VECTOR_WIDETAG)
                      lose("bad shadow bits for %p", where);
                    gc_assert(header_widetag(bits->header) == SIMPLE_BIT_VECTOR_WIDETAG);
                    gc_assert(vector_len(bits) >= vector_len((struct vector*)object));
                }
            }
#endif
            boolean strict_containment = state->flags & VERIFY_FINAL;
            if (strict_containment && gencgc_verbose && widetag == SAP_WIDETAG && where[1])
                fprintf(stderr, "\nStrange SAP %p -> %p\n", where, (void*)where[1]);
        } else {
            if (verify_headered_object(where, nwords, state)) break;
        }
        where += nwords;
    }
    return 0;
}

static int verify(lispobj start, lispobj* end, struct verify_state* state, int flags)
{
    int savedflags = state->flags;
    state->flags |= flags;
    int result = verify_range((lispobj*)start, end, state);
    state->flags = savedflags;
    return result;
}

extern void save_gc_crashdump(char *, lispobj*);
/* Return the number of verification errors found.
 * You might want to use that as a deciding factor for dump the heap
 * to a file (which takes time, and generally isn't needed).
 * But if a particular verification fails, then do dump it */
int verify_heap(__attribute__((unused)) lispobj* cur_thread_approx_stackptr,
                int flags)
{
    int verbose = gencgc_verbose | ((flags & VERIFY_VERBOSE) != 0);

    struct verify_state state;
    memset(&state, 0, sizeof state);
    state.flags = flags;

    if (verbose)
        fprintf(stderr,
                flags & VERIFY_PRE_GC ? "Verify before GC" :
                flags & VERIFY_POST_GC ? "Verify after GC(%d)" :
                "Heap check", // if called at a random time
                (int)(flags>>16)); // generation number
    else
        state.flags |= PRINT_HEADER_ON_FAILURE;

#ifdef LISP_FEATURE_IMMOBILE_SPACE
#  ifdef __linux__
    // Try this verification if immobile-space was compiled with extra debugging.
    // But weak symbols don't work on macOS.
    extern void __attribute__((weak)) check_text_pages();
    if (&check_text_pages) check_text_pages();
#  endif
    if (verbose)
        fprintf(stderr, " [immobile]");
    if (verify(FIXEDOBJ_SPACE_START,
               fixedobj_free_pointer, &state,
               flags | VERIFYING_GENERATIONAL)) goto out;
    if (verify(TEXT_SPACE_START,
               text_space_highwatermark, &state,
               flags | VERIFYING_GENERATIONAL)) goto out;
#endif
    struct thread *th;
    if (verbose)
        fprintf(stderr, " [threads]");
    state.object_addr = 0;
    state.object_gen = 0;
    for_each_thread(th) {
        if (verify((lispobj)th->binding_stack_start,
                   (lispobj*)get_binding_stack_pointer(th), &state,
                   VERIFYING_UNFORMATTED)) goto out;
        if (verify((lispobj)&th->lisp_thread,
                   (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th), &state,
                   VERIFYING_UNFORMATTED))
            goto out;
    }
    if (verbose)
        fprintf(stderr, " [RO]");
    if (verify(READ_ONLY_SPACE_START, read_only_space_free_pointer, &state, 0)) goto out;
#ifdef LISP_FEATURE_METASPACE
    if (verify(METASPACE_START, (lispobj*)READ_ONLY_SPACE_END, &state, 0)) goto out;
#endif
    if (verbose)
        fprintf(stderr, " [static]");
    // Just don't worry about NIL, it's seldom the problem
    // if (verify(NIL_SYMBOL_SLOTS_START, (lispobj*)NIL_SYMBOL_SLOTS_END, &state, 0)) goto out;
    if (verify(STATIC_SPACE_OBJECTS_START, static_space_free_pointer, &state, 0)) goto out;
    if (verbose)
        fprintf(stderr, " [dynamic]");
    state.flags |= VERIFYING_GENERATIONAL;
    walk_generation((uword_t(*)(lispobj*,lispobj*,uword_t))verify_range,
                    -1, (uword_t)&state);
    if (verbose && state.nerrors==0) fprintf(stderr, " passed\n");
 out:
    if (state.nerrors && !(flags & VERIFY_DONT_LOSE)) {
        // dump_spaces(&state, "verify failed");
        lose("Verify failed: %d errors", state.nerrors);
    }
    return state.nerrors;
}

void gc_show_pte(lispobj obj)
{
    char marks[1+CARDS_PER_PAGE];
    page_index_t page = find_page_index((void*)obj);
    if (page>=0) {
        printf("page %"PAGE_INDEX_FMT" base %p gen %d type %x ss %p used %x",
               page, page_address(page), page_table[page].gen, page_table[page].type,
               page_scan_start(page), page_bytes_used(page));
        if (page_starts_contiguous_block_p(page)) printf(" startsblock");
        if (page_ends_contiguous_block_p(page, page_table[page].gen)) printf(" endsblock");
        printf(" (%s)\n", page_card_mark_string(page, marks));
        return;
    }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    page = find_text_page_index((void*)obj);
    if (page>=0) {
        lispobj* text_page_scan_start(low_page_index_t page);
        int gens = text_page_genmask[page];
        char genstring[9];
        int i;
        for (i=0;i<8;++i) genstring[i] = (gens & (1<<i)) ? '0'+i : '-';
        genstring[8] = 0;
        printf("page %ld (v) base %p gens %s ss=%p%s\n",
               page, text_page_address(page), genstring,
               text_page_scan_start(page),
               card_markedp((void*)obj)?"":" WP");
        return;
    }
    page = find_fixedobj_page_index((void*)obj);
    if (page>=0) {
        printf("page %ld (f) align %d gens %x%s\n", page,
               fixedobj_pages[page].attr.parts.obj_align,
               fixedobj_pages[page].attr.parts.gens_,
               card_markedp((void*)obj)?"": " WP");
        return;
    }
#endif
    printf("not in GC'ed space\n");
}

#include <stdio.h>

static int hexdump_counter;

extern void dump_immobile_fixedobjs(lispobj* where, lispobj* end, FILE*f);
extern void dump_immobile_text(lispobj* where, lispobj* end, FILE*f);
static int dump_completely_p(lispobj* obj, struct verify_state* state)
{
    int i;
    if (!state) {
        page_index_t pg = find_page_index(obj);
        if (pg >= 10470 && pg <= 10485) return 1; // (as an example)
        return 0;
    }
    for (i=0; i<MAX_ERR_OBJS; ++i)
        if (state->err_objs[i] == (uword_t)obj) return 1;
    return 0;
}
static void hexdump_control_stacks(__attribute__((unused)) void* approximate_stackptr,
                                   __attribute__((unused)) FILE *stream)
{
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    struct thread* th;
    for_each_thread(th) {
        if (th->state_word.state == STATE_DEAD) continue;
        lispobj* stackptr;
        if (th == get_sb_vm_thread()) {
            stackptr = approximate_stackptr;
        } else {
            int ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th))-1;
            os_context_t *c = nth_interrupt_context(ici, th);
            stackptr = (lispobj*) *os_context_register_addr(c,reg_SP);
        }
        gc_assert(((uword_t)stackptr & (LOWTAG_MASK>>1)) == 0); // lispword-aligned
        lispobj* where = th->control_stack_end;
        fprintf(stream, "\nThread @ %p\n", th);
        for (--where; where >= stackptr; --where) {
            lispobj word = *where;
            if (!fixnump(word) && gc_managed_addr_p(word))
                fprintf(stream, "  %p: %"OBJ_FMTX"\n", where, word);
        }
    }
#endif
}

#define HEXDUMP_PATH_TEMPLATE "/var/tmp/heap-%d-%d.txt"

/* Dump spaces as human-readable text (hexadecimal) */
void hexdump_spaces(struct verify_state* state, char *reason)
{
    char path[100];

    ++hexdump_counter;
    sprintf(path, HEXDUMP_PATH_TEMPLATE, getpid(), hexdump_counter);
    FILE *f = fopen(path, "w");

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    fprintf(f, "Fixedobj space:\n");
    dump_immobile_fixedobjs((lispobj*)FIXEDOBJ_SPACE_START, fixedobj_free_pointer, f);
    fprintf(f, "Text space (tlsf mem @ %p):\n", tlsf_mem_start);
    dump_immobile_text((lispobj*)TEXT_SPACE_START, text_space_highwatermark, f);
#endif

    fprintf(f, "Dynamic space:\n");
    page_index_t firstpage = 0, lastpage;
    while (firstpage < next_free_page) {
        lastpage = firstpage;
        while (!page_ends_contiguous_block_p(lastpage, page_table[firstpage].gen))
            lastpage++;
        if (!page_bytes_used(firstpage)) {
            firstpage = 1+lastpage;
            continue;
        }
        lispobj* base = (lispobj*)page_address(firstpage);
        lispobj* limit = (lispobj*)page_address(lastpage) + page_words_used(lastpage);
        fprintf(f, "range %d:%d (%p:%p) t%d g%d ",
                (int)firstpage, (int)lastpage, base, limit,
                page_table[firstpage].type, page_table[firstpage].gen);
        page_index_t p;
        for (p = firstpage; p <= lastpage; ++p) {
            char marks[1+CARDS_PER_PAGE];
            putc((p == firstpage) ? '(' : ' ', f);
            fprintf(f, "%s", page_card_mark_string(p, marks));
        }
        fprintf(f, ")\n");
        lispobj *where = base;
        while (where<limit){
            sword_t nwords = object_size(where);
            /* If your'e having trouble with a subset of objects, and you can get
             * a reliable reproducer, this predicate can decide which objects to
             * output in full. Generally you don't need that much output */
            if (widetag_of(where) == FILLER_WIDETAG) {
                lispobj* end = where + (1+HeaderValue(*where));
                fprintf(f, " %06x: fill to %p\n", (int)(uword_t)where & 0xffffff, end);
            } else if (dump_completely_p(where, state)) {
                sword_t i;
                for(i=0;i<nwords;++i) {
                    uword_t word = where[i];
                    if (i==0)
                        fprintf(f, " %06x: %"OBJ_FMTX, (int)(uword_t)(where+i) & 0xffffff, word);
                    else
                        fprintf(f, "   %04x: %"OBJ_FMTX, (int)(uword_t)(where+i) & 0xffff, word);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
                    if (i == 0 && header_widetag(word) == INSTANCE_WIDETAG) word >>= 32;
#endif
                    if (is_lisp_pointer(word)
                        && (find_page_index((void*)word)>=0 || immobile_space_p(word)))
                        fprintf(f, " (g%d)", gc_gen_of(word, 0xff));
                    fprintf(f,"\n");
                }
            } else {
                int min_gen = 8;
                int prefix = ' ';
                if (widetag_of(where)==CODE_HEADER_WIDETAG && header_rememberedp(*where))
                    prefix = '#';
                else if (card_dirtyp(addr_to_card_index(where)))
                    prefix = '|';
                fprintf(f, "%c%06x: %"OBJ_FMTX, prefix, (int)(uword_t)where & 0xffffff, *where);
                int i;
                int boxed_nwords = nwords;
                // This is just a heuristic guess of pointee generation.
                // For code it's (mostly) right, for other things it's slightly less right
                // because we're really not respecting the tagged or raw nature of each word.
                if (widetag_of(where)==CODE_HEADER_WIDETAG)
                    boxed_nwords = code_header_words((struct code*)where);
                for (i=0; i<boxed_nwords; ++i) {
                    uword_t word = where[i];
                    page_index_t pointee_page;
                    if (is_lisp_pointer(word) && (pointee_page=find_page_index((void*)word))>=0
                        && page_table[pointee_page].gen < min_gen)
                        min_gen = page_table[pointee_page].gen;
                }
                if (min_gen != 8)
                    fprintf(f, " (>g%d)\n", min_gen);
                else
                  fprintf(f, "\n");
            }
            where += nwords;
        }
        fprintf(f,"--\n");
        firstpage = 1+lastpage;
    }
    hexdump_control_stacks(&reason, f);
    fclose(f);
    fprintf(stderr, "%s: wrote [%s]\n", reason, path);
}

int hexdump_enabled = 0;

int hexdump_and_verify_heap(lispobj* cur_thread_approx_stackptr, int flags)
{
    if (hexdump_enabled) hexdump_spaces(0, flags & VERIFY_POST_GC ? "post-GC" : "pre-GC");
#if 0
    if (hexdump_counter >= 9) {
        char pathname[128];
        sprintf(pathname, "gc-%d-%d-%d-%s.bin",
                getpid(), n_gcs, from_space,
                flags & VERIFY_POST_GC ? "post" : "pre");
        save_gc_crashdump(pathname, cur_thread_approx_stackptr);
        fprintf(stderr, "Wrote [%s]\n", pathname);
    }
#endif
    return verify_heap(cur_thread_approx_stackptr, flags);
}
