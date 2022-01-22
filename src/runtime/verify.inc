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
    page_index_t page_index = find_page_index((void*)thing);
    if (!(page_index >= 0 || immobile_space_p(thing))) return 0; // can't do much with it
    if ((state->flags & VERIFY_TAGS) && page_index >= 0) {
        if (listp(thing)) {
            FAIL_IF(!(is_cons_half(CONS(thing)->car) && is_cons_half(CONS(thing)->cdr)),
                    "non-cons");
        } else {
            FAIL_IF(LOWTAG_FOR_WIDETAG(widetag_of(native_pointer(thing))) != lowtag_of(thing),
                    "incompatible widetag");
        }
    }
    generation_index_t to_gen = gc_gen_of(thing, ARTIFICIALLY_HIGH_GEN);
    if (to_gen < state->min_pointee_gen) state->min_pointee_gen = to_gen;
    if (state->flags & VERIFY_QUICK) return 0;
    if (page_index >= 0) {
        // If it's within the dynamic space it should point to a used page.
        FAIL_IF(page_free_p(page_index), "free page");
        FAIL_IF(!(page_table[page_index].type & OPEN_REGION_PAGE_FLAG)
                && (thing & (GENCGC_PAGE_BYTES-1)) >= page_bytes_used(page_index),
                "unallocated space");
    } else {
        // The object pointed to must not have been discarded as garbage.
        FAIL_IF(!other_immediate_lowtag_p(*native_pointer(thing)) ||
                filler_obj_p(native_pointer(thing)),
                "trashed object");
    }
    // Must not point to a forwarding pointer
    FAIL_IF(*native_pointer(thing) == 0x01, "forwarding ptr");
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
    } else if ((state->flags & VERIFYING_GENERATIONAL) && to_gen < state->object_gen) {
        /* The WP criteria are:
         *  - SIMPLE-VECTOR marks the card containing the cell with the old->young pointer.
         *  - Everything else marks the object header -OR- the card with the pointer.
         *    (either/or because Lisp marks the header card,
         *     but the collector marks the cell's card.) */
      int marked = card_markedp(where)
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        || (header_widetag(state->object_header) != SIMPLE_VECTOR_WIDETAG
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
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER
                gc_assert(bitmap.bits[0] == (sword_t)-1 || bitmap.bits[0] == (sword_t)6);
#else
                gc_assert(bitmap.bits[0] == (sword_t)-4);
#endif
            }
            if (lockfree_list_node_layout_p(LAYOUT(layout))) {
                struct instance* node = (struct instance*)object;
                lispobj next = node->slots[INSTANCE_DATA_START];
                if (fixnump(next) && next)
                  CHECK(next | INSTANCE_POINTER_LOWTAG, node->slots+INSTANCE_DATA_START);
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
#if defined(LISP_FEATURE_COMPACT_INSTANCE_HEADER)
            lispobj __attribute__((unused)) layout = funinstance_layout((lispobj*)fheaderp);
            gc_assert(!layout || layout == LAYOUT_OF_FUNCTION);
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
#ifdef LISP_FEATURE_COMPACT_SYMBOL
    if (widetag == SYMBOL_WIDETAG) {
        struct symbol* s = (void*)object;
        CHECK(s->hash, &s->hash);
        CHECK(s->value, &s->value);
        CHECK(s->info, &s->info);
        CHECK(decode_symbol_name(s->name), &s->name);
        CHECK(s->fdefn, &s->fdefn);
        // augmented symbol?
        if (nwords > ALIGN_UP(SYMBOL_SIZE,2)) CHECK(1[&s->fdefn], 1+&s->fdefn);
        return 0;
    }
#endif
    if (widetag == FDEFN_WIDETAG) {
        struct fdefn* f = (void*)object;
        CHECK(f->name, &f->name);
        CHECK(f->fun, &f->fun);
        CHECK(fdefn_callee_lispobj(f), (lispobj*)&f->raw_addr);
        return 0;
    }
    for (i=1; i<nwords; ++i) CHECK(object[i], object+i);
    return 0;
}

static int verify_range(lispobj start, lispobj* end, struct verify_state* state)
{
    lispobj* where = (lispobj*)start;
    if ((state->flags & VERIFYING_UNFORMATTED)) {
        while (where < end) {
            if (*where != NO_TLS_VALUE_MARKER_WIDETAG) {
                int result = verify_pointer(*where, where, state);
                if (result) return result;
            }
            ++where;
        }
        return 0;
    }
    while (where < end) {
        int widetag = is_header(*where) ? header_widetag(*where) : LIST_POINTER_LOWTAG;
        long nwords = OBJECT_SIZE(*where,where);
        state->object_addr = where;
        state->object_header = *where;
        if (state->flags & VERIFYING_GENERATIONAL) {
            page_index_t pg = find_page_index(where);
            state->object_gen = pg >= 0 ? page_table[pg].gen :
              gc_gen_of((lispobj)where, ARTIFICIALLY_HIGH_GEN);
            // FIXME: should not have code acting as filler
            if (widetag != FILLER_WIDETAG && !filler_obj_p(where)) {
                // Any page can have a filler on it
                if (pg >= 0) {
                    // Assert proper page type
                    if (widetag == CODE_HEADER_WIDETAG) {
                        if (!is_code(page_table[pg].type))
                            lose("object @ %p is code on non-code page", where);
                    } else {
                        if (is_code(page_table[pg].type))
                            lose("object @ %p is non-code on code page", where);
                    }
                }
            }
        }
        if (is_cons_half(*where)) {
            if (verify_pointer(where[0], where+0, state) ||
                verify_pointer(where[1], where+1, state)) break;
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
    int result = verify_range(start, end, state);
    state->flags = savedflags;
    return result;
}

/* Return the number of verification errors found.
 * You might want to use that as a deciding factor for dump the heap
 * to a file (which takes time, and generally isn't needed).
 * But if a particular verification fails, then do dump it */
int verify_heap(int flags)
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
    extern void __attribute__((weak)) check_varyobj_pages();
    if (&check_varyobj_pages) check_varyobj_pages();
#  endif
    if (verbose)
        fprintf(stderr, " [immobile]");
    if (verify(FIXEDOBJ_SPACE_START,
               fixedobj_free_pointer, &state,
               flags | VERIFYING_GENERATIONAL)) goto out;
    if (verify(VARYOBJ_SPACE_START,
               varyobj_free_pointer, &state,
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
#ifdef LISP_FEATURE_SB_THREAD
        if (verify((lispobj)&th->lisp_thread,
                   (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th), &state,
                   VERIFYING_UNFORMATTED))
            goto out;
#endif
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
    page_index_t page = find_page_index((void*)obj);
    if (page>=0) {
        printf("page %"PAGE_INDEX_FMT" base %p gen %d type %x ss %p used %x%s\n",
               page, page_address(page), page_table[page].gen, page_table[page].type,
               page_scan_start(page), page_bytes_used(page),
               PAGE_WRITEPROTECTED_P(page)? " WP":"");
        return;
    }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    page = find_varyobj_page_index((void*)obj);
    if (page>=0) {
        printf("page %ld (v) ss=%p gens %x%s\n", page,
               varyobj_scan_start(page),
               varyobj_pages[page].generations,
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

static int n_dumps;
__attribute__((unused)) static void dump_immobile(lispobj* where, lispobj* end, FILE*f)
{
    while (where < end) {
        fprintf(f, "%"OBJ_FMTX": %"OBJ_FMTX"\n", (uword_t)where, *where);
        where += OBJECT_SIZE(*where, where);
    }
}
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
void dump_spaces(struct verify_state* state, char *reason)
{
    char path[100];

    ++n_dumps;
    sprintf(path, "heap-%d-%d.txt", getpid(), n_dumps);
    FILE *f = fopen(path, "w");

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    fprintf(f, "Fixedobj space:\n");
    dump_immobile((lispobj*)FIXEDOBJ_SPACE_START, fixedobj_free_pointer, f);
    fprintf(f, "Varyobj space:\n");
    dump_immobile((lispobj*)VARYOBJ_SPACE_START, varyobj_free_pointer, f);
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
            putc((p == firstpage) ? '(' : ' ', f);
            fprintf(f, card_markedp(page_address(p))?"~WP":"WP");
        }
        fprintf(f, ")\n");
        lispobj *where = base;
        while (where<limit){
            int nwords = OBJECT_SIZE(*where,where);
            /* If your'e having trouble with a subset of objects, and you can get
             * a reliable reproducer, this predicate can decide which objects to
             * output in full. Generally you don't need that much output */
            if (dump_completely_p(where, state)) {
                int i;
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
                int touched = ' ';
                if (widetag_of(where)==CODE_HEADER_WIDETAG && header_rememberedp(*where))
                    touched = '*';
                fprintf(f, "%c%06x: %"OBJ_FMTX, touched, (int)(uword_t)where & 0xffffff, *where);
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
    fclose(f);
    fprintf(stderr, "%s: wrote [%s]\n", reason, path);
}
