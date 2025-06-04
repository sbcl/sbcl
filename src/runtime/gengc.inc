static void trigger_gc(__attribute__((unused)) struct thread* thread)
{
        /* Don't flood the system with interrupts if the need to gc is
         * already noted. This can happen for example when SUB-GC
         * allocates or after a gc triggered in a WITHOUT-GCING. */
        if (read_TLS(GC_PENDING,thread) == NIL) {
            /* set things up so that GC happens when we finish the PA
             * section */
            write_TLS(GC_PENDING, LISP_T, thread);
            if (read_TLS(GC_INHIBIT,thread) == NIL) {
#ifdef LISP_FEATURE_SB_SAFEPOINT
                thread_register_gc_trigger();
#else
                set_pseudo_atomic_interrupted(thread);
                maybe_save_gc_mask_and_block_deferrables
# if HAVE_ALLOCATION_TRAP_CONTEXT
                    (thread_interrupt_data(thread).allocation_trap_context);
# else
                    (0);
# endif
#endif
            }
        }
}

static bool card_markedp(void* addr)
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (immobile_space_p((lispobj)addr))
        return !immobile_card_protected_p(addr);
#endif
    return gc_card_mark[addr_to_card_index(addr)] != CARD_UNMARKED;
}

void gc_show_pte(lispobj obj, FILE* f)
{
    char marks[1+CARDS_PER_PAGE];
    page_index_t page = find_page_index((void*)obj);
    if (page>=0) {
        fprintf(f, "page %"PAGE_INDEX_FMT" base %p gen %d type %x ss %p used %x",
               page, page_address(page), page_table[page].gen, page_table[page].type,
               page_scan_start(page), page_bytes_used(page));
        if (page_starts_contiguous_block_p(page)) fprintf(f, " startsblock");
        if (page_ends_contiguous_block_p(page, page_table[page].gen)) fprintf(f, " endsblock");
        fprintf(f, " (%s)\n", page_card_mark_string(page, marks));
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
        fprintf(f, "page %d (v) base %p gens %s ss=%p%s\n",
               (int)page, text_page_address(page), genstring,
               text_page_scan_start(page),
               card_markedp((void*)obj)?"":" WP");
        return;
    }
    page = find_fixedobj_page_index((void*)obj);
    if (page>=0) {
        fprintf(f, "page %d (f) align %d gens %x%s\n", (int)page,
               fixedobj_pages[page].attr.parts.obj_align,
               fixedobj_pages[page].attr.parts.gens_,
               card_markedp((void*)obj)?"": " WP");
        return;
    }
#endif
    fprintf(f, "not in GC'ed space\n");
}

static uword_t verify_range(lispobj* start, lispobj* end, void* arg);
static int verify(lispobj start, lispobj* end, struct verify_state* state, int flags)
{
    int savedflags = state->flags;
    state->flags |= flags;
    int result = verify_range((lispobj*)start, end, state);
    state->flags = savedflags;
    return result;
}

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
                flags & VERIFY_POST_GC ? "Verify after GC(%d,%d)" :
                "Heap check", // if called at a random time
                (flags >> 1) & 7, // generation number
                flags & 1); // 'raise'
    else
        state.flags |= VERIFY_PRINT_HEADER_ON_FAILURE;

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
        if (th->state_word.state == STATE_DEAD) continue;
        if (verbose)
            fprintf(stderr, " [thread bindings]");
        if (verify((lispobj)th->binding_stack_start,
                   (lispobj*)get_binding_stack_pointer(th), &state,
                   VERIFYING_UNFORMATTED)) goto out;
        if (verbose)
            fprintf(stderr, " [thread TLS]");
        if (verify((lispobj)&th->lisp_thread,
                   (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th), &state,
                   VERIFYING_UNFORMATTED))
            goto out;
    }
    if (verbose)
        fprintf(stderr, " [RO]");
    if (verify(READ_ONLY_SPACE_START, read_only_space_free_pointer, &state, 0)) goto out;
    if (verbose)
        fprintf(stderr, " [static]");
    if (verify((uword_t)NIL_SYMBOL_SLOTS_START, NIL_SYMBOL_SLOTS_END, &state, 0)) goto out;
    if (verify(STATIC_SPACE_OBJECTS_START, static_space_free_pointer, &state, 0)) goto out;
    if (verbose)
        fprintf(stderr, " [permgen]");
    if (verify(PERMGEN_SPACE_START, permgen_space_free_pointer, &state, 0)) goto out;
    if (verbose)
        fprintf(stderr, " [dynamic]");
    state.flags |= VERIFYING_GENERATIONAL;
#ifdef LISP_FEATURE_MARK_REGION_GC
    parallel_walk_generation(verify_range, -1, &state);
    check_free_pages();
#else
    walk_generation(verify_range, -1, &state);
#endif
    if (verbose && state.nerrors==0) fprintf(stderr, " passed\n");
 out:
    if (state.nerrors && !(flags & VERIFY_DONT_LOSE)) {
        // dump_spaces(&state, "verify failed");
        lose("Verify failed: %d errors", state.nerrors);
    }
    return state.nerrors;
}

static int verify_pointer(lispobj thing, lispobj *where, struct verify_state *state);

#define CHECK(pointer, where) if (verify_pointer(pointer, where, state)) return 1
#define CHECK_LINKAGE_CELL(index, where) CHECK(linkage_cell_function(index), where)
/* Return 0 if good, 1 if bad.
 * Take extra pains to process weak SOLIST nodes - Finalizer list nodes weakly point
 * to a referent via an untagged pointer, so the GC doesn't even have to know that
 * the reference is weak - it simply is ignored as a non-pointer.
 * This makes invariant verification a little tricky. We want to restore the tagged
 * pointer, but only if the list is the finalizer list. */
static int verify_headered_object(lispobj* object, sword_t nwords,
                                  struct verify_state *state)
{
    long i;
    int widetag = widetag_of(object);
    if (instanceoid_widetag_p(widetag)) {
        lispobj layout = layout_of(object);
        if (layout) {
          CHECK(layout, (lispobj*)&layout_of(object));
            struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
            if (lockfree_list_node_layout_p(LAYOUT(layout))) {
                // These objects might have _two_ untagged references -
                //  1) the 'next' slot may or may not have tag bits
                //  2) finalizer list node always stores its referent as untagged
                struct list_node* node = (void*)object;
                lispobj next = node->_node_next;
                if (fixnump(next) && next)
                  CHECK(next | INSTANCE_POINTER_LOWTAG, &node->_node_next);
                if (finalizer_node_layout_p(LAYOUT(layout))) {
                    struct solist_node* node = (void*)object;
                    // !fixnump(next) implies that this node is NOT deleted, nor in
                    // the process of getting deleted by CANCEL-FINALIZATION
                    if (node->so_key && !fixnump(next)) {
                        gc_assert(fixnump(node->so_key));
                        lispobj key = compute_lispobj((lispobj*)node->so_key);
                        CHECK(key, &node->so_key);
                    }
                }
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
        for (i=2; i <nheader_words; ++i) CHECK(object[i], object+i);
#ifndef NDEBUG // avoid "unused" warnings on auto vars of for_each_simple_fun()
        // Check the SIMPLE-FUN headers
        for_each_simple_fun(i, fheaderp, code, 1, {
#if defined LISP_FEATURE_COMPACT_INSTANCE_HEADER
            lispobj __attribute__((unused)) layout = funinstance_layout((lispobj*)fheaderp);
            gc_assert(!layout || layout == LAYOUT_OF_FUNCTION);
#elif defined LISP_FEATURE_64_BIT
            gc_assert((fheaderp->header >> 32) == 0);
#endif
        });
#endif
#if 0 // this looks redundant. It's checked with each pointer, no?
        bool rememberedp = header_rememberedp(code->header);
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
#if FUN_SELF_FIXNUM_TAGGED
    if (widetag == CLOSURE_WIDETAG && object[1] != 0) {
        __attribute__((unused)) struct simple_fun* sf = (void*)(object[1] - 2*N_WORD_BYTES);
        gc_assert(header_widetag(sf->header) == SIMPLE_FUN_WIDETAG);
        gc_assert(header_widetag(fun_code_header(sf)->header) == CODE_HEADER_WIDETAG);
    }
#endif
    if (widetag == SYMBOL_WIDETAG) {
        struct symbol* s = (void*)object;
        CHECK(s->value, &s->value);
        CHECK(s->fdefn, &s->fdefn);
        CHECK(s->info, &s->info);
#ifdef LISP_FEATURE_LINKAGE_SPACE
        CHECK_LINKAGE_CELL(symbol_linkage_index(s), &s->fdefn);
#endif
        CHECK(decode_symbol_name(s->name), &s->name);
        return 0;
    }
    if (widetag == FDEFN_WIDETAG) {
        struct fdefn* f = (void*)object;
        CHECK(f->name, &f->name);
        CHECK(f->fun, &f->fun);
#ifdef LISP_FEATURE_LINKAGE_SPACE
        CHECK_LINKAGE_CELL(fdefn_linkage_index(f), &f->fun);
#else
        CHECK(decode_fdefn_rawfun(f), (lispobj*)&f->raw_addr);
#endif
        return 0;
    }
    for (i=1; i<nwords; ++i) CHECK(object[i], object+i);
    return 0;
}

static __attribute__((unused)) bool acceptable_filler_cons_p(lispobj* where)
{
    if (where[0] == 0 && where[1] == 0) return 1;
    // These "conses" can result from bignum multiplication-
    // trailing insigificant sign bits which get chopped.
    if (where[0] == (uword_t)-1 && where[1] == (uword_t)-1) return 1;
    if (where[0] == (uword_t)-1 && where[1] == 0) return 1;
    return 0;
}
