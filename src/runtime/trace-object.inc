// -*- mode: c -*-

#include "weak-hash-pred.inc"
#include "genesis/split-ordered-list.h"

/* TRACE_NAME = Base name for generated tracing functions
 * ACTION = Function name to call with every pointer
 * STRENGTHEN_WEAK_REFS = Whether to always trace through weak objects as if strong
 */
#if !defined TRACE_NAME || \
    !defined ACTION || \
    !defined STRENGTHEN_WEAK_REFS || \
    !defined HT_ENTRY_LIVENESS_FUN_ARRAY_NAME
#error "Please define all required macros before including trace-object"
#endif

/* Function name to call with weak pointers that have been deferred */
#ifndef WATCH_DEFERRED
#define WATCH_DEFERRED(where, from, to) (void)(0)
#endif

/* Mutex stuff */
#ifndef LOCK
#define LOCK (void)(0)
#define UNLOCK (void)(0)
#endif

/* Generated names */
#define cat2(x, y) x ## y
#define cat(x, y) cat2(x, y)
#define USING_LAYOUT cat(TRACE_NAME, _using_layout)
#define TRACE_PAIR cat(TRACE_NAME, _pair)

#define ORDINARY_SLOT(x) ACTION(x, &x, SOURCE_NORMAL)
static void TRACE_PAIR(lispobj* where)
{
    ORDINARY_SLOT(where[0]);
    ORDINARY_SLOT(where[1]);
}

static void USING_LAYOUT(lispobj layout, lispobj* where, int nslots)
{
    // Apart from the allowance for untagged pointers in lockfree list nodes,
    // this contains almost none of the special cases that gencgc does.
    if (!layout) return;
#if !(defined LISP_FEATURE_MARK_REGION_GC && defined LISP_FEATURE_COMPACT_INSTANCE_HEADER)
  /* FIXME: pmrgc's incremental-compact has a problem here because the layout is a half-sized
   * pointer and compaction doesn't know how to write it back to the object header.
   * But (luckily?) we can only enable compact-instance and mark-region if #+permgen
   * in which case layouts are traced as roots and never GCable.
   * Any other feature combo needs this. As things are we're OK because the one other use of
   * trace-object.inc is from fullcgc which is not used with mark-region. However, it does mean
   * we could not use trace-object.inc to perform the path finding steps in traceroot */
    ACTION(layout, &layout_of(where), SOURCE_NORMAL);
#endif
    if (lockfree_list_node_layout_p(LAYOUT(layout))) { // allow untagged 'next'
        struct list_node* node = (void*)where;
        lispobj next = node->_node_next;
        // ignore if 0
        if (fixnump(next) && next)
            ACTION(next|INSTANCE_POINTER_LOWTAG, &node->_node_next, SOURCE_ZERO_TAG);
    }
    struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
    int i;
    lispobj* slots = where+1;
    for (i=0; i<nslots; ++i)
        if (bitmap_logbitp(i, bitmap) && is_lisp_pointer(slots[i]))
            ORDINARY_SLOT(slots[i]);
}

static void TRACE_NAME(lispobj* where)
{
    lispobj header = *where;
    int widetag = header_widetag(header);

    if (instanceoid_widetag_p(widetag))
        return USING_LAYOUT(layout_of(where), where,
                            instanceoid_length(header));
    sword_t scan_from = 1;
    sword_t scan_to = sizetab[widetag](where);
    sword_t i;
    struct weak_pointer *weakptr;
    switch (widetag) {
    case SIMPLE_VECTOR_WIDETAG:
#ifdef LISP_FEATURE_UBSAN
        if (is_lisp_pointer(where[1])) ORDINARY_SLOT(where[1]);
#endif
        /* Would be more precise to only make hash-tables rehash when
         * some key actually moves, and we don't really trace large and
         * old tables when scavenging properly. So we _don't_ touch
         * rehash bits here. */
        // non-weak hashtable kv vectors are trivial in fullcgc. Keys don't move
        // so the table will not need rehash as a result of gc.
        // Ergo, those may be treated just like ordinary simple vectors.
        // However, weakness remains as a special case.
        if (!STRENGTHEN_WEAK_REFS && vector_flagp(header, VectorWeak)) {
            WATCH_DEFERRED(where, 1, scan_to);
            if (!vector_flagp(header, VectorHashing)) {
                LOCK;
                add_to_weak_vector_list(where, header);
                UNLOCK;
                return;
            }
            // Ok, we're looking at a weak hash-table.
            struct vector* v = (struct vector*)where;
            lispobj *plhash_table = &v->data[vector_len(v)-1];
            lispobj lhash_table = *plhash_table;
            gc_dcheck(instancep(lhash_table));
            ACTION(lhash_table, plhash_table, SOURCE_NORMAL);
            struct hash_table* hash_table = (void*)native_pointer(lhash_table);
            gc_assert(hashtable_weakp(hash_table));
            // An object can only be removed from the queue once.
            // Therefore the 'next' pointer has got to be nil.
            gc_assert((lispobj)hash_table->next_weak_hash_table == NIL);
            int weakness = hashtable_weakness(hash_table);
            bool defer = 1;
            LOCK;
            if (weakness != WEAKNESS_KEY_AND_VALUE)
                defer = scan_weak_hashtable(hash_table, HT_ENTRY_LIVENESS_FUN_ARRAY_NAME[weakness],
                                            TRACE_PAIR);
            if (defer) {
                hash_table->next_weak_hash_table = weak_hash_tables;
                weak_hash_tables = hash_table;
            }
            UNLOCK;
            return;
        }
        break;
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || defined (LISP_FEATURE_ARM64)
    /* on x86[-64], closure->fun is a fixnum-qua-pointer. Convert it to a lisp
     * pointer to mark it, but not on platforms where it's already a descriptor */
    case CLOSURE_WIDETAG: {
        lispobj *self = &((struct closure*)where)->fun;
        ACTION(fun_taggedptr_from_self(*self), self, SOURCE_CLOSURE);
        scan_from = 2;
        break; // scan slots normally
    }
#endif
    case CODE_HEADER_WIDETAG: {
        scan_to = code_header_words((struct code*)where);
        __attribute__((unused)) struct code* code = (struct code*)where;
#ifdef LISP_FEATURE_LINKAGE_SPACE
        const unsigned int smallvec_elts =
            (GENCGC_PAGE_BYTES - offsetof(struct vector,data)) / N_WORD_BYTES;
        if (code->linkage_elts) {
            lispobj name_map = SYMBOL(LINKAGE_NAME_MAP)->value;
            gc_assert(simple_vector_p(name_map));
            struct vector* outer_vector = VECTOR(name_map);
            struct varint_unpacker unpacker;
            varint_unpacker_init(&unpacker, code->linkage_elts);
            int prev_index = 0, index;
            while (varint_unpack(&unpacker, &index) && index != 0) {
                index += prev_index;
                prev_index = index;
                int index_high = (unsigned int)index / smallvec_elts;
                int index_low = (unsigned int)index % smallvec_elts;
                lispobj smallvec = outer_vector->data[index_high];
                gc_assert(other_pointer_p(smallvec));
                struct vector* inner_vector = VECTOR(smallvec);
                // Ensure liveness of function name
                ACTION(inner_vector->data[index_low], 0, SOURCE_NORMAL);
            }
        }
#endif
#ifdef LISP_FEATURE_UNTAGGED_FDEFNS
        lispobj* where = code->constants + code_n_funs(code) * CODE_SLOTS_PER_SIMPLE_FUN;
        lispobj* limit = where + code_n_named_calls(code);
        for ( ; where < limit ; ++where )
            ACTION(*where | OTHER_POINTER_LOWTAG, where, SOURCE_ZERO_TAG);
        // Fall into general case. Untagged fdefns will be ignored as fixnums.
#endif
        break;
    }
    case SYMBOL_WIDETAG:
        {
        struct symbol* s = (void*)where;
        ORDINARY_SLOT(s->name);
        ORDINARY_SLOT(s->value);
        ORDINARY_SLOT(s->info);
        ORDINARY_SLOT(s->fdefn);
#ifdef LISP_FEATURE_LINKAGE_SPACE
        int i = symbol_linkage_index(s);
        ACTION(linkage_cell_taggedptr(i), linkage_space+i, SOURCE_LINKAGE_CELL);
#endif
        }
        return;
    case FDEFN_WIDETAG: {
        struct fdefn *fdefn = (struct fdefn*)where;
        ORDINARY_SLOT(fdefn->name);
        ORDINARY_SLOT(fdefn->fun);
#ifdef LISP_FEATURE_LINKAGE_SPACE
        int i = fdefn_linkage_index(fdefn);
        ACTION(linkage_cell_taggedptr(i), linkage_space+i, SOURCE_LINKAGE_CELL);
#else
        ACTION(decode_fdefn_rawfun(fdefn), (lispobj*)&fdefn->raw_addr, SOURCE_FDEFN_RAW);
#endif
        return;
    }
    case WEAK_POINTER_WIDETAG:
        weakptr = (struct weak_pointer*)where;
        if (STRENGTHEN_WEAK_REFS) ORDINARY_SLOT(weakptr->value);
        else if (weakptr_vectorp(weakptr)) {
            WATCH_DEFERRED(where, 1, scan_to);
            LOCK;
            add_to_weak_vector_list(where, header);
            UNLOCK;
        } else if (is_lisp_pointer(weakptr->value) && interesting_pointer_p(weakptr->value)) {
            /* XXX: This hard-codes the layout of a weak pointer. Make a scalar WATCH_DEFERRED? */
            WATCH_DEFERRED(where, 1, 2);
            LOCK;
            add_to_weak_pointer_chain(weakptr);
            UNLOCK;
        }
        return;
    default:
        if (leaf_obj_widetag_p(widetag)) return;
    }
    for(i=scan_from; i<scan_to; ++i)
        ORDINARY_SLOT(where[i]);
}

#undef ORDINARY_SLOT
