#include <stdio.h>
#include <sys/time.h>
#include <stdlib.h>

#include "os.h"
#include "gc.h"
#include "code.h"
#include "lispobj.h"
#include "gc-assert.h"
#include "queue.h"
#include "tiny-lock.h"
#include "queue-suballocator.h"
#include "incremental-compact.h"
#include "genesis/static-symbols.h"
#include "mark-region.h"

#include "genesis/closure.h"
#include "genesis/gc-tables.h"
#include "genesis/symbol.h"
#include "genesis/instance.h"

/* The fix_slots loop does less work per pointer, so we
 * prefetch further than we do in the tracing loop. */
#define FIX_PREFETCH_DISTANCE 128

/* Duplicated from mark-region.c */
#define METER(name, action) \
  { uword_t before = get_time(); \
  action; \
  atomic_fetch_add(name, get_time() - before); }

static uword_t get_time() {
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return t.tv_sec * 1000000 + t.tv_nsec/1000;
}

/* Maximum ratio between pages used and pages "needed" to compact. */
float page_overhead_threshold = 1.3;
/* Minimum fraction of bytes used on a page to compact it. */
float page_utilisation_threshold = 0.5;
/* Maximum number of bytes to copy in one collection. */
uword_t bytes_to_copy = 30000000;
/* Minimum generation to consider compacting when collecting. */
generation_index_t minimum_compact_gen = 1;
/* To force compacting GC or not. Set by prepare_dynamic_space_for_final_gc. */
bool force_compaction = 0;

static generation_index_t target_generation;
/* A queue of interesting slots. */
static struct Qblock *remset;
static lock_t remset_lock = LOCK_INITIALIZER;
static struct suballocator remset_suballocator = SUBALLOCATOR_INITIALIZER("compaction remset");
bool compacting;
unsigned char *target_pages;

void compactor_init() {
  target_pages = calloc(dynamic_space_size / GENCGC_PAGE_BYTES, 1);
  if (!target_pages)
    lose("Failed to allocate target pages table");
}

/* Deciding how to compact */

static bool should_compact(char __attribute__((unused)) *why) {
  /* If there are many more small-object pages than there could
   * be, start compacting. */
  uword_t pages = 0, bytes = 0;
  for (page_index_t p = 0; p < page_table_pages; p++) {
    if (!page_free_p(p) && !page_single_obj_p(p)) {
      pages++;
      bytes += page_bytes_used(p);
    }
  }
  float ratio = (float)(pages * GENCGC_PAGE_BYTES) / bytes;
  // fprintf(stderr, "%s, ratio = %.2f\n", why, ratio);
  return force_compaction || ratio > page_overhead_threshold;
}

static void pick_targets() {
  uword_t bytes_moving = 0, __attribute__((unused)) pages_moving = 0;
  page_index_t p = page_table_pages - 1;
  /* Ideally we'd like to avoid selecting pinned pages here, but the phase
   * ordering is tricky. We need to know (an over-estimation of) the pages
   * to move for mark() to log, we only find out which pages are pinned
   * while marking from the roots. */
  while (bytes_moving < bytes_to_copy) {
    if (!page_single_obj_p(p) &&
        !page_free_p(p) &&
        page_table[p].gen < PSEUDO_STATIC_GENERATION &&
        (float)(page_bytes_used(p)) / GENCGC_PAGE_BYTES <= page_utilisation_threshold) {
      bytes_moving += page_bytes_used(p);
      pages_moving++;
      target_pages[p] = 1;
    }
    if (p == 0) break;
    p--;
  }
}

void consider_compaction(generation_index_t gen) {
  if (gen >= minimum_compact_gen && should_compact("Enabling remset")) {
    compacting = 1;
    target_generation = gen;
    pick_targets();
  } else {
    compacting = 0;
  }
}

/* Remset */

/* We cram a source into the low bits of a pointer, to save space in the
 * remset. */
static inline lispobj tag_source(lispobj *where, enum source s) { return (lispobj)where | (lispobj)s; }
static inline enum source source_from_tagged(lispobj t) { return t & 7; }
static inline lispobj *slot_from_tagged(lispobj t) { return (lispobj*)(t &~ 7); }
/* Each tracing thread records sources into thread-local blocks, like they
 * do with grey objects. */
static _Thread_local struct Qblock *output_block = NULL;

void commit_thread_local_remset() {
  if (output_block && output_block->count) {
    acquire_lock(&remset_lock);
    output_block->next = remset;
    remset = output_block;
    release_lock(&remset_lock);
  }
  output_block = NULL;
}

/* We need to know which object a slot resides in for two reasons:
 * - If the object has been moved, we need to adjust the location of
 *   the slot to update accordingly. We assume the slot does not move
 *   w.r.t the object, which seems fine to do.
 * - We need to inform some hash tables that they need to be rehashed,
 *   if we just invalidated some address-based hash.
 * We could figure out the start of an object from its slot, but I'd
 * prefer not to.
 */
void log_relevant_slot(lispobj *slot, lispobj *source, enum source source_type) {
  if (!output_block || output_block->count == QBLOCK_CAPACITY) {
    commit_thread_local_remset();
    output_block = suballoc_allocate(&remset_suballocator);
  }
  output_block->elements[output_block->count++] = tag_source(slot, source_type);
  output_block->elements[output_block->count++] = (lispobj)source;
}

/* Compacting */
static void apply_pins() {
  /* Now that we know which pages are pinned, we should update the targeted
   * pages for compaction. */
  for (page_index_t p = 0; p < page_table_pages; p++)
    target_pages[p] &= !gc_page_pins[p];
}

static void move_objects() {
  /* Note that this function is very un-thread-safe; list linearisation
   * can cause a thread to copy any objects. But if it weren't for list
   * linearisation, no synchronisation between threads mightn't be needed, as
   * each thread would be confined to the pages it claimed.
   * But early experiements in parallel copying suggested we're bottlenecked
   * by refilling TLABs too. */
  uword_t pages_moved = 0;
  unsigned char *allocation = (unsigned char*)allocation_bitmap;
  for (page_index_t p = 0; p < page_table_pages; p++)
    // What's wrong with testing page_single_obj_p in pick_targets?
    if (target_pages[p] && !page_single_obj_p(p)) {
      pages_moved++;
      /* Move every object in the right generation in this page. */
      for_lines_in_page (l, p)
        if (line_bytemap[l] && DECODE_GEN(line_bytemap[l]) == target_generation)
          for (int i = 0; i < 8; i++)
            if (allocation[l] & (1 << i)) {
              lispobj *where = (lispobj*)line_address(l) + 2 * i;
              lispobj bogus = compute_lispobj(where);
              scavenge(&bogus, 1);
            }
    }
  if (force_compaction && !lisp_startup_options.noinform)
      fprintf(stderr, "Forced compaction moved %ld pages\n", pages_moved);
}

/* Fix up the address of a slot, when the object containing the slot
 * may have been forwarded. */
static inline lispobj *forward_slot(lispobj *slot, lispobj *source) {
  lispobj *forwarded_source = native_pointer(follow_fp((lispobj)source));
  return (lispobj*)((lispobj)forwarded_source + ((lispobj)slot - (lispobj)source));
}

static void fix_slot(lispobj *slot, lispobj *source, enum source source_type) {
#ifdef LISP_FEATURE_LINKAGE_SPACE
  if (source_type == SOURCE_LINKAGE_CELL) {
    /* Source (the cell) can't move but I don't know how to pass nullptr or NIL
     * as the source while also allowing the value to move. We have to check the
     * source type and avoid calling forward_slot */
    lispobj entrypoint = *slot; // this is the function entry address
    lispobj* funobj = (lispobj*)entrypoint - 2; // KLUDGE: base pointer from entry point
    if (forwarding_pointer_p(funobj))
        *slot = fun_self_from_taggedptr(forwarding_pointer_value(funobj));
    return;
  }
#endif
  /* TLS may not be moved, and isn't really an object.
   * mr_preserve_range eventually calls fix_slot with
   * source == NULL to indicate that the source cannot move.
   * And remember that NIL has a funny alignment, which
   * we shouldn't "fix". Nor does NIL move. */
  if (source && source != native_pointer(NIL) - 1) {
    slot = forward_slot(slot, source);
    source = native_pointer(follow_fp((lispobj)source));
  }
  switch (source_type) {
  case SOURCE_NORMAL:
    *slot = barrier_load(slot);
    if (source &&
        widetag_of(source) == SIMPLE_VECTOR_WIDETAG &&
        vector_flagp(*source, VectorHashing)) {
      /* Tell any hash tables to rehash. This causes unnecessary rehashing,
       * but we compact infrequently and incrementally, so it shouldn't
       * hurt much. */
      /* TODO: should "rehash a hash table" be part of the source type? */
      struct vector* kv_vector = (struct vector*)source;
      // Is it possible that this needs to be sync_fetch_and_or, or are we
      // definitely single-threaded here?
      KV_PAIRS_REHASH(kv_vector->data) |= make_fixnum(1);
    }
    break;
  case SOURCE_ZERO_TAG:
    /* I'm only guessing that this line is right. It's hard to test because compaction
     * is unlikely to occur when there are lockfree lists containing logically-deleted nodes. */
    *slot = (lispobj)native_pointer(follow_fp(*slot));
    break;
  case SOURCE_CLOSURE: {
    /* SOURCE_CLOSURE can only be the source type of the taggedptr of
     * a closure. */
    struct closure *closure = (struct closure*)source;
    closure->fun = fun_self_from_taggedptr(follow_fp(fun_taggedptr_from_self(closure->fun)));
    break;
  }
#ifdef LISP_FEATURE_LINKAGE_SPACE
  default: lose("Can't happen");
#else
  case SOURCE_FDEFN_RAW: {
    /* SOURCE_FDEFN_RAW can only be the source type of the fdefn->raw_addr
     * slot. */
    struct fdefn *f = (struct fdefn*)source;
    lispobj obj = decode_fdefn_rawfun(f);
    f->raw_addr += (sword_t)(follow_fp(obj) - obj);
  }
#endif
  }
}

static void fix_slots() {
  __attribute__((unused)) int c = 0;
  for (; remset; remset = remset->next) {
    for (int n = 0; n < remset->count; n += 2) {
      lispobj *slot = slot_from_tagged(remset->elements[n]),
              *source = (lispobj*)remset->elements[n + 1];
      enum source source_type = source_from_tagged(remset->elements[n]);
      fix_slot(slot, source, source_type);
      if (n + FIX_PREFETCH_DISTANCE < remset->count)
        __builtin_prefetch(slot_from_tagged(remset->elements[n]));
      c++;
    }
  }
}

static void resweep_moved_lines() {
  unsigned char *allocation = (unsigned char*)allocation_bitmap;
  for (page_index_t p = 0; p < page_table_pages; p++) {
    if (target_pages[p] && !page_single_obj_p(p)) {
      uword_t decrement = 0;
      for_lines_in_page (l, p)
        if (DECODE_GEN(line_bytemap[l]) == target_generation) {
          line_bytemap[l] = 0;
          allocation[l] = 0;
          decrement++;
        }
      set_page_bytes_used(p, page_bytes_used(p) - LINE_SIZE * decrement);
      generations[target_generation].bytes_allocated -= LINE_SIZE * decrement;
      bytes_allocated -= LINE_SIZE * decrement;
      if (page_words_used(p) == 0) {
        set_page_need_to_zero(p, 1);
#ifdef LISP_FEATURE_DARWIN_JIT
        reset_page_flags(p);
#else
        set_page_type(page_table[p], FREE_PAGE_FLAG);
        page_table[p].scan_start_offset_ = 0;
#endif
      }
    }
  }
}

void run_compaction(_Atomic(uword_t) *copy_meter,
                    _Atomic(uword_t) *fix_meter,
                    _Atomic(uword_t) *resweep_meter) {
  if (compacting) {
    /* Check again, in case fragmentation somehow improves.
     * Not likely, but it's a cheap test which avoids effort. */
    if (should_compact("Performing compaction")) {
      apply_pins();
      METER(copy_meter, move_objects());
      gc_close_collector_regions(0);
      METER(fix_meter, fix_slots());
      METER(resweep_meter, resweep_moved_lines());
      should_compact("I just moved, but still");
      extern int check_hash_tables;
      if (check_hash_tables) check_hash_tables = 2;
    }
    memset(target_pages, 0, page_table_pages);
    remset = NULL;
    suballoc_release(&remset_suballocator);
  }
  compacting = 0;
}
