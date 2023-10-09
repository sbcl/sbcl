/*
 * heap_concurrent.c
 * @copyright (c) 2015, Tohoku University.
 * @author UENO Katsuhiro
 */

#define _GNU_SOURCE
#ifdef HAVE_GENESIS_CONFIG
#include "genesis/config.h"
#include "genesis/constants.h"
#include "genesis/hash-table.h"
#include "genesis/weak-pointer.h"
#include "genesis/thread.h"
#include "genesis/vector.h"
#include <stdio.h>
#endif

#include "smlsharp.h"
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#ifdef HAVE_CONFIG_H
#ifdef MINGW32
# include <windows.h>
#endif /* MINGW32 */
#endif /* HAVE_CONFIG_H */

#include "object.h"
#include "heap.h"

#define load_add_store_relaxed(p,n)  store_relaxed((p), load_relaxed(p) + (n))
#define load_sub_store_relaxed(p,n)  store_relaxed((p), load_relaxed(p) - (n))

#ifdef GCTIME
#include "timer.h"
static struct {
        sml_timer_t exec_start, exec_end;
        sml_time_t exec_time, gc_time, pause_time;
        unsigned int gc_count, pause_count, full_gc_count;
        double util_ratio_sum, reclaim_ratio_sum;
        unsigned int util_under_half;
        double max_pause_time;
#ifndef WITHOUT_MULTITHREAD
        pthread_mutex_t pause_time_lock;
#endif /* !WITHOUT_MULTITHREAD */
} gctime = {
#ifndef WITHOUT_MULTITHREAD
        .pause_time_lock = PTHREAD_MUTEX_INITIALIZER,
#endif /* !WITHOUT_MULTITHREAD */
};
#endif /* GCTIME */

/********** linked list and look-free stack **********/

/* thread-local use only */
struct list {
        struct list_item *head;
        struct list_item **last;
};

/* lock-free stack */
struct stack {
        _Atomic(struct list_item *) top;
};

static void
list_init(struct list *l)
{
        l->head = NULL;
        l->last = &l->head;
}

static void
list_append(struct list *l, struct list_item *item)
{
        *l->last = item;
        l->last = &item->next;
}

static ATTR_UNUSED void *
list_finish(struct list *l, struct list_item *last)
{
        *l->last = last;
        return l->head;
}

static struct list_item **
list_last(struct list_item *l)
{
        while (l->next)
                l = l->next;
        return &l->next;
}

#if 0
static void *
stack_top(struct stack *s)
{
        return load_acquire(&s->top);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if defined WITHOUT_MULTITHREAD
static void *
stack_pop(struct stack *s)
{
        struct list_item *item;
        item = load_relaxed(&s->top);
        if (item)
                store_relaxed(&s->top, item->next);
        return item;
}

#else /* !WITHOUT_MULTITHREAD */
static void *
stack_pop(struct stack *s)
{
        struct list_item *item, *next;

        /* ABA problem never occur in our usage since only the collector
         * push items with SYNC2 agreements with all mutators.  During
         * the CAS loop, the mutator who calls stack_pop never respond
         * to SYNC2 request from the collector.
         */
        item = load_acquire(&s->top);
        do {
                if (!item) break;
                next = item->next;
        } while (!cmpswap_weak_acquire(&s->top, &item, next));

        return item;
}

#endif /* !WITHOUT_MULTITHREAD */

#if defined WITHOUT_MULTITHREAD
static void
stack_push(struct stack *s, struct list_item *item)
{
        item->next = load_relaxed(&s->top);
        store_relaxed(&s->top, item);
}

#else /* !WITHOUT_MULTITHREAD */
static void
stack_push(struct stack *s, struct list_item *item)
{
        struct list_item *old = load_relaxed(&s->top);
        do {
                item->next = old;
        } while (!cmpswap_weak_release(&s->top, &old, item));
}

#endif /* !WITHOUT_MULTITHREAD */

#if defined WITHOUT_MULTITHREAD
static void
stack_push_list(struct stack *s, struct list *l)
{
        if (l->head) {
                *l->last = load_relaxed(&s->top);
                store_relaxed(&s->top, l->head);
        }
}

#else /* !WITHOUT_MULTITHREAD */
static void
stack_push_list(struct stack *s, struct list *l)
{
        struct list_item *old;

        if (l->head) {
                old = load_relaxed(&s->top);
                do {
                        *l->last = old;
                } while (!cmpswap_weak_release(&s->top, &old, l->head));
        }
}

#endif /* !WITHOUT_MULTITHREAD */

#if defined WITHOUT_MULTITHREAD
static void *
stack_flush(struct stack *s)
{
        struct list_item *item = load_relaxed(&s->top);
        store_relaxed(&s->top, NULL);
        return item;
}

#else /* !WITHOUT_MULTITHREAD */
static void *
stack_flush(struct stack *s)
{
        return swap(acquire, &s->top, NULL);
}

#endif /* !WITHOUT_MULTITHREAD */

/********** segments ***********/

#define BLOCKSIZE_MIN_LOG2  3U   /* 2^3 = 8 */
#define BLOCKSIZE_MIN       (1U << BLOCKSIZE_MIN_LOG2)
#define BLOCKSIZE_MAX_LOG2  12U  /* 2^4 = 16 */
#define BLOCKSIZE_MAX       (1U << BLOCKSIZE_MAX_LOG2)

/*
 * segment layout:
 *
 * 0000 +------------------------+
 *      | struct segment         |
 *      +------------------------+
 *      | padding (if needed)    |
 *      +------------------------+ SEG_BITMAP0_OFFSET (aligned in sml_bmword_t)
 *      | bitmap[0]              | ^
 *      :                        : | N bits + sentinel bits
 *      |                        | V
 *      +------------------------+ bitmap_base[1]
 *      | bitmap[1]              | ^
 *      :                        : | ceil(N/32) bits + sentinel bits
 *      |                        | V
 *      +------------------------+ bitmap_base[2]
 *      | bitmap[2]              | ^
 *      :                        : | ceil(N/32^n) bits + sentinel bits
 *      |                        | v
 *      +------------------------+ bitmap_base[3]
 *      | padding (if needed)    |
 *      +------------------------+ stack_offset (aligned in void*)
 *      | stack area             | ^
 *      |                        | | N pointers
 *      |                        | v
 *      +------------------------+ stack_limit
 *      | padding (if needed)    |
 *      +------------------------+ block_offset (aligned in MAXALIGN
 *      | obj block area         | ^                        - OBJ_HEADER_SIZE)
 *      |                        | | N blocks
 *      |                        | v
 *      +------------------------+ block_limit
 *      : padding                :
 * 8000 +------------------------+
 *
 * N-th bit of bitmap[0] indicates whether N-th block is used (1) or not (0).
 * N-th bit of bitmap[n] indicates whether N-th word of bitmap[n-1] is
 * filled (1) or not (0).
 */
#include "segment.inc"
static struct segment_layout segment_layout[BLOCKSIZE_MAX_LOG2 + 1];

#define SEG_INITIAL_OFFSET \
        CEILING(sizeof(struct segment), sizeof(sml_bmword_t))
#define SEG_BITMAP0_OFFSET \
        SEG_INITIAL_OFFSET

#define BITS_TO_WORDS(n)  (((n) + BITPTR_WORDBITS - 1) / BITPTR_WORDBITS)
#define WORDS_TO_BYTES(n) ((n) * sizeof(sml_bmword_t))
#define BITS_TO_BYTES(n)  WORDS_TO_BYTES(BITS_TO_WORDS(n))

#define BITMAP_SENTINEL_WORD(sentinel_bits) \
        ((~(sml_bmword_t)0) << (BITPTR_WORDBITS - (sentinel_bits)))

#define ADD_BYTES(p,n)    ((void*)((char*)(p) + (n)))
#define DIF_BYTES(p1,p2)  ((uintptr_t)((char*)(p1)) - (uintptr_t)((char*)(p2)))

#define BITMAP_BASE(seg, level) \
        ((sml_bmword_t*)ADD_BYTES(seg, (seg)->layout->bitmap_base[level]))
#define BITMAP_LIMIT(seg, level) \
        ((sml_bmword_t*)ADD_BYTES(seg, (seg)->layout->bitmap_base[(level)+1]))
#define BITMAP0_BASE(seg) \
        ((sml_bmword_t*)ADD_BYTES(seg, SEG_BITMAP0_OFFSET))
#define BLOCK_LIMIT(seg) \
        ((char*)ADD_BYTES((seg), (seg)->layout->block_limit))


#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
#define COLLECT_BITMAP_BASE(seg) \
        ((sml_bmword_t*)ADD_BYTES(seg, (seg)->layout->bitmap_base[SEG_RANK]))
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */


static inline unsigned int
object_index(struct segment *seg, void *obj)
{
        assert(segment_addr(obj) == seg);
        assert((char*)obj >= seg->block_base);
        assert((char*)obj < (char*)seg + SEGMENT_SIZE);
        // TODO: non-power-of-2 block sizes
        return DIF_BYTES(obj, seg->block_base) >> seg->blocksize_log2;
}

static inline int
objindex_or_end(struct segment *seg, void *obj)
{
        assert(seg->blocksize_log2 != 0);
        assert((char*)obj >= seg->block_base);
        if ((char*)obj < BLOCK_LIMIT(seg))
          return DIF_BYTES(obj, seg->block_base) >> seg->blocksize_log2;
        else
          return -1;
}

static unsigned int
estimate_num_blocks(unsigned int blocksize_bytes)
{
        unsigned int i;
        double e1, e2;

        /* The num_blocks must be the maximum integer N satisfying
         *   F3 < SEGMENT_SIZE
         * where
         *   b_0 = CEILING(num_blocks + 1, BITMAP_WORDBITS)
         *   b_1 = CEILING(BITS_TO_WORDS(b_1) + 1, BITMAP_WORDBITS)
         *   b_2 = CEILING(BITS_TO_WORDS(b_2) + 1, BITMAP_WORDBITS)
         *   F0 = SEG_INITIAL_OFFSET
         *   F1 = F0 + BITS_TO_WORDS(b_0 + b_1 + b_2)
         *   F2 = CEILING(F1, sizeof(void*)) + N * sizeof(stack_slot)
         *   F3 = CEILING(F2 + OBJ_HEADER_SIZE, MAXALIGN) - OBJ_HEADER_SIZE
         *        + N * block_size
         *
         * This condition can be approximated by
         *   SEG_INITIAL_OFFSET
         *   + N * \Sigma_{i=0}^2 (1 / BITPTR_WORDBITS^i) / 8
         *   + \Sigma_{i=0}^2 \Sigma_{j=0}^i (1 / BITPTR_WORDBITS^j) / 8
         *   + N * sizeof(stack_slot)
         *   + N * block_size
         *   < SEGMENT_SIZE
         *
         * The following code computes the approximated solution of the above
         * inequality.
         * The solution may be slightly larger than the exact num_blocks.
         */
        e1 = 1.0, e2 = 1.0;
        for (i = 1; i < SEG_RANK; i++) {
                e1 = 1.0 + e1 / (double)BITPTR_WORDBITS;
                e2 += e1;
        }
        return (double)(SEGMENT_SIZE - SEG_INITIAL_OFFSET - e2 / 8.0)
                / (sizeof(struct stack_slot) + blocksize_bytes + e1 / 8.0);
}

static void
compute_bitmap_layout(struct segment_layout *layout, unsigned int num_blocks)
{
        unsigned int num_bits, sentinels, num_words, i, offset;

        offset = SEG_BITMAP0_OFFSET;

        for (i = 0; i < SEG_RANK; i++) {
                layout->bitmap_base[i] = offset;
                num_bits = CEILING(num_blocks + 1, BITPTR_WORDBITS);
                sentinels = num_bits - num_blocks;
                layout->bitmap_sentinel[i] = BITMAP_SENTINEL_WORD(sentinels);
                num_words = BITS_TO_WORDS(num_bits);
                offset += WORDS_TO_BYTES(num_words);
                /* do not count sentinels if they occupy a word */
                num_blocks = num_words - (~layout->bitmap_sentinel[i] == 0);
        }
        layout->bitmap_base[i] = offset;
}

static void
compute_layout(unsigned int blocksize_log2)
{
        struct segment_layout *layout;
        unsigned int stack_offset, stack_size, block_limit;

        assert(blocksize_log2 <= BLOCKSIZE_MAX_LOG2);

        layout = &segment_layout[blocksize_log2];
        layout->blocksize_bytes = 1U << blocksize_log2;
        layout->num_blocks = estimate_num_blocks(layout->blocksize_bytes);

        /* find the optimal num_blocks from the approximated num_blocks */
        for (;;) {
                compute_bitmap_layout(layout, layout->num_blocks);
                stack_offset = layout->bitmap_base[SEG_RANK];
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
                stack_offset += BITS_TO_BYTES(layout->num_blocks);
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
                layout->stack_offset = CEILING(stack_offset, sizeof(void*));
                stack_size = layout->num_blocks * sizeof(struct stack_slot);
                layout->stack_limit = layout->stack_offset + stack_size;
                layout->block_offset =
                        CEILING(layout->stack_limit + OBJ_HEADER_SIZE,
                                MAXALIGN);
                block_limit =
                        layout->block_offset
                        + layout->num_blocks * layout->blocksize_bytes;
                if (block_limit <= SEGMENT_SIZE)
                        break;
                layout->num_blocks--;
        }

        layout->block_limit =
                layout->block_offset
                + layout->num_blocks * layout->blocksize_bytes;

#if 0 && !defined NDEBUG
        {
                unsigned int i;
                sml_debug("--- %u ---\n", blocksize_log2);
                sml_debug("block_size = %u\n", layout->blocksize_bytes);
                sml_debug("num_blocks = %u\n", layout->num_blocks);
                for (i = 0; i <= SEG_RANK; i++)
                        sml_debug("bitmap_base[%u] = %u\n", i,
                                  layout->bitmap_base[i]);
                for (i = 0; i < SEG_RANK; i++)
                        sml_debug("bitmap_sentinel[%u] = 0x%08x\n", i,
                                  layout->bitmap_sentinel[i]);
                sml_debug("stack_offset = %u\n", layout->stack_offset);
                sml_debug("stack_limit = %u\n", layout->stack_limit);
                sml_debug("block_offset = %u\n", layout->block_offset);
                sml_debug("block_limit = %u\n", layout->block_limit);
        }
#endif /* NDEBUG */
}

static void
init_segment_layout()
{
        unsigned int i;

        /* dummy layout for fresh segments;
         * segment_layout[0] may be used in init_segment.
         */
        compute_layout(0);

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                compute_layout(i);
                /* struct segment_layout * l = &segment_layout[i];
                   printf("init_layout: i=%d %d %d %d %d\n",
                   i, l->bitmap_base[0], l->bitmap_base[1], l->bitmap_base[2], l->bitmap_base[3]); */
        }
}

/* for debug */
static int ATTR_UNUSED
check_pattern(uword_t* block, unsigned int nwords) {
    unsigned int i;
    if (nwords == 1) return 1; // single-word objects do not get scribbled
    if (block[0] != DEADBEEF) return 0;
    if (nwords == 2) return block[1] == (uword_t)-1;
    for (i=1; i<nwords; ++i) if (block[i]) return 0; // fail
    return 1; // succeed
}

static void ATTR_UNUSED
scribble_segment(struct segment *seg)
{
        char *block = seg->block_base;
        sml_bitptr_t b = BITPTR(BITMAP0_BASE(seg), 0);
        unsigned int i;

        switch (seg->layout->blocksize_bytes) {
        case 8: // fill pattern is 0xAAAAAAAAAAAAAAAA
#if 0
            tprintf("scribble on %p", seg);
            for (i = 0; i < seg->layout->num_blocks; i++) {
                    if (!BITPTR_TEST(b)) *(uword_t*)block = 0xAAAAAAAAAAAAAAAA;
                    BITPTR_INC(b);
                    block += 8;
            }
#endif
            break;
        case 16: // fill pattern is DEADBEEF then 0xff
            for (i = 0; i < seg->layout->num_blocks; i++) {
                    if (!BITPTR_TEST(b)) {
                            ((uword_t*)block)[0] = DEADBEEF;
                            ((uword_t*)block)[1] = (uword_t)-1;
                    }
                    BITPTR_INC(b);
                    block += 16;
            }
            break;
        default: // fill pattern is DEADBEEF then 0
            for (i = 0; i < seg->layout->num_blocks; i++) {
                    if (!BITPTR_TEST(b)) {
                            *(uword_t*)block = DEADBEEF;
                            memset(block + 8, 0, seg->layout->blocksize_bytes - 8);
                    }
                    BITPTR_INC(b);
                    block += seg->layout->blocksize_bytes;
            }
        }
}
void print_free_blocks(struct segment *seg)
{
        char *block = seg->block_base;
        sml_bitptr_t b = BITPTR(BITMAP0_BASE(seg), 0);
        unsigned int i;
        ATTR_UNUSED int n = 0;

        for (i = 0; i < seg->layout->num_blocks; i++) {
          if (!BITPTR_TEST(b)) fprintf(stderr, " free @ %p: %lx %lx\n",
                                       block, 0[(long*)block], 1[(long*)block]);
          BITPTR_INC(b);
          block += seg->layout->blocksize_bytes;
        }
}

/* for debug */
static int ATTR_UNUSED
check_filled(const void *buf, unsigned char c, size_t n)
{
        const unsigned char *p;
        for (p = buf; n > 0; p++, n--) {
                if (*p != c)
                        return 0;
        }
        return 1;
}

static void
init_segment(struct segment *seg, unsigned int blocksize_log2)
{
        const struct segment_layout *new_layout;
        unsigned int i;
        char *old_limit, *new_limit;

        if (blocksize_log2 == 3) TPRINTF(0, "init_segment %p for size 8 (was size %d)", seg, 1<<seg->blocksize_log2);

        assert(BLOCKSIZE_MIN_LOG2 <= blocksize_log2
               && blocksize_log2 <= BLOCKSIZE_MAX_LOG2);

        /* assumption: seg is initialized for some block size with its
         * bitmap and stack area (including padding between the two) being
         * filled with zero except for bitmap sentinels.
         * (We also assume that NULL is equal to zero)
         */

        /* if seg is already initialized for blocksize_log2, do nothing. */
        if (seg->blocksize_log2 == blocksize_log2) {
                seg->block_base = ADD_BYTES(seg, seg->layout->block_offset);
                return;
        }

        new_layout = &segment_layout[blocksize_log2];

        /* clear the bitmap and stack area for the new block size by
         * filling the difference between old and new stack_limit with
         * zero and removing old bitmap sentinels.
         */
        old_limit = ADD_BYTES(seg, seg->layout->stack_limit);
        new_limit = ADD_BYTES(seg, new_layout->stack_limit);
        if (new_limit > old_limit)
                memset(old_limit, 0, new_limit - old_limit);
        /* clear old sentinels */
        for (i = 0; i < SEG_RANK; i++)
                BITMAP_LIMIT(seg, i)[-1] = 0;

        assert(check_filled(BITMAP0_BASE(seg), 0,
                            new_layout->stack_limit - SEG_BITMAP0_OFFSET));

        seg->layout = new_layout;
        seg->blocksize_log2 = blocksize_log2;
        seg->stack = ADD_BYTES(seg, new_layout->stack_offset);
        seg->block_base = ADD_BYTES(seg, new_layout->block_offset);
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        seg->snapshot_free = seg->block_base;
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
        assert(seg->free_count >= 0);
        seg->free_count = 0;

        /* set new sentinels */
        for (i = 0; i < SEG_RANK; i++)
                BITMAP_LIMIT(seg, i)[-1] = new_layout->bitmap_sentinel[i];

        scribble_segment(seg);
}

static void
clear_bitmap(struct segment *seg)
{
        unsigned int i;

        memset(BITMAP0_BASE(seg), 0,
               seg->layout->bitmap_base[SEG_RANK] - SEG_BITMAP0_OFFSET);
        for (i = 0; i < SEG_RANK; i++)
                BITMAP_LIMIT(seg, i)[-1] = seg->layout->bitmap_sentinel[i];
}

static void
mark_bits(struct segment *seg, unsigned int index, sml_bitptrw_t b)
{
        unsigned int i;

        assert(BITPTRW_EQUAL(b, BITPTRW(BITMAP0_BASE(seg), index)));
        assert(index < seg->layout->num_blocks);

        BITPTRW_SET(b);
        for (i = 1; ~BITPTRW_WORD(b) == 0U && i < SEG_RANK; i++) {
                index /= BITPTR_WORDBITS;
                b = BITPTRW(BITMAP_BASE(seg, i), index);
                assert(b.wptr < BITMAP_LIMIT(seg, i));
                BITPTRW_SET(b);
        }
}

struct stat_segment {
        unsigned int num_marked;
        unsigned int num_unmarked;
        unsigned int num_marked_before_free;
        unsigned int num_unmarked_before_free;
};

static struct stat_segment
stat_segment(const struct segment *seg)
{
        sml_bitptr_t b = BITPTR(BITMAP0_BASE(seg), 0);
        char *block = seg->block_base;
        struct stat_segment stat = {0, 0, 0, 0};
        unsigned int i;

        for (i = 0; i < seg->layout->num_blocks; i++) {
                if (BITPTR_TEST(b)) {
                        stat.num_marked++;
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
                        if (block < seg->snapshot_free)
                                stat.num_marked_before_free++;
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
                } else {
                        stat.num_unmarked++;
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
                        if (block < seg->snapshot_free)
                                stat.num_unmarked_before_free++;
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
                }
                BITPTR_INC(b);
                block += seg->layout->blocksize_bytes;
        }
        return stat;
}

/* for debug */
static ATTR_UNUSED void
dump_segment(const char *prefix, const struct segment *seg)
{
        struct stat_segment stat = stat_segment(seg);
        sml_debug("%s%p: blksiz=%u, 1/0=%u/%u|%u/%u, fill/total=%u/%u\n",
                  prefix, seg, seg->layout->blocksize_bytes,
                  stat.num_marked_before_free,
                  stat.num_unmarked_before_free,
                  stat.num_marked - stat.num_marked_before_free,
                  stat.num_unmarked - stat.num_unmarked_before_free,
                  stat.num_marked + stat.num_unmarked_before_free,
                  seg->layout->num_blocks);
}

/********** heap space ************/

struct heap_space {
        void *begin, *end;
        struct segment *free;
        unsigned int min_num_segments, max_num_segments;
        unsigned int num_committed;
};

static void
init_heap_space(struct heap_space *heap, size_t min_size, size_t max_size)
{
        size_t freesize_post;
        void *p;

        if (SEGMENT_SIZE % GetPageSize() != 0)
                sml_fatal(0, "SEGMENT_SIZE is not aligned in page size.");

        min_size = CEILING(min_size, SEGMENT_SIZE);
        max_size = CEILING(max_size, SEGMENT_SIZE);

        /* reserve address space of max_size bytes beginning with the
         * address of multiple of SEGMENT_SIZE.  To ensure this constraint,
         * we first reserve the address space with additional SEGMENT_SIZE
         * bytes and then release SEGMENT_SIZE bytes in total from the head
         * and end of the space.
         */
        void* want = (void*)0x1100000000; // SBCL likes its heap at a known address
        //void* want  = 0;
        p = ReservePage(want, max_size + SEGMENT_SIZE);
        if (p == AllocPageError)
                sml_fatal(0, "failed to alloc virtual memory.");
        freesize_post = (uintptr_t)p & (SEGMENT_SIZE - 1);
        if (freesize_post == 0) {
                ReleasePage(ADD_BYTES(p, max_size), SEGMENT_SIZE);
        } else {
                ReleasePage(p, SEGMENT_SIZE - freesize_post);
                p = ADD_BYTES(p, SEGMENT_SIZE - freesize_post);
                ReleasePage(ADD_BYTES(p, max_size), freesize_post);
        }

        fprintf(stderr, "freesize_post=%lx heap_base=%p\n", freesize_post, p);
        heap->begin = p;
        heap->end = ADD_BYTES(p, max_size);
        heap->free = p;
        heap->min_num_segments = min_size / SEGMENT_SIZE;
        heap->max_num_segments = max_size / SEGMENT_SIZE;
        heap->num_committed = 0;

#ifdef EAGER_MMAP
        CommitPage(p, max_size);
        for (; p < heap->end; p = ADD_BYTES(p, 4096))
                *(void**)p = NULL;
#endif /* EAGER_MMAP */
}

static void
destroy_heap_space(struct heap_space *heap)
{
        ReleasePage(heap->begin, (char*)heap->end - (char*)heap->begin);
}

static void
allocate_segments(struct heap_space *heap, unsigned int count,
                  struct list *segs)
{
        struct segment *seg;
        unsigned int alloc;

        alloc = (count <= heap->max_num_segments - heap->num_committed)
                ? count : heap->max_num_segments - heap->num_committed;
        heap->num_committed += alloc;
        sml_debug("allocate %u segments (total %u segments)\n",
                  alloc, heap->num_committed);
#ifndef EAGER_MMAP
        CommitPage(heap->free, SEGMENT_SIZE * (unsigned long)alloc);
#endif /* EAGER_MMAP */

        for (; alloc > 0; alloc--) {
                seg = heap->free;
                /* Memory pages allocated by CommitPage is filled
                 * with zero.  Set seg->layout for init_segment. */
                seg->layout = &segment_layout[0];
                list_append(segs, &seg->as_list);
                heap->free = ADD_BYTES(seg, SEGMENT_SIZE);
        }

        /* TODO: deallocate segments */
}

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
static void
clear_collect_bitmaps(struct heap_space *heap)
{
        struct segment *seg;

        // This clears all 'visited' bits for all segments regardless of whether
        // the segment is in filled, active, partial.
        for (seg = heap->begin; seg < heap->free;
             seg = ADD_BYTES(seg, SEGMENT_SIZE)) {
                if (!seg->block_base)
                        continue;
                /*fprintf(stderr, "clear_collect_bitmaps: seg @ %p, blksize = %d, num_blocks = %d, nbytes = %d\n",
                        seg, seg->layout->blocksize_bytes,
                        seg->layout->num_blocks,
                        seg->layout->stack_offset - seg->layout->bitmap_base[SEG_RANK]);*/
                memset(COLLECT_BITMAP_BASE(seg), 0,
                       seg->layout->stack_offset
                       - seg->layout->bitmap_base[SEG_RANK]);
        }
}
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */


/********** segment pool ***********/

static struct {
        struct stack freelist;
        struct heap_space heap;
        unsigned int extend_step;
} segment_pool;

static int ATTR_UNUSED segment_number(void* s) {
  return ((char*)s - (char*)segment_pool.heap.begin) / SEGMENT_SIZE;
}

void get_segment_pool_bounds(char* bounds[2]) {
    bounds[0] = segment_pool.heap.begin;
    bounds[1] = segment_pool.heap.end;
}

static void
init_segment_pool(size_t min_size, size_t max_size)
{
        struct list segs;

        init_heap_space(&segment_pool.heap, min_size, max_size);

        segment_pool.extend_step =
                segment_pool.heap.min_num_segments > 0
                ? segment_pool.heap.min_num_segments : 1;

        list_init(&segs);
        allocate_segments(&segment_pool.heap,
                          segment_pool.heap.min_num_segments,
                          &segs);
        stack_push_list(&segment_pool.freelist, &segs);
}

static void
destroy_segment_pool()
{
        destroy_heap_space(&segment_pool.heap);
}

static struct segment *
new_segment(unsigned int blocksize_log2)
{
        struct segment *seg;
        seg = stack_pop(&segment_pool.freelist);
        if (seg) {
                assert((char*)segment_pool.heap.begin <= (char*)seg
                       && (char*)seg < (char*)segment_pool.heap.end);
                init_segment(seg, blocksize_log2);
        }
        return seg;
}

/********** sub-heaps ***********/

struct subheap {
        /* segments each of which has at least one free block */
        struct stack partial;
        /* segments consumed by mutators */
        struct stack filled;
        /* number of segments belonging to this subheap */
        _Atomic(unsigned int) num_segments;
        /* number of free blocks in partial */
        _Atomic(unsigned int) num_free_blocks;
        /* disallow allocating new segment to this subheap */
        _Atomic(unsigned int) do_not_extend;
        /* sizeof(struct subheap) must be power of 2 for performance */
        unsigned int dummy_;
};

static struct subheap global_subheaps[BLOCKSIZE_MAX_LOG2 + 1];

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
static void
collect_subheap(struct subheap *subheap, struct segment **seg_p)
{
        struct list filled;
        struct segment *partial;

        filled.head = stack_flush(&subheap->filled);
        filled.last = filled.head ? list_last(filled.head) : &filled.head;
        partial = stack_flush(&subheap->partial);
        *seg_p = list_finish(&filled, &partial->as_list);
}

#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
static void
gather_subheap(struct subheap *subheap, struct segment **seg_p)
{
        *seg_p = stack_flush(&subheap->filled);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

static void
gather_subheaps(struct segment **segarray)
{
        unsigned int i;

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++)
                gather_subheap(&global_subheaps[i], &segarray[i]);
}

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
static void
move_partial_to_filled()
{
        unsigned int i;
        struct segment *seg;
        struct list l;

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                struct subheap *subheap = &global_subheaps[i];
                seg = stack_flush(&subheap->partial);
                store_relaxed(&subheap->num_free_blocks, 0);
                if (seg) {
                        l.head = &seg->as_list;
                        l.last = list_last(l.head);
                        stack_push_list(&global_subheaps[i].filled, &l);
                }
        }
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

struct stat_subheap {
        unsigned int num_filled;
        unsigned int num_partial;
        struct stat_segment total;
};

static struct stat_subheap
stat_subheap(struct subheap *subheap)
{
        struct segment *seg;
        struct stat_subheap st = {0, 0, {0, 0, 0, 0}};
        struct stat_segment s;

        for (seg = (struct segment *)load_relaxed(&subheap->filled.top); seg;
             seg = (struct segment *)seg->as_list.next) {
                st.num_filled++;
                s = stat_segment(seg);
                st.total.num_marked += s.num_marked;
                st.total.num_unmarked += s.num_unmarked;
                st.total.num_marked_before_free += s.num_marked;
                st.total.num_unmarked_before_free += s.num_unmarked;
        }
        for (seg = (struct segment *)load_relaxed(&subheap->partial.top); seg;
             seg = (struct segment *)seg->as_list.next) {
                st.num_partial++;
                s = stat_segment(seg);
                st.total.num_marked += s.num_marked;
                st.total.num_unmarked += s.num_unmarked;
                st.total.num_marked_before_free += s.num_marked_before_free;
                st.total.num_unmarked_before_free += s.num_unmarked_before_free;
        }

        return st;
}

/* for debug */
static ATTR_UNUSED void
dump_subheap(struct subheap *subheap)
{
        struct stat_subheap st = stat_subheap(subheap);
        struct segment *seg;

        sml_debug("%u filled / %u partial\n", st.num_filled, st.num_partial);
        for (seg = (struct segment *)load_relaxed(&subheap->filled.top); seg;
             seg = (struct segment *)seg->as_list.next)
                dump_segment("F ", seg);
        for (seg = (struct segment *)load_relaxed(&subheap->partial.top); seg;
             seg = (struct segment *)seg->as_list.next)
                dump_segment("P ", seg);
}

/* for debug */
/*static ATTR_UNUSED*/ void
dump_global_subheaps()
{
        unsigned int i;
        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                sml_debug("subheap %u:\n", i);
                dump_subheap(&global_subheaps[i]);
        }
}

/********** malloc segments *********/

struct malloc_segment {
        struct list_item as_list;
        struct stack_slot stack;
#ifdef LISP_FEATURE_SBCL
        unsigned long markbit : 1;
        unsigned long size : 63; // user-requested size (excluding the mseg header)
        uword_t* voucher; // MUST be 1 word prior to the object header
#else
        sml_bmword_t markbit;
        char dummy_for_objheader[OBJ_HEADER_SIZE];
#endif
};

#define MALLOC_OBJECT_OFFSET \
        CEILING(sizeof(struct malloc_segment), MAXALIGN)
#define OBJ_TO_MALLOC_SEGMENT(obj) \
        ((struct malloc_segment *)ADD_BYTES(obj, -MALLOC_OBJECT_OFFSET))
#define MALLOC_SEGMENT_TO_OBJ(mseg) \
        ADD_BYTES(mseg, MALLOC_OBJECT_OFFSET)
#define MALLOC_SEGMENT_SIZE(mseg) (MALLOC_OBJECT_OFFSET + mseg->size)

/* initialized correctly by default */
static struct stack malloc_segments;
/* For initializing the large codeblob tree and not used thereafter */
extern void lose(char*,...);
void* get_allocated_msegs() { return malloc_segments.top; }

int in_bitmapped_subheap(void* addr) {
    if (addr >= segment_pool.heap.begin && addr < (void*)segment_pool.heap.end) return 1;
    return 0;
}

/*int in_smlgc_bitmapped_space(void* addr) { // FIXME?
    if (in_bitmapped_subheap(addr)) return 1;
    return 0;
}*/

uword_t* small_block_from_interior_ptr(char* addr, int* pblocksize) {
    if (addr >= (char*)segment_pool.heap.begin && addr < (char*)segment_pool.heap.end) {
        struct segment *seg = segment_addr(addr);
        if (seg->block_base && addr >= seg->block_base && addr < BLOCK_LIMIT(seg)) {
            // TODO: non-power-of-2 block sizes
            unsigned int index = DIF_BYTES(addr, seg->block_base) >> seg->blocksize_log2;
            if (pblocksize) *pblocksize = seg->layout->blocksize_bytes;
            return (uword_t*)(seg->block_base + (index << seg->blocksize_log2));
        }
    }
    return 0;
}
int seg_blocksize_of(void* addr) {
    if (addr >= segment_pool.heap.begin && addr < segment_pool.heap.end) {
        struct segment *seg = segment_addr(addr);
        return 1 << seg->blocksize_log2; // TODO: non-power-of-2 size
    }
    return 0;
}

/*static*/ struct malloc_segment *
alloc_mseg(int executable, unsigned int alloc_size)
{
        struct malloc_segment *mseg;

#ifdef LISP_FEATURE_SBCL
        extern void* alloc_mseg_impl(int, size_t);
        mseg = alloc_mseg_impl(executable, alloc_size);
        assert(((uword_t)mseg & 0xF) == 0);
        mseg->size = alloc_size;
#else
        mseg = xmalloc(MALLOC_OBJECT_OFFSET + alloc_size);
        DEBUG(memset(mseg, 0x55, MALLOC_OBJECT_OFFSET + alloc_size));
#endif
        store_relaxed(&mseg->stack.next, NULL);
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        /* Indicate that this is allocated before SYNC2 */
        mseg->markbit = (sml_current_phase() >= SYNC2);
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
        stack_push(&malloc_segments, &mseg->as_list);
        return mseg;
}

_Atomic(struct malloc_segment*) msegs_pending_free;

static void
destroy_malloc_segment(struct malloc_segment *mseg)
{
        assert(mseg->markbit == 0);
        /* If GC decided that it can free the voucher (which it should have decided),
         * based on it being in a GC-able segment, then it would have called scribble()
         * on that segment and deposited the filler into the voucher.
         * Therefore this MUST NOT try to write a 0 or something into the voucher
         * because the filler pattern might not be 0. I tried storing a DEADBEEF
         * but that didn't do me any favors, as it tripped the check_pattern assertion.
         * So just leave the voucher alone if scribbling is enabled */
        *mseg->voucher = 0;
        // DEBUG(memset(mseg, 0xFF, MALLOC_SEGMENT_SIZE(mseg)));
        uword_t* userobj = MALLOC_SEGMENT_TO_OBJ(mseg);
        // { char buf[80]; int n = snprintf(buf, sizeof buf, "free %p\n", mseg); write(2, buf, n); }
        if ((*userobj & WIDETAG_MASK) == CODE_HEADER_WIDETAG) {
            userobj[-1] = 0; // delete the pointer to the voucher (doesn't matter really)
            userobj[1] = userobj[0]; // copy the old header
            userobj[0] = 0; // not a valid header
            struct malloc_segment* next = atomic_load(&msegs_pending_free);
            TPRINTF(1, "defer free mseg %p size %ld", mseg, mseg->size);
            // FIXME: think about whether this succumbs to the ABA problem
            // and explain why it doesn't, if it doesn't.
            do  {
                mseg->as_list.next = (void*)next;
            } while (!cmpswap_relaxed(&msegs_pending_free, &next, mseg));
            // printf("Deferred free code @ %p\n", mseg);
        } else {
            TPRINTF(1, "free %p size %ld", mseg, mseg->size);
            xfree(mseg, "mseg");
        }
}

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
static void
collect_malloc_segments(struct malloc_segment **collect_msegs)
{
        *collect_msegs = stack_flush(&malloc_segments);
}

#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
static void
gather_malloc_segments(struct malloc_segment **collect_msegs)
{
        *collect_msegs = stack_flush(&malloc_segments);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

static void
destroy_malloc_segments()
{
        struct malloc_segment *mseg, *next;

        abort();
        mseg = stack_flush(&malloc_segments);
        while (mseg) {
                next = (struct malloc_segment *)mseg->as_list.next;
                free(mseg);
                mseg = next;
        }
}

/********** heap summary ***********/

static void
print_heap_summary()
{
        unsigned int i, num_free = 0, num_malloc = 0, num_malloc_bytes = 0;
        unsigned int num_segments_total = 0;
        unsigned int num_total_blocks = 0, num_filled_blocks = 0;
        struct stat_subheap st;
        struct segment *seg;
        struct subheap *subheap;
        struct malloc_segment *mseg;

        sml_notice("heap usage summary:");
        sml_notice("heap size = %u segments (min %u -- max %u)",
                   segment_pool.heap.num_committed,
                   segment_pool.heap.min_num_segments,
                   segment_pool.heap.max_num_segments);
        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                subheap = &global_subheaps[i];
                st = stat_subheap(subheap);
                sml_notice("subheap %2u: %4u + %4u / %4u segs, "
                           "%7u / %7u blocks filled %s",
                           i, st.num_filled, st.num_partial,
                           (unsigned)load_relaxed(&subheap->num_segments),
                           st.total.num_marked
                           + st.total.num_unmarked_before_free,
                           st.total.num_marked + st.total.num_unmarked,
                           load_relaxed(&subheap->do_not_extend) ? "*" : "");
                num_segments_total += st.num_filled + st.num_partial;
                num_filled_blocks += st.total.num_marked
                        + st.total.num_unmarked_before_free;
                num_total_blocks += st.total.num_marked + st.total.num_unmarked;
        }
        sml_notice("block utilization : %u / %u (%6.2f%%)",
                   num_filled_blocks, num_total_blocks,
                   (double)num_filled_blocks / num_total_blocks * 100.0);

        for (mseg = (struct malloc_segment *)load_relaxed(&malloc_segments.top);
             mseg;
             mseg = (struct malloc_segment *)mseg->as_list.next) {
                num_malloc++;
                num_malloc_bytes += MALLOC_SEGMENT_SIZE(mseg);
        }
        sml_notice("subheap malloc: %5u segments, %8u bytes in total",
                   num_malloc, num_malloc_bytes);

        for (seg = (struct segment *)load_relaxed(&segment_pool.freelist.top);
             seg;
             seg = (struct segment *)seg->as_list.next)
                num_free++;
        num_segments_total += num_free;
        sml_notice("%u segments in freelist", num_free);
        sml_notice("%u segments in total", num_segments_total);
}

/* for debug */
static void ATTR_UNUSED
print_heap_summary_light()
{
        unsigned int total_alloc = 0, total_filled = 0;
        unsigned int i;

        sml_notice("heap usage summary:");
        sml_debug("heap size = %u segments (min %u -- max %u)\n",
                   segment_pool.heap.num_committed,
                   segment_pool.heap.min_num_segments,
                   segment_pool.heap.max_num_segments);
        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                struct subheap *s = &global_subheaps[i];
                unsigned num_blocks = segment_layout[i].num_blocks;
                unsigned num_segments = load_relaxed(&s->num_segments);
                unsigned num_free_blocks = load_relaxed(&s->num_free_blocks);
                unsigned num_alloc = num_segments * num_blocks;
                unsigned num_filled = num_alloc - num_free_blocks;
                total_alloc += num_alloc;
                total_filled += num_filled;
                sml_debug("subheap %2u: %4u segs, %7u / %7u blocks filled\n",
                          i, num_segments, num_filled, num_alloc);
        }
        sml_debug("block utilization: %7u / %7u (%6.2f%%)\n",
                  total_filled, total_alloc,
                  (double)total_filled / total_alloc * 100.0);
}

/********** object list ***********/

#define NIL ((void*)1)

static struct stack_slot *
object_stack_slot(void *obj)
{
        struct segment *seg;
        struct malloc_segment *mseg;

#ifndef LISP_FEATURE_SBCL
        if (obj == NULL)
                return NULL;
        if (OBJ_HEADER(obj) & OBJ_FLAG_SKIP)
                return NULL;
#endif
        if (!in_bitmapped_subheap(obj)) {
                mseg = OBJ_TO_MALLOC_SEGMENT(obj);
                return &mseg->stack;
        } else {
                seg = segment_addr(obj);
                assert((char*)segment_pool.heap.begin <= (char*)seg
                       && (char*)seg < (char*)segment_pool.heap.end);
                return &seg->stack[object_index(seg, obj)];
        }
}

struct stack_slot *careful_lispobj_stack_slot(lispobj taggedptr)
{
        unsigned char lowtag;
        void* obj = (void*)taggedptr;

        if (in_bitmapped_subheap(obj)) {
                struct segment *seg = segment_addr(obj);
                return &seg->stack[object_index(seg, obj)];
        } else if ((lowtag = lowtag_of(taggedptr)) == LIST_POINTER_LOWTAG
                   || lowtag == INSTANCE_POINTER_LOWTAG
                   || ignorable_space_p(taggedptr)) {
                return 0;
        } else if (large_code_subspace_p(obj)) {
                lispobj* code = untagged_baseptr((lispobj)obj);
                return &(OBJ_TO_MALLOC_SEGMENT(code)->stack);
        } else if (lowtag == OTHER_POINTER_LOWTAG) {
                struct malloc_segment *mseg = otherptr_mseg(taggedptr);
                return mseg ? &mseg->stack : 0;
        }
        return NULL;
}

static int is_gcable_object(lispobj taggedptr)
{
        void* obj = (void*)taggedptr;

        if (in_bitmapped_subheap(obj) || large_code_subspace_p(obj)) return 1;
        unsigned char lowtag = lowtag_of(taggedptr);
        if (lowtag == LIST_POINTER_LOWTAG || lowtag == INSTANCE_POINTER_LOWTAG
            || ignorable_space_p(taggedptr)) return 0;
        if (lowtag == OTHER_POINTER_LOWTAG && otherptr_mseg(taggedptr) != NULL) return 1;
        return 0;
}

//int forwardable_descriptor_p(lispobj taggedptr) { return is_gcable_object(taggedptr); }

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
static void
object_list_init(struct object_list *l)
{
        atomic_init(&l->begin.next, NIL);
        l->last = &l->begin;
        l->count = 0;
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
/*static*/ void
object_list_append(struct object_list *l, void *obj)
{
        struct stack_slot *slot = object_stack_slot(obj);
        void *old = NULL;

        /* this may fail if obj already belongs to another list or stack */
        if (slot
            && load_relaxed(&slot->next) == old
            && cmpswap_relaxed(&slot->next, &old, NIL)) {
                store_relaxed(&l->last->next, obj);
                l->last = slot, ++l->count;
        }
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
static void ATTR_UNUSED
enum_obj_to_list_notused(void **slot, void *data)
{
        void *obj = *slot;
        struct object_list *objs = data;
        object_list_append(objs, obj);
}
static void enum_obj_to_list(uintptr_t pointer, void *data) {
        object_list_append(data, (void*)pointer);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

/********** object stack ***********/

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
struct object_stack {
        void *top;
        sml_spinlock_t lock;
};
#endif /* !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY */

#define EMPTY_OBJECT_STACK \
        {.top = ATOMIC_VAR_INIT(NIL), .lock = SPIN_LOCK_INIT}

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
static void
push_objects(struct object_stack *stack, struct object_list *l)
{
        void *begin = load_relaxed(&l->begin.next);

        if (begin == NIL)
                return;

        spin_lock(&stack->lock);
        store_relaxed(&l->last->next, stack->top);
        stack->top = begin;
        spin_unlock(&stack->lock);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY

int obj_chain_length_capped(void* chain, int max) {
  int n = 0;
  while (chain != NIL) {
    ++n;
    if (n == max) return -1;
    struct stack_slot * slot = object_stack_slot(chain);
    chain = slot->next;
  }
  return n;
}

_Atomic(int) weakRefState = weakRefState_NORMAL;
int weakTableOpsInhibited;
pthread_rwlock_t weakTableLock = PTHREAD_RWLOCK_INITIALIZER;
//void* visit_weak_hash_tables();

extern void add_weak_tables_edges(void*,struct object_list*);
extern void smash_weak_table_pairs(void*);
static void* GetWeakTableList();

static void *flush_gray_stack(struct object_stack *stack)
{
        void *top;
        int verbosity = 1;

        spin_lock(&stack->lock);
        top = stack->top;
        stack->top = NIL;
        if (verbosity) {
            int n = obj_chain_length_capped(top, 500);
            fprintf(stderr, n<0?"flush_gray: >500 to go\n":"flush_gray: %d to go\n", n);
        }
        if (top == NIL) {
            if (verbosity) fprintf(stderr, " -- gray stack is empty, tableInhibit=%d\n", weakTableOpsInhibited);
            if (!weakTableOpsInhibited) {
                /* If gray list was empty then there we are almost done,
                 * other than considering weak hash tables. To prevent races,
                 * suspend all weak table operations by mutators henceforth
                 * until the gray list is completely drained.
                 * As long as a worker thread has nothing to do with weak tables,
                 * it won't have to wait */
                spin_unlock(&stack->lock);
                pthread_rwlock_wrlock(&weakTableLock);
                weakTableOpsInhibited = 1;
                struct object_list more_objects;
                object_list_init(&more_objects);
                add_weak_tables_edges(GetWeakTableList(), &more_objects);
                push_objects(stack, &more_objects);
                return flush_gray_stack(stack);
            }
            int old = weakRefState_TRACING;
            if (atomic_compare_exchange_strong(&weakRefState, &old, weakRefState_SPLAT)) {
                if (verbosity) fprintf(stderr, "weakRefState changed\n");
            } else {
                lose("wat?");
            }
        }
        spin_unlock(&stack->lock);
        return top;
}
static void *
flush_object_stack(struct object_stack *stack)
{
        void *top;

        spin_lock(&stack->lock);
        top = stack->top;
        stack->top = NIL;
        spin_unlock(&stack->lock);
        return top;
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

/********** allocation pointers ***********/

union sml_alloc {
        struct alloc_ptr ptr[BLOCKSIZE_MAX_LOG2 + 1]; /* ptr[0] is not used. */
        struct list_item as_list;
};

/* use freebit.ptr instead of free because free may point to outside */
#define ALLOC_PTR_TO_SEGMENT(alloc_ptr)        \
        segment_addr((alloc_ptr)->freebit.ptr)

/* no need to set a destructor specific to alloc_ptr_set since it is
 * deallocated correctly when the current control is deallocated. */
worker_tlv_alloc(union sml_alloc *, alloc_ptr_set, (void));

static const unsigned int dummy_bitmap = ~0U;
static const sml_bitptr_t dummy_bitptr = { (unsigned int *)&dummy_bitmap, 1 };

/* initialized correctly by default */
struct {
        struct stack freelist;
} alloc_ptr_set_pool;

static void
destroy_alloc_ptr_set_pool()
{
        union sml_alloc *p, *next;

        p = stack_flush(&alloc_ptr_set_pool.freelist);
        while (p) {
                next = (union sml_alloc *)p->as_list.next;
                xfree(p, "ap_set");
                p = next;
        }
}

#define clear_alloc_ptr smlgc_clear_alloc_ptr
/*static*/ void
clear_alloc_ptr(struct alloc_ptr *ptr)
{
        ptr->freebit = dummy_bitptr;
        ptr->free = NULL;
}

static void
set_alloc_ptr(struct alloc_ptr *ptr, struct segment *seg)
{
        ptr->freebit = BITPTR(BITMAP0_BASE(seg), 0);
        ptr->free = seg->block_base;
        assert(ptr->blocksize_bytes == (1U << seg->blocksize_log2));
        DEBUG(seg->as_list.next = (void*)-1);
        assert(seg->free_count >= 0);
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        assert(seg->snapshot_free == seg->block_base);
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
}

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
static struct segment *
save_alloc_ptr(struct alloc_ptr *ptr)
{
        return ALLOC_PTR_TO_SEGMENT(ptr);
}

#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
static struct segment *
save_alloc_ptr(struct alloc_ptr *ptr)
{
        enum sml_sync_phase phase = sml_current_phase();
        struct segment *seg = ALLOC_PTR_TO_SEGMENT(ptr);

        assert(ptr->free != NULL);

        /* Note that "phase" may differ from the current phase since the
         * current phase may be changed after reading it.
         * if phase is       then the current phase may be
         *  ASYNC              ASYNC or SYNC1
         *  PRESYNC1           PRESYNC1
         *  SYNC1              SYNC1 or PRESYNC2
         *  PRESYNC2           PRESYNC2
         *  SYNC2              SYNC2, MARK, ASYNC, or PRESYNC1
         *  MARK               MARK, ASYNC, or PRESYNC1
         */
        if (phase <= PRESYNC1) {
                /* It is ensured that the current phase is ASYNC or PRESYNC1
                 * and therefore seg is going to be included in the collect
                 * set.  We do not need to set snapshot_free here. */
        } else if (phase <= PRESYNC2) {
                /* The current phase is either SYNC1 or PRESYNC2.
                 * The collector will decide to include seg in the collect set
                 * by seg->snapshot_free.  Here, we set snapshot_free to the
                 * current allocation pointer in order to indicate that all
                 * blocks are filled before SYNC2. */
                seg->snapshot_free = ptr->free;
        } else {
                /* The current phase is either SYNC2, MARK, ASYNC, or PRESYNC1.
                 * An allocation pointer has been saved in snapshot_free.
                 * During SYNC2 and MARK, the saved snapshot_free must be kept
                 * unchanged.  In ASYNC and PRESYNC1, seg is going to be
                 * included in the collect set at the next collection
                 * regardless of snapshot_free.
                 * Concequently, we do not need to set snapshot_free here. */
        }

        return seg;
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
static void
save_alloc_ptr_set_sync2(union sml_alloc *ptr_set)
{
        struct alloc_ptr *ptr;
        unsigned int i;

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                ptr = &ptr_set->ptr[i];
                if (!ptr->free) continue;
#if 0
                struct segment* s = ALLOC_PTR_TO_SEGMENT(ptr);
                if (logfile)
                    fprintf(logfile, "ap[%2d] seg=%5d @ %p free=%d/%d\n",
                            i, segment_number(s), s,
                            (ptr->free < BLOCK_LIMIT(s) ? (int)object_index(s, ptr->free) : -1),
                            s->layout->num_blocks);
#endif
                ALLOC_PTR_TO_SEGMENT(ptr)->snapshot_free = ptr->free;
        }
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

static union sml_alloc *
new_alloc_ptr_set()
{
        union sml_alloc *p;
        unsigned int i;

        p = stack_pop(&alloc_ptr_set_pool.freelist);

        if (!p) {
                p = xmalloc(sizeof(union sml_alloc), "ap_set");
                for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                        p->ptr[i].blocksize_bytes = 1U << i;
                        clear_alloc_ptr(&p->ptr[i]);
                }
        }

        return p;
}

static void
move_to_filled(struct alloc_ptr *ptr, struct subheap *subheap)
{
        struct segment *seg;
        seg = save_alloc_ptr(ptr);
#if 0
        if (seg->blocksize_log2 == 3) {
            unsigned int free_index = object_index_or_end(seg, ptr->free);
            TPRINTF(0, "move_to_filled %p %d/%d (ϕ%s)",
                    seg, free_index, seg->layout->num_blocks,
                    phase_name(sml_current_phase()));
        }
#endif
        assert(subheap == &global_subheaps[seg->blocksize_log2]);
        assert(seg->as_list.next == (void*)-1);
        stack_push(&subheap->filled, &seg->as_list);
}

void move_cons_ap_to_filled(struct alloc_ptr *ap) {
    move_to_filled(ap, &global_subheaps[4]);
}

static void
move_all_to_filled(union sml_alloc *ptr_set)
{
        unsigned int i;
        struct alloc_ptr *ptr;

        /* FIXME: need to take note of the extra alloc_ptr which is embedded
         * in 'struct thread' */
        lose("move_all_to_filled - not done");
        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                ptr = &ptr_set->ptr[i];
                if (ptr->free) {
                        move_to_filled(ptr, &global_subheaps[i]);
                        clear_alloc_ptr(ptr);
                }
        }
        TPRINTF(0, "end move_all_to_filled");
}

static void
gather_alloc_ptr_set(union sml_alloc *p, struct segment **segarray)
{
        unsigned int i;
        struct segment *seg;

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                if (!p->ptr[i].free)
                        continue;
                seg = ALLOC_PTR_TO_SEGMENT(&p->ptr[i]);
                assert(seg->blocksize_log2 == i);
                seg->as_list.next = &segarray[i]->as_list;
                segarray[i] = seg;
        }
}

static void
gather_alloc_ptr_set_pool(struct segment **segarray)
{
        union sml_alloc *p, *next;

        p = stack_flush(&alloc_ptr_set_pool.freelist);
        TPRINTF(1, "alloc_ptr_set_pool.freelist = %p", p);
        while (p) {
                gather_alloc_ptr_set(p, segarray);
                next = (union sml_alloc *)p->as_list.next;
                xfree(p, "ap_set");
                p = next;
        }
}

/********** collect set **********/

union collect_set {
        struct segment *segments[BLOCKSIZE_MAX_LOG2 + 1];
        struct malloc_segment *malloc_segments;
};

static FILE* gc_thread_log;
FILE* get_gc_thread_log(){
  return 0;
  //if (!gc_thread_log) setlinebuf(gc_thread_log = fopen("gc-thread.txt","w"));
  return gc_thread_log;
}

static void
gather_segments(union collect_set *c)
{
        gather_subheaps(c->segments);
        gather_malloc_segments(&c->malloc_segments);
        gather_alloc_ptr_set_pool(c->segments);

        struct segment* seg = c->segments[3];
        for ( ; seg ; seg = (void*)seg->as_list.next ) TPRINTF(0, "condemned %p", seg);
}

static unsigned int
clear_collect_set(union collect_set *c)
{
        struct segment *seg;
        struct malloc_segment *mseg;
        unsigned int i, count = 0;

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                seg = c->segments[i];
                for (; seg; seg = (struct segment *)seg->as_list.next) {
                        clear_bitmap(seg);
                        assert(seg->free_count >= 0);
                        seg->free_count = -seg->layout->num_blocks;
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
                        /* Set snapshot_free to the end of the segment since
                         * all objects in the segment was allocated before
                         * determining the collect set. */
                        seg->snapshot_free = ADD_BYTES(seg, SEGMENT_SIZE);
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
                        count++;
                }
        }

        for (mseg = c->malloc_segments; mseg;
             mseg = (struct malloc_segment *)mseg->as_list.next) {
                mseg->markbit = 0;
                /* count++; */
        }

        return count;
}

struct reclaim {
        struct pre_subheap {
                struct list partial, filled;
                unsigned int num_free;
                unsigned int num_free_blocks;
        } subheap[BLOCKSIZE_MAX_LOG2 + 1];
        unsigned int num_filled;
        struct list freelist;
        struct list malloc_segments;
};

static void
separate_segments(union collect_set *c, struct reclaim *r)
{
        unsigned int i;
        struct segment *seg;
        struct malloc_segment *mseg, *next;
        struct pre_subheap *s;

        list_init(&r->freelist);
        r->num_filled = 0;

        TPRINTF(0, "BEGIN separate_segments");

        char buf[200];
        int bufp = 0;

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                s = &r->subheap[i];
                list_init(&s->partial);
                list_init(&s->filled);
                s->num_free = 0;
                s->num_free_blocks = 0;
                int quick_scribbled = 0, careful_scribbled = 0;
                for (seg = c->segments[i]; seg;
                     seg = (struct segment *)seg->as_list.next) {
                        assert(seg->free_count <= 0);
                        if (i==3) TPRINTF(0, "segment @ %p: freect=%d", seg, seg->free_count);
                        seg->free_count *= -1;
                        if (seg->free_count == (int)seg->layout->num_blocks) {
                            char *block = seg->block_base;
                            int fillbyte = seg->layout->blocksize_bytes == 2*N_WORD_BYTES ? -1 : 0;
                            memset(block, fillbyte,
                                   seg->layout->num_blocks * seg->layout->blocksize_bytes);
                            ++quick_scribbled;
                        } else {
                            scribble_segment(seg);
                            ++careful_scribbled;
                        }
                        /*DEBUG(scribble_segment(seg));*/
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
                        seg->snapshot_free = seg->block_base;
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

                        // assert(stat_segment(seg).num_marked == seg->layout->num_blocks - seg->free_count);
                        if (seg->free_count == 0) {
                                list_append(&s->filled, &seg->as_list);
                                r->num_filled++;
                        } else if (seg->free_count
                                   == (int)seg->layout->num_blocks) {
                                list_append(&r->freelist, &seg->as_list);
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
                                seg->block_base = NULL;
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
                                s->num_free++;
                        } else {
                                list_append(&s->partial, &seg->as_list);
                                s->num_free_blocks += seg->free_count;
                        }
                }
                int n = snprintf(buf+bufp, sizeof buf-bufp,
                                 " %d+%d", quick_scribbled, careful_scribbled);
                bufp += n;
        }

        fprintf(stderr, "Sweep (empty,partial):%.*s\n", bufp, buf);

        TPRINTF(0, "BEGIN malloc segments");
        list_init(&r->malloc_segments);
        int n = 0, nlive = 0;
        for (mseg = c->malloc_segments; mseg; mseg = next) {
                next = (struct malloc_segment *)mseg->as_list.next;
                ++n;
                if (mseg->markbit) {
                        ++nlive;
                        list_append(&r->malloc_segments, &mseg->as_list);
                        /* r->num_filled++; */
                } else {
                        destroy_malloc_segment(mseg);
                }
        }
        TPRINTF(1, "%d msegs, %d live", n, nlive);
}

/* for debug */
static void ATTR_UNUSED
print_reclaim_summary(struct reclaim *r)
{
        unsigned int i, num_free = 0, num_malloc = 0, num_malloc_bytes = 0;
        unsigned int num_segments_total = 0;
        struct stat_subheap st;
        struct segment *seg;
        struct malloc_segment *mseg;
        struct subheap s;

        sml_notice("heap reclaim summary:");
        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                *r->subheap[i].partial.last = NULL;
                *r->subheap[i].filled.last = NULL;
                store_relaxed(&s.partial.top, r->subheap[i].partial.head);
                store_relaxed(&s.filled.top, r->subheap[i].filled.head);
                st = stat_subheap(&s);
                sml_notice("subheap %2u: %4u > %4u > %4u segs, "
                           "%7u / %7u blocks alloced",
                           i, st.num_filled, st.num_partial,
                           r->subheap[i].num_free,
                           st.total.num_marked
                           + st.total.num_unmarked_before_free,
                           st.total.num_marked + st.total.num_unmarked);
                assert(st.total.num_unmarked == r->subheap[i].num_free_blocks);
                num_segments_total += st.num_filled + st.num_partial;
        }

        *r->malloc_segments.last = NULL;
        for (mseg = (struct malloc_segment *)r->malloc_segments.head; mseg;
             mseg = (struct malloc_segment *)mseg->as_list.next) {
                num_malloc++;
                num_malloc_bytes += MALLOC_SEGMENT_SIZE(mseg);
        }
        sml_notice("subheap malloc: %5u segments, %8u bytes in total",
                   num_malloc, num_malloc_bytes);

        *r->freelist.last = NULL;
        for (seg = (struct segment *)r->freelist.head; seg;
             seg = (struct segment *)seg->as_list.next)
                num_free++;
        num_segments_total += num_free;
        sml_notice("%u segments in freelist", num_free);
        sml_notice("%u segments in total", num_segments_total);
}

static void
rebalance_subheaps(struct reclaim *r, unsigned int num_request)
{
        unsigned int num_committed = segment_pool.heap.num_committed;
        unsigned int num_free = num_committed;
        unsigned int num_required = num_request;
        unsigned int i;
        unsigned int old_num_blocks = 0, old_num_free = 0;
        ATTR_UNUSED unsigned int new_num_blocks = 0, new_num_free = 0;

        /* Make sure that the free segments are many enough to allocate
         * free blocks as many as filled blocks in each subheap.
         * If not enough, allocate new free segments by extending the heap. */

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                struct subheap *s = &global_subheaps[i];
                unsigned num_blocks = segment_layout[i].num_blocks;
                unsigned num_segments = load_relaxed(&s->num_segments);
                unsigned num_free_blocks = load_relaxed(&s->num_free_blocks);
                old_num_blocks += num_segments * num_blocks;
                old_num_free += num_free_blocks;

                num_segments -= r->subheap[i].num_free;
#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
                num_free_blocks = r->subheap[i].num_free_blocks;
#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
                num_free_blocks += r->subheap[i].num_free_blocks;
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
                num_free -= num_segments;

                new_num_blocks += num_segments * num_blocks;
                new_num_free += num_free_blocks;

                unsigned num_filled_blocks =
                        num_segments * num_blocks - num_free_blocks;
                unsigned num_need =
                        (num_filled_blocks > num_free_blocks)
                        ? (num_filled_blocks - num_free_blocks + num_blocks - 1)
                        / num_blocks
                        : 0;
                num_required =
                        (num_required > num_need) ? num_required : num_need;

                /* if this subheap occupies more than 3/4 of the entire
                 * heap and its free blocks are sufficient, prevent this
                 * subheap from allocating new segment. */
                store_relaxed(&s->do_not_extend,
                              num_need == 0
                              && num_segments > num_committed * 3 / 4);
        }

        /* add free segments if less than half of the heap was filled
         * before reclaimation. */
        if (old_num_free > old_num_blocks / 2
            && num_required < num_committed / 8)
                num_required = num_committed / 8;

        if (num_required > num_free) {
                allocate_segments(&segment_pool.heap,
                                  num_required - num_free,
                                  &r->freelist);
        }

#ifdef GCTIME
        unsigned int old_filled = old_num_blocks - old_num_free;
        unsigned int new_filled = new_num_blocks - new_num_free;
        double d1 = (double)old_filled / old_num_blocks;
        gctime.util_ratio_sum += d1;
        if (d1 < 0.5) gctime.util_under_half++;
        double d2 = (double)(old_filled - new_filled) / old_num_blocks;
        gctime.reclaim_ratio_sum += d2;
#endif /* GCTIME */
}

static void
reclaim_segments(struct reclaim *r)
{
        unsigned int i;
        struct subheap *s;
        struct pre_subheap *p;

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                s = &global_subheaps[i];
                p = &r->subheap[i];
                stack_push_list(&s->filled, &p->filled);
                stack_push_list(&s->partial, &p->partial);
#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
                store_relaxed(&s->num_free_blocks, p->num_free_blocks);
                load_sub_store_relaxed(&s->num_segments, p->num_free);
#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
                fetch_add(relaxed, &s->num_free_blocks, p->num_free_blocks);
                fetch_sub(relaxed, &s->num_segments, p->num_free);
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
        }

        stack_push_list(&malloc_segments, &r->malloc_segments);
        stack_push_list(&segment_pool.freelist, &r->freelist);
}

/********** collector **********/

struct {
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        /* To avoid frequent memory contention between mutators and the
         * collector, the tracing stack is separated into two distinct
         * object lists: objects_from_mutators for communicating to mutators,
         * and trace_queue for local usage in the collector.
         */
        struct object_stack objects_from_mutators;
        /* Weak-vectors includes single-element "vectors" (alternatively known
         * as weak-pointer). Weak-tables are extra special because the strength
         * of the reference to the value (resp key) is conditional */
        struct object_stack weak_vectors, weak_tables;
        _Atomic(unsigned int) num_filled_total;
        unsigned int gc_threshold;
        pthread_rwlock_t weakobj_lock;
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
        void *root_objects;
        union collect_set collect_set;
        _Atomic(int) collector_bitmap_cleared;
        _Atomic(unsigned int) num_request;
} collector = {
        .root_objects = NIL,
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        .objects_from_mutators = EMPTY_OBJECT_STACK,
        .weak_tables = EMPTY_OBJECT_STACK,
        .weak_vectors = EMPTY_OBJECT_STACK
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
};

static void* GetWeakTableList() { return collector.weak_tables.top; }

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
static struct stack_slot *
visit(void *obj)
{
        struct stack_slot *slot;
        struct malloc_segment *mseg;
        struct segment *seg;
        unsigned int index;
        sml_bitptrw_t b;

        if (obj == NULL || (OBJ_HEADER(obj) & OBJ_FLAG_SKIP))
                return NULL;
        if (OBJ_TOTAL_SIZE(obj) > BLOCKSIZE_MAX) {
                mseg = OBJ_TO_MALLOC_SEGMENT(obj);
                if (mseg->markbit)
                        return NULL;
                mseg->markbit = 1;
                slot = &mseg->stack;
        } else {
                seg = segment_addr(obj);
                index = object_index(seg, obj);
                b = BITPTRW(BITMAP0_BASE(seg), index);
                if (BITPTRW_TEST(b))
                        return NULL;
                mark_bits(seg, index, b);
                seg->free_count++;
                slot = &seg->stack[object_index(seg, obj)];
        }
        return slot;
}

#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
FILE* marklog;
/* In this collector, "visit" means:
 *    - set the collector markbit if not already marked
 *    - decide whether the object was white,
 *      returning non-NULL if it was.
 * In this way we won't schedule for re-enumeration of slots
 * when visit returns NULL.
 */
static struct stack_slot *visit(void *obj)
{
        struct stack_slot *slot;
        sml_bitptrw_t b;

#ifndef LISP_FEATURE_SBCL // SBCL does not need this filter on 'obj'
        if (obj == NULL || (OBJ_HEADER(obj) & OBJ_FLAG_SKIP))
                return NULL;
#endif
        if (!in_bitmapped_subheap(obj)) {
                uword_t* voucher = (uword_t*)((uword_t*)obj)[-1];
                assert(in_bitmapped_subheap(voucher));
                assert(seg_blocksize_of(voucher) == N_WORD_BYTES);
                assert(*voucher == (uword_t)obj);
                visit(voucher);
                struct malloc_segment *mseg = OBJ_TO_MALLOC_SEGMENT(obj);
                if (mseg->markbit)
                        return NULL;
                mseg->markbit = 1;
                slot = &mseg->stack;
        } else {
                struct segment *seg = segment_addr(obj);
                unsigned int index = object_index(seg, obj);
                b = BITPTRW(COLLECT_BITMAP_BASE(seg), index);
                if (BITPTRW_TEST(b))
                        return NULL;
                BITPTRW_SET(b);
                b = BITPTRW(BITMAP0_BASE(seg), index);
                if ((char*)obj >= seg->snapshot_free && !BITPTRW_TEST(b))
                        return NULL;
                if (seg->free_count < 0) {
                        assert(!BITPTRW_TEST(b));
                        mark_bits(seg, index, b);
                        seg->free_count++;
                }
                slot = &seg->stack[index];
        }
        return slot;
}

#if 0
int mseg_obj_alivep(void* obj) {
        struct malloc_segment *mseg = OBJ_TO_MALLOC_SEGMENT(obj);
        return mseg->markbit;
}
int subheap_obj_alivep(void* obj) {
        struct segment *seg = segment_addr(obj);
        unsigned int index = object_index(seg, obj);
        sml_bitptrw_t b = BITPTRW(COLLECT_BITMAP_BASE(seg), index);
        if (BITPTRW_TEST(b)) return 1;
        // See if it was allocated after the snapshot (so is allocated black)
        b = BITPTRW(BITMAP0_BASE(seg), index);
        if ((char*)obj >= seg->snapshot_free && !BITPTRW_TEST(b)) return 1; // yup
        return 0;
}

static int obj_alivep(lispobj* obj) {
    extern int subheap_obj_alivep(void*), mseg_obj_alivep(void*);
    if (in_bitmapped_subheap(obj)) return subheap_obj_alivep(obj);
    if (ignorable_space_p((lispobj)obj)) return 1; // always alive
    return mseg_obj_alivep(obj);
}
#endif

char color_of(uword_t* obj) {
    struct stack_slot *ss = object_stack_slot(obj);
    assert(ss);
    // objects in the collector's worklist are grey
    if (ss->next) return 'g';
    if (!in_bitmapped_subheap(obj)) {
      // does it matter whethr mseg is in the threatened set?
      struct malloc_segment * mseg = OBJ_TO_MALLOC_SEGMENT(obj);
      return mseg->markbit ? 'b' : 'w';
    }
    struct segment *seg = segment_addr(obj);
    unsigned index = object_index(seg, obj);
    sml_bitptrw_t b = BITPTRW(COLLECT_BITMAP_BASE(seg), index);
    if (BITPTRW_TEST(b)) return 'b';
    b = BITPTRW(BITMAP0_BASE(seg), index);
    // if past the snapshot free ptr, the allocator bit doesn't matter.
    // If the bit is off, it could be just-allocated
    // and if the bit is on, it's not in the threatened set.
    if ((char*)obj >= seg->snapshot_free) return 'b';
    return 'w';
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
static void
push(void **objslot, void *data)
{
        void *obj = *objslot;
        void **top = data;
        struct stack_slot *slot;

        slot = visit(obj);
        if (slot) {
                store_relaxed(&slot->next, *top);
                *top = obj;
        }
}

#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
static void ATTR_UNUSED
push(void **objslot, void *data)
{
        void *obj = *objslot;
        void **top = data;
        struct stack_slot *slot;
        void *old = NULL;

        slot = visit(obj);
        if (slot) {
                if (load_relaxed(&slot->next) == old
                    && cmpswap_relaxed(&slot->next, &old, *top))
                        *top = obj;
        }
}

static void push_lispobj(lispobj obj, void *data) {
        struct stack_slot *slot = visit((void*)obj);
        if (slot) {
                void *old = NULL;
                void **top = data;
                if (load_relaxed(&slot->next) == old /* TODO: this load is not needed */
                    && cmpswap_relaxed(&slot->next, &old, *top))
                        *top = (void*)obj;
        }
}
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

static void *
pop(void **top)
{
        struct stack_slot *slot;
        void *obj = *top;
        assert(obj != NIL);
        slot = object_stack_slot(obj);
        assert(load_relaxed(&slot->next) != NULL);
        *top = load_relaxed(&slot->next);
        store_relaxed(&slot->next, NULL);
        return obj;
}
static int ATTR_UNUSED stack_count(void *obj)
{
        if (obj == NIL) return 0;
        int n = 0;
        do {
          struct stack_slot *slot = object_stack_slot(obj);
          obj = slot->next;
          ++n;
        } while (obj != NIL);
        return n;
}

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
static void
visit_all(void *obj)
{
        struct stack_slot *slot;

        while (obj != NIL) {
                slot = visit(obj);
                if (!slot)
                        slot = object_stack_slot(obj);
                obj = load_relaxed(&slot->next);
        }
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
static void
trace_all()
{
        void *top = collector.root_objects;
        while (top != NIL)
                sml_obj_enum_ptr(pop(&top), push, &top);
        collector.root_objects = NIL;
}

#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
/*static*/ void
trace_all()
{
        void *top = collector.root_objects;

        do {
                atomic_store(&weakRefState, weakRefState_TRACING);
                while (top != NIL)
                        lispobj_enum_ptr(pop(&top), push_lispobj, &top);
                /* objects_from_mutators contains objects that is visited
                 * but not enumerated yet due to failure of CAS in push. */
                top = flush_gray_stack(&collector.objects_from_mutators);
                visit_all(top);
        } while (atomic_load(&weakRefState) != weakRefState_SPLAT);
        assert(top ==  NIL);
        collector.root_objects = NIL;
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
void
sml_heap_collector_sync1()
{
        gather_segments(&collector.collect_set);
}

#endif /* !WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY */

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
void
sml_heap_collector_sync2()
{
        collect_segments(&collector.collect_set);
        clear_collect_set(&collector.collect_set);
        assert(collector.root_objects == NIL);
        sml_global_enum_ptr(push, &collector.root_objects);
        sml_callback_enum_ptr(push, &collector.root_objects);
}

#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

void sml_heap_collector_sync2()
{
        struct object_list objs;
        object_list_init(&objs);
#ifdef LISP_FEATURE_SBCL
        struct timespec t_before, t_after;
        clock_gettime(CLOCK_MONOTONIC, &t_before);
        lisp_global_enum_ptr(enum_obj_to_list, &objs);
        clock_gettime(CLOCK_MONOTONIC, &t_after);
        long delta_usec = (1000000*(t_after.tv_sec - t_before.tv_sec)) +
                          (t_after.tv_nsec - t_before.tv_nsec) / 1000;
#else
        sml_global_enum_ptr(enum_obj_to_list, &objs);
        sml_callback_enum_ptr(enum_obj_to_list, &objs);
#endif
        fprintf(stderr, "global_enum: %d objects in %ld \u00B5sec\n",
                objs.count, delta_usec);
        //TPRINTF(2, "global_enum: %d objects in %ld \u00B5sec", n, delta_usec);

#if 0
        {
        // Print the SML# heap objects that we saw
        FILE* f = get_gc_thread_log();
        int n = 0;
        uword_t* obj = objs.begin.next;
        if (f) fprintf(f, "Globally enumerated objects:\n");
        while (obj != NIL) {
          uword_t** link = (void*)object_stack_slot(obj);
          //fprintf(f, "%p [%16lx]\n" /* link=%p\n"*/, obj, *obj, link);
          if (f) fprintf(f, "%p [%16lx]\n", obj, *obj);
          obj = *link;
          ++n;
        }
        if (f) fprintf(f, "Global root objects: %d\n", n);
        }
#endif
        // Is this right??? Should we not push into collector.root_objects?
        // Or does it simply not matter?
        push_objects(&collector.objects_from_mutators, &objs);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
void
sml_heap_collector_mark()
{
        trace_all();
}

#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

static void ATTR_UNUSED print_bitmap(struct segment* seg, sml_bitptr_t b)
{
  char buf[210];

  int nchars = 0;
  unsigned int i;
  for (i = 0; i < seg->layout->num_blocks; i++) {
    if (nchars == 200) { buf[200] = 0; TPRINTF(0, "%s", buf); nchars = 0; }
    buf[nchars++] = BITPTR_TEST(b) ? '*' : '_';
    BITPTR_INC(b);
  }
  buf[nchars] = 0;
  TPRINTF(0, "%s", buf);
}

int count_weakobj_chain(void* list) {
    int n = 0;
    while ( (void*)list != NIL ) { ++n; list = get_weak_pointer_next(list); }
    return n;
}
void show_weakptr_chain(void* list) {
    while ( list != NIL ) { fprintf(stderr, " %lx", (uword_t)list); list = get_weak_pointer_next(list); }
    putc('\n', stderr);
}

_Atomic(int) remset_omit_lg, remset_omit_sm, weakrefgray_omit;
void clear_weak_references(void);

void
sml_heap_collector_mark()
{
        struct timespec t_before, t_after;
        clock_gettime(CLOCK_MONOTONIC, &t_before);
        TPRINTF(1, "marking started");
        unsigned int n;
        /* ToDo: replace clear bitmap with copy from collector bitmap */
        n = clear_collect_set(&collector.collect_set);
        fetch_sub(relaxed, &collector.num_filled_total, n);
        clear_collect_bitmaps(&segment_pool.heap);
        // After this, mutators can attempt to avoid reinserting objects
        // into the remembered set by looking at the collector's mark bit.
        spin_lock(&collector.objects_from_mutators.lock);
        atomic_store(&collector.collector_bitmap_cleared, 1);
        spin_unlock(&collector.objects_from_mutators.lock);
        trace_all();
        clear_weak_references();
        clock_gettime(CLOCK_MONOTONIC, &t_after);
        long delta_usec = (t_after.tv_sec - t_before.tv_sec) * 1000000
                          + (t_after.tv_nsec - t_before.tv_nsec) / 1000;
        TPRINTF(1, "marking done: %ld \u00B5sec", delta_usec);
        // mutators can push objects _after_ loop termination
        void* foo = collector.objects_from_mutators.top;
        if (foo != NIL) {
          TPRINTF(1, "mutator pushed %d items more after termination", stack_count(foo));
        }
}
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

int
sml_heap_check_alive(void *obj)
{
        struct segment *seg;
        unsigned int index;
        sml_bitptr_t b;

#ifndef LISP_FEATURE_SBCL
        if (OBJ_HEADER(obj) & OBJ_FLAG_SKIP)
                return 1;
#endif
        if (!in_bitmapped_subheap(obj)) {
            struct malloc_segment *mseg = OBJ_TO_MALLOC_SEGMENT(obj);
            return mseg->markbit;
        }
        seg = segment_addr(obj);
        index = object_index(seg, obj);
        b = BITPTR(BITMAP0_BASE(seg), index);
        return seg->free_count >= 0 || BITPTR_TEST(b);
}

extern void log_effectless_barrier_ops_to_file();

void
sml_heap_collector_async()
{
        struct reclaim r;
        //if (get_gc_thread_log()) { suspend_mutator("post-mark usage map"); show_map("post-mark", &r); unsuspend_mutator(); }
        separate_segments(&collector.collect_set, &r);
        rebalance_subheaps(&r, load_relaxed(&collector.num_request));
        reclaim_segments(&r);
        //show_map("post-sweep", 0);
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        fetch_add(relaxed, &collector.num_filled_total, r.num_filled);
        collector.gc_threshold =
                (segment_pool.heap.num_committed + r.num_filled) / 2;
#endif /* !WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY */
        extern int otherptr_mseg_calls,
            mseg_rej_static, mseg_rej_stack, mseg_rej_filter, mseg_rej_memfault, mseg_rej_unknown;
        int tot_reject = mseg_rej_static + mseg_rej_stack + mseg_rej_filter +
                         mseg_rej_memfault + mseg_rej_unknown;
        int tot_accept = otherptr_mseg_calls - tot_reject;
        TPRINTF(1, "mseg_test: %d accept=%d reject=(static=%d stk=%d filt=%d segv=%d unk=%d)",
                otherptr_mseg_calls, tot_accept,
                mseg_rej_static, mseg_rej_stack, mseg_rej_filter, mseg_rej_memfault, mseg_rej_unknown);
        otherptr_mseg_calls = 0;
        mseg_rej_static = mseg_rej_stack = mseg_rej_filter = mseg_rej_memfault = mseg_rej_unknown = 0;
        extern void dispose_weak_table_edge_lists();
        dispose_weak_table_edge_lists();
}

static void
do_gc()
{
#ifdef GCTIME
        sml_timer_t t1, t2;
        sml_timer_now(t1);
#endif /* GCTIME */
        //        fprintf(stderr, "do_gc: won't actually GC\n"); fflush(stderr);
        //        sleep(100000);
        sml_gc();
#ifdef GCTIME
        sml_timer_now(t2);
        sml_timer_accum(t1, t2, gctime.gc_time);
        gctime.gc_count++;
#endif /* GCTIME */
}

/********** collector thread **********/

#if defined WITHOUT_MULTITHREAD
static void
inc_num_filled_total()
{
}

#elif defined WITHOUT_CONCURRENCY
struct collector_control {
        pthread_mutex_t lock;
        pthread_cond_t cond_collector;
        enum { EXIT=1, GC=2} exit;
        pthread_t collector_thread;
} collector_control = {
        .lock = PTHREAD_MUTEX_INITIALIZER,
        .cond_collector = PTHREAD_COND_INITIALIZER,
};

static void wait_for_gc()
{
        mutex_lock(&collector_control.lock);
        collector_control.exit = GC;
        cond_signal(&collector_control.cond_collector);
        while (collector_control.exit)
                cond_wait(&collector_control.cond_collector,
                          &collector_control.lock);
        mutex_unlock(&collector_control.lock);
}

static void
inc_num_filled_total()
{
}

static void *
collector_main(void *arg ATTR_UNUSED)
{
        mutex_lock(&collector_control.lock);
        for (;;) {
                while (!collector_control.exit)
                        cond_wait(&collector_control.cond_collector,
                                  &collector_control.lock);
                if (collector_control.exit == EXIT)
                        break;
                do_gc();
                collector_control.exit = 0;
                cond_signal(&collector_control.cond_collector);
        }
        mutex_unlock(&collector_control.lock);
        return NULL;
}

#else /* !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY */

struct collector_control {
        pthread_mutex_t lock;
        pthread_cond_t cond_collector;
        pthread_cond_t cond_mutators;
        unsigned int gc_count;
        unsigned int num_mutators;
        unsigned int mutators_stalled;
        unsigned int vote_continue, vote_abort;
        int exit;
        pthread_t collector_thread;
} collector_control = {
        .lock = PTHREAD_MUTEX_INITIALIZER,
        .cond_collector = PTHREAD_COND_INITIALIZER,
        .cond_mutators = PTHREAD_COND_INITIALIZER,
        .exit = -1, /* SBCL: for delaying start of collector_main */
};
int get_gc_cycle_number() { return collector_control.gc_count; }
int is_collector_thread() {
    return pthread_equal(pthread_self(), collector_control.collector_thread);

}

static void
inc_num_filled_total()
{
        fetch_add(relaxed, &collector.num_filled_total, 1);
#if 0
        if (collector.num_filled_total >= collector.gc_threshold)
          fprintf(stderr, "DPK: inc_num_filled: num_filled_total=%d threshold=%d\n",
                  collector.num_filled_total,
                  collector.gc_threshold);
#endif
        mutex_lock(&collector_control.lock);
        cond_signal(&collector_control.cond_collector);
        mutex_unlock(&collector_control.lock);
}

static void
cleanup_wait_for_gc(void *arg)
{
        unsigned int gc_count = (uintptr_t)arg;

        if (collector_control.gc_count == gc_count)
                collector_control.mutators_stalled--;
        else if (collector_control.gc_count == gc_count + 1)
                collector_control.vote_continue++;
        cond_signal(&collector_control.cond_collector);
        mutex_unlock(&collector_control.lock);
}

static unsigned int
wait_for_gc()
{
        uintptr_t gc_count;
        collector_control.mutators_stalled++;
        cond_signal(&collector_control.cond_collector);
        gc_count = collector_control.gc_count;
        pthread_cleanup_push(cleanup_wait_for_gc, (void*)gc_count);
        while (!(collector_control.gc_count != gc_count))
                cond_wait(&collector_control.cond_mutators,
                          &collector_control.lock);
        pthread_cleanup_pop(0);
        return gc_count + 1;
}

static unsigned int
wait_for_vote()
{
        unsigned int gc_count;
        mutex_lock(&collector_control.lock);
        gc_count = wait_for_gc();
        mutex_unlock(&collector_control.lock);
        return gc_count;
}

static unsigned int
vote_abort(unsigned int gc_count)
{
        mutex_lock(&collector_control.lock);
        if (collector_control.gc_count == gc_count)
                collector_control.vote_abort++;
        gc_count = wait_for_gc();
        mutex_unlock(&collector_control.lock);
        return gc_count;
}

static void
vote_continue(unsigned int gc_count)
{
        mutex_lock(&collector_control.lock);
        if (collector_control.gc_count == gc_count) {
                collector_control.vote_continue++;
                cond_signal(&collector_control.cond_collector);
        }
        mutex_unlock(&collector_control.lock);
}

static int
count_vote(unsigned int num_stalled)
{
        struct collector_control * const cc = &collector_control;
        cc->vote_continue = 0;
        cc->vote_abort = 0;
        // TODO: try to explain this test
        while (!(cc->num_mutators != num_stalled
                 || cc->vote_continue > 0
                 || cc->vote_continue + cc->vote_abort == num_stalled
                 || cc->exit))
                cond_wait(&cc->cond_collector, &cc->lock);
        sml_debug("vote count: %u/%u mutators, continue/abort=%u/%u\n",
                  num_stalled, cc->num_mutators,
                  cc->vote_continue, cc->vote_abort);
        return (cc->num_mutators != num_stalled
                || cc->vote_continue > 0
                || cc->exit);
}

int force_gc_flag;
static void *
collector_main(void *arg ATTR_UNUSED)
{
        struct collector_control * const cc = &collector_control;
        unsigned int num_stalled;

#ifdef __linux__
        pthread_setname_np(pthread_self(), "GC");
#endif
        mutex_lock(&cc->lock);
        while (cc->exit < 0) {
                cond_wait(&cc->cond_collector, &cc->lock);
        }
        fprintf(stderr, "SML# collector enabled, num_filled=%d threshold=%d\n",
                collector.num_filled_total, collector.gc_threshold);

 loop:
        /* wait until:
         * - at least one mutator requires a GC in order to proceed,
         * - the number of filled-to-capacity segments exceeds a threshold
         * - process wants to exit */
        while (!(cc->mutators_stalled > 0
                 || force_gc_flag
                 || (load_relaxed(&collector.num_filled_total)
                     >= collector.gc_threshold)
                 || cc->exit)) {
                cond_wait(&cc->cond_collector, &cc->lock);
        }
        store_relaxed(&collector.num_request, cc->mutators_stalled);
        mutex_unlock(&cc->lock);
        if (cc->exit)
                return NULL;

        force_gc_flag = 0;
        do_gc();
        mutex_lock(&cc->lock);

        cc->gc_count++;
        // If _everybody_ can make progress, we're good
        if (cc->mutators_stalled == 0) goto loop;
 retry:
        /* Some mutators are being stalled due to lack of segments.
         * If at least one mutator is working, then the program continues. */
        sml_debug("%u out of %u threads stalled\n",
                  cc->mutators_stalled, cc->num_mutators);
        num_stalled = cc->mutators_stalled;
        cc->mutators_stalled = 0;
        cond_broadcast(&cc->cond_mutators);
        // If _somebody_ can make progress, we're good
        if (count_vote(num_stalled)) goto loop;

        /* All mutators voted for aborting the program and still stalled.
         * Try full GC and expect that it recovers the situation. */
        TPRINTF(0, "FULL GC");
        sml_debug("full gc\n");
#ifdef GCTIME
        gctime.full_gc_count++;
#endif /* GCTIME */
        mutex_unlock(&cc->lock);
        move_partial_to_filled();
        do_gc();
        mutex_lock(&cc->lock);

        cc->gc_count++;
        sml_debug("%u out of %u threads still stalled\n",
                  cc->mutators_stalled, cc->num_mutators);
        if (cc->mutators_stalled != num_stalled) goto retry;
        cc->mutators_stalled = 0;
        cond_broadcast(&cc->cond_mutators);
        if (count_vote(num_stalled)) goto loop;

        print_heap_summary();
        sml_fatal(0, "heap exhausted; all threads stalled");
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

/********** mutators **********/

union sml_alloc *
sml_heap_worker_init()
{
        union sml_alloc *p = new_alloc_ptr_set();
        worker_tlv_set(alloc_ptr_set, p);

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        mutex_lock(&collector_control.lock);
        collector_control.num_mutators++;
        cond_signal(&collector_control.cond_collector);
        mutex_unlock(&collector_control.lock);
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
        return p;
}

void
sml_heap_worker_destroy(union sml_alloc *ptr_set)
{
        /* This function is called only in ASYNC phase, therefore we do
         * not need to take a snapshot of allocation pointers.
         * Just put alloc_ptr in free list. */
        stack_push(&alloc_ptr_set_pool.freelist, &ptr_set->as_list);

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        mutex_lock(&collector_control.lock);
        collector_control.num_mutators--;
        cond_signal(&collector_control.cond_collector);
        mutex_unlock(&collector_control.lock);
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
}

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
void
sml_heap_user_sync2(struct sml_user *user)
{
        sml_stack_enum_ptr(user, push_lispobj, &collector.root_objects);
}

void
sml_heap_worker_sync2(union sml_alloc *ptr_set)
{
        move_all_to_filled(ptr_set);
}

#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
void
sml_heap_user_sync2(struct sml_user *user)
{
        struct object_list objs;

        /* enumerate pointers in the stack and send them to the collector */
        object_list_init(&objs);
#ifdef LISP_FEATURE_SBCL
        if (pthread_equal(pthread_self(), collector_control.collector_thread)) {
          TPRINTF(1, "performing SYNC2 on behalf of inactive thread");
        } else {
          TPRINTF(1, "performing SYNC2");
        }
        lisp_stack_enum_ptr(user, enum_obj_to_list, &objs);
#else
        sml_stack_enum_ptr(user, enum_obj_to_list, &objs);
#endif
        push_objects(&collector.objects_from_mutators, &objs);
        TPRINTF(1, "Done with stack enumeration");
}

void
sml_heap_worker_sync2(union sml_alloc *ptr_set)
{
        /* Do not use worker_tlv_get(alloc_ptr_set) in this function and use
         * "ptr_set" instead.  This function may be called with "ptr_set" of
         * another thread, which differs from worker_tlv_get(alloc_ptr_set).
         */

        /* save current allocation pointers to segments in ptr_set. */
        save_alloc_ptr_set_sync2(ptr_set);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
int collector_markbit(void *obj) {
    if (!in_bitmapped_subheap(obj)) {
        struct malloc_segment* mseg = OBJ_TO_MALLOC_SEGMENT(obj);
        return mseg->markbit;
    }
    struct segment *seg = segment_addr(obj);
    unsigned int index = object_index(seg, obj);
    sml_bitptr_t b = BITPTR(COLLECT_BITMAP_BASE(seg), index);
    return BITPTR_TEST(b) != 0;
}
#if 0
int managed_object_alivep(void *obj) {
  /* FIXME: what about the allocate black logic ?
     i.e. the test in visit() which says 'obj' need not have its children visited
     if ((char*)obj >= seg->snapshot_free && !BITPTRW_TEST(b))
                        return NULL;
  */
  int answer1 = sml_heap_check_alive(obj);
  int answer2 = collector_markbit(obj);
  if (answer1 != answer2) lose("my_heap_check_alive: mismatch on %p: %d vs %d", obj, answer1, answer2);
  return answer1;
}
#endif

static void remember_internal(struct object_stack *stack, lispobj taggedptr)
{
        struct stack_slot *slot = careful_lispobj_stack_slot(taggedptr);
        if (!slot) return;
        void* obj = untagged_baseptr(taggedptr);

#if 1
        /* Avoid re-enumerating slots of 'obj' by testing whether the collector already
         * visited it. This check is inadmissible unless in MARK phase.
         * This is perfectly acceptable because avoiding extra insertions into the remembered
         * set is beneficial only when the collector is trying to terminate the marking loop */
        if (atomic_load(&collector.collector_bitmap_cleared) && collector_markbit(obj)) {
            fetch_add(relaxed,
                      in_bitmapped_subheap(obj) ? &remset_omit_sm : &remset_omit_lg,
                      1);
            return;
        }
#endif

        /* ensure that the collector receives all objects whose stack_slot
         * were occupied by mutators. */
        spin_lock(&stack->lock);

        /* this may fail if obj already belongs to another list or stack */
        /* N.B.: This is an atomic operation because we're competing
         * with enum_obj_to_list() which does not use the spinlock.
         * (This has to compete with other mutators for access to the stack top.
         * It might work better to cmpswap on the stack top, and reassign
         * slot->next on value. We're certain to be the unique owner of this object
         * for purposes of graying it (I think). On the other hand,
         * since we're also racing with the "flush" operation,
         * maybe the spinlock _is_ needed for that? */
        void *old = NULL;
        if (load_relaxed(&slot->next) == NULL
            && cmpswap_relaxed(&slot->next, &old, stack->top))
                stack->top = obj;

        spin_unlock(&stack->lock);
}
void rememberedset_insert(lispobj taggedptr) {
    return remember_internal(&collector.objects_from_mutators, taggedptr);
}

int n_weak_hashtables, n_weak_vectors, n_weak_ptrs;
void record_weak_object(uword_t taggedptr) {
    if (!use_smlgc) return;
    struct object_stack *stack = &collector.weak_vectors;
    switch (lowtag_of(taggedptr)) {
    case INSTANCE_POINTER_LOWTAG: {
#ifdef DISABLE_HASH_TABLE_WEAKNESS
        fprintf(stderr, "record_weak_object: ignoring HASH-TABLE %p\n", (void*)taggedptr); return;
#endif
        // XXX: Does this need to grab the weakTableLock as a writer
        // in order to ensure mutual-exclusivity with GC ? Not really
        struct hash_table *ht = (void*)(taggedptr - INSTANCE_POINTER_LOWTAG);
        stack = &collector.weak_tables;
        spin_lock(&stack->lock);
        ht->next_weak_hash_table = collector.weak_tables.top;
        collector.weak_tables.top = (void*)ht;
        ++n_weak_hashtables;
        break;
    }
    case OTHER_POINTER_LOWTAG: {
        struct weak_pointer* wp = (void*)(taggedptr - OTHER_POINTER_LOWTAG);
        if (weakptr_vectorp(wp)) {
            ++n_weak_vectors;
        } else {
            lispobj referent = wp->value;
            assert(is_lisp_pointer(referent));
            // do not record if it points to non-GCable space
            if (!careful_lispobj_stack_slot(referent)) return;
            ++n_weak_ptrs;
        }
        spin_lock(&stack->lock);
        set_weak_pointer_next(wp, collector.weak_vectors.top);
        collector.weak_vectors.top = wp;
        break;
    }
    default:
        lose("bad ptr in record_weak_object: %lx", taggedptr);
    }
    spin_unlock(&stack->lock);
}
void show_weak_objects()
{
  printf("Hash-Tables:\n");
  struct hash_table* ht = collector.weak_tables.top;
  for ( ; ht != NIL ; ht = ht->next_weak_hash_table )
    printf(" %p\n", ht);
  printf("Pointers:\n");
  struct weak_pointer* wp = collector.weak_vectors.top;
  for ( ; wp != NIL ; wp = get_weak_pointer_next(wp) )
    printf(" %p\n", wp);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
SML_PRIMITIVE void
sml_write(void *obj ATTR_UNUSED, void **writeaddr, void *new_value)
{
        *writeaddr = new_value;
}

#elif 0 /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
static void
barrier(void **old_value, void *new_value)
{
        enum sml_sync_phase phase = sml_current_phase();

        /* Note that "phase" may differ from the current phase since the
         * current phase may be changed after reading it.
         * If "phase" is:   Then the current phase may be:
         *   ASYNC            ASYNC or PRESYNC1
         *   PRESYNC1         PRESYNC1
         *   SYNC1            SYNC1 or PRESYNC2
         *   PRESYNC2         PRESYNC2
         *   SYNC2            SYNC2, MARK, ASYNC, or PRESYNC1
         *   MARK             MARK, ASYNC, or PRESYNC1
         */
        if (phase <= PRESYNC1) {
                /* It is ensured that the current phase is ASYNC or PRESYNC1.
                 * No write barrier is needed. */
        } else {
                /* The current phase is expected to be either SYNC1, PRESYNC2,
                 * SYNC2, or MARK, but may be artbitrary.  This means that
                 * write barrier may be performed even in ASYNC and PRESYNC1.
                 * This should hardly happen but even this is safe. */
#ifdef WITHOUT_MASSIVETHREADS
                /* In either SYNC1, PRESYNC2, or SYNC2 phase, snooping
                 * write barrier is required. */
                if (phase <= SYNC2)
                        remember(new_value);  /* snooping barrier */
#else /* !WITHOUT_MASSIVETHREADS */
                /* Snooping barrier is needed in SYNC1 and PRESYNC2 phase.
                 * In contrast to non-massivethread version, it is not
                 * needed in SYNC2 since it is ensured that root set
                 * enumeration is completed before SYNC2. */
                if (phase <= PRESYNC2)
                        remember(new_value);
#endif /* !WITHOUT_MASSIVETHREADS */
                remember(old_value);  /* snapshot barrier */
        }
}

SML_PRIMITIVE void
sml_write(void *obj ATTR_UNUSED, void **writeaddr, void *new_value)
{
        barrier(*writeaddr, new_value);
        *writeaddr = new_value;
}

int
sml_cmpswap(void *obj, void *old_value, void *new_value)
{
        _Atomic(void *) *ref = (_Atomic(void *)*)obj;

        if (cmpswap_acq_rel(ref, &old_value, new_value)) {
                barrier(old_value, new_value);
                return 1;
        } else {
                return 0;
        }
}
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

static sml_bitptr_t
bitptr_linear_search(const unsigned int *start, const unsigned int *limit)
{
        sml_bitptr_t b = {start, 0};
        while (b.ptr < limit) {
                b.mask = 0;
                BITPTR_NEXT0(b);
                if (!BITPTR_NEXT_FAILED(b)) break;
                b.ptr++;
        }
        return b;
}

/* find_bitmap will either find something in the segment
 * to which 'ptr' points, or else fail. */
#define find_bitmap smlgc_search_freebit
/*static*/ NOINLINE void *
find_bitmap(struct alloc_ptr *ptr)
{
  if (ptr->freebit.mask == (sml_bmword_t)-1) { lose("Should not reach smlgc with a mask of -1"); }
        unsigned int i, index, *base, *limit;
        const unsigned int *p;
        struct segment *seg;
        sml_bitptr_t b = ptr->freebit;
        void *obj;

        if (ptr->free == NULL)
                return NULL;

        seg = ALLOC_PTR_TO_SEGMENT(ptr);
        base = BITMAP0_BASE(seg);

        BITPTR_NEXT0(b);
        if (BITPTR_NEXT_FAILED(b)) {
                for (i = 1;; i++) {
                        index = BITPTR_WORDINDEX(b, base) + 1;
                        base = BITMAP_BASE(seg, i);
                        b = BITPTR(base, index);
                        BITPTR_NEXT0(b);
                        if (!BITPTR_NEXT_FAILED(b))
                                break;
                        if (i >= SEG_RANK - 1) {
                                p = &BITPTR_WORD(b) + 1;
                                limit = BITMAP_LIMIT(seg, i);
                                b = bitptr_linear_search(p, limit);
                                if (BITPTR_NEXT_FAILED(b))
                                        return NULL;
                                break;
                        }
                }
                for (; i > 0; i--) {
                        index = BITPTR_INDEX(b, base);
                        base = BITMAP_BASE(seg, i - 1);
                        b = BITPTR(base + index, 0);
                        BITPTR_NEXT0(b);
                        assert(!BITPTR_NEXT_FAILED(b));
                }
        }

        index = BITPTR_INDEX(b, base);
        assert(index < seg->layout->num_blocks);
        obj = seg->block_base + (index << seg->blocksize_log2);

        BITPTR_INC(b);
        ptr->freebit = b;
        ptr->free = (char*)obj + ptr->blocksize_bytes;

        return obj;
}

static void *
try_find_segment(struct subheap *subheap, struct alloc_ptr *ptr,
                 unsigned int blocksize_log2)
{
        struct segment *seg;
        void *obj;

        assert(1U << (subheap - &global_subheaps[0]) == ptr->blocksize_bytes);
        assert(1U << blocksize_log2 == ptr->blocksize_bytes);

        seg = stack_pop(&subheap->partial);
        if (seg) {
                assert(seg->free_count > 0);
#if defined WITHOUT_MULTITHREAD
                load_sub_store_relaxed(&subheap->num_free_blocks,
                                       seg->free_count);
#else /* !WITHOUT_MULTITHREAD */
                fetch_sub(relaxed, &subheap->num_free_blocks, seg->free_count);
#endif /* !WITHOUT_MULTITHREAD */
                set_alloc_ptr(ptr, seg);
                obj = find_bitmap(ptr);
                assert(obj != NULL);
                return obj;
        }

        if (load_relaxed(&subheap->do_not_extend)) {
                sml_debug("do not extend\n");
                return NULL;
        }

        seg = new_segment(blocksize_log2);
        if (seg) {
#if defined WITHOUT_MULTITHREAD
                load_add_store_relaxed(&subheap->num_segments, 1);
#else /* !WITHOUT_MULTITHREAD */
                fetch_add(relaxed, &subheap->num_segments, 1);
#endif /* !WITHOUT_MULTITHREAD */
                set_alloc_ptr(ptr, seg);
                assert(!BITPTR_TEST(ptr->freebit));
                BITPTR_INC(ptr->freebit);
                obj = ptr->free;
                ptr->free += ptr->blocksize_bytes;
                assert(obj != NULL);
                return obj;
        }

        return NULL;
}

#if defined WITHOUT_MULTITHREAD
static void *
request_segment(struct subheap *subheap, struct alloc_ptr *ptr,
                unsigned int blocksize_log2, void *frame_pointer)
{
        void *old_top, *obj;

        assert(1U << (subheap - &global_subheaps[0]) == ptr->blocksize_bytes);
        assert(1U << blocksize_log2 == ptr->blocksize_bytes);

        sml_debug("no block found in subheap %u\n", blocksize_log2);
        load_add_store_relaxed(&collector.num_request, 1);

        old_top = sml_leave_internal(frame_pointer);
        do_gc();
        sml_enter_internal(old_top);

        obj = try_find_segment(subheap, ptr, blocksize_log2);
        if (!obj) {
                print_heap_summary();
                sml_fatal(0, "heap exhausted; all threads stalled");
        }

        return obj;
}

#elif defined WITHOUT_CONCURRENCY
static void *
request_segment(struct subheap *subheap, struct alloc_ptr *ptr,
                unsigned int blocksize_log2, void *frame_pointer)
{
        int sml_stop_the_world(void);
        void sml_run_the_world(void);
        void *old_top, *obj;

        assert(1U << (subheap - &global_subheaps[0]) == ptr->blocksize_bytes);
        assert(1U << blocksize_log2 == ptr->blocksize_bytes);

        sml_debug("no block found in subheap %u\n", blocksize_log2);
        fetch_add(relaxed, &collector.num_request, 1);

        old_top = sml_leave_internal(frame_pointer);

        do {
                if (sml_stop_the_world()) {
                        wait_for_gc();
                        obj = try_find_segment(subheap, ptr, blocksize_log2);
                        if (!obj) {
                                print_heap_summary();
                                sml_fatal(0, "heap exhausted; "
                                          "all threads stalled");
                        }
                        sml_run_the_world();
                        break;
                }
                obj = try_find_segment(subheap, ptr, blocksize_log2);
        } while (!obj);

        sml_enter_internal(old_top);

        return obj;
}

#else /* !WITHOUT_MULTITHREAD || !WITHOUT_CONCURRENCY */
static void *
request_segment(struct subheap *subheap, struct alloc_ptr *ptr,
                unsigned int blocksize_log2, void *frame_pointer)
{
        void *obj, *old_top;
        unsigned int gc_count;

        assert(1U << (subheap - &global_subheaps[0]) == ptr->blocksize_bytes);
        assert(1U << blocksize_log2 == ptr->blocksize_bytes);

        /* request a garbage collection and wait for its completion */
        old_top = sml_leave_internal(frame_pointer);
        sml_debug("no block found in subheap %u\n", blocksize_log2);
        fetch_add(relaxed, &collector.num_request, 1);
        gc_count = wait_for_vote();
        for (;;) {
                obj = try_find_segment(subheap, ptr, blocksize_log2);
                if (obj) {
                        vote_continue(gc_count);
                        break;
                }
                /* release all segments owned by this thread for full GC */
                move_all_to_filled(worker_tlv_get(alloc_ptr_set));
                gc_count = vote_abort(gc_count);
        }
        sml_enter_internal(old_top);

        assert(obj != NULL);
        return obj;
}

#endif /* !WITHOUT_MULTITHREAD || !WITHOUT_CONCURRENCY */

static NOINLINE void *
find_segment(struct alloc_ptr *ptr, void *frame_pointer)
{
        unsigned int blocksize_log2;
        struct subheap *subheap;
        void *obj;

#ifndef WITHOUT_MULTITHREAD
        if (load_relaxed(&sml_check_flag))
                sml_check_internal(frame_pointer);
#endif /* !WITHOUT_MULTITHREAD */

        /* calculate blocksize_log2 from ptr instead of taking it as an
         * argument.  This is an optimization to minimize the number of
         * instructions on the most frequently executed path in sml_alloc.
         */
        union sml_alloc* ptrset = worker_tlv_get(alloc_ptr_set);
        if (ptr >= &ptrset->ptr[BLOCKSIZE_MIN_LOG2] &&
            ptr <= &ptrset->ptr[BLOCKSIZE_MAX_LOG2])
            blocksize_log2 = ptr - &ptrset->ptr[0];
        else {
            blocksize_log2 = CEIL_LOG2(ptr->blocksize_bytes);
            assert(BLOCKSIZE_MIN_LOG2 <= blocksize_log2
                   && blocksize_log2 <= BLOCKSIZE_MAX_LOG2);
        }
        subheap = &global_subheaps[blocksize_log2];

        if (ptr->free) {
                move_to_filled(ptr, subheap);
                inc_num_filled_total();
        }

        obj = try_find_segment(subheap, ptr, blocksize_log2);
        if (obj)
                return obj;

#ifdef GCTIME
        sml_timer_t t1;
        sml_timer_now(t1);
#endif /* GCTIME */

        clear_alloc_ptr(ptr);
        obj = request_segment(subheap, ptr, blocksize_log2, frame_pointer);

#ifdef GCTIME
        sml_timer_t t2;
        sml_time_t t;
        sml_timer_now(t2);
        sml_timer_dif(t1, t2, t);
        mutex_lock(&gctime.pause_time_lock);
        sml_time_accum(t, gctime.pause_time);
        double d = TIMEFLOAT(t);
        if (d > gctime.max_pause_time) gctime.max_pause_time = d;
        gctime.pause_count++;
        mutex_unlock(&gctime.pause_time_lock);
#endif /* GCTIME */

        assert(obj != NULL);
        return obj;
}

static NOINLINE void *
malloc_object(size_t alloc_size)
{
        struct malloc_segment *mseg = alloc_mseg(0, alloc_size);
        /* inc_num_filled_total(); */
        return MALLOC_SEGMENT_TO_OBJ(mseg);
}

SML_PRIMITIVE void *
sml_alloc(unsigned int objsize)
{
        size_t alloc_size;
        unsigned int blocksize_log2;
        struct alloc_ptr *ptr;
        void *obj;

        if (objsize > BLOCKSIZE_MAX - OBJ_HEADER_SIZE)
                return malloc_object(objsize);

        /* ensure that alloc_size is at least BLOCKSIZE_MIN. */
        alloc_size = CEILING(OBJ_HEADER_SIZE + objsize, BLOCKSIZE_MIN);
        blocksize_log2 = CEIL_LOG2(alloc_size);
        assert(BLOCKSIZE_MIN_LOG2 <= blocksize_log2
               && blocksize_log2 <= BLOCKSIZE_MAX_LOG2);
        ptr = &(worker_tlv_get(alloc_ptr_set)->ptr[blocksize_log2]);

        if (!BITPTR_TEST(ptr->freebit)) {
                BITPTR_INC(ptr->freebit);
                obj = ptr->free;
                ptr->free += ptr->blocksize_bytes;
                assert(obj != NULL);
                goto alloced;
        }

        obj = find_bitmap(ptr);
        if (obj) goto alloced;

        obj = find_segment(ptr, CALLER_FRAME_END_ADDRESS());

alloced:
        // assert(check_pattern(obj, objsize >> WORD_SHIFT));
        return obj;
}

void* getallocptr_for_log2size(unsigned int log2_size) {
    return &(worker_tlv_get(alloc_ptr_set)->ptr[log2_size]);
}

int showfallback = 1;
void* sml_alloc_fallback2(struct alloc_ptr *ptr, void *roots_start) {
  if(showfallback & 2) fprintf(stderr, "alloc_fallback2(%p,%p)", ptr, roots_start);
    void* result = find_segment(ptr, roots_start);
    if(showfallback & 2) fprintf(stderr, " -> %p\n", result);
    return result;
}
// This is for allocating a cons cell with the assumption that it will be stored
// in the CDR of the preceding (not contiguously) cons cell.
// It might be necessary to invoke the snooping barrier.
// I'm not sure if this is really needed.
void* sml_cons_fallback2(struct alloc_ptr *ptr, void *roots_start) {
    enum sml_sync_phase oldphase = sml_current_phase();
    if(showfallback & 4) fprintf(stderr, "cons_fallback2(%p,%p)", ptr, roots_start);
    void* result = find_segment(ptr, roots_start);
    if(showfallback & 4) fprintf(stderr, " -> %p\n", result);
    enum sml_sync_phase newphase = sml_current_phase();
    if (oldphase != newphase)
      TPRINTF(1, "fallback2 cons allocator sees phase change from %s to %s",
              phase_name(oldphase), phase_name(newphase));
    // FIXME: this might not be needed, in which case it can be folded in with alloc_fallback2
    if (newphase >= SYNC1 && newphase <= SYNC2)
        rememberedset_insert((uword_t)result);
    return result;
}

/********** initialize/finalize garbage collection ***********/

void
sml_heap_init(size_t min_size, size_t max_size)
{
        init_segment_layout();
        init_segment_pool(min_size, max_size);
        pthread_rwlock_init(&collector.weakobj_lock, 0);

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        collector.gc_threshold = segment_pool.heap.min_num_segments / 2;
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#ifndef WITHOUT_MULTITHREAD
        if (pthread_create(&collector_control.collector_thread, NULL,
                           collector_main, NULL) != 0)
                sml_sysfatal("pthread_create");
#endif /* WITHOUT_MULTITHREAD */

#ifdef GCTIME
        sml_timer_now(gctime.exec_start);
#endif /* GCTIME */
}

void enable_collector_thread() {
        collector_control.exit = 0;
        cond_signal(&collector_control.cond_collector);
}

#if !defined WITHOUT_MULTITHREAD
void
sml_heap_stop()
{
        mutex_lock(&collector_control.lock);
        collector_control.exit = 1;
        cond_signal(&collector_control.cond_collector);
        mutex_unlock(&collector_control.lock);
        pthread_join(collector_control.collector_thread, NULL);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

void
sml_heap_destroy()
{
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        assert(collector_control.exit == 1);
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#ifdef GCTIME
        sml_timer_now(gctime.exec_end);
        sml_timer_dif(gctime.exec_start, gctime.exec_end, gctime.exec_time);
        sml_notice("exec time        : "TIMEFMT" #sec",
                   TIMEARG(gctime.exec_time));
        sml_notice("gc time total    : "TIMEFMT" #sec",
                   TIMEARG(gctime.gc_time));
        sml_notice("gc count         : %u", gctime.gc_count);
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        sml_notice("full gc count    : %u", gctime.full_gc_count);
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
        sml_notice("pause time total : "TIMEFMT" #sec",
                   TIMEARG(gctime.pause_time));
        sml_notice("pause count      : %u", gctime.pause_count);
        sml_notice("max pause time   : %.6f #sec", gctime.max_pause_time);
        sml_notice("util ratio avg   : %5.2f #%%",
                   gctime.util_ratio_sum / gctime.gc_count * 100);
        sml_notice("reclaim ratio avg: %5.2f #%%",
                   gctime.reclaim_ratio_sum / gctime.gc_count * 100);
        sml_notice("num segments     : %u", segment_pool.heap.num_committed);
        sml_notice("under-half count : %u", gctime.util_under_half);
        sml_notice("timer type       : %s", TIMERTYPE);
#if defined WITHOUT_MULTITHREAD
        sml_notice("gc type          : sequential");
#elif defined WITHOUT_CONCURRENCY
        sml_notice("gc type          : stop-the-world");
#else /* !WITHOUT_MULTITHREAD &&& !WITHOUT_CONCURRENCY */
        sml_notice("gc type          : concurrent");
#endif /* !WITHOUT_MULTITHREAD &&& !WITHOUT_CONCURRENCY */
#endif /* GCTIME */

        destroy_alloc_ptr_set_pool();
        destroy_segment_pool();
        destroy_malloc_segments();
}

uword_t* reject_ptr(void* a, char *kind, char* reason)
{
    // Show something about each rejected pointer that potentially sees into managed space
    if (in_bitmapped_subheap(a)) {
      struct segment* seg = segment_addr(a);
      void* block_base = seg->block_base;
      void* block_limit = BLOCK_LIMIT(seg);
      uword_t* baseptr = (uword_t*)((uintptr_t)a & ~LOWTAG_MASK);
      TPRINTF(0, "REJECT %s root: %p blocks=%p:%p size %d (%s) header=%lx",
              kind, a,
              block_base, block_limit,
              1<<seg->layout->blocksize_bytes,
              a < block_base ? "<base" :
              a >= block_limit ? ">=limit":
              reason, baseptr);
    } else {
      TPRINTF(0, "REJECT %s root: %p LARGE? %s", kind, a, reason);
    }

  return 0;
}

/* See "Reference Object Processing in On-The-Fly Garbage Collection"
 * by Tomoharu Ugawa, Richard E. Jones, Carl G. Ritson
 * This is almost but not quite the algorithm of fig. 6.
 * It differs because strengthenWeakRef acquires the spinlock on the gray list
 * and tests whether refs are considered to be cleared before invoking the logic
 * of remember(). */

/* published algo-
get()
{
  while (true) {
    switch(refState) {
      case NORMAL:
        return referent;
      case REPEAT:
        if (referent=null||COLOR(referent))=WHITE)
        return referent;
        COLOR(referent) ← GREY;
        return referent;
    case TRACING:
      if (referent=null||COLOR(referent))=WHITE)
      return referent;
      if (CAS(refState, TRACING, REPEAT)) {
        COLOR(referent) ← GREY;
        return referent;
      }
      break; // retry
    case CLEANING:
      if (referent=null||COLOR(referent))=WHITE)
      return referent;
      return null;
    }
  }
}
*/

#include "genesis/instance.h"
#include "genesis/hash-table.h"
static int is_weak_hash_table(lispobj x) {
    if (!instancep(x)) return 0;
    struct layout* layout = (void*)native_pointer(instance_layout(INSTANCE(x)));
    if (layout_depth2_id(layout) != HASH_TABLE_LAYOUT_ID) return 0;
    return hashtable_weakp((struct hash_table*)INSTANCE(x));
}

static void weakRefGray(lispobj referent)
{
    /* This shades white to gray, but might accidentally turn black to gray
     * which is generally harmless other than causing duplicated work.
     * We can't avoid it here because the collector's bitmap may not have been
     * initialized yet (is that true???) which makes distinguishing gray
     * from black impossible. All we can distinguish is "in a list" or not.
     * Since "not in the list" could be white or black, we have to assume
     * that it means "white".
     * Another difference vs. rememberedset_insert() is that we hold the spinlock
     * on the gray list already, which prevents the collector from terminating
     * its marking loop */
    struct stack_slot *slot = careful_lispobj_stack_slot(referent);
    if (!slot) return; // not a GC-managed object

    void* obj = untagged_baseptr(referent);
    if (atomic_load(&collector.collector_bitmap_cleared) && collector_markbit(obj)) {
        // fprintf(stderr, "Yay!\n");
        ++weakrefgray_omit;
    } else {
        /* If the referent is a weak hash-table we have to avoid a race
         * with the collector visiting the table since the collector
         * has to make a decision about liveness of the table.
         * FIXME: can there be a lock ordering problem? Mutator holds
         * the spinlock on objects_from_mutators. Does GC want that first or
         * second when it needs the weak table lock?  */
        int tablep = is_weak_hash_table(referent);
        if (tablep) lose("Weak object livens weak object: not working yet");
        if (tablep) pthread_rwlock_rdlock(&weakTableLock);
        struct object_stack *stack = &collector.objects_from_mutators;
        void *old = NULL;
        if (load_relaxed(&slot->next) == NULL && cmpswap_relaxed(&slot->next, &old, stack->top))
            stack->top = untagged_baseptr(referent);
        if (tablep) pthread_rwlock_unlock(&weakTableLock);
    }
}

_Atomic(int) weakrefget_ct[4];
/* Unlike the published algorithm for this, we don't need a loop.
 * Cmpswap can't fail, because either the collector or at most one mutator
 * will perform the cmpswap. So we have a little bit more synchronization
 * but the advantage is that it can't fail, and it is easier to reason about */
lispobj weakRefGetImpl(lispobj* where, lispobj splatted_value)
{
    lispobj referent = *where;

    // Could already be splatted in this GC cycle or a prior cycle
    if (referent == splatted_value || !is_lisp_pointer(referent)) return referent;
    switch (atomic_load(&weakRefState)) {
    case weakRefState_NORMAL: fetch_add(relaxed, weakrefget_ct+0, 1); return referent;
    case weakRefState_REPEAT:
        fetch_add(relaxed, weakrefget_ct+1, 1);
        weakRefGray(referent);
        return referent;
    case weakRefState_TRACING:
        fetch_add(relaxed, weakrefget_ct+2, 1);
        // All unmanaged objects are live
        if (!is_gcable_object(referent)) return referent;
        if (sml_heap_check_alive(untagged_baseptr(referent))) return referent;
        int old = weakRefState_TRACING;
        if (!cmpswap_acq_rel(&weakRefState, &old, weakRefState_REPEAT))
            lose("unexpected failure in weakRefGetImpl");
        weakRefGray(referent);
        return referent;
    default: // actually weakRefState_SPLAT:
        fetch_add(relaxed, weakrefget_ct+3, 1);
        // All unmanaged objects are live
        if (!is_gcable_object(referent)) return referent;
        if (sml_heap_check_alive(untagged_baseptr(referent))) return referent;
        *where = splatted_value;
        return splatted_value;
    }
}

lispobj weak_ref_get(lispobj* slot, lispobj splatted_value) {
    // Acquring the gray list spinlock prevents the collector from deciding to terminate
    spin_lock(&collector.objects_from_mutators.lock);
    lispobj result = weakRefGetImpl(slot, splatted_value);
    spin_unlock(&collector.objects_from_mutators.lock);
    //fprintf(stderr, "wrg returns %lx\n", result);
    return result;
}

#if 0
/* Compare a key in a weak hash-table. This is more efficient than using weakRefGet
 * because on a mismatch no object will be enlivened. */
int weakRefEq(lispobj* slot, lispobj key) {
    pthread_rwlock_rdlock(&collector.weakobj_lock);
    // for now I'm just doing the simplest thing since the hash-table code
    // hasn't been modified to use this.
    lispobj result = weakRefGetLocked(slot);
    pthread_rwlock_unlock(&collector.weakobj_lock);
    return result == key;
}
#endif

// This returns 1 if the weak object (NOT what it refers to)
// is considered smashed, i.e. gone - and GC can henceforth ignore it.
extern int weakobj_smashed_p(void*);

static void clear_weak_vector_refs()
{
    int verbose = 1;
    struct weak_pointer dummy;
    memset(&dummy, 0, sizeof dummy);
    struct object_stack *stack = &collector.weak_vectors;
    struct weak_pointer *prev = &dummy,
                        *this = flush_object_stack(stack),
                        *next = NIL;
    set_weak_pointer_next(prev, this);
    int count = 0;
    for ( ; (void*)this != NIL ; this = next ) {
        ++count;
        assert(header_widetag(this->header) == WEAK_POINTER_WIDETAG);
        next = get_weak_pointer_next(this);
        // weak pointers of the vector kind might not be in a bitmapped subheap
        int this_alive = !is_gcable_object(make_lispobj(this, OTHER_POINTER_LOWTAG))
            || sml_heap_check_alive(this);
        if (!this_alive || weakobj_smashed_p(this)) { // snip it out
            set_weak_pointer_next(this, 0);
            set_weak_pointer_next(prev, next);
        } else {
            prev = this;
        }
    }
    extern int n_weak_refs_broken;
    if (verbose) printf("weak refs: cleared %d\n", n_weak_refs_broken);
    n_weak_refs_broken = 0;
    if (prev == &dummy) return;
    // There are some weak objects that need to be stuffed back in to the global list
    spin_lock(&stack->lock);
    uword_t* top = stack->top;
    assert((void*)top == NIL || *(unsigned char*)top == WEAK_POINTER_WIDETAG);
    set_weak_pointer_next(prev, top);
    // the 'next' of dummy is the real head of the list
    collector.weak_vectors.top = get_weak_pointer_next(&dummy);
    // printf("weakobjs: %d remain\n", count_weakobj_chain(stack->top));
    spin_unlock(&stack->lock);
}
static void clear_weak_table_refs()
{
    struct hash_table dummy;
    memset(&dummy, 0, sizeof dummy);
    struct object_stack *stack = &collector.weak_tables;
    struct hash_table *prev = &dummy,
                      *this = flush_object_stack(stack),
                      *next = NIL;
    prev->next_weak_hash_table = this;
    for ( ; (void*)this != NIL ; this = next ) {
        assert(header_widetag(this->header) == INSTANCE_WIDETAG);
        next = this->next_weak_hash_table;
        /* weak tables NOT in a managed space are always alive, but are
         * subject to splatting */
        int this_alive = !is_gcable_object(make_lispobj(this, INSTANCE_POINTER_LOWTAG))
            || sml_heap_check_alive(this);
        if (!this_alive) { // snip it out
            this->next_weak_hash_table = 0;
            prev->next_weak_hash_table = next;
        } else {
            smash_weak_table_pairs(this);
            prev = this;
        }
    }
    if (prev == &dummy) return;
    // Stuff the tables back into the global list
    spin_lock(&stack->lock);
    uword_t* top = stack->top;
    assert((void*)top == NIL || *(unsigned char*)top == INSTANCE_WIDETAG);
    prev->next_weak_hash_table = (void*)top;
    // the 'next' of dummy is the real head of the list
    stack->top = dummy.next_weak_hash_table;
    spin_unlock(&stack->lock);
}

void clear_weak_references() {
    fprintf(stderr, "Clearing weak refs\n");
    // First clear hash-table elements since we're holding a lock that globally
    // inhibits table access from mutators. Releasing it sooner than later is the polite thing.
    clear_weak_table_refs();
    weakTableOpsInhibited = 0;
    pthread_rwlock_unlock(&weakTableLock);
    fprintf(stderr, "released global weak table lock\n");
    clear_weak_vector_refs();
    atomic_store(&weakRefState, weakRefState_NORMAL);
}

void sml_heap_collector_after_mark() {
        atomic_store(&collector.collector_bitmap_cleared, 0);
        if (1) {
            char msg[100];
            int n = snprintf(msg, sizeof msg,
                             "weakRefGray omit: %d; remset_ins omit: %d small + %d large\n",
                             weakrefgray_omit, remset_omit_sm, remset_omit_lg);
            write(2, msg, n);
        }
        atomic_store(&weakrefgray_omit, 0);
        atomic_store(&remset_omit_lg, 0);
        atomic_store(&remset_omit_sm, 0);
        if (smlgc_verbose)
          printf("weakref_get: %d + %d + %d + %d normal,tracing,repeat,splat\n",
                 weakrefget_ct[0], weakrefget_ct[1], weakrefget_ct[2], weakrefget_ct[3]);

}

static inline int snapshot_block_index(struct segment* seg) {
  return objindex_or_end(seg, seg->snapshot_free);
}
int seglist_find(struct segment* seg, struct segment* list)
{
  while (list && (void*)list != (void*)NIL) {
    if (seg == list) return 1;
    list = (void*)list->as_list.next;
  }
  return 0;
}
void diagnose_obj(ATTR_UNUSED char* label, lispobj taggedptr) {
  void* obj = native_pointer(taggedptr);
  if (in_bitmapped_subheap(obj)) {

    struct segment* seg = segment_addr(obj);
    int blocksize_log2 = seg->blocksize_log2;
    struct segment* partial = (void*)atomic_load(&global_subheaps[blocksize_log2].partial.top);
    struct segment* filled = (void*)atomic_load(&global_subheaps[blocksize_log2].filled.top);
    struct segment* collect = collector.collect_set.segments[blocksize_log2];
    fprintf(stderr, "seg %5d @ %p blksize %d snap=%d/%d free=%d%s%s%s\n",
             segment_number(seg), seg,
            seg->layout->blocksize_bytes,
            snapshot_block_index(seg), seg->layout->num_blocks,
            seg->free_count,
            seglist_find(seg, partial) ? " PARTIAL":"",
            seglist_find(seg, filled) ? " FILLED":"",
            seglist_find(seg, collect) ? " COLLECT":"");

    unsigned int obindex = object_index(seg, obj);
    sml_bitptr_t b;
    b = BITPTR(BITMAP0_BASE(seg), obindex);
    int allocated = BITPTR_TEST(b) != 0;
    b = BITPTR(COLLECT_BITMAP_BASE(seg), obindex);
    int marked = BITPTR_TEST(b) != 0;
    struct stack_slot *stackslot = object_stack_slot(obj);

    fprintf(stderr, "obj %d link=%p%s%s\n",
            obindex, stackslot->next, allocated ? " ALLOC" : "", marked ? " MARK" : "");

    // Show thread alloc ptrs for objects of this size class
    extern struct thread* all_threads;
    struct thread* th;
    for (th = all_threads; th ; th = th->next) {
      struct alloc_ptr* ap = &th->ap4;
      ap += (blocksize_log2 - 4);
      if (ap->free) {
        seg = segment_addr(ap->freebit.ptr);
        printf("thread %p: free=%p (index %d)\n", th,
               ap->free, objindex_or_end(seg, ap->free));
      }
    }

  } else {

    struct malloc_segment* mseg = OBJ_TO_MALLOC_SEGMENT(obj);
    assert(mseg);
    fprintf(stderr, "Large (size=%d) next=%p marked=%d%s\n",
            (int)mseg->size, mseg->stack.next,
            mseg->markbit,
            seglist_find((struct segment*)mseg,
                         (struct segment*)collector.collect_set.malloc_segments)
            ? " THREATENED":"");

  }
}
void gc_show_seg(lispobj x)
{
    //struct segment* seg = segment_addr(x);
    //dump_segment("", x);
    diagnose_obj("", x);
}

void force_gc() {
  force_gc_flag = 1;
  pthread_cond_signal(&collector_control.cond_collector);
}
