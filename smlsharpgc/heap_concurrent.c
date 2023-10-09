/*
 * heap_concurrent.c
 * @copyright (c) 2015, Tohoku University.
 * @author UENO Katsuhiro
 */

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

struct list_item {
        struct list_item *next;
};

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

static void *
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

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
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

#ifndef SEGMENT_SIZE_LOG2
#define SEGMENT_SIZE_LOG2  15   /* 32k */
#endif /* SEGMENT_SIZE_LOG2 */
#define SEGMENT_SIZE (1U << SEGMENT_SIZE_LOG2)
#ifndef SEG_RANK
#define SEG_RANK  3
#endif /* SEG_RANK */

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
struct segment_layout {
        unsigned int blocksize_bytes;
        unsigned int bitmap_base[SEG_RANK + 1];
        sml_bmword_t bitmap_sentinel[SEG_RANK];
        unsigned int stack_offset;
        unsigned int stack_limit;
        unsigned int block_offset;
        unsigned int num_blocks;
        unsigned int block_limit;
};

struct segment {
        struct list_item as_list;
        struct stack_slot {
                _Atomic(void *) next;
        } *stack;  /* == seg + layout->stack_offset */
        char *block_base;  /* == seg + layout->block_offset */
        /* If block_base is null, this segment is in free list */
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        /* do not modify snapshot_free during the collector traces objects. */
        char *snapshot_free;
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
        const struct segment_layout *layout;
        unsigned int blocksize_log2;
        int free_count;
        /* The free_count field not only holds the count (as its name) but
         * indicates which set the segment is in.
         * If free_count is negative, the segment is in the collect set
         * and its absolute value is the count of unmarked blocks.
         * If free_count is zero, the segment is in the filled set.
         * If free_count is positive, the segment is in the partial set.
         */
};

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
        ((char*)ADD_BYTES((seg)->block_base, (seg)->layout->block_limit))


#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
#define COLLECT_BITMAP_BASE(seg) \
        ((sml_bmword_t*)ADD_BYTES(seg, (seg)->layout->bitmap_base[SEG_RANK]))
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */


/* assume that segment address is a multiple of SEGMENT_SIZE */
static inline struct segment *
segment_addr(const void *p)
{
        return (void*)((uintptr_t)p & ~((uintptr_t)(SEGMENT_SIZE - 1)));
}

static inline unsigned int
object_index(struct segment *seg, void *obj)
{
        assert(segment_addr(obj) == seg);
        assert((char*)obj >= seg->block_base);
        assert((char*)obj < (char*)seg + SEGMENT_SIZE);
        return DIF_BYTES(obj, seg->block_base) >> seg->blocksize_log2;
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

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++)
                compute_layout(i);
}

/* for debug */
static void ATTR_UNUSED
scribble_segment(struct segment *seg)
{
        char *block = seg->block_base;
        sml_bitptr_t b = BITPTR(BITMAP0_BASE(seg), 0);
        unsigned int i;

        for (i = 0; i < seg->layout->num_blocks; i++) {
                if (!BITPTR_TEST(b))
                        memset(block - OBJ_HEADER_SIZE, 0x55,
                               seg->layout->blocksize_bytes);
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

        DEBUG(scribble_segment(seg));
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
        p = ReservePage(NULL, max_size + SEGMENT_SIZE);
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
        CommitPage(heap->free, SEGMENT_SIZE * alloc);
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

        for (seg = heap->begin; seg < heap->free;
             seg = ADD_BYTES(seg, SEGMENT_SIZE)) {
                if (!seg->block_base)
                        continue;
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
collect_subheap(struct subheap *subheap, struct segment **seg_p)
{
        *seg_p = stack_flush(&subheap->filled);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

static void
collect_subheaps(struct segment **segarray)
{
        unsigned int i;

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++)
                collect_subheap(&global_subheaps[i], &segarray[i]);
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
static ATTR_UNUSED void
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
        sml_bmword_t markbit;
        char dummy_for_objheader[OBJ_HEADER_SIZE];
};

#define MALLOC_OBJECT_OFFSET \
        CEILING(sizeof(struct malloc_segment), MAXALIGN)
#define OBJ_TO_MALLOC_SEGMENT(obj) \
        ((struct malloc_segment *)ADD_BYTES(obj, -MALLOC_OBJECT_OFFSET))
#define MALLOC_SEGMENT_TO_OBJ(mseg) \
        ADD_BYTES(mseg, MALLOC_OBJECT_OFFSET)
#define MALLOC_SEGMENT_SIZE(mseg) \
        (MALLOC_OBJECT_OFFSET \
         + OBJ_TOTAL_SIZE(MALLOC_SEGMENT_TO_OBJ(mseg)) \
         - OBJ_HEADER_SIZE)

/* initialized correctly by default */
static struct stack malloc_segments;

static struct malloc_segment *
malloc_segment(unsigned int alloc_size)
{
        struct malloc_segment *mseg;

        mseg = xmalloc(MALLOC_OBJECT_OFFSET + alloc_size);
        DEBUG(memset(mseg, 0x55, MALLOC_OBJECT_OFFSET + alloc_size));
        store_relaxed(&mseg->stack.next, NULL);
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        /* Indicate that this is allocated before SYNC2 */
        mseg->markbit = (sml_current_phase() >= SYNC2);
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
        stack_push(&malloc_segments, &mseg->as_list);
        return mseg;
}

static void
destroy_malloc_segment(struct malloc_segment *mseg)
{
        assert(mseg->markbit == 0);
        DEBUG(memset(mseg, 0x55, MALLOC_SEGMENT_SIZE(mseg)));
        free(mseg);
}

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
static void
collect_malloc_segments(struct malloc_segment **collect_msegs)
{
        *collect_msegs = stack_flush(&malloc_segments);
}

#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
static void
collect_malloc_segments(struct malloc_segment **collect_msegs)
{
        *collect_msegs = stack_flush(&malloc_segments);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

static void
destroy_malloc_segments()
{
        struct malloc_segment *mseg, *next;

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

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
/* thread-local use only */
struct object_list {
        struct stack_slot begin;
        struct stack_slot *last;
};

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

static struct stack_slot *
object_stack_slot(void *obj)
{
        struct segment *seg;
        struct malloc_segment *mseg;

        if (obj == NULL)
                return NULL;
        if (OBJ_HEADER(obj) & OBJ_FLAG_SKIP)
                return NULL;
        if (OBJ_TOTAL_SIZE(obj) > BLOCKSIZE_MAX) {
                mseg = OBJ_TO_MALLOC_SEGMENT(obj);
                return &mseg->stack;
        } else {
                seg = segment_addr(obj);
                assert((char*)segment_pool.heap.begin <= (char*)seg
                       && (char*)seg < (char*)segment_pool.heap.end);
                return &seg->stack[object_index(seg, obj)];
        }
}

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
static void
object_list_init(struct object_list *l)
{
        atomic_init(&l->begin.next, NIL);
        l->last = &l->begin;
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
static void
object_list_append(struct object_list *l, void *obj)
{
        struct stack_slot *slot = object_stack_slot(obj);
        void *old = NULL;

        /* this may fail if obj already belongs to another list or stack */
        if (slot
            && load_relaxed(&slot->next) == old
            && cmpswap_relaxed(&slot->next, &old, NIL)) {
                store_relaxed(&l->last->next, obj);
                l->last = slot;
        }
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
static void
enum_obj_to_list(void **slot, void *data)
{
        void *obj = *slot;
        struct object_list *objs = data;
        object_list_append(objs, obj);
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
push_object(struct object_stack *stack, void *obj)
{
        struct stack_slot *slot = object_stack_slot(obj);
        void *old = NULL;

        if (!slot)
                return;

         /* ensure that the collector receives all objects whose stack_slot
          * were occupied by mutators. */
         spin_lock(&stack->lock);

        /* this may fail if obj already belongs to another list or stack */
        if (load_relaxed(&slot->next) == old
             && cmpswap_relaxed(&slot->next, &old, stack->top))
                 stack->top = obj;

         spin_unlock(&stack->lock);
}

#endif /* !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY */

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

/* sizeof(struct alloc_ptr) must be power of 2 for performance */
struct alloc_ptr {
        sml_bitptr_t freebit;
        char *free;
        unsigned int blocksize_bytes;
};

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
                free(p);
                p = next;
        }
}

static void
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
                if (ptr->free)
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
                p = xmalloc(sizeof(union sml_alloc));
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
        assert(subheap == &global_subheaps[seg->blocksize_log2]);
        assert(seg->as_list.next == (void*)-1);
        stack_push(&subheap->filled, &seg->as_list);
}

static void
move_all_to_filled(union sml_alloc *ptr_set)
{
        unsigned int i;
        struct alloc_ptr *ptr;

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                ptr = &ptr_set->ptr[i];
                if (ptr->free) {
                        move_to_filled(ptr, &global_subheaps[i]);
                        clear_alloc_ptr(ptr);
                }
        }
}

static void
collect_alloc_ptr_set(union sml_alloc *p, struct segment **segarray)
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
collect_alloc_ptr_set_pool(struct segment **segarray)
{
        union sml_alloc *p, *next;

        p = stack_flush(&alloc_ptr_set_pool.freelist);
        while (p) {
                collect_alloc_ptr_set(p, segarray);
                next = (union sml_alloc *)p->as_list.next;
                free(p);
                p = next;
        }
}

/********** collect set **********/

union collect_set {
        struct segment *segments[BLOCKSIZE_MAX_LOG2 + 1];
        struct malloc_segment *malloc_segments;
};

static void
collect_segments(union collect_set *c)
{
        collect_subheaps(c->segments);
        collect_malloc_segments(&c->malloc_segments);
        collect_alloc_ptr_set_pool(c->segments);
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

        for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
                s = &r->subheap[i];
                list_init(&s->partial);
                list_init(&s->filled);
                s->num_free = 0;
                s->num_free_blocks = 0;
                for (seg = c->segments[i]; seg;
                     seg = (struct segment *)seg->as_list.next) {
                        assert(seg->free_count <= 0);
                        seg->free_count *= -1;
                        DEBUG(scribble_segment(seg));
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
                        seg->snapshot_free = seg->block_base;
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
                        assert(stat_segment(seg).num_marked
                               == seg->layout->num_blocks - seg->free_count);
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
        }

        list_init(&r->malloc_segments);
        for (mseg = c->malloc_segments; mseg; mseg = next) {
                next = (struct malloc_segment *)mseg->as_list.next;
                if (mseg->markbit) {
                        list_append(&r->malloc_segments, &mseg->as_list);
                        /* r->num_filled++; */
                } else {
                        destroy_malloc_segment(mseg);
                }
        }
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
        unsigned int new_num_blocks = 0, new_num_free = 0;

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
        _Atomic(unsigned int) num_filled_total;
        unsigned int gc_threshold;
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
        void *root_objects;
        union collect_set collect_set;
        _Atomic(unsigned int) num_request;
} collector = {
        .root_objects = NIL,
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        .objects_from_mutators = EMPTY_OBJECT_STACK
#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
};

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
static void
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
static void
trace_all()
{
        void *top = collector.root_objects;
        do {
                while (top != NIL)
                        sml_obj_enum_ptr(pop(&top), push, &top);
                /* objects_from_mutators contains objects that is visited
                 * but not enumerated yet due to failure of CAS in push. */
                top = flush_object_stack(&collector.objects_from_mutators);
                visit_all(top);
        } while (top != NIL);
        collector.root_objects = NIL;
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
void
sml_heap_collector_sync1()
{
        collect_segments(&collector.collect_set);
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
void
sml_heap_collector_sync2()
{
        struct object_list objs;
        object_list_init(&objs);
        sml_global_enum_ptr(enum_obj_to_list, &objs);
        sml_callback_enum_ptr(enum_obj_to_list, &objs);
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
void
sml_heap_collector_mark()
{
        unsigned int n;
        /* ToDo: replace clear bitmap with copy from collector bitmap */
        n = clear_collect_set(&collector.collect_set);
        fetch_sub(relaxed, &collector.num_filled_total, n);
        clear_collect_bitmaps(&segment_pool.heap);
        trace_all();
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

int
sml_heap_check_alive(void **slot)
{
        void *obj = *slot;
        struct segment *seg;
        unsigned int index;
        sml_bitptr_t b;

        if (OBJ_HEADER(obj) & OBJ_FLAG_SKIP)
                return 1;

        seg = segment_addr(obj);
        index = object_index(seg, obj);
        b = BITPTR(BITMAP0_BASE(seg), index);
        return seg->free_count >= 0 || BITPTR_TEST(b);
}

void
sml_heap_collector_async()
{
        struct reclaim r;
        separate_segments(&collector.collect_set, &r);
        rebalance_subheaps(&r, load_relaxed(&collector.num_request));
        reclaim_segments(&r);
#if !defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
        fetch_add(relaxed, &collector.num_filled_total, r.num_filled);
        collector.gc_threshold =
                (segment_pool.heap.num_committed + r.num_filled) / 2;
#endif /* !WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY */
}

static void
do_gc()
{
#ifdef GCTIME
        sml_timer_t t1, t2;
        sml_timer_now(t1);
#endif /* GCTIME */
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

static void
wait_for_gc()
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
        unsigned int exit;
        pthread_t collector_thread;
} collector_control = {
        .lock = PTHREAD_MUTEX_INITIALIZER,
        .cond_collector = PTHREAD_COND_INITIALIZER,
        .cond_mutators = PTHREAD_COND_INITIALIZER,
};

static void
inc_num_filled_total()
{
        fetch_add(relaxed, &collector.num_filled_total, 1);
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

static void *
collector_main(void *arg ATTR_UNUSED)
{
        struct collector_control * const cc = &collector_control;
        unsigned int num_stalled;

        mutex_lock(&cc->lock);
 loop:
        while (!(cc->mutators_stalled > 0
                 || (load_relaxed(&collector.num_filled_total)
                     >= collector.gc_threshold)
                 || cc->exit))
                cond_wait(&cc->cond_collector, &cc->lock);
        store_relaxed(&collector.num_request, cc->mutators_stalled);
        mutex_unlock(&cc->lock);
        if (cc->exit)
                return NULL;

        do_gc();
        mutex_lock(&cc->lock);

        cc->gc_count++;
        if (cc->mutators_stalled == 0) goto loop;
 retry:
        /* Some mutators are being stalled due to lack of segments.
         * If at least one mutator is working, then the program continues. */
        sml_debug("%u out of %u threads stalled\n",
                  cc->mutators_stalled, cc->num_mutators);
        num_stalled = cc->mutators_stalled;
        cc->mutators_stalled = 0;
        cond_broadcast(&cc->cond_mutators);
        if (count_vote(num_stalled)) goto loop;

        /* All mutators voted for aborting the program and still stalled.
         * Try full GC and expect that it recovers the situation. */
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
        sml_stack_enum_ptr(user, push, &collector.root_objects);
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
        sml_stack_enum_ptr(user, enum_obj_to_list, &objs);
        push_objects(&collector.objects_from_mutators, &objs);
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
static inline void
remember(void *obj)
{
        push_object(&collector.objects_from_mutators, obj);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#if defined WITHOUT_MULTITHREAD || defined WITHOUT_CONCURRENCY
SML_PRIMITIVE void
sml_write(void *obj ATTR_UNUSED, void **writeaddr, void *new_value)
{
        *writeaddr = new_value;
}

#else /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */
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

static NOINLINE void *
find_bitmap(struct alloc_ptr *ptr)
{
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
        blocksize_log2 = ptr - &worker_tlv_get(alloc_ptr_set)->ptr[0];
        assert(BLOCKSIZE_MIN_LOG2 <= blocksize_log2
               && blocksize_log2 <= BLOCKSIZE_MAX_LOG2);
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
        struct malloc_segment *mseg = malloc_segment(alloc_size);
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
        assert(check_filled(OBJ_BEGIN(obj), 0x55, objsize));
        return obj;
}

/********** initialize/finalize garbage collection ***********/

void
sml_heap_init(size_t min_size, size_t max_size)
{
        init_segment_layout();
        init_segment_pool(min_size, max_size);

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
