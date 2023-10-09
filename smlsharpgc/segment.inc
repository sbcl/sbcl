#ifndef SEGMENT_SIZE_LOG2
#define SEGMENT_SIZE_LOG2  15   /* 32k */
#endif /* SEGMENT_SIZE_LOG2 */
#define SEGMENT_SIZE (1U << SEGMENT_SIZE_LOG2)
#ifndef SEG_RANK
#define SEG_RANK  3
#endif /* SEG_RANK */

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

/* assume that segment address is a multiple of SEGMENT_SIZE */
static inline struct segment *
segment_addr(const void *p)
{
        return (void*)((uintptr_t)p & ~((uintptr_t)(SEGMENT_SIZE - 1)));
}

/* thread-local use only */
struct object_list {
        struct stack_slot begin;
        struct stack_slot *last;
        int count;
};
void object_list_append(struct object_list *l, void *obj);
