/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

struct Qblock {
  int count;
  int tail;
  struct Qblock* next;
  lispobj elements[1];
};

#if 1
#ifdef LISP_FEATURE_MARK_REGION_GC
#define QBLOCK_BYTES (sizeof(lispobj) << 10)
#else
#define QBLOCK_BYTES 16384
#endif
// 1+ because struct QBlock has space for a single element within it
#define QBLOCK_CAPACITY (1+(QBLOCK_BYTES-sizeof(struct Qblock))/sizeof(lispobj))
#else
#define QBLOCK_CAPACITY 12 /* artificially low, for testing */
#endif

struct unbounded_queue {
  struct Qblock* head_block;
  struct Qblock* tail_block;
  struct Qblock* recycler;
  long tot_count; // Not used
};

static void __attribute__((unused)) gc_queue_init(struct unbounded_queue* q)
{
    struct Qblock* block = (struct Qblock*)os_allocate(QBLOCK_BYTES);
    q->head_block = block;
    q->tail_block = block;
    q->recycler   = 0;
    block->next = 0;
    block->tail = block->count = 0;
}

static void __attribute__((unused)) gc_queue_empty_recyclebin(struct unbounded_queue* q)
{
    gc_assert(q->head_block == q->tail_block);
    struct Qblock* block = q->recycler;
    while (block) {
        struct Qblock* next = block->next;
        os_deallocate((void*)block, QBLOCK_BYTES);
        block = next;
    }
    q->recycler = 0;
}
static void __attribute__((unused)) gc_queue_destroy(struct unbounded_queue* q)
{
    gc_queue_empty_recyclebin(q);
    os_deallocate((void*)q->head_block, QBLOCK_BYTES);
    q->head_block = q->tail_block = 0;
}

// fullcgc enqueues lispobj, but immobile-space enqueues lispobj*,
// which it will have to cast to lispobj.
static void __attribute__((unused)) gc_worklist_enqueue(struct unbounded_queue* q, lispobj elt)
{
    struct Qblock* block = q->tail_block;
    if (block->count == QBLOCK_CAPACITY) {
        struct Qblock* next;
        next = q->recycler;
        if (next) {
            q->recycler = next->next;
        } else {
            // Sure you could use some kind of size-doubling strategy,
            // but I don't want to.
            next = (struct Qblock*)os_allocate(QBLOCK_BYTES);
        }
        block = block->next = next;
        block->next = 0;
        block->tail = block->count = 0;
        q->tail_block = block;
    }
    block->elements[block->tail] = elt;
    if (++block->tail == QBLOCK_CAPACITY) block->tail = 0;
    ++block->count;
}

static lispobj __attribute__((unused)) gc_worklist_dequeue(struct unbounded_queue* q)
{
    struct Qblock* block = q->head_block;
    gc_assert(block->count);
    int index = block->tail - block->count;
    lispobj object = block->elements[index + (index<0 ? QBLOCK_CAPACITY : 0)];
    if (--block->count == 0) {
        struct Qblock* next = block->next;
        if (next) {
            q->head_block = next;
            block->next = q->recycler;
            q->recycler = block;
        }
    }
    return object;
}
