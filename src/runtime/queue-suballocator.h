/* To allocate queue blocks without mmaping every time, we allocate larger and
 * larger "chunks" of memory. As the size of each subsequent chunk grows
 * exponentially, the metadata needed to represent a mark stack of any size
 * and the number of mmaps grow logarithmically. */
#define INITIAL_SIZE (QBLOCK_BYTES * 32)
#define CHUNKS 20
/* Only give back chunks when we haven't used them after this many
 * calls to suballoc_release. */
#define AGE_LIMIT 10

struct suballocator_chunk {
  uword_t start;
  uword_t free;
  uword_t size;
  uword_t age;
};
/* suballoc_allocate is lazily written and skips over the first chunk, so
 * add one here to make CHUNKS make sense. No big loss. */
struct suballocator {
  char *name;
  struct suballocator_chunk chunks[CHUNKS + 1];
  unsigned int current_chunk;
  uword_t hwm;
  lock_t suballocator_lock;
};
/* Zero-initialising structs in an array in a struct - hooray */
#define SUBALLOCATOR_INITIALIZER(name) { name, { { 0 } }, 0, 0, LOCK_INITIALIZER }

static bool chunk_has_space(struct suballocator *s, int index) {
  return s->chunks[index].free < s->chunks[index].start + s->chunks[index].size;
}

/* Free all memory used by the mark stack, giving back to the OS. */
static void suballoc_release(struct suballocator *s) {
  uword_t used = 0;
  for (int i = 0; i < CHUNKS; i++) {
    if (s->chunks[i].start) {
      used += s->chunks[i].size;
      if (s->chunks[i].age >= AGE_LIMIT) {
        os_deallocate((void*)s->chunks[i].start, s->chunks[i].size);
        s->chunks[i].start = s->chunks[i].free = s->chunks[i].size = 0;
      } else {
        s->chunks[i].age++;
        s->chunks[i].free = s->chunks[i].start;
      }
    }
  }
  if (used > s->hwm) {
#if 0
    fprintf(stderr, "Used %ld bytes for %s\n", used, s->name);
#endif
    s->hwm = used;
  }
  s->current_chunk = 0;
}

static struct Qblock *suballoc_allocate(struct suballocator *s) {
  acquire_lock(&s->suballocator_lock);
  struct suballocator_chunk *chunk = s->chunks + s->current_chunk;
  /* Check if we need to make a new chunk first. */
  if (!chunk_has_space(s, s->current_chunk)) {
    /* This skips over chunk[0]. Oh well. */
    if (s->current_chunk == CHUNKS) lose("Ran out of suballocator chunks.");
    s->current_chunk++;
    chunk = s->chunks + s->current_chunk;
    /* Check if we can reuse a chunk which was allocated before. */
    if (!chunk_has_space(s, s->current_chunk)) {
      uword_t size = INITIAL_SIZE << s->current_chunk;
      uword_t address = (uword_t)os_allocate(size);
      if (!address) lose("Failed to allocate suballocator chunk with %ld bytes.", size);
      // fprintf(stderr, "alloc #%d: %ld at %ld\n", current_chunk, size, address);
      chunk->start = chunk->free = address;
      chunk->size = size;
    }
    chunk->age = 0;
  }
  /* Now get another block from the current chunk. */
  struct Qblock *where = (struct Qblock*)chunk->free;
  gc_assert(where != NULL);
  where->count = 0;
  chunk->free += QBLOCK_BYTES;
  release_lock(&s->suballocator_lock);
  return where;
}
