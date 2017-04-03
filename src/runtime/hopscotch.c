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

/*
 * Our implementation of the hopscotch algorithm described in
 * http://people.csail.mit.edu/shanir/publications/disc2008_submission_98.pdf
 * which is extremely simple variation on linear probing
 * that provides a guaranteed bound on number of probes.
 */

#include <pthread.h> // only because of our dang non-self-contained .h files
#ifdef COLLECT_STATISTICS
#include <stdio.h>
#endif

#include "genesis/constants.h"
#include "runtime.h"
#include "gc-internal.h" // for os_validate()
#include "hopscotch.h"

typedef struct hopscotch_table* tableptr;
void hopscotch_integrity_check(tableptr,char*,int);

#define table_size(table) (1+(table).mask)
#define hash(x) x
#define INTEGRITY_CHECK(when) {}

/// Set a single bit in the hop mask for logical cell at 'index'
static inline void set_hop_bit(tableptr ht, unsigned index, int bit)
{
    unsigned mask = 1<<bit;
    ht->hops[index] |= mask;
}
/// Set all the bits in the hop mask for logical cell at 'index'
static inline void set_hop_mask(tableptr ht, unsigned index, unsigned bits)
{
    ht->hops[index] = bits;
}
static inline unsigned get_hop_mask(tableptr ht, unsigned index)
{
    return ht->hops[index];
}

/// Hopscotch storage allocation granularity.
/// Our usual value of "page size" is the GC page size, which is
/// coarser than necessary (cf {target}/backend-parms.lisp).
static int hh_allocation_granularity = 4096;
#define ALLOCATION_OVERHEAD (2*sizeof(unsigned int))
/// Return the number of usable bytes (excluding the header) in an allocation
#define usable_size(x) ((unsigned int*)x)[-1]

/// Sizing up a table can't be done in-place, so reserve a few blocks
/// of memory for when resize has to happen during GC. We don't return
/// these blocks to the OS.  If even more is required, it will be allocated
/// as needed, but we'll only keep on reserve at most two blocks.
#define N_CACHED_ALLOCS 2
char* cached_alloc[N_CACHED_ALLOCS];
void hopscotch_init() // Called once on runtime startup, from gc_init().
{
    // Get 16KB from the OS and evenly divide it into two pieces.
    int n_bytes_per_slice = 8 * 1024;
    int n_bytes_total = N_CACHED_ALLOCS * n_bytes_per_slice;
    char* mem = (char*)os_validate(0, n_bytes_total);
    gc_assert(mem);
    cached_alloc[0] = mem + ALLOCATION_OVERHEAD;
    cached_alloc[1] = cached_alloc[0] + n_bytes_per_slice;
    // Write the user-visible size of each allocation into the block header
    usable_size(cached_alloc[0]) = n_bytes_per_slice - ALLOCATION_OVERHEAD;
    usable_size(cached_alloc[1]) = n_bytes_per_slice - ALLOCATION_OVERHEAD;
}

/* Return the address of at least 'nbytes' of storage.
 * This is not a general-purpose thing - it's only intended to keep
 * one or perhaps two hopscotch hash tables around during GC,
 * for pinned objects, and maybe something else.
 * As such, no attempt is made to minimize storage use,
 * and if used more generally, would badly suffer from fragmentation.
 */
static char* cached_allocate(os_vm_size_t nbytes)
{
    // See if either cached allocation is large enough.
    if (cached_alloc[0] && usable_size(cached_alloc[0]) >= nbytes) {
        // Yup, just give the consumer the whole thing.
        char* result = cached_alloc[0];
        cached_alloc[0] = 0; // Remove from the pool
        return result;
    }
    if (cached_alloc[1] && usable_size(cached_alloc[1]) >= nbytes) {  // Ditto.
        char* result = cached_alloc[1];
        cached_alloc[1] = 0;
        return result;
    }
    // Request more memory, not using malloc().
    // Round up, since the OS will give more than asked if the request is
    // not a multiple of the mmap granularity, which we'll assume is 4K.
    // (It doesn't actually matter.)
    nbytes = CEILING(nbytes, hh_allocation_granularity);
    char* result = os_validate(0, nbytes);
    gc_assert(result);
    result += ALLOCATION_OVERHEAD;
    usable_size(result) = nbytes - ALLOCATION_OVERHEAD;
    return result;
}

/* Return 'mem' to the cache, first zero-filling to the specified length.
 * Though the memory size is recorded in the header of the memory block,
 * the allocator doesn't know how many bytes were touched by the requestor,
 * which is why the length is specified again.
 * If returning it to the OS and not the cache, then don't bother 0-filling.
 */
static void cached_deallocate(char* mem, int zero_fill_length)
{
    int line = 0;
    if (!cached_alloc[0]) {
    } else if (!cached_alloc[1])
        line = 1;
    else {
        // Try to retain whichever 2 blocks are largest (the given one and
        // cached ones) in the hope of fulfilling future requests from cache.
        int this_size = usable_size(mem);
        int cached_size0 = usable_size(cached_alloc[0]);
        int cached_size1 = usable_size(cached_alloc[1]);
        if (!(this_size > cached_size0 || this_size > cached_size1)) {
            // mem is not strictly larger than either cached block. Release it.
            os_deallocate(mem - ALLOCATION_OVERHEAD,
                          usable_size(mem) + ALLOCATION_OVERHEAD);
            return;
        }
        // Evict and replace the smaller of the two cache entries.
        if (cached_size1 < cached_size0)
            line = 1;
        os_deallocate(cached_alloc[line] - ALLOCATION_OVERHEAD,
                      usable_size(cached_alloc[line]) + ALLOCATION_OVERHEAD);
    }
    bzero(mem, zero_fill_length);
    cached_alloc[line] = mem;
}

/* Initialize 'ht' for 'size' logical bins with a max hop of 'hop_range'.
 * 'valuesp' makes a hash-map if true; a hash-set if false.
 * Hop range will be selected automatically if specified as 0.
 */
static void hopscotch_realloc(tableptr ht, boolean valuesp, int size, char hop_range)
{
    // Somewhat arbitrary criteria that improve the worst-case probes for
    // small hashtables. The reference algorithm uses a fixed max hop of 32,
    // but fewer is better, and our pinned object table tends to be very small.
    if (hop_range == 0) { // Let us pick.
        if      (size <=   1024) hop_range =  8;
        else if (size <=  16384) hop_range = 16;
        else                     hop_range = 32;
    }

    // The key/value arrays are *not* circular.
    // The last few logical cells in the key array can use physical cells
    // at indices greater than 'size'; there's no wrapping back to index 0.
    int n_keys = size + (hop_range - 1);
    unsigned storage_size = sizeof (uword_t) * n_keys
        + sizeof (int) * size // hop bitmasks
        + (valuesp ? (sizeof (int) * n_keys) : 0); // values

    if (ht->keys)
        gc_assert(usable_size(ht->keys) >= storage_size);
    else
        ht->keys = (uword_t*)cached_allocate(storage_size);

    ht->mem_size  = storage_size;
    ht->mask      = size - 1;
    ht->hop_range = hop_range;
    ht->hops      = (unsigned*)((char*)ht->keys + sizeof (uword_t) * n_keys);
    ht->values    = !valuesp ? 0 :
        (unsigned*)((char*)ht->hops + sizeof (int) * size);
}

/* Initialize 'ht' for first use, which entails zeroing the counters
 * and allocating storage.
 */
void hopscotch_create(tableptr ht, boolean valuesp, int size, char hop_range)
{
    gc_assert((size & (size-1)) == 0); // ensure power-of-2
    ht->count = 0;
    ht->rehashing = 0;
    ht->resized = 0;
    // Ensure that the first reset() doesn't do something screwy.
    ht->prev_size = size;
    // Clear these even if not collecting statistics,
    // because it looks ugly if we don't.
    ht->hit .n_seeks = ht->hit .n_probes = 0;
    ht->miss.n_seeks = ht->miss.n_probes = 0;
    //
    ht->keys = 0; // Forces allocation of backing storage.
    hopscotch_realloc(ht, valuesp, size, hop_range);
}

/* Delete the storage associated with 'ht' */
void hopscotch_delete(tableptr ht)
{
    if (ht->mem_size) { // Free it, zero-filling if ever used.
        cached_deallocate((char*)ht->keys, ht->count ? ht->mem_size : 0);
        ht->keys   = 0;
        ht->hops   = 0;
        ht->values = 0;
    }
}

/* Prepare 'ht' for re-use. Same as CLRHASH */
void hopscotch_reset(tableptr ht)
{
    if (ht->count) {
        bzero(ht->keys, ht->mem_size);
        ht->count = 0;
    }
    // If the size exceeds twice the final size from the prior run,
    // or is the same size and was not enlarged, then downsize,
    // but don't go below a certain minimal size.
    int size = table_size(*ht);
#if 0
    fprintf(stderr, "hh reset: size=%d prev=%d upsized=%d\n",
            size, ht->prev_size, ht->resized);
#endif
    if (size > (ht->prev_size << 1)
        || (size == ht->prev_size && !ht->resized && size > 8))
        // Halve the size for the next GC cycle
        hopscotch_realloc(ht, ht->values != 0, size >> 1, 0);
    ht->prev_size = size;
    ht->resized = 0;
    INTEGRITY_CHECK("after reset");
}

/* Double the size of 'ht'. Called when an insertion overflows the table */
tableptr hopscotch_resize_up(tableptr ht)
{
    int size = ht->mask + 1; // Logical bin count
    int old_max_index = hopscotch_max_key_index(*ht);
    struct hopscotch_table copy;

#if 0
    fprintf(stderr, "resize up: ct=%d cap=%d hop=%d LF=%f\n",
            ht->count, 1+old_max_index, ht->hop_range,
            (float)ht->count/(1+old_max_index));
#endif
    INTEGRITY_CHECK("before resize");
    // Copy the keys or key/value pairs.
    //
    // It's conceivable, however improbable, that there is a hash function
    // which causes more collisions at the new size than the old size.
    // Due to the fixed hop range, failure to insert while rehashing
    // must be caught so that we can try again with a larger size.
    // But usually this loop will execute exactly once.
    int i;
    do {
        size *= 2;
        hopscotch_create(&copy, ht->values != 0, size, 0);
        copy.rehashing = 1; // Causes put() to return 0 on failure
        if (ht->values) {
            for(i=old_max_index ; i >= 0 ; --i)
              if (ht->keys[i])
                if (!hopscotch_put(&copy, ht->keys[i], ht->values[i]))
                  break;
        } else {
            for(i=old_max_index ; i >= 0 ; --i)
              if (ht->keys[i])
                if (!hopscotch_put(&copy, ht->keys[i], 1)) {
#if 0
                  fprintf(stderr, "resize failed with new size %d, hop_range %d\n",
                          size, copy.hop_range);
#endif
                  break;
                }
        }
    } while (i >= 0 && (hopscotch_delete(&copy), 1));

    // Zero-fill and release the old storage.
    cached_deallocate((char*)ht->keys, ht->mem_size);

    // Move all of the data pointers from 'copy' into ht.
    // mem_size is passed to bzero() when resetting the table,
    // so definitely be sure to use the new, not the old.
    // And of course _don't_ hopscotch_delete() copy when done.
    ht->mem_size  = copy.mem_size;
    ht->mask      = copy.mask;
    ht->hop_range = copy.hop_range;
    ht->keys      = copy.keys;
    ht->hops      = copy.hops;
    ht->values    = copy.values;
    ht->resized   = 1;
    INTEGRITY_CHECK("after resize");
    return ht;
}

void hopscotch_log_stats(tableptr ht)
{
#ifdef COLLECT_STATISTICS
    static FILE *hh_logfile;
    if (!hh_logfile)
      hh_logfile = fopen("hash-stats.txt","a");
    fprintf(hh_logfile,
            "hopscotch: ct=%5d cap=%5d LF=%f seek=%5d+%5d probe/seek=%f+%f (hit+miss)\n",
            ht->count,
            (ht->mask + ht->hop_range),
            (float)ht->count / (ht->mask + ht->hop_range),
            ht->hit.n_seeks, ht->miss.n_seeks,
            ht->hit.n_seeks>0 ? (float)ht->hit.n_probes / ht->hit.n_seeks : 0.0,
            ht->miss.n_seeks>0 ? (float)ht->miss.n_probes / ht->miss.n_seeks : 0.0);
    fflush(hh_logfile);
    ht->hit.n_seeks = ht->hit.n_probes = 0;
    ht->miss.n_seeks = ht->miss.n_probes = 0;
#endif
}

/* Return an integer with 'n' low-order 1 bits.
 * This does not do the right thing for n = 0, but that's fine!
 * (Shifting an unsigned 32-bit integer rightward by 32 is not defined.
 * 32-bit x86 masks the shift amount to 5 bits, so you get 0 shift)
 */
static inline unsigned int bitmask_of_width(int n) {
    return (0xFFFFFFFFU >> (32 - n));
}

#define put_pair(i,k,v) ht->keys[i] = k; if(ht->values) ht->values[i] = v

/* Add key/val to 'ht'. 'val' is ignored for a hash-set */
int hopscotch_put(tableptr ht, uword_t key, unsigned int val)
{
    // 'desired_index' is where 'key' logically belongs, but it
    // may physically go in any cell to the right up to (range-1) away.
    int desired_index = hash(key) & ht->mask;
    if (ht->keys[desired_index] == 0) {  // Instant win
        put_pair(desired_index, key, val);
        set_hop_bit(ht, desired_index, 0);
        return ++ht->count;
    }
    // 'limit' is the inclusive bound on cell indices.
    int limit = hopscotch_max_key_index(*ht);
    int free_index = desired_index;
    int displacement;
    while (ht->keys[++free_index] != 0) // While cell is occupied
        if (free_index == limit)
            return ht->rehashing ? 0 : // fail if rehash table is too small
                hopscotch_put(hopscotch_resize_up(ht), key, val);

    // 'free_index' is where *some* item could go,
    // but it might be too far away for this key.
 retry:
    if ((displacement = free_index - desired_index) < ht->hop_range) {
        put_pair(free_index, key, val);
        set_hop_bit(ht, desired_index, displacement);
        return ++ht->count;
    }
    // Find the empty cell furthest away from and to the left of free_index,
    // within the hop_range, that contains an item that can be moved.
    int logical_bin = free_index - (ht->hop_range - 1);
    // limit is the max index (inclusive) of the available free cells
    // up to but excluding 'free_index'
    limit = free_index - 1;
    // In case free_index currently points to a physical bin "off the end"
    // of the logical bins, confine to the highest logical bin,
    // which is just the table mask.
    if (limit >= (int)ht->mask)
        limit = ht->mask;
    // Now 'free_index' is fixed, and 'logical_bin' is what we search
    // over to find something to displace into the free_index.
    // Example:                       v----- free index
    //     |     |  X  |  X  |  O  |     |   [X = filled. O = filled + owned]
    //              ^--- logical bin (displacement = 3)
    // Look at the low 3 bits of the hop bits for 'logical_bin'.
    // Those indicate the physical cells "owned" by the logical cell
    // and within the needed distance to the free cell.
    // If any are set, the leftmost bit is robbed to make room.
    // In the above example, bit index 2 (0-based index) would be claimed.
    for ( ; logical_bin <= limit ; ++logical_bin ) {
        displacement = free_index - logical_bin;
        unsigned bits = get_hop_mask(ht, logical_bin);
        unsigned masked_bits = bits & bitmask_of_width(displacement);
        if (masked_bits) {
            int victim = ffs(masked_bits) - 1;  // ffs() is 1-based
            int physical_elt = logical_bin + victim;
            // Relocate the contents of 'physical_elt' to 'free_index'
            put_pair(free_index, ht->keys[physical_elt], ht->values[physical_elt]);
            put_pair(physical_elt, 0, 0);
            // This logical bin no longer owns the index where the victim was,
            // but does own the index where it got moved to.
            set_hop_mask(ht, logical_bin, bits ^ (1<<displacement | 1<<victim));
            // Now free_index gets smaller, and we try again from the top.
            free_index = physical_elt;
            goto retry;
        }
    }
    // Too many collisions and not enough room to move things around.
    return ht->rehashing ? 0 : hopscotch_put(hopscotch_resize_up(ht), key, val);
#undef put_pair
}

/// When probing on lookup, while we could use the mask bits in the
/// desired logical bin to restrict the number of key comparisons made,
/// this turns out to be worse. Though slightly counter-intuitive,
/// it is likely due to one fewer conditional branch when we hit the
/// first choice physical cell. The probe() macro will decide whether
/// to use the mask bits and/or record the number of key comparisons.
/// Statistics gathering also slows us down a lot, so only do it when
/// making comparative benchmarks, not in real-world use.
#ifdef COLLECT_STATISTICS
#define probe(mask,i,retval) ++probes; if (ht->keys[i] == key) { \
      ++ht->hit.n_seeks; ht->hit.n_probes += probes; \
      return retval; }
#else
#define probe(mask,i,retval) if (ht->keys[i] == key) return retval
#endif

/* Test for membership in a hashset. Return 1 or 0. */
int hopscotch_containsp(tableptr ht, uword_t key)
{
    // index needn't be 'long' but the code generated is better with it.
    unsigned long index = hash(key) & ht->mask;
    unsigned bits = get_hop_mask(ht, index);
#ifdef COLLECT_STATISTICS
    int probes = 0;
#endif
    // *** Use care when modifying this code, and benchmark it thoroughly! ***
    // TODO: use XMM register to test 2 keys at once if properly aligned.
    if (bits & 0xff) {
        probe((1<<0), index+0, 1);
        probe((1<<1), index+1, 1);
        probe((1<<2), index+2, 1);
        probe((1<<3), index+3, 1);
        probe((1<<4), index+4, 1);
        probe((1<<5), index+5, 1);
        probe((1<<6), index+6, 1);
        probe((1<<7), index+7, 1);
    }
    // There's a trade-off to be made: checking for fewer bits at a time
    // (such as by "bits & 0x0f") would give finer grain to the set of
    // physical cells tested, but would mean more iterations.
    // It seems like 8 bits at a time is a good number, especially if the
    // hop range is 8, because this general case need never execute.
    while ((bits >>= 8) != 0) {
        index += 8;
        if (bits & 0xff) {
            probe((1<<0), index+0, 1);
            probe((1<<1), index+1, 1);
            probe((1<<2), index+2, 1);
            probe((1<<3), index+3, 1);
            probe((1<<4), index+4, 1);
            probe((1<<5), index+5, 1);
            probe((1<<6), index+6, 1);
            probe((1<<7), index+7, 1);
        }
    }
#ifdef COLLECT_STATISTICS
    ++ht->miss.n_seeks;
    ht->miss.n_probes += probes;
#endif
    return 0;
}

/* Return the value associated with 'key', or -1 if not found */
int hopscotch_get(tableptr ht, uword_t key)
{
    int index = hash(key) & ht->mask;
    unsigned bits = get_hop_mask(ht, index);
#ifdef COLLECT_STATISTICS
    int probes = 0;
#endif
    // This is not as blazingly fast as the hand-unrolled loop
    // in containsp(), but the GC does not need it, so ...
    while (bits) {
        if (bits & 0xf) {
            probe(1, index+0, ht->values[index+0]);
            probe(2, index+1, ht->values[index+1]);
            probe(4, index+2, ht->values[index+2]);
            probe(8, index+3, ht->values[index+3]);
        }
        index += 4;
        bits >>= 4;
    }
#ifdef COLLECT_STATISTICS
    ++ht->miss.n_seeks;
    ht->miss.n_probes += probes;
#endif
    return -1;
}

#undef probe

#if 0
int popcount(unsigned x)
{
  int count = 0;
  for ( ; x != 0 ; x >>= 1 )
    if (x&1) ++count;
  return count;
}

/* Perform a bunch of sanity checks on 'ht' */
void hopscotch_integrity_check(tableptr ht, char*when, int verbose)
{
  int n_items = 0, tot_bits_set = 0, i;
  int size = table_size(*ht);
  int n_kv_pairs = size + ht->hop_range-1;
  int fail = 0;
  FILE * s = stderr;

  for(i=n_kv_pairs-1 ; i >= 0 ; --i) if (ht->keys[i]) ++n_items;
  for(i=ht->mask; i>= 0; --i) tot_bits_set += popcount(get_hop_mask(ht,i));
  if (verbose)
    fprintf(s, "(%s) Verifying table @ %p. count=%d actual=%d bits=%d\n",
            when, ht, ht->count, n_items, tot_bits_set);
  for (i=0;i<n_kv_pairs;++i) {
    uword_t key = ht->keys[i];
    int claimed;
    if (key != 0 || (i<=ht->mask && get_hop_mask(ht,i) != 0)) {
      // Compute the logical cell that owns this physical cell.
      int start_index = i - (ht->hop_range-1);
      if (start_index < 0) start_index = 0;
      int end_index = i;
      if (end_index > ht->mask) end_index = ht->mask;
      claimed = -1;
      int logical_cell;
      for (logical_cell = start_index ; logical_cell <= end_index ; ++logical_cell) {
        unsigned hop_bits = get_hop_mask(ht, logical_cell);
        if (hop_bits & (1<<(i - logical_cell))) {
          if (claimed == -1)
            claimed = logical_cell;
          else {
            fprintf(stderr,
                    "physical cell %d duplicately claimed: %d and %d",
                   i, claimed, logical_cell);
            fail = 1;
          }
        }
      }
      if (verbose) {
          if (claimed==i || (claimed==-1 && !key))
            fprintf(s, "      ");
          else if (claimed!=-1) {
            fprintf(s, "[%4d]", claimed);
            if ((int)(ht->mask & hash(key)) != claimed)
              lose("key hashes to wrong logical cell?");
          } else { // should have been claimed
            fprintf(s, " **** ");
            fail = 1;
          }
          fprintf(s, " %4d: %04x", i, i <= ht->mask ? get_hop_mask(ht,i) : 0);
          if (key)
            fprintf(s, " %12p -> %d",
                    (void*)(key<<(1+WORD_SHIFT)),
                    (int)(ht->mask & hash(key)));
          putc('\n', s);
      }
    }
  }
  if (ht->count != n_items || tot_bits_set != n_items || fail)
    lose("integrity check on hashtable %p failed", ht);
  fflush(s);
}
#endif
