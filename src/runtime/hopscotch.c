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

#include "os.h"
#include "gc-internal.h" // for sizetab[] and os_allocate()
#include "hopscotch.h"
#include <stdint.h>
#include <stdio.h>
#ifdef LISP_FEATURE_WIN32
/* I don't know where ffs() is prototyped */
extern int ffs(int);
#else
/* https://www.freebsd.org/cgi/man.cgi?query=fls&sektion=3&manpath=FreeBSD+7.1-RELEASE
   says strings.h */
#include <strings.h>
#endif
#include "genesis/vector.h"
#include "murmur_hash.h"

#define hopscotch_allocate(nbytes) os_allocate(nbytes)
#define hopscotch_deallocate(addr,length) os_deallocate(addr, length)

typedef struct hopscotch_table* tableptr;
void hopscotch_integrity_check(tableptr,char*,int);

#define table_size(table) (1+(table).mask)
#define INTEGRITY_CHECK(when) {}

/// By default, XOR values based on the page number and the
/// relative index into the page, disregarding lowtag bits.
/// If a specific function has been set, then use that.
static inline uint32_t hash(tableptr ht, lispobj x) {
    return ht->hash ? ht->hash(x) :
#ifdef LISP_FEATURE_GENCGC
      (x >> GENCGC_CARD_SHIFT) ^ (x >> (1+WORD_SHIFT));
#else
      (x >> (1+WORD_SHIFT));
#endif
}

/// From https://github.com/google/cityhash/blob/master/src/city.cc
/// which attributes it in turn to
/// https://github.com/PeterScott/murmur3/blob/master/murmur3.c
/// The right shift by N_LOWTAG_BITS is specific to SBCL.
uint32_t hopscotch_hmix(uword_t key)
{
  uint32_t h = key >> N_LOWTAG_BITS;
  h ^= h >> 16;
  h *= 0x85ebca6b;
  h ^= h >> 13;
  h *= 0xc2b2ae35;
  h ^= h >> 16;
  return h;
}

/// Set a single bit in the hop mask for logical cell at 'index'
static inline void set_hop_bit(tableptr ht, unsigned index, int bit)
{
    unsigned mask = 1U<<bit;
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
static void set_val(tableptr ht, int index, sword_t val)
{
    switch(ht->value_size) {
#ifdef LISP_FEATURE_64_BIT
    case 8: ht->values[index] = val; break;
#endif
    case 4: ((int32_t*)ht->values)[index] = val; break;
    case 2: ((int16_t*)ht->values)[index] = val; break;
    case 1: ((int8_t* )ht->values)[index] = val; break;
    }
}
static sword_t get_val(tableptr ht, int index)
{
    switch(ht->value_size) {
#ifdef LISP_FEATURE_64_BIT
    case 8: return ht->values[index];
#endif
    case 4: return ((int32_t*)ht->values)[index];
    case 2: return ((int16_t*)ht->values)[index];
    case 1: return ((int8_t *)ht->values)[index];
    }
    // For a hashset, return the found index + 1
    // so that 0 can mean "not found"
    return index + 1;
}
#ifdef LISP_FEATURE_64_BIT
static sword_t get_val8(tableptr ht, int index) { return ht->values[index]; }
#endif
static sword_t get_val4(tableptr ht, int index) { return ((int32_t*)ht->values)[index]; }
static sword_t get_val2(tableptr ht, int index) { return ((int16_t*)ht->values)[index]; }
static sword_t get_val1(tableptr ht, int index) { return ((int8_t *)ht->values)[index]; }

#ifdef LISP_FEATURE_SB_SAFEPOINT

// We can safely use malloc + free because there should be no
// problem of holding a malloc lock from another thread.
#include <stdlib.h>
#define cached_allocate(n) calloc(1,n)
#define cached_deallocate(ptr,size) free(ptr)
void hopscotch_init() { }

#else

/// We can't safely use malloc because the stop-for-GC signal might be received
/// in the midst of a malloc while holding a global malloc lock.

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
    // Prefill the cache with 2 entries, each the size of a kernel page.
    int n_bytes_per_slice = os_reported_page_size;
    int n_bytes_total = N_CACHED_ALLOCS * n_bytes_per_slice;
    char* mem = hopscotch_allocate(n_bytes_total);
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
    nbytes = ALIGN_UP(nbytes, hh_allocation_granularity);
    char* result = hopscotch_allocate(nbytes);
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
static void cached_deallocate(char* mem, uword_t zero_fill_length)
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
            hopscotch_deallocate(mem - ALLOCATION_OVERHEAD,
                                 usable_size(mem) + ALLOCATION_OVERHEAD);
            return;
        }
        // Evict and replace the smaller of the two cache entries.
        if (cached_size1 < cached_size0)
            line = 1;
        hopscotch_deallocate(cached_alloc[line] - ALLOCATION_OVERHEAD,
                             usable_size(cached_alloc[line]) + ALLOCATION_OVERHEAD);
    }
    memset(mem, 0, zero_fill_length);
    cached_alloc[line] = mem;
}
#endif

/* Initialize 'ht' for 'size' logical bins with a max hop of 'hop_range'.
 * 'valuesp' makes a hash-map if true; a hash-set if false.
 * Hop range will be selected automatically if specified as 0.
 */
static void hopscotch_realloc(tableptr ht, int size, char hop_range)
{
    // Somewhat arbitrary criteria that improve the worst-case probes for
    // small hashtables. The reference algorithm uses a fixed max hop of 32,
    // but fewer is better, and our pinned object table tends to be very small.
    if (hop_range == 0) { // Let us pick.
        // The arbitrary cutoff of 1023 is based on the observed final size
        // of 2063 (=2048+15) with commonly fewer than 80 items.
        // Collisions must have been so frequent that the only way out
        // was to bump the table size and rehash. We may as well allow more
        // probes at smaller sizes for the sake of improved table density.
        if      (size <=   1023) hop_range =  8;
        else if (size <=  16384) hop_range = 16;
        else                     hop_range = 32;
    }

    // The key/value arrays are *not* circular.
    // The last few logical cells in the key array can use physical cells
    // at indices greater than 'size'; there's no wrapping back to index 0.
    int n_keys = size + (hop_range - 1);
    uword_t storage_size = (sizeof (uword_t) + ht->value_size) * n_keys
        + sizeof (int) * size; // hop bitmasks

    if (ht->keys) {
#ifndef LISP_FEATURE_SB_SAFEPOINT
        // the usable size is a private-but-visible aspect of the memory block
        // if not using malloc(). But with malloc we can't really ask the question.
        gc_assert(usable_size(ht->keys) >= storage_size);
#endif
    } else
        ht->keys = (uword_t*)cached_allocate(storage_size);

    ht->mem_size  = storage_size;
    ht->mask      = size - 1;
    ht->hop_range = hop_range;
    ht->threshold = n_keys * 13 / 16; // target load ~= 81.25%
    ht->hops      = (unsigned*)(ht->keys + n_keys);
    // Values will be properly aligned no matter what,
    // because the largest alignment we'd need is 8 bytes,
    // which is twice the alignment of a hop entry,
    // and 'size' is an even number.
    ht->values    = ht->value_size ? (sword_t*)(ht->hops + size) : 0;
}

/// Same as SB-KERNEL:%SXHASH-SIMPLE-STRING
uword_t sxhash_simple_string(struct vector* string)
{
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
    unsigned int* char_string = (unsigned int*)(string->data);
#endif
    unsigned char* base_string = (unsigned char*)(string->data);
    sword_t len = vector_len(string);
    uword_t result = 0;
    sword_t i;
    switch (widetag_of(&string->header)) {
#define MIX(ch) {result += ch; result += result<<10; result ^= (result>>6);}
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
    case SIMPLE_CHARACTER_STRING_WIDETAG:
        for(i=0;i<len;++i) MIX(char_string[i])
        break;
#endif
    case SIMPLE_BASE_STRING_WIDETAG:
        for(i=0;i<len;++i) MIX(base_string[i])
        break;
    }
    result += result << 3;
    result ^= result >> 11;
    result ^= result << 15;
    result &= (~(uword_t)0) >> (1+N_FIXNUM_TAG_BITS);
    return result;
}

// This works on vector-like objects, which includes most numerics.
static uword_t vector_sxhash(lispobj* object)
{
    lispobj header = *object;
    int widetag = header_widetag(header);
    sword_t nwords = sizetab[widetag](object);
    // Mix words. At present this works correctly only for specialized vectors,
    // general vectors with eq-comparable elements,
    // and numbers that do not contain pointers.
    return gpr_murmur_hash3(object+1,
                            (nwords-1) * N_WORD_BYTES,  // exclude header word
                            // Ignore vector header data bits except for the widetag
                            widetag >= SIMPLE_ARRAY_WIDETAG
                              ? hopscotch_hmix(widetag)
                              : hopscotch_hmix(header & 0xFFFFFFFF));
}

/* Compare vectors elementwise for EQLness.
 * This is unlike EQUAL because it works on any specialized vector,
 * and unlike EQUALP because it compares strings case-sensitively.
 * Note: this works on most numbers too, not just vectors.
 */
static boolean vector_eql(uword_t arg1, uword_t arg2)
{
    if (arg1 == arg2) return 1;
    lispobj* obj1 = (lispobj*)arg1;
    lispobj* obj2 = (lispobj*)arg2;
    lispobj header1 = *obj1;
    if (((header1 ^ *obj2) & WIDETAG_MASK) != 0)
        return 0; // not eql - different type objects

    int widetag1 = header_widetag(header1);
    sword_t nwords = sizetab[widetag1](obj1);
    // OMGWTF! Widetags have been rearranged so many times, I am not sure
    // how to keep this code from regressing.
    // ASSUMPTION: the range of number widetags ends with (COMPLEX DOUBLE-FLOAT)
    if (widetag1 <= COMPLEX_DOUBLE_FLOAT_WIDETAG)
        // All words must match exactly. Start by comparing the length
        // (as encoded in the header) since we don't yet know that obj2
        // occupies the correct number of words.
        return header1 == *obj2
            && !memcmp(obj1 + 1, obj2 + 1, (nwords-1) << WORD_SHIFT);

    // Vector elements must have already been coalesced
    // when comparing simple-vectors for similarity.
    // Note that only vectors marked "shareable" will get here, so no
    // hash-table storage vectors or anything of that nature.
    struct vector *v1 = (void*)obj1;
    struct vector *v2 = (void*)obj2;
    return (vector_len(v1) == vector_len(v2)) // same length vectors
        && !memcmp(v1->data, v2->data,
                   // ASSUMPTION: exactly 2 non-data words
                   (nwords-2) << WORD_SHIFT);
}

/* Initialize 'ht' for first use, which entails zeroing the counters
 * and allocating storage.
 */
void hopscotch_create(tableptr ht, int hashfun,
                      int bytes_per_value, int size, char hop_range)
{
    gc_assert((size & (size-1)) == 0); // ensure power-of-2
    ht->hashfun = hashfun;
    switch (hashfun) {
    case HOPSCOTCH_HASH_FUN_DEFAULT:
      ht->compare = 0; ht->hash = 0; break;
    case HOPSCOTCH_HASH_FUN_MIX:
      ht->compare = 0; ht->hash = hopscotch_hmix; break;
    case HOPSCOTCH_STRING_HASH:
      ht->compare = vector_eql;
      ht->hash = (uint32_t(*)(uword_t))sxhash_simple_string;
      break;
    case HOPSCOTCH_VECTOR_HASH:
      ht->compare = vector_eql;
      ht->hash = (uint32_t(*)(uword_t))vector_sxhash;
      break;

    default: lose("Bad hash function");
    }
    switch (bytes_per_value) {
    case 0: ht->get_value = 0; break; // no value getter
    case 1: ht->get_value = get_val1; break;
    case 2: ht->get_value = get_val2; break;
    case 4: ht->get_value = get_val4; break;
#ifdef LISP_FEATURE_64_BIT
    case 8: ht->get_value = get_val8; break;
#endif
    default: lose("Bad value size");
    }
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
    ht->value_size = bytes_per_value;
    ht->keys = 0; // Forces allocation of backing storage.
    hopscotch_realloc(ht, size, hop_range);
}

#define need_to_zero(h) (h->count!=0)

/* Delete the storage associated with 'ht' */
void hopscotch_destroy(tableptr ht)
{
    if (ht->mem_size) { // Free it, zero-filling if ever used.
        cached_deallocate((char*)ht->keys, need_to_zero(ht) ? ht->mem_size : 0);
        ht->keys   = 0;
        ht->hops   = 0;
        ht->values = 0;
    }
}

/* Prepare 'ht' for re-use. Same as CLRHASH */
void hopscotch_reset(tableptr ht)
{
    if (need_to_zero(ht)) {
        memset(ht->keys, 0, ht->mem_size);
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
        hopscotch_realloc(ht, size >> 1, 0);
    ht->prev_size = size;
    ht->resized = 0;
    // Possibly reset the hash function to the fast-but-dumb one
    if (ht->hashfun == HOPSCOTCH_HASH_FUN_DEFAULT)
        ht->hash = 0;
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
        hopscotch_create(&copy, ht->hashfun, ht->value_size, size, 0);
        // Maybe change the hash function if it's the dumb one
        if (copy.hop_range > 16 && copy.hash == 0)
          copy.hash = hopscotch_hmix;
        copy.rehashing = 1; // Causes put() to return 0 on failure
        if (ht->values) {
            for(i=old_max_index ; i >= 0 ; --i)
              if (ht->keys[i])
                if (!hopscotch_insert(&copy, ht->keys[i], get_val(ht,i)))
                  break;
        } else {
            for(i=old_max_index ; i >= 0 ; --i)
              if (ht->keys[i])
                if (!hopscotch_insert(&copy, ht->keys[i], 1)) {
#if 0
                  fprintf(stderr, "resize failed with new size %d, hop_range %d\n",
                          size, copy.hop_range);
#endif
                  break;
                }
        }
    } while (i >= 0 && (hopscotch_destroy(&copy), 1));

    // Zero-fill and release the old storage.
    cached_deallocate((char*)ht->keys, ht->mem_size);

    // Move all of the data pointers from 'copy' into ht.
    // mem_size is passed to bzero() when resetting the table,
    // so definitely be sure to use the new, not the old.
    // And of course _don't_ hopscotch_destroy() copy when done.
    ht->hash      = copy.hash;
    ht->mem_size  = copy.mem_size;
    ht->mask      = copy.mask;
    ht->hop_range = copy.hop_range;
    ht->threshold = copy.threshold;
    ht->keys      = copy.keys;
    ht->hops      = copy.hops;
    ht->values    = copy.values;
    ht->resized   = 1;
    INTEGRITY_CHECK("after resize");
    return ht;
}

void hopscotch_log_stats(tableptr __attribute__((unused)) ht,
                         char __attribute__((unused)) *tablename)
{
#ifdef HOPSCOTCH_INSTRUMENT
    static FILE *hh_logfile;
    if (!hh_logfile)
      hh_logfile = fopen("hash-stats.txt","a");
    fprintf(hh_logfile,
            "[hopscotch]: %s ct=%5d cap=%5d LF=%f seek=%5d+%5d probe/seek=%f+%f (hit+miss)\n",
            tablename, ht->count,
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

#define put_pair(i,k,v) ht->keys[i] = k; if(ht->values) set_val(ht, i, v)

/* Add key/val to 'ht'. 'val' is ignored for a hash-set.
 * Key MUST NOT be present in the table */
int hopscotch_insert(tableptr ht, uword_t key, sword_t val)
{
    // Because a 1 bit in the hops bitmask indicates an occupied cell, there is
    // not a technical requirement to reserve a value as the empty cell marker.
    // However the algorithms currently make a simplifying assumption
    // that a key of 0 means "not found"
    gc_dcheck(key);
    // 'desired_index' is where 'key' logically belongs, but it
    // may physically go in any cell to the right up to (range-1) away.
    int desired_index = hash(ht, key) & ht->mask;
    if (ht->keys[desired_index] == 0) {  // Instant win
        put_pair(desired_index, key, val);
        set_hop_bit(ht, desired_index, 0);
        return ++ht->count;  // Allow rehash threshold to be exceeded
    }
    if (!ht->rehashing && ht->count >= ht->threshold)
        return hopscotch_insert(hopscotch_resize_up(ht), key, val);
    // 'limit' is the inclusive bound on cell indices.
    int limit = hopscotch_max_key_index(*ht);
    int free_index = desired_index;
    int displacement;
    while (ht->keys[++free_index] != 0) // While cell is occupied
        if (free_index == limit)
            return ht->rehashing ? 0 : // fail if rehash table is too small
                hopscotch_insert(hopscotch_resize_up(ht), key, val);

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
            put_pair(free_index, ht->keys[physical_elt], get_val(ht, physical_elt));
            put_pair(physical_elt, 0, 0);
            // This logical bin no longer owns the index where the victim was,
            // but does own the index where it got moved to.
            set_hop_mask(ht, logical_bin, bits ^ (1U<<displacement | 1U<<victim));
            // Now free_index gets smaller, and we try again from the top.
            free_index = physical_elt;
            goto retry;
        }
    }
    // Too many collisions and not enough room to move things around.
    return ht->rehashing ? 0 : hopscotch_insert(hopscotch_resize_up(ht), key, val);
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
#ifdef HOPSCOTCH_INSTRUMENT
#define probe(mask,i,action) ++probes; if (ht->keys[i] == key) { \
    ++ht->hit.n_seeks; ht->hit.n_probes += probes; action; }
#define tally_miss(table,n) ++table->miss.n_seeks; table->miss.n_probes += n
#else
#define probe(mask,i,action) if (ht->keys[i] == key) action;
#define tally_miss(table,n)
#endif

/* Test for membership in a hashset. Return 1 or 0. */
int hopscotch_containsp(tableptr ht, uword_t key)
{
    // index needn't be 'long' but the code generated is better with it.
    unsigned long index = hash(ht, key) & ht->mask;
    unsigned bits = get_hop_mask(ht, index);
    int __attribute__((unused)) probes = 0;

    if (ht->compare) { // Custom comparator
        for ( ; bits ; bits >>= 1, ++index )
            if ((bits & 1) && ht->compare(ht->keys[index], key))
                return 1;
        return 0;
    }
    // *** Use care when modifying this code, and benchmark it thoroughly! ***
    // TODO: use XMM register to test 2 keys at once if properly aligned.
    if (bits & 0xff) {
        probe((1<<0), index+0, return 1);
        probe((1<<1), index+1, return 1);
        probe((1<<2), index+2, return 1);
        probe((1<<3), index+3, return 1);
        probe((1<<4), index+4, return 1);
        probe((1<<5), index+5, return 1);
        probe((1<<6), index+6, return 1);
        probe((1<<7), index+7, return 1);
    }
    // There's a trade-off to be made: checking for fewer bits at a time
    // (such as by "bits & 0x0f") would give finer grain to the set of
    // physical cells tested, but would mean more iterations.
    // It seems like 8 bits at a time is a good number, especially if the
    // hop range is 8, because this general case need never execute.
    while ((bits >>= 8) != 0) {
        index += 8;
        if (bits & 0xff) {
            probe((1<<0), index+0, return 1);
            probe((1<<1), index+1, return 1);
            probe((1<<2), index+2, return 1);
            probe((1<<3), index+3, return 1);
            probe((1<<4), index+4, return 1);
            probe((1<<5), index+5, return 1);
            probe((1<<6), index+6, return 1);
            probe((1<<7), index+7, return 1);
        }
    }
    tally_miss(ht, probes);
    return 0;
}

/* Return the value associated with 'key', or 'notfound' if not found */
sword_t hopscotch_get(tableptr ht, uword_t key, sword_t notfound)
{
    gc_dcheck(key);
    int index = hash(ht, key) & ht->mask;
    unsigned bits = get_hop_mask(ht, index);
    int __attribute__((unused)) probes = 0;
    // This is not as blazingly fast as the hand-unrolled loop
    // in containsp(), but the GC does not need it, so ...
    if (ht->compare) // Custom comparator
        for ( ; bits ; bits >>= 1, ++index ) {
            if ((bits & 1) && ht->compare(ht->keys[index], key))
                goto found0;
        }
    else for ( ; bits ; bits >>= 4, index += 4)
        if (bits & 0xf) {
            probe(1, index+0, goto found0);
            probe(2, index+1, goto found1);
            probe(4, index+2, goto found2);
            probe(8, index+3, goto found3);
        }
    tally_miss(ht, probes);
    return notfound;
found3: ++index;
found2: ++index;
found1: ++index;
found0:
    return get_val(ht, index);
}

/* Return the address of the value associated with 'key',
   insert 'key' with value 0 if it was not found. */
void* hopscotch_get_ref(tableptr ht, uword_t key)
{
    gc_dcheck(key);
    int index = hash(ht, key) & ht->mask;
    unsigned bits = get_hop_mask(ht, index);
    int __attribute__((unused)) probes = 0;
    if (ht->compare) // Custom comparator
        for ( ; bits ; bits >>= 1, ++index ) {
            if ((bits & 1) && ht->compare(ht->keys[index], key))
                goto found0;
        }
    else for ( ; bits ; bits >>= 4, index += 4)
        if (bits & 0xf) {
            probe(1, index+0, goto found0);
            probe(2, index+1, goto found1);
            probe(4, index+2, goto found2);
            probe(8, index+3, goto found3);
        }
    tally_miss(ht, probes);
    hopscotch_insert(ht, key, 0);
    return hopscotch_get_ref(ht, key);
found3: ++index;
found2: ++index;
found1: ++index;
found0:
    switch(ht->value_size) {
#ifdef LISP_FEATURE_64_BIT
    case 8: return (int64_t*)ht->values + index;
#endif
    case 4: return (int32_t*)ht->values + index;
    case 2: return (int16_t*)ht->values + index;
    case 1: return (int8_t *)ht->values + index;
    }
    return 0;
}

/* Update or insert a key/value pair. Return nonzero if
 * the key was inserted, or zero if the key existed. */
int hopscotch_put(tableptr ht, uword_t key, sword_t val)
{
    gc_dcheck(key);
    int index = hash(ht, key) & ht->mask;
    unsigned bits = get_hop_mask(ht, index);
    int __attribute__((unused)) probes = 0;
    // This is not as blazingly fast as the hand-unrolled loop
    // in containsp(), but the GC does not need it, so ...
    if (ht->compare) // Custom comparator
        for ( ; bits ; bits >>= 1, ++index ) {
            if ((bits & 1) && ht->compare(ht->keys[index], key))
                goto found0;
        }
    else for ( ; bits ; bits >>= 4, index += 4 )
        if (bits & 0xf) {
            probe(1, index+0, goto found0);
            probe(2, index+1, goto found1);
            probe(4, index+2, goto found2);
            probe(8, index+3, goto found3);
        }
    tally_miss(ht, probes);
    return hopscotch_insert(ht, key, val);
found3: ++index;
found2: ++index;
found1: ++index;
found0:
    set_val(ht, index, val);
    return 0;
}

#undef probe

boolean hopscotch_delete(tableptr ht, uword_t key)
{
    gc_dcheck(key);
    int logical_index = hash(ht, key) & ht->mask;
    int physical_index = logical_index;
    unsigned bits = get_hop_mask(ht, logical_index);
    // Finding the item to delete is not unrolled
    if (ht->compare) { // Custom comparator
        for ( ; bits ; bits >>= 1, ++physical_index )
            if ((bits & 1) && ht->compare(ht->keys[physical_index], key))
                break;
    } else {
        for ( ; bits ; bits >>= 1, ++physical_index )
            if ((bits & 1) && ht->keys[physical_index] == key)
                break;
    }
    if (!bits)
        return 0;
    ht->keys[physical_index] = 0;
    set_val(ht, physical_index, 0);
    ht->hops[logical_index] ^= (1<<(physical_index - logical_index));
    --ht->count;
    return 1;
}

#if 0
#include <stdio.h>
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
            fprintf(s, "        ");
          else if (claimed!=-1) {
            fprintf(s, "[%6d]", claimed);
            if ((int)(ht->mask & hash(ht, key)) != claimed)
              lose("key hashes to wrong logical cell?");
          } else { // should have been claimed
            fprintf(s, " **** ");
            fail = 1;
          }
          fprintf(s, " %6d: %04x", i, i <= ht->mask ? get_hop_mask(ht,i) : 0);
          if (key) {
            fprintf(s, " %lx -> %d", key, (int)(ht->mask & hash(ht, key)));
            if (ht->values)
              fprintf(s, " {val=%lx}", hopscotch_get(ht, key, -1));
          }
          putc('\n', s);
      }
    }
  }
  if (ht->count != n_items || tot_bits_set != n_items || fail)
    lose("integrity check on hashtable %p failed", ht);
  fflush(s);
}
#endif
