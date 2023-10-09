#define _GNU_SOURCE
#ifdef HAVE_GENESIS_CONFIG
#undef LISP_FEATURE_SBCL // already defined
#include "os.h"
#include "runtime.h"
#include "globals.h"
#include "core.h"
#include "interr.h"

#include "validate.h"
//#include "gc.h"
#include "genesis/gc-tables.h"
#include "code.h"
#include "search.h"
#include "genesis/primitive-objects.h"
#include "genesis/brothertree.h"
#include "murmur_hash.h"
#include "genesis/hash-table.h"
#define GENCGC_IS_PRECISE 0
#include "gencgc-impl.h"

#include "smlsharp.h"
#include "segment.inc"

#include "../tlsf-bsd/tlsf/tlsf.h"
#include <unistd.h>
#define BLOCKSIZE_MAX (1<<12)
//#define BLOCKSIZE_MAX 4096

extern void *alloc_mseg(int,size_t);

#include "genesis/list-node.h"
#include "print.h"

#else
#define LISPOBJ(x) ((lispobj)x)
#include "static-symbols.h"
#endif

#include "align.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "smlsharp.h"
#include "object.h"
#include "heap.h"
#include "genesis/binding.h"

#define MALLOC_OBJECT_OFFSET 32 /* KLUDGE: 4 words */

void* tlsf_control;
char* text_space_start = (char*)0x2500000000;
int text_space_size = 256*1024*1024;

int large_code_subspace_p(char* a) {
    return a >= text_space_start && a < (text_space_start+text_space_size);
}

extern void hexdump_large_object(lispobj, FILE*);

extern int in_bitmapped_subheap(void* addr);
extern lispobj* small_block_from_interior_ptr(char* addr, int* blocksize);

extern uword_t exception_handling_load(uword_t*);

/* In my simple test of COMPILE-FILE, the filter ends up with
 * only 21 bits on. It might be oversized in fact */
#define BLOOMFILTER_NBITS (1<<10) /* 1024 bits */
#define BLOOMFILTER_NBYTES (BLOOMFILTER_NBITS / 8) /* 128 bytes */
static _Atomic(unsigned char) bloomfilter[BLOOMFILTER_NBYTES];

// produce 3 different hashes
static void compute_bloomfilter_hashes(uword_t key, unsigned int hashes[3])
{
#if 1
    // This is the same multiplier as in SB-LOCKLESS::MULTIPLICATIVE-HASH
    // Pavel said it looks redundant to use a multiplicative hash AND the murmur
    lispobj product = 11400714819323198485UL * key;
    uword_t fmix = murmur3_fmix64(product);
#else
    uword_t fmix = murmur3_fmix64(key);
#endif
    /* Taking 3 nonoverlapping 10-bit slices is like using different hash functions
     * since murmur_hash output bits should be uncorrelated */
    hashes[0] = fmix & 0x3FF;
    fmix >>= 10;
    hashes[1] = fmix & 0x3FF;
    fmix >>= 10;
    hashes[2] = fmix & 0x3FF;
}
_Atomic(int) bloomfilter_on_bits;
static inline void set_filter_bit(_Atomic(unsigned char)* bits, unsigned int index) {
    unsigned char old;
    unsigned char bit = 1 << (index & 7);
    old = atomic_fetch_or(&bits[index / 8], bit);
    if (!(old & bit)) {
      int foo = fetch_add(relaxed, &bloomfilter_on_bits, 1);
      TPRINTF(0, "Bloom filter has %d bits on", foo + 1);
    }
}
void bloomfilter_insert(_Atomic(unsigned char)* bits, uword_t key)
{
    unsigned int h[3];
    compute_bloomfilter_hashes(key, h);
    set_filter_bit(bits, h[0]);
    set_filter_bit(bits, h[1]);
    set_filter_bit(bits, h[2]);
}
int bloomfilter_containsp(_Atomic(unsigned char)* bits, uword_t key)
{
    // The result of 'logbitp' is only the least-significant bit.
    // It has to be masked to get the final answer
#define logbitp(i) (bits[i / 8] >> (i & 7))
    unsigned int h[3];
    compute_bloomfilter_hashes(key, h);
    return logbitp(h[0]) & logbitp(h[1]) & logbitp(h[2]) & 1;
#undef bitp
}

int ignorable_space_p(uword_t ptr) {
    if ((STATIC_SPACE_START <= ptr && ptr < (lispobj)static_space_free_pointer) ||
        (READ_ONLY_SPACE_START <= ptr && ptr < (lispobj)read_only_space_free_pointer) ||
        (DYNAMIC_SPACE_START <= ptr && ptr < (lispobj)dynamic_space_highwatermark()))
      return 1; // yes, this pointer is ignorable
    return 0; // not ignorable
}

/* A "superpage" is a semi-arbitrarily chosen quantum of allocation which we use
 * to detect plausible addresses in conservative stack scan. Nearby objects on
 * the same superpage will use the same filter bits.
 * The reasoning behind quantizing is as follows: we're not trying to get a 100% accurate
 * idea of whether a large object passes the filter, because two objects on the same MMU
 * page are either both readable or both not, and the filter can always have false positives,
 * so ultimately we have to discriminate by other criteria anyway.
 * The filter just helps avoid an excessive number of SIGSEGVs.
 * It's less of a problem in interesting_taggedptr_p because in that case,
 * the argument was at some time in recent past a legal pointer.
 * Several things can render pointers invalid though:
 * - a malloc'ed segment that was freed and eagerly returned to the OS.
 * - a heap object pointing to a dynamic-extent object where the heap object is live
 *   (possibly because it was allocated black) but the thread stack it references
 *   was already deallocated
 * Either of those scenarios is possible, as long as you don't read through
 * an inaccessible object. */
#define SUPERPAGE_SHIFT 21

#undef MALLOC_LOGGING
#ifdef MALLOC_LOGGING
#include <fcntl.h>
int malloc_logfd = -1;
#endif

pthread_mutex_t tlsf_mutex = PTHREAD_MUTEX_INITIALIZER;

void* alloc_mseg_impl(int executable, size_t nbytes)
{
    void* mseg;
    uword_t* userobj;
    lispobj* voucher = sml_alloc(N_WORD_BYTES); // this could induce a phase change to sync2
    if (executable) {
        /* The size given to the TLSF allocator has to be an odd numbef of words
         * so that the total size consumed including the 1-word block header is even,
         * thereby preserving dualword alignment */
        size_t request = MALLOC_OBJECT_OFFSET + nbytes + N_WORD_BYTES;
        mutex_lock(&tlsf_mutex);
        char* memory = tlsf_malloc(tlsf_control, request);
        mutex_unlock(&tlsf_mutex);
        mseg = memory + N_WORD_BYTES;
        gc_assert((LOWTAG_MASK & (uword_t)mseg) == 0);
        memset(memory, 0, request);
        userobj = (uword_t*)((char*)mseg + MALLOC_OBJECT_OFFSET);
    } else {
#if 1
        mseg = calloc(1, MALLOC_OBJECT_OFFSET + nbytes);
        //mseg = xmalloc(MALLOC_OBJECT_OFFSET + nbytes, "mseg"); memset(mseg, 0, MALLOC_OBJECT_OFFSET + nbytes);
        gc_assert((LOWTAG_MASK & (uword_t)mseg) == 0);
#else
        size_t request = ALIGN_UP(MALLOC_OBJECT_OFFSET + nbytes, 4096);
        mseg = mmap(other_mmap_freeptr, request,
                    PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, 0, 0);
        assert((char*)mseg == other_mmap_freeptr);
        other_mmap_freeptr += request;
#endif
        userobj = (uword_t*)((char*)mseg + MALLOC_OBJECT_OFFSET);
        unsigned int superpage = ((uword_t)userobj - N_WORD_BYTES) >> SUPERPAGE_SHIFT;
        bloomfilter_insert(bloomfilter, superpage);
        gc_assert(bloomfilter_containsp(bloomfilter, superpage));
    }
    memset(mseg, 0x55, MALLOC_OBJECT_OFFSET);
    /* No barrier needed. This is a new object. Also no barrier on storing
     * into the voucher since it does not strongly reference the mseg */
    userobj[-1] = (uword_t)voucher;
    *voucher = (uword_t)userobj;
#ifdef MALLOC_LOGGING
    char buf[100];
    int n = snprintf(buf, sizeof buf, "a %ld%s @ %p\n",
                     nbytes, executable?" exe":"", mseg);
    write(malloc_logfd, buf, n);
#endif
    return mseg;
}

// This is called by Lisp. Don't remove it !
void delete_code_mseg(void* mseg) {
    // TLSF gave us an odd word-address, so 1 word was added. Subtract it before freeing.
    TPRINTF(1, "TLSF freeing mseg %p", mseg);
    char* original_addr = (char*)mseg - N_WORD_BYTES;
    mutex_lock(&tlsf_mutex);
    tlsf_free(tlsf_control, original_addr);
    mutex_unlock(&tlsf_mutex);
}

_Atomic(int) otherptr_mseg_calls,
    mseg_rej_static,
    mseg_rej_stack,
    mseg_rej_filter,
    mseg_rej_memfault,
    mseg_rej_unknown;

/* Cautiously determine whether 'obj' belongs to a malloc segment and
 * return the segment if so. 'obj' will be a properly tagged pointer,
 * but it is unknown whether it points outside of the space
 * managed by the collector (for example stack, arena, lisp string
 * in foreign memory)
 * This is trickier than would appear at first glance.
 * Suppose we have a DX object in the car of a heap cons, but the thread
 * whose stack it was on exited. Performing a barriered RPLACA on the cons should
 * never crash, but figuring out if 'oldval' is a managed object
 * requires that we analyze the address that formerly belonged to a thread */
void* otherptr_mseg(lispobj taggedptr) {
    fetch_add(relaxed, &otherptr_mseg_calls, 1);
    assert(!large_code_subspace_p((char*)taggedptr));
    if ((taggedptr == (uword_t)-1) || ignorable_space_p(taggedptr)) {
        fetch_add(relaxed, &mseg_rej_static, 1);
        return NULL;
    }
    struct thread* th = get_sb_vm_thread();
    if ((th != 0 && (taggedptr >= (lispobj)th->control_stack_start &&
                     taggedptr < (lispobj)th->control_stack_end))) {
        fetch_add(relaxed, &mseg_rej_stack, 1);
        return NULL;
    }
    lispobj* base_address = native_pointer(taggedptr);
    lispobj* pvoucher = base_address - 1;
    unsigned int superpage = ((uword_t)pvoucher) >> SUPERPAGE_SHIFT;
    /* The cost of a SIGSEGV is higher than the cost of a little extra filtering */
    if (!bloomfilter_containsp(bloomfilter, superpage)) {
        fetch_add(relaxed, &mseg_rej_filter, 1);
        return NULL;
    }
    // There's a good chance - but no guarantee - that this address is readable
    lispobj voucher = exception_handling_load(pvoucher);
    // The exception handler if invoked will leave 'voucher' set to 0
    if ((voucher & 7) == 0) { // aligned to a lispword
        uword_t* block;
        /* small_block_from_interior_ptr does not store to 'blocksize'
         * unless the block is in a subheap */
        int blocksize = 0;
        block = small_block_from_interior_ptr((char*)voucher, &blocksize);
        if (blocksize == N_WORD_BYTES && *block == (lispobj)base_address) {
            unsigned char ATTR_UNUSED widetag = widetag_of(base_address);
            gc_assert((widetag >= SIMPLE_VECTOR_WIDETAG &&
                       widetag <= SIMPLE_CHARACTER_STRING_WIDETAG)
                      || widetag == WEAK_POINTER_WIDETAG
                      || widetag == BIGNUM_WIDETAG
                      || *base_address == 1); // forwarded
            return (void*)((uword_t)base_address - MALLOC_OBJECT_OFFSET);
        }
    }
    fetch_add(relaxed, &mseg_rej_unknown, 1);
    return NULL;
}

static lispobj* large_codeblob_from_interior_pointer(lispobj ptr)
{
#if 0
  struct thread* thread = get_sb_vm_thread();
  if (thread) {
    // user threads can only look up codeblobs in ASYNC phase if they ensure that GC
    // is not about to free the object.
    if (thread->gc_phase == ASYNC &&) {
      lose("can't lookup codeblob in async phase");
    }
  }
#endif
    lispobj codetree = SYMBOL(IMMOBILE_CODEBLOB_TREE)->value;
    if (codetree == NIL) return 0;
    lispobj node = brothertree_find_lesseql(ptr, codetree);
    if (node == NIL) return 0;
    uword_t key = ((struct binary_node*)INSTANCE(node))->uw_key;
    int widetag = widetag_of((lispobj*)key);
    if (widetag == CODE_HEADER_WIDETAG) {
        /* FIXME: to be pedantic we want to require that either 'ptr' be properly
         * tagged for a SIMPLE-FUN or the the code, OR it is to instruction bytes.
         * Just the bounds check is good enough for now. */
        struct code *code = (void*)key;
        if (ptr == key ||
            (ptr > key + code_boxed_len(code) && ptr < key + (code_total_nwords(code)<<WORD_SHIFT))
            || ptr == make_lispobj((lispobj*)code, OTHER_POINTER_LOWTAG))
            return (lispobj*)code;
    } else {
        // We can't assert anything. 'ptr' is ambiguous so it could be looking
        // at old garbage
    }
    return 0;
}
uword_t* codeblob_from_interior_ptr(void* addr) {
    lispobj* object_base = 0;
    /* Checks:
     * 1. is it in smallobj subheap?
     * 2. is it code?
     * 3. is 'addr' in bounds?
     */
    if ((object_base = small_block_from_interior_ptr(addr, 0)) != 0
        && widetag_of(object_base) == CODE_HEADER_WIDETAG
        && addr < (void*)(object_base + object_size(object_base)))
        return object_base;
    if ((uword_t)addr >= asm_routines_start && (uword_t)addr < asm_routines_end) {
        TPRINTF(1, "%p is in lisp ASM code", addr);
        return (uword_t*)asm_routines_start;
    }
    return large_codeblob_from_interior_pointer((uword_t)addr);
}

extern uword_t* reject_ptr(void* a, char *kind, char* reason);

/* Return true if 'thing' is a pointer that the CMS collector cares about.
 * It is guaranteed that 'thing' is a properly-tagged pointer.
 * However, with code objects, 'thing' might be a interior (SIMPLE-FUN) pointer,
 * and in general 'thing' could point to an exited or overritten stack frame,
 * which might mean that the bits currently there are completey random */
int interesting_taggedptr_p(lispobj thing) {
    // quick & easy range check
    if (in_bitmapped_subheap((void*)thing)) return 1; // yes, interesting
    switch (lowtag_of(thing)) {
    case FUN_POINTER_LOWTAG:
        return large_code_subspace_p((char*)thing);
    case OTHER_POINTER_LOWTAG:
        // could be a tagged pointer to the codeblob
        return large_code_subspace_p((char*)thing) || otherptr_mseg(thing) != 0;
    default:
        if (ignorable_space_p(thing)) return 0; // don't log this
        return reject_ptr((void*)thing,"exact","tag"), 0; // why though?
    }
}
int in_sml_heap_range_p(void* addr) {
    return in_bitmapped_subheap(addr)
        || large_code_subspace_p(addr)
        || otherptr_mseg(make_lispobj(addr, 0));
}

/* Return true if 'descriptor' is a pointer that the CMS collector
 * needs to care about. Just reject immediate objects
 * (fixnum, character, single-float, unbound-marker) and then punt */
static inline int interesting_word_p(lispobj descriptor) {
    return is_lisp_pointer(descriptor) && interesting_taggedptr_p(descriptor);
}

/* If 'word' can be considered a pointer, then return the object to which
 * it points. This is fairly easy as most objects can only be pointed to by
 * a tagged pointer, and are in a small-object subheap. So really the only
 * complication is for cases where we allow interior pointers (program counters)
 * or untagged pointers (lockfree node next) */
uword_t* obj_from_ambiguous_ptr(lispobj word /*, int in_register*/) {
    if (ignorable_space_p(word)) return 0;
    void* addr = (void*)word;
    uword_t* object_base = 0;
    int blocksize = 0;

    if (in_bitmapped_subheap(addr)) {
      if ((object_base = small_block_from_interior_ptr(addr, &blocksize)) == 0)
        return reject_ptr(addr, "ambiguous-small", "");
      if (blocksize == N_WORD_BYTES) return reject_ptr(addr, "ambiguous", "1-word object");
    } else if (large_code_subspace_p((char*)word)) {
        object_base = large_codeblob_from_interior_pointer((lispobj)addr);
        if (object_base) return object_base;
        return reject_ptr(addr, "ambiguous-large-code", "not found");
    } else if (lowtag_of(word) == OTHER_POINTER_LOWTAG &&
               otherptr_mseg(word)!= NULL) {
        object_base = native_pointer(word);
    } else {
        return NULL;
    }
    // check a few more things to make sure the reference is legal
    lispobj header = *object_base;
    unsigned char widetag = header_widetag(header);
    if (blocksize == 16) { // cons or 2-word headered object
        /* Consider when allocating a list, we make the first cons and store its car but not the
         * cdr yet. The default fill pattern on 16-byte blocks is 0xff so it has to be allowed */
        // if (is_cons_half(header) && object_base[1] == (uword_t)-1) lose("can't happen");
        if (header == (uword_t)-1) return reject_ptr(addr, "ambiguous", "uninitialized");
        if (header == DEADBEEF) return reject_ptr(addr, "ambiguous", "killed");
        assert(widetag != FILLER_WIDETAG);
        if (is_cons_half(header)) {
            int lowtag = lowtag_of((uword_t)addr);
            // disallow untagged list pointers
            return lowtag == LIST_POINTER_LOWTAG ? object_base : 0;
        }
    } else {
        if (!header || header == (uword_t)-1 || header == DEADBEEF || widetag == FILLER_WIDETAG)
          return reject_ptr(addr, "ambiguous", "bad-header");
    }
    if (((uword_t)object_base | LOWTAG_FOR_WIDETAG(widetag)) == (uword_t)addr)
        return object_base;
    // Special-case a few primitive objec types
    switch (widetag) {
    case CODE_HEADER_WIDETAG:
        /* Any pointer to interior of code is OK, though technically this could reject
         * untagged simple-fun pointers. (see also large_codeblob_from_interior_pointer
         * which makes the same observation) */
        {
        struct code* code = (void*)object_base;
        char* text = (char*)code + code_boxed_len(code);
        if (addr > (void*)text && addr < (void*)(object_base + code_total_nwords(code)))
            return object_base;
        else
            return reject_ptr(addr, "ambiguous-code", "bounds");
        }
    case INSTANCE_WIDETAG:
        {
        lispobj layout = layout_of(object_base);
        if (layout != 0 && lockfree_list_node_layout_p(LAYOUT(layout))
            && (uword_t)addr == (uword_t)object_base) {
            // TODO: allow this only if the value is in a register,
            // (and not just because it was spilled to stack)
            return object_base;
        }
        }
        break;
    case FUNCALLABLE_INSTANCE_WIDETAG: // allow any ambiguous pointer for now
        // TODO: tighten this up to allow only a tagged pointer
        // or a pointer to an instruction in the embedded trampoline
        return object_base;
    }
    reject_ptr(addr, "ambiguous","tag");
    return 0;
}

// For MAKE-LISP-OBJ to check its argument. It is unknwn whether 'candidate' is valid.
// Interior pointers are not allowed.
int smlgc_valid_tagged_pointer_p(lispobj candidate) {
    lispobj* base = obj_from_ambiguous_ptr(candidate);
    if (!base) return 0;
    lispobj tagged = compute_lispobj(base);
    if (tagged == candidate) return 1; // good
    if (widetag_of(base) != CODE_HEADER_WIDETAG) return 0;
    // See if pointing to any of the embedded functions
    for_each_simple_fun(i, f, ((struct code*)base), 1, {
        if (make_lispobj(f, FUN_POINTER_LOWTAG) == candidate) return 1;
    })
    return 0;
}

#define points_to_stack(w) (w >= (uword_t)hot_end && w < (uword_t)cold_end)

#if 0
void* untagged_baseptr(lispobj thing) {
    // no need to read a word at native_pointer(thing) if it isn't fun-pointer tagged
    lispobj* base = native_pointer(thing);
    if (lowtag_of(thing) != FUN_POINTER_LOWTAG) return base;
    return widetag_of(base) == SIMPLE_FUN_WIDETAG ? fun_code_header(base) : base;
}
#endif

static char* classify_ptr(lispobj word, struct thread* th) {
  if (word >= STATIC_SPACE_START && word < (lispobj)static_space_free_pointer) return "Static";
  if (word >= READ_ONLY_SPACE_START && word < (lispobj)read_only_space_free_pointer) return "RO";
  if ((uword_t)th->control_stack_start <= word && word < (uword_t)th->control_stack_end) return "DX";
  if (DYNAMIC_SPACE_START <= word && word < dynamic_space_highwatermark()) return "tenured";
  if (interesting_taggedptr_p(word)) return 0;
  lose("wat? %lx", word);
}
#include "pseudo-atomic.h"

struct sml_user {
    struct frame_stack_range {
        void *bottom, *top;
        struct frame_stack_range *next;
    } *frame_stack;
    void *arbdata;
};

void print_concise(lispobj ptr, FILE* f)
{
  lispobj* this = native_pointer(ptr);
      int widetag_of_this = widetag_of(this);
      putc(' ', f);
      if (widetag_of_this == SYMBOL_WIDETAG
          && widetag_of(native_pointer(decode_symbol_name(((struct symbol*)this)->name)))
          == SIMPLE_BASE_STRING_WIDETAG) {
        struct vector* string = VECTOR(decode_symbol_name(((struct symbol*)this)->name));
        fprintf(f, "%s", (char*)string->data);
      } else if (widetag_of_this == SIMPLE_VECTOR_WIDETAG) {
        fprintf(f, "(SIMPLE-VECTOR %ld)", vector_len((struct vector*)this));
      } else if (widetag_of_this == INSTANCE_WIDETAG) {
        //struct vector* classoid_name =(void*)instance_classoid_name(this);
        fprintf(f, "[instance-of-type %p]", (void*)instance_layout((lispobj*)this));
      } else if (is_cons_half(*this)) {
        fprintf(f, "(%lx . %lx)", this[0], this[1]);
      } else {
        fprintf(f, "%s", widetag_names[widetag_of_this>>2]);
      }
}

void scan_vmthread_stack(struct thread *th, lispobj* hot_end, lispobj* cold_end,
                         void (*trace)(lispobj, void *), void *data)
{
    FILE* f = NULL;
    char* bounds[2];
    get_segment_pool_bounds(bounds);
    TPRINTF(0, "Stack range %p..%p (%ld words) heap=%p:%p", hot_end, cold_end, (lispobj*)cold_end - (lispobj*)hot_end,
            bounds[0], bounds[1]);
    assert((((uintptr_t)hot_end) & 7 ) == 0);

    struct binding* base = (void*)th->binding_stack_start;
    struct binding* bsp  = (void*)get_binding_stack_pointer(th);
    struct binding* binding = base;
    if (f) fprintf(f, "Thread root publication\nBindings: %d entries\n", (int)(bsp - base));
    for ( ; binding < bsp ; ++binding ) {
        lispobj word = binding->value;
        int interesting = word != (uword_t)-1 && is_lisp_pointer(word) && !points_to_stack(word)
                          && interesting_taggedptr_p(word);
        if (f != NULL && word != (uword_t)-1 && word != NIL && word != LISP_T && is_lisp_pointer(word)) {
            char *kind = classify_ptr(word, th);
            char flag = kind ? ' ' : '*';
            fprintf(f, " %c [%3d] %4x = %lx", flag, (int)(binding - base),
                    (int)binding->symbol, word);
            if (kind) fprintf(f, " %s", kind); else print_concise(word, f);
            putc('\n', f);
        }
        if (interesting) trace((lispobj)untagged_baseptr(word), data);
    }

    if (f) fprintf(f, "TLS\n");
    lispobj* from = &th->lisp_thread;
    lispobj* to = (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th);
    lispobj* p;
    for (p = from ; p < to ; ++p) {
        lispobj word = *p;
        int interesting = word != (uword_t)-1 && is_lisp_pointer(word) && !points_to_stack(word)
                          && interesting_taggedptr_p(word);
        if (f != NULL && word != NIL && word != (uword_t)-1 && is_lisp_pointer(word)) {
          char *kind = classify_ptr(word, th);
          char flag = kind ? ' ' : '*';
          fprintf(f, " %c %4x: %lx", flag, (int)((char*)p-(char*)th), word);
          if (kind) fprintf(f, " %s", kind); else print_concise(word, f);
          putc('\n', f);
        }
        if (interesting) trace((lispobj)untagged_baseptr(word), data);
    }

    // Ambiguous root scan
    th->stack_root_scan_start = 1;
    if (f) fprintf(f, "Nonvolatile GPRs\n");
    lispobj gprs[5];
    extern void get_nonvolatile_gprs(lispobj[5]);
    get_nonvolatile_gprs(gprs);
    int i;
    for (i = 0; i < 5; ++i) {
        lispobj word = gprs[i];
        lispobj* obj = obj_from_ambiguous_ptr(word);
        if (f) fprintf(f, " %lx -> %p\n", word, obj);
        if (obj) { trace((lispobj)obj, data); }
    }
    if (f) fprintf(f, "Control stack %p..%p\n", hot_end, cold_end);
    lispobj* sp;
    // print it highest address first so that it resembles the stack in memory
    for (sp = cold_end - 1; sp >= hot_end; --sp) {
        lispobj word = *sp;
        lispobj* obj = obj_from_ambiguous_ptr(word);
        int stackindex = (char*)sp - (char*)hot_end;
        if (large_code_subspace_p((char*)word) && !obj) {
          // fprintf(stderr,"large code not found %lx\n", word);
          //dump_immobile_code_tree();
          //lose("very weird - %lx", word);
        }
        if (obj) trace((lispobj)obj, data);
        if (!f) continue;
        if (!obj) {
          // small numbers are totally uninteresting
          if (word >= DYNAMIC_SPACE_START) fprintf(f, "  @ sp[%5x]: %lx\n", stackindex, word);
        } else {
          fprintf(f, "* @ sp[%5x]: %16lx -> %p =", stackindex, word, obj);
          if (is_cons_half(*obj)) {
            fprintf(f, " (%lx . %lx)\n", obj[0], obj[1]);
          } else {
            print_concise(compute_lispobj(obj), f);
            int small = small_block_from_interior_ptr((char*)obj, 0) != 0;
            int exact = native_pointer(word) == obj;
            fprintf(f, "%s\n",
                    small ? (exact ? "" : " Interior") :
                    (exact ? " Large" : " Large,Interior"));
          }
        }
    }
    th->stack_root_scan_start = 0;

    if (f) fprintf(f, "done with thread roots\n");
}

const int show_stack_scan_realtime = 1;
void lisp_stack_enum_ptr(struct sml_user *user, void (*trace)(lispobj, void *), void *data)
{
    struct timespec t_before, t_after;
    if (show_stack_scan_realtime) clock_gettime(CLOCK_MONOTONIC, &t_before);

    struct thread* th = get_sb_vm_thread();
    /*TPRINTF(0, "lisp_stack_enum_ptr called with user %p, sb_vm_th=%p, user_arbdata=%p", user,
            th, get_sml_user_arbdata(user));*/
    lispobj *hot_end, *cold_end; /* https://garethrees.org/2018/03/12/stack/ */
    if (th) { // lisp thread was asked to publish roots
        TPRINTF(0, "stack scan: pseudoatomic=%d", get_pseudo_atomic_atomic(th));
        hot_end = (lispobj*)&data;
    } else {
        th = user->arbdata; // get_sml_user_arbdata(user);
        hot_end = user->frame_stack->top;
    }
    cold_end = th->control_stack_end;
    scan_vmthread_stack(th, hot_end, cold_end, trace, data);

    // This time is negligible, probably at worst 100 microseconds
    if (show_stack_scan_realtime) {
        clock_gettime(CLOCK_MONOTONIC, &t_after);
        long delta_usec = (1000000*(t_after.tv_sec - t_before.tv_sec)) +
          (t_after.tv_nsec - t_before.tv_nsec) / 1000;
        char buf[80];
        // We're in a signal handler, so can't use printf to a stream, but
        // can (probably) use snprintf though technically not allowed.
        int n = snprintf(buf, sizeof buf, "Stack scan time: %ld \u00B5sec\n", delta_usec);
        write(2, buf, n);
    }
}

//int gray_protected(ATTR_UNUSED lispobj* obj) {
//  // If the mutator is gray and the stack directly points to 'obj'
//  // then obj is gray-protected because we haven't scanned the stack yet.
//  return 0;
//}

lispobj* lisp_global_root[10]; // untagged pointers

// For the time being, we have to pass native_pointers to the 'trace' procedure so
// that it can compute the object size so that it can decide whether the object is
// in an ordinary segment versus a malloc_segment.
// It would be more efficient to pass a tagged pointer, certainly for conses,
// because those will obviously be 2 words.

FILE *gc_debug_log;
int log_enumeration;
ATTR_UNUSED static FILE *enumlog;

int n_dynamic_space_roots;

ATTR_UNUSED static int gencgc_dynspace_p(lispobj ptr) {
  return ptr >= DYNAMIC_SPACE_START && ptr < (lispobj)dynamic_space_highwatermark();
}

/* Decide whether to visit a weakly referenced object.
 * Due to the nature of this collector, there are objects
 * which are known live but never got shaded gray (i.e.
 * never had their slots enumerated). We need to take care to
 * enumerate the slots of those objects
 */
int should_visit_weak_referent(void* obj) {
    /* If large and the markbit is off, then the object is white, so do not visit.
     * If the markbit is on, the either:
     *  - it was allocated black (so do not visit)
     *  - it was allocated white, then shaded gray and scheduled for enumeration
     *    so does not need visit.
     * Therefore in no case do we visit a large object
     */
    if (!in_bitmapped_subheap(obj)) return 0;
    /*
    sml_bitptr_t b = BITPTR(COLLECT_BITMAP_BASE(seg), index);
    if (BITPTR_TEST(b)) return 0;
    int livep1 = 0, livep2 = 0;
    b = BITPTR(BITMAP0_BASE(seg), index);
    if ((char*)obj >= seg->snapshot_free && !BITPTRW_TEST(b)) livep1 = 1;
    livep2 = seg->free_count >= 0 || BITPTR_TEST(b);
    assert(livep1 == livep2);
    return livep1;
    */
    return sml_heap_check_alive(obj);
}

extern int seg_blocksize_of(void*);

#if 0
lispobj find_who_points_to;
static const int want_dynspace_pointers = 0;
static void debugit(lispobj* parent, lispobj ptr, int strongp) {
  lispobj* base = native_pointer(ptr);
  unsigned char widetag = *(unsigned char*)base;
  if (LOWTAG_FOR_WIDETAG(widetag) != lowtag_of(ptr)) lose("srsly wtf @ %lx\n", ptr);
  if (find_who_points_to == ptr && strongp) fprintf(stderr, "*** FOUND STRONG REFERER %p -> %p\n", parent, (void*)ptr);
}
#define ENUMERATE(addr) { lispobj thing = *(addr);                      \
    if (interesting_word_p(thing) || (want_dynspace_pointers && dynspace_p(thing))) \
      debugit(obj,thing,1), trace((lispobj)untagged_baseptr(thing), data); }
#else
#define ENUMERATE(addr) { lispobj thing = *(addr);                      \
    if (interesting_word_p(thing)) trace((lispobj)untagged_baseptr(thing), data), ++count; }
#endif

// edges implies by <k,v> pair in some live weak table. Existence of the edge does not
// imply that the edge's target is live.
struct hopscotch_table implied_edges;

int lispobj_enum_ptr(void *obj, void (*trace)(lispobj, void*), void *data)
{
    int count = 0;

    /* If implied_edges is nonempty (so the collector has begun examining weak tables)
     * then see if this object livens anything "remotely" through a table */
    struct cons *edges, *next;
    edges = (void*)(implied_edges.count > 0 ?
                    hopscotch_get(&implied_edges, (uword_t)obj, 0) : 0);
    if (edges) {
      // As long as we don't hit this case, the build succeeds.
        fprintf(stderr, "Hey Now! obj %p in a weaktable livens stuff...\n", (void*)compute_lispobj(obj));
        do {
            // remote is the other half of a cell pair in a weak table
            // where 'obj' is the triggering half of the cell.
            lispobj remote = edges->car; // is untagged_baseptr but cast to 'uword_t'
            fprintf(stderr, " other=%lx=%lx\n", remote, compute_lispobj((lispobj*)remote));
            trace(remote, data);
            ++count;
            next = (void*)edges->cdr;
            free(edges); // delete the edge list as we go
        } while ((edges = next) != NULL);
        hopscotch_delete(&implied_edges, (uword_t)obj);
    }

    lispobj* this = obj;

    lispobj header = *this;
    if (is_cons_half(header)) {
        /* If this is a 1-word object, is is a "voucher" for a malloc'ed segment,
         * and does not enliven the corresponding large object
         * (though it shouldn't matter if it did) */
        if (seg_blocksize_of(obj) == N_WORD_BYTES) lose("Should never enumerate a 1-word object");
        ENUMERATE(this + 0);
        // ONLY a cons can have a -1 in its cdr. Nothing else can have that (invalid) bit pattern
        if (this[1] != (uword_t)-1) ENUMERATE(this + 1);
        return count;
    }
    long first = 1;
    long nwords = object_size2(this, header);
#if 0
    if (nwords > (4096/N_WORD_BYTES)
        && !in_bitmapped_subheap(this) && !ignorable_space_p((uword_t)this)) {
        lispobj voucher = this[-1];
        // voucher must be nonzero and has to be on a page for size N_WORD_BYTES objects
        gc_assert(voucher);
        gc_assert(seg_blocksize_of((void*)voucher) == N_WORD_BYTES);
        trace(voucher, data);
    }
#endif
    unsigned char widetag = header & WIDETAG_MASK;
    lispobj layout;
    switch (widetag) {
    case INSTANCE_WIDETAG:
        layout = instance_layout(this);
        if (!layout) return 0;
        ENUMERATE(&layout);
        struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
        sword_t mask = bitmap.bits[0]; // there's always at least 1 bitmap word
        if (bitmap.nwords == 1 && (uword_t)mask == (uword_t)-1 << INSTANCE_DATA_START)
            break; // For efficiency don't use the bitmap when all boxed
        if (lockfree_list_node_layout_p(LAYOUT(layout))) {
            struct list_node* node = (void*)this;
            // 'next' might be NIL. ORing in the lowtag doesn't change NIL's bits
            lispobj next = node->_node_next |= INSTANCE_POINTER_LOWTAG;
            ENUMERATE(&next);
        }
        int i;
        for (i=INSTANCE_DATA_START; i<(nwords-1); ++i)
            if (bitmap_logbitp(i, bitmap)) ENUMERATE(&this[1+i]);
        return count;
    case SIMPLE_VECTOR_WIDETAG:
        if (vector_flagp(*this, VectorWeak)) {
            assert(vector_flagp(*this, VectorHashing));
#ifdef DISABLE_HASH_TABLE_WEAKNESS
            fprintf(stderr, "weak hash table vector @ %p NOT being treated as weak\n", this); break;
#endif
            long n = vector_len((struct vector*)this);
            lispobj* elements = this + 2;
            long i;
            // Treat each element as weakly pointed to. At this stage we are not
            // concerned with key-livens-value or any of the other possibilities.
            // Elements 0 and 1 can be skipped - they're fixnums.
            for (i = 2; i < n-1; ++i) {
                lispobj elt = elements[i];
                if (interesting_word_p(elt) && should_visit_weak_referent((void*)elt))
                    trace((lispobj)untagged_baseptr(elt), data), ++count;
            }
            // The final element is a (strong) backpointer to the hash-table instance.
            ENUMERATE(elements + n-1);
            return count;
        }
        break;
    case CLOSURE_WIDETAG: {
        lispobj underlying = fun_taggedptr_from_self(this[1]);
        ENUMERATE(&underlying);
        first = 2;
        break;
    }
    case CODE_HEADER_WIDETAG: {
        sword_t nboxed = code_header_words((struct code*)this);
        // Constant pool
        int i;
        for (i=2; i<nboxed; ++i) ENUMERATE(this + i);
        // Simple-funs
        for_each_simple_fun(i, f, ((struct code*)this), 1, {
            lispobj other = (lispobj)fun_code_header((struct simple_fun*)f);
            if (other != (lispobj)this) { fprintf(stderr, "Traced simple-fun? %lx\n", other); ENUMERATE(&other); }
        })
        return count;
    }
    case SYMBOL_WIDETAG: {
        struct symbol* sym = (void*)this;
        ENUMERATE(&sym->value);
        ENUMERATE(&sym->fdefn);
        ENUMERATE(&sym->info);
        lispobj name = decode_symbol_name(sym->name);
        ENUMERATE(&name);
        return count;
    }
    case FDEFN_WIDETAG: {
        struct fdefn* fdefn = (void*)this;
        ENUMERATE(&fdefn->name);
        ENUMERATE(&fdefn->fun);
        lispobj taggedptr = decode_fdefn_rawfun(fdefn);
        ENUMERATE(&taggedptr);
        return count;
    }
    case WEAK_POINTER_WIDETAG: {
        long i;
        for (i = 1; i < nwords; ++i) {
            lispobj elt = this[i];
            if (interesting_word_p(elt) && should_visit_weak_referent((void*)elt))
                trace((lispobj)untagged_baseptr(elt), data), ++count;
        }
        return count;
    }
    default:
        if (leaf_obj_widetag_p(widetag)) return 0;
    }
    // For everything else, scan from 'first' below 'nwords'
    if (nwords) { // when would it ever be 0 ? And is this really important to test for ?
        long i;
        for (i = first; i < nwords; ++i) ENUMERATE(this+i);
    }
    return count;
}
#undef ENUMERATE

struct global_enum_arg {
    void (*trace)(lispobj, void *);
    void *data;
};

ATTR_UNUSED static uword_t
visit_dynamic_space_range(lispobj* where, lispobj* limit, uword_t arg)
{
    void (*trace)(lispobj, void *) = ((struct global_enum_arg*)arg)->trace;
    void* data = ((struct global_enum_arg*)arg)->data;
    lispobj* obj = where;
    while (obj < limit) {
        if (widetag_of(obj) != FILLER_WIDETAG) {
            lispobj_enum_ptr(obj, trace, data);
        }
        obj += object_size(obj);
    }
    return 0;
}

static inline bool boxed_type_p(int type) { return type > 1; }
static inline bool page_boxed_p(page_index_t page) {
    // ignore SINGLE_OBJECT_FLAG and OPEN_REGION_PAGE_FLAG
    return boxed_type_p(page_table[page].type & PAGE_TYPE_MASK);
}

void
lisp_global_enum_ptr(void (*trace)(lispobj, void *), void *data)
{
    // Note: The 'trace' function supplied to this function doesn't trace _into_ each object,
    // it just records them into a list for later processing. Not that it matters.
  TPRINTF(0, "ENTER global_enum_ptr");
    /*lispobj pin_list = SYMBOL(STARTING_THREADS)->value;
    for ( ; pin_list != NIL ; pin_list = CONS(pin_list)->cdr ) {
      lispobj thing = CONS(pin_list)->car;
      if (thing)
        lose("can't global_enum w/ starting thread %lx, sorry", thing);
    }*/
    {
    // This operation can be unsafe if allocation to dynamic space is allowed
    struct thread* th;
    for_each_thread(th) { gc_close_thread_regions(th, 0); }

    // All of dynamic space is a root of SML# GC space
    int n_dirty = 0, n_roots = 0, n_ptrs = 0;
    {
        page_index_t page = 0, last_page;
        while (page < next_free_page) {
            int marked = 0;
            generation_index_t gen = page_table[page].gen;
            for (last_page = page; ; last_page++) {
                long card_index = page_to_card_index(last_page);
                marked = marked || cardseq_any_marked(card_index);
                if (page_ends_contiguous_block_p(last_page, gen)) break;
            }
            if (/*marked &&*/ page_boxed_p(page)) {
                lispobj* where = (lispobj*)page_address(page);
                lispobj* limit = (lispobj*)page_address(last_page) + page_words_used(last_page);
                for ( ; where < limit ; where += object_size(where) ) {
                    ++n_roots;
                    n_ptrs += lispobj_enum_ptr(where, trace, data);
                }
                //printf("Marked range %ld..%ld: %d roots\n", page, last_page, nobjs);
                n_dirty += 1 + (last_page - page);
            }
            page = 1 + last_page;
        }
    }
    fprintf(stderr, "dynamic space: %d/%ld marked cards, %d roots, %d pointers\n",
            n_dirty, next_free_page, n_roots, n_ptrs);

    FILE* f = get_gc_thread_log();
    if (f) fprintf(f, "Roots from dynamic space: %d\n", n_dynamic_space_roots);
    n_dynamic_space_roots = 0;
    }
    {
    // static objects
    lispobj_enum_ptr((lispobj*)NIL_SYMBOL_SLOTS_START, trace, data);
    lispobj* where = (lispobj*)STATIC_SPACE_OBJECTS_START;
    for ( ; where < static_space_free_pointer ; where += object_size(where) ) {
        if (where == (lispobj*)(LFLIST_TAIL_ATOM-INSTANCE_POINTER_LOWTAG)) {
            // Skip this object because its 'next' pointer violates the convention
            // that either it has an instance lowtag or 0 lowtag
        } else {
            lispobj_enum_ptr(where, trace, data);
        }
    }
    TPRINTF(0, "done with static space");
    }
    int i;
    for (i=0; i<NSIG; ++i) {
        lispobj handler = lisp_sig_handlers[i];
        if (interesting_taggedptr_p(handler))
            trace((lispobj)untagged_baseptr(handler), data);
    }

    // random lisp global roots
    for (i=0; i<10; ++i) {
        lispobj ptr = (lispobj)lisp_global_root[i];
        if (ptr) { TPRINTF(0, "global root %lx", ptr); trace(ptr, data); }
    }
    if (interesting_taggedptr_p(lisp_package_vector)) {
        lispobj ptr = lisp_package_vector & ~LOWTAG_MASK;
        trace(ptr, data);
    }
}

lispobj sml_make_list(struct alloc_ptr* ap, lispobj element, unsigned num_conses)
{
    struct cons dummy;
    lispobj last = make_lispobj(&dummy, LIST_POINTER_LOWTAG);
    lose("sml_make_lis: not done");
    do {
        sml_bmword_t const* bmwordptr = ap->freebit.ptr;
        sml_bmword_t curbit = ap->freebit.mask;
        sml_bmword_t allocated = *bmwordptr;
        // fprintf(stderr, "bmwordptr=%p avail=%x bit=%x\n", bmwordptr, allocated, curbit);
        if (!(allocated & curbit)) {
            // Get as many consecutive blocks as possible not to exceed the requested number.
            lispobj this_cons = make_lispobj(ap->free, LIST_POINTER_LOWTAG);
            do {
                CONS(last)->cdr = this_cons;
                CONS(this_cons)->car = element;
                last = this_cons;
                this_cons += 2*N_WORD_BYTES;
                curbit = (curbit<<1) | (curbit>>31); // rol curbit
                if (curbit & 1) allocated = *++bmwordptr;
            } while (--num_conses && !(allocated & curbit));
            // store in-register values back to 'ap'
            ap->freebit.ptr = bmwordptr;
            ap->freebit.mask = curbit;
            ap->free = (char*)native_pointer(this_cons);
        } else { // allocate one cons using the slow path
            lispobj this_cons = make_lispobj(sml_alloc(16), LIST_POINTER_LOWTAG);
            CONS(last)->cdr = this_cons;
            CONS(this_cons)->car = element;
            last = this_cons;
            --num_conses;
        }
    } while (num_conses);
    CONS(last)->cdr = NIL;
    return dummy.cdr;
}

extern void barrier(void**, void**);

#if 0
extern long fake_phaseptr;
int barrier_forced_on_p() {
    void** tls = (void**)get_sb_vm_thread();
    return (tls[THREAD_GC_PHASEPTR_SLOT] == &fake_phaseptr);
}
#endif
char *categorize_object(lispobj obj) {
    if (obj >= STATIC_SPACE_START
        && obj <= (lispobj)static_space_free_pointer) return "static";
    struct thread* th = get_sb_vm_thread();
    if (obj >= (lispobj)th->control_stack_start
        && obj <= (lispobj)th->control_stack_end) return "control-stack";
    return "off-heap";
}

#if VERBOSE_LOGGING
static enum sml_sync_phase last_phase_printed;
ATTR_UNUSED FILE* getworkerlog() {
  if (!workerlog) {
    workerlog = fopen("worker.txt", "w");
    assert(workerlog != NULL);
    setlinebuf(workerlog);
    TPRINTF(0, "Opened workerlog (phase=%s)", phase_name(sml_current_phase()));
  }
  return workerlog;
}
#endif
#define VERBOSE_LOGGING 0
#if VERBOSE_LOGGING
void notice_new_segment_initialized(void* s, enum sml_sync_phase ph, int log2size, char* blocks) {
  fprintf(getworkerlog(), "init segment @ %p in %s, blocks=%p, log2size=%d\n",
          s, phase_name(ph), blocks, log2size);
}
void note_large_object(size_t size, int codep, char* where) {
  fprintf(getworkerlog(), "malloc_object(%ld,%d) => %p\n", size, codep, where);
}
#endif

int n_barrier_nop;
extern void rememberedset_insert(lispobj);
extern char color_of(uword_t*);
extern void diagnose_obj(char*, lispobj*);

#define MAX_BARRIER_OPS 100000 // two hundred thousand (arb)
struct { lispobj obj, oldval, newval; } barrier_ops[MAX_BARRIER_OPS];
char barrier_op_phase[MAX_BARRIER_OPS];
char barrier_op_kind[MAX_BARRIER_OPS];

void lisp_gcbar_aux(lispobj old, lispobj new, ATTR_UNUSED lispobj obj) {
    if (old) gc_assert(is_lisp_pointer(old));
    if (new) gc_assert(is_lisp_pointer(new));
#if VERBOSE_LOGGING
    if (phase != last_phase_printed) {
      fprintf(getworkerlog(), "Phase change to %s\n", phase_name(phase));
      last_phase_printed = phase;
    }
    if ((old == 0 || old == NIL) && new == NIL) return; // don't bother logging it
    fprintf(getworkerlog(), "%p %s%lx %s%lx%s\n",
            ea,
            interesting_exact_ref_p(old)?"!":"", (uword_t)old,
            interesting_exact_ref_p(new)?"!":"", (uword_t)new,
            phase == MARK && (old==new) ? " (ign)":"");
#endif
    enum sml_sync_phase phase = load_relaxed(&current_thread->gc_phase);
    if (phase >= SYNC1) {
        if (new && new != NIL && phase <= SYNC2) rememberedset_insert(new);
        if (old && old != NIL) rememberedset_insert(old);
    }
}

ATTR_UNUSED static void record_effectless_barrier(lispobj obj, lispobj old, lispobj new, char kind) {
  n_barrier_nop++;
  (void)obj;
  (void)old;
  (void)new;
  (void)kind;
#if 0
      int index = n_barrier_nop++;
      if (index < MAX_BARRIER_OPS) {
        barrier_ops[index].obj = obj;
        barrier_ops[index].oldval = old;
        barrier_ops[index].newval = new;
        barrier_op_phase[index] = load_relaxed(&current_thread->gc_phase);
        barrier_op_kind[index] = kind;
      }
      if (n_barrier > 1000 && (float)n_barrier_nop / (float)n_barrier > .02) {
        log_nop_barriers_to_file(0);
        lose("oh dear, too many useless store barriers");
      }
#endif
}

void lisp_gcbar(lispobj old, lispobj new, lispobj obj) {
    lisp_gcbar_aux(is_lisp_pointer(old) ? old : 0,
                   is_lisp_pointer(new) ? new : 0,
                   obj);
}
// This can only be used on a lockfree list's node-next slot, NOT on fdefn-raw-addr or closure-fun
void lisp_gcbar_untagged(lispobj old, lispobj new, lispobj obj) {
    lisp_gcbar_aux(old ? old|INSTANCE_POINTER_LOWTAG : 0,
                   new ? new|INSTANCE_POINTER_LOWTAG : 0,
                   obj);
}

void set_fdefn_fun(lispobj fdefn_tagged, lispobj fun, char* raw_addr) {
    extern lispobj entrypoint_taggedptr(char*);
    struct fdefn* fdefn = FDEFN(fdefn_tagged);
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    gc_card_mark[addr_to_card_index(fdefn)] = CARD_MARKED;
#endif
    lispobj old_tagged_fun = atomic_exchange((_Atomic(lispobj)*)&fdefn->fun, fun);
    char* old_raw_addr = atomic_exchange((_Atomic(char*)*)&fdefn->raw_addr, raw_addr);
    lisp_gcbar_aux(old_tagged_fun, fun, fdefn_tagged);
    lisp_gcbar_aux(entrypoint_taggedptr(old_raw_addr),
                   entrypoint_taggedptr(raw_addr),
                   fdefn_tagged);
}

void set_funinstance_slot(lispobj funinstance, _Atomic(lispobj)* addr, lispobj newval) {
    // if in gencgc space then set the card mark, otherwise call lisp_gcbar.
    if (funinstance >= DYNAMIC_SPACE_START && funinstance < dynamic_space_highwatermark()) {
        // code pages always use soft marks
        gc_card_mark[addr_to_card_index(funinstance)] = CARD_MARKED;
        *addr = newval;
    } else {
        lispobj oldval = atomic_exchange(addr, newval);
        lisp_gcbar(oldval, newval, funinstance);
    }
}

/* weak-pointer and weak-vector have different "clobbered" values.
 * because for some reason we seem to think it's important for weak pointers
 * to be able to represent that they store NIL. So the accessor converts
 * the unbound-marker to NIL and returns a secondary value indicating whether
 * it's holding an actual NIL vs a broken pointer.
 * Vectors however use NIL as the clobbered value. */
extern lispobj weak_ref_get(lispobj*, lispobj);
lispobj weak_pointer_ref(lispobj* slot) {
  //  if (use_smlgc) fprintf(stderr, "weak_pointer_ref\n");
    return weak_ref_get(slot, UNBOUND_MARKER_WIDETAG);
}
lispobj weak_vector_ref(lispobj* slot) {
  //  fprintf(stderr, "weak_vector_ref\n");
    return weak_ref_get(slot, NIL);
}
void weak_vector_set(lispobj* slot, lispobj new) {
    if (!use_smlgc) { *slot = new; return; }
    enum sml_sync_phase phase = load_relaxed(&current_thread->gc_phase);
  // fprintf(stderr, "weak_vector_set %p, %lx\n", slot, newval);
    /* if tracing has terminated and the 'oldval' was not strongly reachable,
     * don't gray it due to the action of the barrier */
    lispobj old = weak_vector_ref(slot);
    if (phase >= SYNC1) {
        if (is_lisp_pointer(old)) rememberedset_insert(old);
        /* The snooping barrier, if off, would omit remembering 'new'
         * but here we have to remember it because it provides an access path
         * that would be skipped while enumering pointers in the weak vector. */
        if (is_lisp_pointer(new)) rememberedset_insert(new);
    }
    *slot = new;
}

#include <time.h>

int list_length(lispobj list) {
    int n = 0;
    while (list != NIL) {
        ++n;
        list = ((long*)(list-7))[1];
    }
    return n;
}

//int gclogfd;
unsigned long fake_stack_range[3] = {0, 0, 0};
void smlgc_init(size_t heapsize)
{
    sml_msg_init();
    sml_control_init();
    sml_heap_init(heapsize, heapsize);

    sml_start(fake_stack_range);
    char* bounds[2];
    get_segment_pool_bounds(bounds);
    fprintf(stderr, "bitmap heap bounds %p..%p\n", bounds[0], bounds[1]);
    SYMBOL(BITMAP_HEAP_BASE)->value = (uword_t)bounds[0];
    SYMBOL(BITMAP_HEAP_SIZE)->value = bounds[1] - bounds[0];

    void* actual =
        mmap(text_space_start, text_space_size, PROT_READ|PROT_WRITE|PROT_EXEC,
             MAP_PRIVATE|MAP_ANON|MAP_NORESERVE, -1, 0);
    if (actual != text_space_start) lose("Didn't get text space where expected\n");
    tlsf_control = tlsf_create_with_pool(text_space_start, text_space_size);
#ifdef MALLOC_LOGGING
    malloc_logfd = open("malloc-log.txt", O_WRONLY|O_APPEND|O_CREAT|O_TRUNC, 0666);
    fprintf(stderr, "logging mallocs\n");
#endif
}

void sml_run_finalizer() {}
void sml_finish() {}

void lisp_gc_check(void* arg) {
  // static int foo; ++foo; tprintf("[%d] GC check", foo);
  sml_check_internal(arg);
}

lispobj var_alloc_large(uword_t size, uword_t header) {
    void* mseg = alloc_mseg(header_widetag(header)==CODE_HEADER_WIDETAG, size);
    uword_t* userobj = (uword_t*)((char*)mseg + MALLOC_OBJECT_OFFSET);
    *userobj = header;
    return (lispobj)userobj;
}
lispobj vect_alloc_large(uword_t size, uword_t* pheader) {
    void* mseg = alloc_mseg(0, size);
    uword_t* userobj = (uword_t*)((char*)mseg + MALLOC_OBJECT_OFFSET);
    /* Store the vector length first so that the object is never in a half-baked state
     * (header word looks valid, but size is wrong) */
    userobj[1] = pheader[1];
    userobj[0] = pheader[0];
    return make_lispobj(userobj, OTHER_POINTER_LOWTAG);
}

int hexdump_object_to_file(char allocated, char gcmarked, lispobj* where, FILE* f)
{
    int nwords_tot = object_size(where);
    int n_shown = nwords_tot;
    if (widetag_of(where) == CODE_HEADER_WIDETAG) n_shown = code_header_words((struct code*)where);
    if (nwords_tot > 2 && widetag_of(where)>=0x80 && leaf_obj_widetag_p(widetag_of(where))) {
      fprintf(f, "%c%c %lx: %lx %lx ...\n", gcmarked, allocated, (uword_t)where,
              where[0], where[1]);
      return nwords_tot<<3;
    }
    fprintf(f, "%c%c %lx:", gcmarked, allocated, (uword_t)where);
    int wordindex;
    int nwords_this_line = 0;
    for (wordindex=0; wordindex<n_shown; wordindex += 2) {
      if (nwords_this_line == 0 && wordindex>0) fprintf(f, " :");
      if (where[wordindex  ] == NIL) fprintf(f, " nil"); else fprintf(f, " %lx", where[wordindex]);
      if (where[wordindex+1]== NIL)  fprintf(f, " nil"); else fprintf(f, " %lx", where[wordindex+1]);
      nwords_this_line += 2;
      if (nwords_this_line == 16) { putc('\n', f); nwords_this_line = 0; }
    }
    if (nwords_this_line) putc('\n', f);
    return nwords_tot<<3;
}

#if 0
static struct cons* newcons() {
  struct cons* new = sml_alloc(16);
  assert(new->car == 0xFFFFFFFFDEADBEEF);
  return new;
}

lispobj smlgc_list_helper(lispobj* context, lispobj count) {
    int n_conses = count / 16;
    if (n_conses == 0) return NIL;
    struct cons* head = newcons(), *tail = head, *c = head;
    lispobj item = *context--;
    c->car = item;
    if (--n_conses == 0) {
      c->cdr = NIL;
      return make_lispobj(c, LIST_POINTER_LOWTAG);
    }
    do {
      c =  newcons();
      item = *context--;
      c->car = item;
      // perform store barrier on tail->cdr if required
      tail->cdr = make_lispobj(c, LIST_POINTER_LOWTAG);
      lisp_gcbar_aux(0, tail->cdr, make_lispobj(tail, LIST_POINTER_LOWTAG) /*, &tail->cdr*/);
      tail = c;
    } while (--n_conses);
    tail->cdr = NIL;
    return make_lispobj(head, LIST_POINTER_LOWTAG);
}
lispobj smlgc_liststar_helper(lispobj* context, lispobj count) {
    int n_conses = count / 16;
    struct cons* head = newcons(), *tail = head, *c = head;
    lispobj item = *context--;
    --n_conses;
    c->car = item;
    do {
      c =  newcons();
      item = *context--;
      c->car = item;
      // perform store barrier on tail->cdr if required
      tail->cdr = make_lispobj(c, LIST_POINTER_LOWTAG);
      lisp_gcbar_aux(0, tail->cdr, make_lispobj(tail, LIST_POINTER_LOWTAG) /*, &tail->cdr*/);
      tail = c;
    } while (--n_conses);
    item = *context;
    c->cdr = item;
    return make_lispobj(head, LIST_POINTER_LOWTAG);
}
#endif

#if 0
extern int worker_is_inactive();
// This total hack is for debugging. We ask for the thread to stop, and then check
// that every object reachable by the thread got marked.
int mut_is_suspended;
char* mutator_suspension_reason;
os_context_t* thread_suspension_context_hack;
void suspend_mutator(char* reason) {
  gc_assert(!mut_is_suspended);
  mutator_suspension_reason = reason;
  mut_is_suspended = 1;
  if (worker_is_inactive()) {
    TPRINTF(0, "mutator is already is already inactive, no need to suspend it");
    return;
  }
  pthread_kill(all_threads->os_thread, SIGPWR);
  struct timespec t = {0, 1000*1000*100};
  do {
      nanosleep(&t, 0);
      if (thread_suspension_context_hack == 0) TPRINTF(0, "GC doesn't see thread context yet, waiting...");
      if (t.tv_nsec <= 1000*1000*400) t.tv_nsec *= 2;
    } while (!thread_suspension_context_hack);
}
void unsuspend_mutator() {
  gc_assert(mut_is_suspended);
  mutator_suspension_reason = 0;
  mut_is_suspended = 0;
  if (worker_is_inactive ()) {
    TPRINTF(0, "mutator is already is inactive, not unsuspending it");
    return;
  }
  pthread_kill(all_threads->os_thread, SIGPWR);
}
#endif

lispobj search_smlgc_heap_for_symbol(ATTR_UNUSED char* name)
{
  lose("search_for_symbol: not done");
  return 0;
}

// This is used by the code allocator (for now)
int quasi_munmap(void* addr, size_t len) {
    return madvise(addr, len, MADV_DONTNEED);
}

#include <stdarg.h>
/* thread-printf prepends a thread identifier and by using a single write()
 * is somewhat guaranteed not to produce interleaved output.
 * But the temp buffer is finite, so you can easily shoot yourself in the foot
 * by passing in too much to print */

void tprintf_(char *fmt, ...)
{
#if 0
    va_list ap;
    char buf[256];
    struct thread*th = get_sb_vm_thread();
    extern char get_small_thread_id();
    if (th==0)
      strcpy(buf, "GC  : ");
    else {
      sprintf(buf, "t%03d: ", (int)(th->serialno % 1000));
    }
    va_start(ap, fmt);
    char *ptr = buf+6;
    int n = vsnprintf(ptr, sizeof buf-8, fmt, ap);
    va_end(ap);
    ptr += n;
    *ptr++ = '\n';
    write(gclogfd, buf, ptr-buf);
#endif
}

void sbcl_thread_sync2(struct thread* th)
{
    if (!th) lose("losing - no thread in sbcl_thread_sync2");
    int size_log2;
    struct alloc_ptr* ap = &th->ap4;
    for (size_log2 = 4; size_log2 <= 12; ++size_log2, ++ap) {
        if (ap->free) {
            struct segment* seg = segment_addr(ap->freebit.ptr);
            seg->snapshot_free = ap->free;
        }
    }
    /*
    if (th->cons_ap.free) {
        struct segment* seg = segment_addr(th->cons_ap.freebit.ptr);
        seg->snapshot_free = th->cons_ap.free;
    } else {
        fprintf(stderr, "this AP was not used\n");
    }*/
}

#if 0
void enable_cms_cons() {
    struct thread* th = get_sb_vm_thread();
    smlgc_clear_alloc_ptr(&th->cons_ap);
}
void disable_cms_cons() {
    struct thread* th = get_sb_vm_thread();
    extern void move_cons_ap_to_filled(struct alloc_ptr*);
    fprintf(stderr, "ap.freebit.ptr=%p mask=%08x free=%p\n",
            th->cons_ap.freebit.ptr, th->cons_ap.freebit.mask, th->cons_ap.free);
    move_cons_ap_to_filled(&th->cons_ap);
    th->cons_ap.freebit.ptr = 0;
    th->cons_ap.freebit.mask = 0;
    th->cons_ap.free = 0;
}
#endif

void bump_spinlock_busyct() {
  struct thread* th = get_sb_vm_thread();
  if (th) ++th->ct_spinlock_yields; // GC thread has no vm thread
}

extern FILE* weak_obj_log;
int n_weak_refs_broken;
void print_smashed(lispobj referent, FILE* f) {
  fprintf(f, "%lx = ", referent);
  if (listp(referent)) { fprintf(f, "cons\n"); return; }
  if (instancep(referent)) {
    struct vector* v = instance_classoid_name((lispobj*)INSTANCE(referent));
    fprintf(f, "instance-of %s\n", (char*)v->data);
    return;
  }
  if (functionp(referent)) {
    fprintf(f, "fun-type %x\n", widetag_of((lispobj*)FUNCTION(referent)));
    return;
  }
  unsigned char widetag = widetag_of(native_pointer(referent));
  fprintf(f, "%s\n", widetag_names[widetag>>2]);
}

int weakobj_smashed_p(struct weak_pointer* wp) {
    if (weakptr_vectorp(wp)) {
        int nelements = fixnum_value(wp->value);
        int i;
        uword_t* elements = (uword_t*)wp + 2;
        for (i = 0; i < nelements ; ++i) {
            lispobj referent = elements[i];
            do {
#if 0
              fprintf(ff, " elt %d = %lx %s\n", i, referent,
                     !interesting_word_p(referent) ? "ign"
                     : sml_heap_check_alive(untagged_baseptr(referent)) ? "live" : "dead");
#endif
              if (!interesting_word_p(referent)) break;
              if (sml_heap_check_alive(untagged_baseptr(referent))) break;
              // weakRefGet in a mutator might do this operation, but the referent itself
              // might also change since vectors are mutable.
              if (cmpswap_relaxed((_Atomic(lispobj)*)&elements[i], &referent, NIL)) {
                // print_smashed(referent, weak_obj_log);
                  ++n_weak_refs_broken;
                  break;
              }
            } while(1);
        }
        return 0; // vectors always remain in the chain of weak objects
    }
    uword_t referent = wp->value;
    if (referent == UNBOUND_MARKER_WIDETAG) return 1;
    // If the referent is uninteresting, it is simply not recorded. Hence we skip
    // the interestingness check.
    int clear = !sml_heap_check_alive(untagged_baseptr(referent));
    if (clear) {
        wp->value = UNBOUND_MARKER_WIDETAG;
        // print_smashed(referent, weak_obj_log);
        ++n_weak_refs_broken;
    }
    return clear;
}

struct {
    struct thread* thread;
    os_context_t* context;
} stopped_threads[10];
_Atomic(int) stopped_thread_index, stopped_thread_count;

void instant_stop_handler(int signal,
                          siginfo_t ATTR_UNUSED *info,
                          os_context_t *context)
{
    int i = fetch_add(release, &stopped_thread_index, 1);
    stopped_threads[i].thread = current_thread;
    stopped_threads[i].context = context;
    sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, signal);
    char buf[100];
    int n = snprintf(buf, sizeof buf, "thread %p stopped\n", get_sb_vm_thread());
    write(2, buf, n);
    fetch_add(release, &stopped_thread_count, 1);
    sigwait(&mask, &signal); // wait for another of the same signal
    n = snprintf(buf, sizeof buf, "thread %p resuming\n", get_sb_vm_thread());
    write(2, buf, n);
    stopped_threads[i].thread = 0;
    stopped_threads[i].context = 0;
}

static lispobj* stack_roots;
static int stack_roots_max;
static int stack_roots_ct;

void maybe_barf_if_unmarked(lispobj object, void* data)
{
    struct thread* thread = data;
    if (sml_heap_check_alive((void*)object)) {
        assert(stack_roots_ct < stack_roots_max);
        stack_roots[stack_roots_ct++] = compute_lispobj((lispobj*)object);
    } else if (thread->stack_root_scan_start) {
      fprintf(stderr, "*** ambiguous root in %p sees unmarked obj %p\n", thread, (void*)object);
    } else {
      lose("*** unambiguous root in %p sees unmarked obj %p", thread, (void*)object);
    }
}

static int try_find_wordindex(lispobj* parent, lispobj obj)
{
  int nwords = object_size(parent);
  int i;
  for (i=1; i<nwords; ++i) if (parent[i] == obj) return i;
  return 0;
}
void trace_barf_if_sweepable(lispobj parent, lispobj obj, ATTR_UNUSED void *ignore) {
  if (interesting_taggedptr_p(obj)
      && !sml_heap_check_alive(native_pointer(obj))) {
    lose("Fail heap_check: %lx -> %lx (wordindex %d)\n", parent, obj,
         try_find_wordindex(native_pointer(parent), obj));
  }
}

#include "graphvisit.h"
extern struct thread* all_threads;
extern void sb_nanosleep_float(float seconds);
static long sum_microsec_elapsed;
static int n_verifies;
int smlgc_verbose;
void suspend_threads_and_heapcheck ()
{
  if (!smlgc_verbose) return;
  struct thread* th;
  int count = 0;
  for (th = all_threads; th; th = th->next) ++count, pthread_kill(th->os_thread, SIGPROF);
  while (stopped_thread_count < count) { sb_nanosleep_float(.001); }

  /* any attempt to perform I/O here might deadlock in stdio so just hope for the best.
   * e.g. I've seen:
   * #0  futex_wait (private=0, expected=2, futex_word=0x7f0a9f6bca00 <_IO_stdfile_2_lock>) at ../sysdeps/nptl/futex-internal.h:146
   * #1  __GI___lll_lock_wait_private (futex=0x7f0a9f6bca00 <_IO_stdfile_2_lock>) at ./nptl/lowlevellock.c:34
   * #2  0x00007f0a9f55eabd in __GI__IO_fwrite (buf=buf@entry=0x45c05c, size=size@entry=1, count=count@entry=3, fp=0x7f0a9f6bb680 <_IO_2_1_stderr_>) at ./libio/iofwrite.c:37
   * #3  0x000000000044fb98 in suspend_threads_and_heapcheck () at ../../smlsharpgc/lispobj.c:1957
   * from which there is no recovery */

  char msg1[] = "Lisp threads stopped\n";
  write(2, msg1, sizeof msg1-1);
  stack_roots = calloc(10000, N_WORD_BYTES);
  stack_roots_max = 10000;

  /* Step 1. Check if any thread *directly* sees an about-to-be-swept object.
   * There could be false positives, but it's unlikely */
  int i;
  for (i=0; i<count; ++i) {
    th = stopped_threads[i].thread;
    lispobj* sp = (lispobj*)*os_context_sp_addr(stopped_threads[i].context);
    lispobj* end = th->control_stack_end;
    struct thread_instance *instance = (void*)native_pointer(th->lisp_thread);
    ATTR_UNUSED struct vector* name = VECTOR(instance->_name);
    // printf("Checking %p stack %p..%p (%4d words) [%s]\n", th, sp, end, (int)(end-sp), (char*)name->data);
    scan_vmthread_stack(th, sp, end, maybe_barf_if_unmarked, th);
  }
  char msg2[]  = "Stacks look OK\n"; write(2, msg2, sizeof msg2-1);

  struct hopscotch_table ht;
  //extern int graph_visit_skip_weak_pointers;
  //graph_visit_skip_weak_pointers = 1;
  struct grvisit_context* grvisit =
      visit_heap_from_roots(&ht, trace_barf_if_sweepable, 0, 1,
                            stack_roots, stack_roots_ct);
  //graph_visit_skip_weak_pointers = 0;
  sum_microsec_elapsed += grvisit->microsec_elapsed;
  ++n_verifies;
  char buf[100];
  int n = snprintf(buf, sizeof buf, "graph walk: %d objects, depth %d, ET=%ld\u00B5sec (avg %ld)\n",
                   grvisit->seen->count, grvisit->maxdepth,
                   grvisit->microsec_elapsed,
                   sum_microsec_elapsed / n_verifies);
  write(2, buf, n);
  int any_fail = 0;

#if 0
  struct hopscotch_table* seen = grvisit->seen;
  int index;
  long key;
#if 1 // quick check
  for_each_hopscotch_key(index, key, (*seen)) {
    if (interesting_taggedptr_p(key)) {
        gc_assert(widetag_of(native_pointer(key)) != SIMPLE_FUN_WIDETAG);
        if (!sml_heap_check_alive(native_pointer(key))) {
          fprintf(stderr, "FAIL %lx\n", key);
          ++any_fail;
        }
    }
  }
  assert(!any_fail);
#else
  FILE *f = fopen("visit.txt", "w");
  for_each_hopscotch_key(index, key, (*seen)) {
    fprintf(f, "%lx", key);
    if (interesting_taggedptr_p(key)) {
        gc_assert(widetag_of(native_pointer(key)) != SIMPLE_FUN_WIDETAG);
        if (!sml_heap_check_alive(native_pointer(key))) {
          fprintf(f, " <<< FAIL");
          ++any_fail;
        }
    }
    putc('\n', f);
  }
  fclose(f);
#endif
#endif

  if (any_fail) {
    fprintf(stderr, "failures: %d\n", any_fail);
    hexdump_sml_heap_to_file("heap.dump");
    ldb_monitor();
  } else {
    char msg3[] = "PASSSED\n";
    write(2, msg3, sizeof msg3-1);
  }
  free(grvisit);
  free(stack_roots); stack_roots = 0;
  stack_roots_ct = stack_roots_max = 0;
  hopscotch_destroy(&ht);
  stopped_thread_index = stopped_thread_count = 0;
  for (th = all_threads; th; th = th->next) pthread_kill(th->os_thread, SIGPROF);
}

static char *scan_above;
static char *lowest_new_allocation;
extern void store_code_forwarding_ptrs(struct code*, struct code*);
static lispobj fix_tagged_ptr(lispobj ptr, struct hopscotch_table* hashset)
{
    /* I'm not sure where some undefined behavior is coming from,
     * but without an explicit test for 0 here, we'd get an
     * attempt to dereference null in forwarding_pointer_p even though it occurs
     * after the tests for whether 'ptr' is in a managed space. */
    if (ptr == 0) return ptr;
    lispobj* obj = native_pointer(ptr);
    if (!(in_bitmapped_subheap(obj) || large_code_subspace_p((char*)obj)
          || hopscotch_get(hashset, (uword_t)obj, 0))) {
        if (find_page_index(obj)>=0 || is_in_static_space(obj)) {
        } else {
            // show suspicious pointers
            fprintf(stderr, "SKIP %lx\n", ptr);
        }
        return ptr;
    }
    if (forwarding_pointer_p(obj)) return forwarding_pointer_value(obj);
    // FIXME: ptr_scavtab doesn't readily work because some copiers such as trans_code
    // assert that find_page_index(object_being_copied) is valid. As such, they can't copy
    // "off-heap" objects to heap objects.
    struct alloc_region* region = mixed_region;
    int page_type = PAGE_TYPE_MIXED;
    unsigned char tag = widetag_of(obj);
    if (tag == SIMPLE_FUN_WIDETAG) {
        obj = (lispobj*)fun_code_header((struct simple_fun*)obj);
        tag = CODE_HEADER_WIDETAG;
    }
    // Use three regions: cons, code, and mixed. Final GC can choose a better
    // page type (large, boxed, etc)
    if (is_cons_half(*obj))
        region = cons_region, page_type = PAGE_TYPE_CONS;
    else if (tag == CODE_HEADER_WIDETAG || tag == FUNCALLABLE_INSTANCE_WIDETAG)
        region = code_region, page_type = PAGE_TYPE_CODE;
    sword_t nbytes = object_size(obj) << WORD_SHIFT;
    lispobj* new = gc_general_alloc(region, nbytes, page_type);
    memcpy(new, obj, nbytes);
    if (tag == WEAK_POINTER_WIDETAG) {
        /* Concurrent GC uses header bytes to link the object into the weak-pointer chain
         * upon creation, but gencgc links it in only when scav_weakptr() is called.
         * Clear out the link so that gencgc can use the bytes as intended */
        // fprintf(stderr,"copy weakptr %p : %lx %lx\n", obj, obj[0], obj[1]);
        reset_weak_pointer_next((struct weak_pointer*)new);
    }
    if (tag == INSTANCE_WIDETAG) {
        lispobj* layout = native_pointer(instance_layout(new));
        if (forwarding_pointer_p(layout))
            layout = native_pointer(forwarding_pointer_value(layout));
        if (layout_depth2_id((struct layout*)layout) == HASH_TABLE_LAYOUT_ID) {
          struct hash_table* ht = (struct hash_table*)new;
          if (hashtable_weakp(ht)) {
            gc_assert(ht->implied_edges_done == NIL);
            // NIL means not in the chain, while NULL means end-of-chain
            // because it's a C list, not a Lisp list.
            ht->next_weak_hash_table = (void*)NIL;
          }
        }
    }
    if (tag == CODE_HEADER_WIDETAG)
        store_code_forwarding_ptrs((void*)obj, (void*)new);
    else {
        set_forwarding_pointer(obj, compute_lispobj(new));
        // funcallable instance with embedded code gets trampoline assigned
        if (tag == FUNCALLABLE_INSTANCE_WIDETAG) new[1] = (lispobj)&new[2];
    }
    // address can bounce around because of the different regions
    // for different page types.
    if ((char*)new < lowest_new_allocation) lowest_new_allocation = (char*)new;
    // Adding to 'new' the difference between 'ptr' and 'obj' correctly reconstructs
    // a simple-fun pointer or any other kind of pointer.
    return ptr - (lispobj)obj + (lispobj)new;
}

#define FIX(x) { lispobj old = x; if (is_lisp_pointer(old)) { \
      lispobj new = fix_tagged_ptr(old, msegs); if (new != old) x = new; } }

/* I tried to use 'trace-object.inc' here but unfortunately it needs still more
 * parameterization. This logic wants nothing to do with weak hash-tables- it just
 * copies them as simple-vector. But that's not an option in trace-object.
 * Also it doesn't currently support STRENGTHEN_WEAK_REFS on vector-like weak-pointers
 * but that's probably not so hard to correct */
static void obj_scavenge(lispobj* obj, struct hopscotch_table *msegs)
{
    lispobj header = *obj;
    if (is_cons_half(header)) { FIX(obj[0]); FIX(obj[1]); return; }
    int widetag = header_widetag(header);
    if (instanceoid_widetag_p(widetag)) {
        lispobj layout = layout_of(obj);
        if (!layout) return;
        layout_of(obj) = layout = fix_tagged_ptr(layout, msegs);
        if (lockfree_list_node_layout_p(LAYOUT(layout))) {
            struct list_node* node = (void*)obj;
            lispobj next = node->_node_next;
            assert(!next || is_lisp_pointer(next));
        }
        struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
        int i;
        lispobj* slots = obj+1;
        int nslots = instanceoid_length(header);
        for (i=0; i<nslots; ++i) if (bitmap_logbitp(i, bitmap)) FIX(slots[i]);
        return;
    }
    sword_t scan_from = 1;
    sword_t scan_to = sizetab[widetag](obj);
    sword_t i;
    if (widetag == INSTANCE_WIDETAG) {
      lispobj* layout = native_pointer(instance_layout(obj));
      if (forwarding_pointer_p(layout)) {
        layout = native_pointer(forwarding_pointer_value(layout));
        if (layout_depth2_id((struct layout*)layout) == HASH_TABLE_LAYOUT_ID) {
          struct hash_table* ht = (struct hash_table*)obj;
          if (hashtable_weakp(ht)) fprintf(stderr, "OH SHIT THIS IS A BICH: %p %p\n", obj, ht->next_weak_hash_table);
        }
      }
    }

    switch (widetag) {
    case SIMPLE_VECTOR_WIDETAG:
        if (vector_flagp(header, VectorAddrHashing)) {
            KV_PAIRS_REHASH(((struct vector*)obj)->data) |= make_fixnum(1);
        }
        break;
    case CLOSURE_WIDETAG: {
        struct closure* c = (void*)obj;
        lispobj tagged_self = fun_taggedptr_from_self(c->fun);
        c->fun = fun_self_from_taggedptr(fix_tagged_ptr(tagged_self, msegs));
        scan_from = 2;
        break; // scan rest of slots normally
    }
    case CODE_HEADER_WIDETAG:
        // simple-fun pointers were reassigned already in store_code_forwarding_ptrs
        scan_to = code_header_words((struct code*)obj);
        break;
    case SYMBOL_WIDETAG: {
        struct symbol* s = (void*)obj;
        set_symbol_name(s, fix_tagged_ptr(decode_symbol_name(s->name), msegs));
        FIX(s->value);
        FIX(s->info);
        FIX(s->fdefn);
        return;
    }
    case FDEFN_WIDETAG: {
        struct fdefn *fdefn = (void*)obj;
        lispobj old = decode_fdefn_rawfun(fdefn);
        lispobj new = fix_tagged_ptr(old, msegs);
        if (new != old && old != 0) fdefn->raw_addr = new + (fdefn->raw_addr - old);
        scan_to = 3;
        break;
    }
    default:
        if (leaf_obj_widetag_p(widetag)) return;
    }
    for(i=scan_from; i<scan_to; ++i) FIX(obj[i]);
}
static int print_ranges;
static uword_t range_scavenge(lispobj* where, lispobj* limit, uword_t data)
{
  if (print_ranges) printf("Visit range %p..%p\n", where, limit);
    if ((char*)limit >= scan_above) {
        for ( ; where <  limit ; where += object_size(where) )
            obj_scavenge(where, (void*)data);
    }
    return 0;
}

extern void* get_allocated_msegs();
lispobj copy_smlgc_heap_to_gencgc(lispobj initfun)
{
    struct hopscotch_table mseg_hashset;
    hopscotch_create(&mseg_hashset, HOPSCOTCH_HASH_FUN_DEFAULT, 0 /* hashset */,
                     32 /* logical bin count */, 0 /* default range */);
    lispobj *mseg = get_allocated_msegs();
    for ( ; mseg ; mseg = (lispobj*)*mseg ) {
        if (!large_code_subspace_p((char*)mseg))
            hopscotch_insert(&mseg_hashset, (uword_t)mseg+MALLOC_OBJECT_OFFSET, 1);
    }
    fprintf(stderr, "heap copy: %d msegs\n", mseg_hashset.count);
    // Make all of gencgc dynamic-space writable
    mprotect((void*)DYNAMIC_SPACE_START, dynamic_space_size, OS_VM_PROT_ALL);
    // touch all cards
    memset(gc_card_mark, CARD_MARKED, dynamic_space_size/GENCGC_CARD_BYTES);
    // Copy objects from SMLgc space to gencgc space using static + pseudostatic objects
    // as roots.
    printf("next_free=%p\n", page_address(next_free_page));
    obj_scavenge((lispobj*)NIL_SYMBOL_SLOTS_START, &mseg_hashset);
    range_scavenge((lispobj*)STATIC_SPACE_OBJECTS_START, static_space_free_pointer,
                   (uword_t)&mseg_hashset);
    initfun = fix_tagged_ptr(initfun, &mseg_hashset);
#define ALLOC_SENTINEL (void*)(intptr_t)-1
    lowest_new_allocation = ALLOC_SENTINEL;
    // print_ranges = 1;
    while (1) {
        // fprintf(stderr, "-- top of loop: scan_from=%p --\n", scan_above);
        gc_close_collector_regions(0);
        walk_generation(range_scavenge, -1, (uword_t)&mseg_hashset);
        print_ranges = 0;
        if (lowest_new_allocation == ALLOC_SENTINEL) break;
        scan_above = lowest_new_allocation;
        lowest_new_allocation = ALLOC_SENTINEL;
    }
    gc_close_collector_regions(0);
#undef ALLOC_SENTINEL
    os_vm_size_t original_bytes_allocated = bytes_allocated;
    // assert that we got everything
    scan_above = 0;
    walk_generation(range_scavenge, -1, (uword_t)&mseg_hashset);
    gc_close_collector_regions(0);
    assert(bytes_allocated == original_bytes_allocated);
    hopscotch_destroy(&mseg_hashset);
    get_sb_vm_thread()->ap4.freebit.mask = -1;
    use_smlgc = 0;
    return initfun;
}

/* Before GC starts sweeping, it will acquire this lock as a writer,
 * There's still a race condition here, but I believe that in correctly
 * operating Lisp code, races should not occur. The errneous case would
 * be something like below (with time flowing downward) -
 *
 *    GC                              mutator
 *    -------                         ---------
 *    ...
 *    marking                         get pointer to object X in hand
 *    ...                             store pointer to X in a way opaque to GC
 *    ...
 *    ...                             acquire valid-object read lock
 *    terminate marking
 *     (object X is unmarked)
 *    ...                             restore pointer to X
 *    ...                             inquire whether X is a valid object -> yes
 *    ...                             release reader lock
 *    acquire writer lock
 *    clobber X
 *
 * So the issue with the above is that X was stored opaquely and the deletion barrier
 * didn't mark X which seems clearly like an error in the mutator. (Not the fault
 * of the user, but the compiler/runtime). So it shouldn't happen if those are right.
 * It's slightly concerning that interior pointers to code can be constructed
 * out of thin air.
 */
pthread_rwlock_t valid_obj_lock = PTHREAD_RWLOCK_INITIALIZER;

lispobj *lisp_component_ptr_from_pc(char *pc) {
    extern lispobj *component_ptr_from_pc(char *pc);
    pthread_rwlock_rdlock(&valid_obj_lock);
    lispobj *result = component_ptr_from_pc(pc);
    pthread_rwlock_unlock(&valid_obj_lock);
    return result;
}
int lisp_valid_tagged_pointer_p(lispobj pointer) {
    extern int valid_tagged_pointer_p(lispobj);
    pthread_rwlock_rdlock(&valid_obj_lock);
    int result = valid_tagged_pointer_p(pointer);
    pthread_rwlock_unlock(&valid_obj_lock);
    return result;
}

static int element_alivep(lispobj taggedptr) {
    // "Uninteresting" objects (being immediate or static, most likely) are always alive.
    return !interesting_word_p(taggedptr) || sml_heap_check_alive(untagged_baseptr(taggedptr));
}

void add_implied_edge(lispobj source, lispobj target)
{
    uword_t key = (uword_t)untagged_baseptr(source);
    // 'cons' is a good representation for these edge lists.
    struct cons* list = malloc(sizeof (struct cons));
    list->car = (uword_t)untagged_baseptr(target);
    struct cons* found = (void*)hopscotch_get(&implied_edges, key, 0);
    if (found) {
        /* Destructively modify the list that was found under this key (avoiding
         * a table update). Order of values in the resulting list is irrelevant */
        list->cdr = found->cdr;
        found->cdr = (lispobj)list;
    } else {
        list->cdr = 0;
        hopscotch_insert(&implied_edges, key, (uword_t)list);
    }
}

/* Given a table's storage vector which thus far was treated as simply a weak vector,
 * visit the other half of any cell for which it needs to be livened based on the
 * table's weakness kind; and for any cell where it is still indeterminate whether
 * livening should occur, add 1 or 2 edges to the virtual edge set */
void weak_table_add_edges(struct hash_table* ht, int weakness, struct object_list* objs)
{
    gc_assert(ht->implied_edges_done == NIL);
#ifdef DISABLE_HASH_TABLE_WEAKNESS
    return;
#endif
    int new_edges = 0;
    int old_list_count = objs->count;
    struct vector* pairs = VECTOR(ht->pairs);
    int i, len = vector_len(pairs);
    for (i = 2; i < len; i += 2) {
        lispobj key = pairs->data[i];
        lispobj val = pairs->data[1+i];
        // immediates: skip quickly
        if (!is_lisp_pointer(key) && !is_lisp_pointer(val)) continue;
        int k_live = element_alivep(key);
        int v_live = element_alivep(val);
        //        if (!k_live || !v_live) fprintf(stderr, " %lx,%lx %d %d\n", key, val, k_live, v_live);
        // Note that both of these can occur, for a :WEAKNESS :KEY-OR-VALUE table
        if ((weakness & 1) && !v_live) { // KEY livens value, and value is not known live yet
            // (i.e. if value was reachable anyway, then this does not care what happens)
            if (k_live)
                object_list_append(objs, untagged_baseptr(val));
            else
                add_implied_edge(key, val), ++new_edges;
        }
        if ((weakness & 2) && !k_live) { // VALUE livens key, and key is not known live yet
            if (v_live)
                object_list_append(objs, untagged_baseptr(key));
            else
                add_implied_edge(val, key), ++new_edges;
        }
    }
    ht->implied_edges_done = LISP_T;
    fprintf(stderr, " -> %d objects livened, %d edges added\n",
            objs->count - old_list_count, new_edges);
}

/* 'ht' is a pointer to the first weak table in the chain.
 * Objects that be livened now are placed into 'objs'.
 * Each implied edges is placed into a hopscotch table but only if it is not the case
 * that both cells of a pair are already blackened */
void add_weak_tables_edges(struct hash_table* ht, struct object_list* objs)
{
    hopscotch_create(&implied_edges, HOPSCOTCH_HASH_FUN_DEFAULT, N_WORD_BYTES,
                     32 /* logical bin count */, 0 /* default range */);
    for ( ; (uintptr_t)ht != 1 ; ht = (void*)ht->next_weak_hash_table ) {
        assert(widetag_of((lispobj*)ht) == INSTANCE_WIDETAG);
        int weakness = hashtable_weakness(ht);
        char *weakness_kinds[] = {"AND","KEY","VALUE","OR"};
        int table_alive = sml_heap_check_alive(ht);
        fprintf(stderr, "Weak %s table %p ct=%ld %s\n", weakness_kinds[weakness], ht,
                fixnum_value(ht->_count), table_alive?"alive":"");
        /* Weakness kind 0 does not imply any new edges - both the key and value
         * must be live for the pair to be kept */
        if (table_alive && weakness != 0) weak_table_add_edges(ht, weakness, objs);
    }
    fprintf(stderr, "build_weak_table_implied_edges: new_objects=%d table_count=%d\n",
            objs->count, implied_edges.count);
}

void smash_weak_table_pairs(struct hash_table* ht)
{
    struct vector* pairs = VECTOR(ht->pairs);
    int i, len = vector_len(pairs);
    int oldct = fixnum_value(ht->_count);
    for (i = 2; i < len; i += 2) {
        lispobj key = pairs->data[i];
        lispobj val = pairs->data[1+i];
        int k_live = element_alivep(key);
        int v_live = element_alivep(val);
        if (k_live && v_live) continue;
        // The implicit edge table would have rendered both parts of the cell live
        // if it was supposed to have been kept. So kill it; no decision needed.
        pairs->data[i] = UNBOUND_MARKER_WIDETAG;
        pairs->data[1+i] = UNBOUND_MARKER_WIDETAG;
        ht->_count -= make_fixnum(1);
    }
    ht->implied_edges_done = NIL;
    fprintf(stderr, "prune_table(%p): oldct=%d newct=%d\n",
            ht, oldct, (int)fixnum_value(ht->_count));
}

void dispose_weak_table_edge_lists() {
    int index;
    lispobj key;
    struct cons **values = (struct cons**)implied_edges.values;
    for_each_hopscotch_key(index, key, implied_edges) {
        struct cons *cons = values[index], *next;
        assert(cons);
        do {
            next = (void*)cons->cdr;
            free(cons);
        } while ((cons = next) != 0);
    }
    hopscotch_reset(&implied_edges);
}
