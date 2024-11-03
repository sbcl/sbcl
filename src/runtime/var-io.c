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

#include "gc-assert.h"
#include "var-io.h"
#include "genesis/number-types.h"
#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
# include <stdlib.h>
# include <zstd.h>
# include "genesis/vector.h"
# include "genesis/cons.h"
# include "genesis/compiled-debug-info.h"
# include "code.h"
# include "interr.h" // for lose()
# include "thread.h"
#endif
#include <string.h>

// Read a variable-length encoded 32-bit integer from SOURCE and
// return its value.
//
// If OFFSET is not NULL, start decoding at OFFSET bytes from SOURCE
// and increment the value pointed to by OFFSET by the length of the
// encoded representation.
//
// Keep in sync with {READ,WRITE}-VAR-INTEGER in
// src/code/debug-var-io.lisp
int read_var_integer(unsigned char *source, int *offset) {
    unsigned char *start = source + (offset ? *offset : 0);
    unsigned char *ptr = start;
    int result = 0;
    unsigned char octet;
    int k = 0;
    for (;; k += 7) {
        octet = *(ptr++);
        result |= (octet & 0x7f) << k;
        if (!(octet & 0x80)) {
            break;
        }
    }
    if (offset) {
        *offset += (ptr - start);
    }
    return result;
}

void skip_var_string(unsigned char* source, int *offset) {
    int len = read_var_integer(source, offset);
    int i;
    for (i = 0; i < len; i++) {
        read_var_integer(source, offset);
    }
}

void varint_unpacker_init(struct varint_unpacker* unpacker, lispobj integer)
{
  if (fixnump(integer)) {
      unpacker->word  = fixnum_value(integer);
      unpacker->limit = N_WORD_BYTES;
      unpacker->data  = (char*)&unpacker->word;
  } else {
      gc_assert(lowtag_of(integer) == OTHER_POINTER_LOWTAG
                && widetag_of(native_pointer(integer)) == BIGNUM_WIDETAG);
      struct bignum* bignum = (struct bignum*)(integer - OTHER_POINTER_LOWTAG);
      unpacker->word  = 0;
      unpacker->limit = HeaderValue(bignum->header) * N_WORD_BYTES;
      unpacker->data  = (char*)bignum->digits;
  }
  unpacker->index = 0;
}

// Fetch the next varint from 'unpacker' into 'result'.
// Because there is no length prefix on the number of varints encoded,
// spurious trailing zeros might be observed. The data consumer can
// circumvent that by storing a count as the first value in the series.
// Return 1 for success, 0 for EOF.
int varint_unpack(struct varint_unpacker* unpacker, int* result)
{
    if (unpacker->index >= unpacker->limit) return 0;
    int accumulator = 0;
    int shift = 0;
    while (1) {
#ifdef LISP_FEATURE_LITTLE_ENDIAN
        int byte = unpacker->data[unpacker->index];
#else
        // bignums are little-endian in word order,
        // but machine-native within each word.
        // We could pack bytes MSB-to-LSB in the bigdigits,
        // but that seems less intuitive on the Lisp side.
        int word_index = unpacker->index / N_WORD_BYTES;
        int byte_index = unpacker->index % N_WORD_BYTES;
        int byte = (((uword_t*)unpacker->data)[word_index] >> (byte_index * 8)) & 0xFF;
#endif
        ++unpacker->index;
        accumulator |= (byte & 0x7F) << shift;
        if (!(byte & 0x80)) break;
        gc_assert(unpacker->index < unpacker->limit);
        shift += 7;
    }
    *result = accumulator;
    return 1;
}

void skip_data_stream(struct varint_unpacker* unpacker)
{
    // Read elements until seeing a 0
    int val;
    while (varint_unpack(unpacker, &val) && val != 0) { }
}
#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
static int __attribute__((unused)) mem_is_zeroed(char* from, char* to) {
    for ( ; from < to ; ++from) if (*from) return 0;
    return 1;
}
int pack_varint(unsigned int input, char output[5])
{
    int i = 0;
    do {
        unsigned int next = input>>7;
        output[i++] = (input & 0x7F) | (next ? 0x80 : 0);
        input = next;
    } while (input);
    return i;
}

int compress_vector(lispobj vector, size_t used_length) {
    struct vector *v = VECTOR(vector);
    gc_assert(widetag_of((lispobj*)v) == SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG);
    // Must be zero above the used_length (verified only if -DDEBUG)
    gc_dcheck(mem_is_zeroed(used_length + (char*)&v->data,
                            (size_t)vector_len(v) + (char*)&v->data));

    char prefix[5];
    int prefix_length = pack_varint(used_length, prefix);
    size_t buf_size = ZSTD_compressBound(used_length);
    char* buf = successful_malloc(buf_size);
    size_t new_length = ZSTD_compress(buf, buf_size, v->data, used_length, 22);
    if (ZSTD_isError(new_length)) {
        free(buf);
        return 0;
    }
    int compressed = 0;
    if (prefix_length + new_length < used_length) {
        assign_widetag(&v->header, SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG);
        char* output = (char*)&v->data;
        memcpy(output, prefix, prefix_length);
        memcpy(output + prefix_length, buf, new_length);
        new_length += prefix_length;
        memset(output+new_length, 0, used_length-new_length);
        used_length = new_length;
        compressed = 1;
    }
    // Length is assigned regardless of whether compression occurred, as the physical vector
    // can potentially be shrunk in all cases since vector-push-extend overallocates.
    v->length_ = make_fixnum(used_length);
    free(buf);
    return compressed;
}

/* I think it's no coincidence that any time I would attach to a stuck SBCL process
 * which was attempting to self-diagnose a memory fault using backtrace, it seemed
 * to be stuck in the backtracer thusly or similarly:
 * #0  0x00007faafc41f099 in syscall () from /usr/grte/v5/lib64/libc.so.6
 * #1  0x00005561518e3c54 in AbslInternalSpinLockDelay ()
 * #2  0x00005561519b652d in absl::base_internal::SpinLock::SlowLock() ()
 * #3  0x00005561519af3ed in tcmalloc::tcmalloc_internal::ThreadCache::CreateCacheIfNecessary() ()
 * #4  0x000055615196d32d in tcmalloc::tcmalloc_internal::TCMallocPolicy<...>
 * #5  0x00005561517dc6f0 in ZSTD_createDStream ()
 * #6  0x000055615176141f in decompress_vector ()
 *
 * While ZSTD_decompress is not promised to be signal-safe, it seems to fare better
 * than relying on ZSTD_createDStream.
 */
int decompress_vector(lispobj l_input, int input_offset,
                      unsigned char* output_buffer, int buffer_length) {

    struct vector *iv = VECTOR(l_input);
    unsigned char* src = (unsigned char*)iv->data + input_offset;
    int compressedsize = vector_len(iv) - input_offset;

#ifdef ZSTD_STATIC_LINKING_ONLY
    ZSTD_frameHeader zfh;
    ZSTD_DCtx* dctx = 0;
    bool context_borrowed = 0;
    if (ZSTD_getFrameHeader(&zfh, src, compressedsize)) lose("could not get ZSTD frame header\n");
    gc_assert(buffer_length == zfh.frameContentSize);
    struct thread* th = get_sb_vm_thread();
    if (th) {
        struct extra_thread_data *extra_data = thread_extra_data(th);
        void* my_dcontext = extra_data->zstd_dcontext;
        /* Indicate that the thread's zstd_dcontext is momentarily in-use so that another invocation of
         * backtrace in this thread _might_ work.  Granted such use will probably crash/deadlock anyway,
         * but let's at least ensure that separate invocations don't stomp on the same context. */
        if (my_dcontext &&
            __sync_bool_compare_and_swap(&extra_data->zstd_dcontext, my_dcontext, 0)) {
            dctx = my_dcontext;
            context_borrowed = 1;
        }
    }
    if (!dctx) dctx = ZSTD_createDCtx();
    ZSTD_decompressBegin(dctx);
    size_t regeneratedSize = 0;
    char* input_ptr = src;
    char* input_end = src + compressedsize;
    size_t remainingCapacity = buffer_length;
    char* output_ptr = output_buffer;
    while (input_ptr < input_end) {
        size_t iSize = ZSTD_nextSrcSizeToDecompress(dctx);
        if (iSize == 0) lose("ZSTD API usage error");
        size_t decodedSize = ZSTD_decompressContinue(dctx, output_ptr, remainingCapacity,
                                                     input_ptr, iSize);
        if (ZSTD_isError(decodedSize))
            lose("ZSTD_decompressContinue failed: %s", ZSTD_getErrorName(decodedSize));
        input_ptr += iSize;
        regeneratedSize += decodedSize;
        output_ptr += decodedSize;
        remainingCapacity -= decodedSize;
    }
    gc_assert(regeneratedSize == buffer_length);
    if (context_borrowed)
        thread_extra_data(th)->zstd_dcontext = dctx;
    else
        ZSTD_freeDCtx(dctx);
#else
    size_t result = ZSTD_decompress(output_buffer, buffer_length, src, compressedsize);
    if (result != (size_t)buffer_length)
        lose("Zstd uncompressed size is wrong for input %p: %ld vs %d (%s)",
             (void*)l_input, result, buffer_length, ZSTD_getErrorName(result));
#endif

    return 1;
}
#endif
