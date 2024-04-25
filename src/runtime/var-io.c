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
# include <zstd.h>
# include "genesis/vector.h"
# include "genesis/cons.h"
# include "genesis/compiled-debug-info.h"
# include "code.h"
#endif

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
char* compress_vector(lispobj vector, size_t *result_size) {
    struct vector *v = VECTOR(vector);
    size_t bytes = vector_len(v);


    size_t buf_size = ZSTD_compressBound(bytes);
    char* buf = successful_malloc(buf_size);
    size_t ret = ZSTD_compress(buf, buf_size, v->data, bytes, 22);
    if (ZSTD_isError(ret)) {
        *result_size = 0;
        free(buf);
        return NULL;
    }
    *result_size = ret;
    return buf;
}

unsigned char* decompress_vector(lispobj vector, size_t *result_size) {

    struct vector *v = VECTOR(vector);

    ZSTD_inBuffer input;
    input.src = v->data;
    input.pos = 0;
    input.size = vector_len(v);

    size_t out_increment = ZSTD_CStreamOutSize();
    size_t buf_size = 0;

    char* buf = NULL;
    size_t ret;

    ZSTD_DStream *stream = ZSTD_createDStream();
    if (stream == NULL)
        lose("unable to create zstd decompression context");
    ret = ZSTD_initDStream(stream);
    if (ZSTD_isError(ret))
        lose("ZSTD_initDStream failed with error: %s", ZSTD_getErrorName(ret));

    while (input.pos < input.size) {
        buf = realloc(buf, buf_size + out_increment);

        ZSTD_outBuffer output = { buf+buf_size, out_increment, 0 };

        size_t const ret = ZSTD_decompressStream(stream, &output , &input);
        if (ZSTD_isError(ret))
            lose("ZSTD_decompressStream failed with error: %s",
                 ZSTD_getErrorName(ret));
        buf_size += output.pos;

    }
    ZSTD_freeDStream(stream);

    *result_size = buf_size;
    return buf;
}

void compress_debug_info(lispobj * code_ptr) {
    struct code* code = (struct code*)code_ptr;

    struct compiled_debug_info *di;

    if (instancep(code->debug_info))
        di = (void*)native_pointer(code->debug_info);
    else if (listp(code->debug_info) && instancep(CONS(code->debug_info)->car))
        di = (void*)native_pointer(CONS(code->debug_info)->car);
    else
        return;

    struct vector *v = VECTOR(di->fun_map);
    if (widetag_of(&v->header) != SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG)
        return;
    size_t length;
    char* compressed = compress_vector(di->fun_map, &length);
    size_t current_length = (size_t) vector_len(v);
    if (length > 0 && length < current_length) {
        assign_widetag(&v->header, SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG);
        v->length_ = make_fixnum(length);
        memcpy(&v->data, compressed, length);
        memset(((char*)&v->data)+length, 0, current_length-length);
        free(compressed);
    }

}
#endif
