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

#include "genesis/config.h"
#include "genesis/bignum.h"
#include "gc-assert.h"
#include "var-io.h"

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
    unsigned char *ptr = source + (offset ? *offset : 0);
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
        *offset += (ptr - source);
    }
    return result;
}

void varint_unpacker_init(struct varint_unpacker* unpacker, lispobj integer)
{
  if (fixnump(integer)) {
      unpacker->word  = fixnum_value(integer);
      unpacker->limit = N_WORD_BYTES;
      unpacker->data  = (char*)&unpacker->word;
  } else {
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
