/*
 * Decoding SC offsets.
 */

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

#include "genesis/sc-offset.h"

int
sc_and_offset_extract_bits(int sc_and_offset,
                           unsigned int bytes_count,
                           struct sc_and_offset_byte* bytes) {
    unsigned int result = 0, i = 0, position, size, mask, index = 0;
    for (; i < bytes_count; ++i, index += size) {
        position = bytes[i].position;
        size = bytes[i].size;
        mask = (1 << size) - 1;
        result |= ((sc_and_offset >> position) & mask) << index;
    }
    return result;
}

int
sc_and_offset_sc_number(int sc_and_offset) {
    return sc_and_offset_extract_bits
        (sc_and_offset,
         sizeof(sc_and_offset_sc_number_bytes) / sizeof(struct sc_and_offset_byte),
         sc_and_offset_sc_number_bytes);
}

int
sc_and_offset_offset(int sc_and_offset) {
    return sc_and_offset_extract_bits
        (sc_and_offset,
         sizeof(sc_and_offset_offset_bytes) / sizeof(struct sc_and_offset_byte),
         sc_and_offset_offset_bytes);
}
