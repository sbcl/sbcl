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
