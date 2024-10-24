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

#ifndef _VAR_IO_H_
#define _VAR_IO_H_

extern int read_var_integer(unsigned char *source, int *offset);
void skip_var_string(unsigned char* source, int *offset);

// For streaming varints from a specialized data buffer that is first
// delta-encoded, then varint-encoded.
struct varint_unpacker {
  char*    data;   // pointer to stream of bytes
  int      index;  // byte index from start of data
  int      limit;  // ending value for 'index'
  uword_t  word;   // scratch word used only if the input was a fixnum
};

void varint_unpacker_init(struct varint_unpacker*, lispobj);
int varint_unpack(struct varint_unpacker*, int*);
void skip_data_stream(struct varint_unpacker* unpacker);
int decompress_vector(lispobj, int offset, unsigned char* output, int outputlen);

#endif /* _VAR_IO_H_ */
