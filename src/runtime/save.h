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

#ifndef _SAVE_H_
#define _SAVE_H_
#include <limits.h>
#include <stdbool.h>
#include "core.h"

#define COMPRESSION_LEVEL_NONE INT_MIN

extern FILE *prepare_to_save(char *filename, bool prepend_runtime, void **runtime_bytes, size_t *runtime_size);
extern bool save_runtime_to_filehandle(FILE *output, void *runtime_bytes,
                                          size_t runtime_size, int application_type);
extern bool save_to_filehandle(FILE *file, char *filename, lispobj init_function,
                                  bool make_executable, bool keep_runtime_options,
                                  int core_compression_level);
extern bool save(char *filename, lispobj init_function, bool prepend_runtime,
                    bool keep_runtime_options,
                    bool compressed_core, int core_compression_level,
                    int application_type);

#endif
