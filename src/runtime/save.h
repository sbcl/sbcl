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

#include "core.h"

extern FILE* open_core_for_saving(char *filename);
extern void *load_runtime(char *runtime_path, size_t *size_out);
extern FILE *prepare_to_save(char *filename, boolean prepend_runtime, void **runtime_bytes, size_t *runtime_size);
extern boolean save_runtime_to_filehandle(FILE *output, void *runtime_bytes, size_t runtime_size);
extern boolean save_to_filehandle(FILE *file, char *filename, lispobj initfun, boolean make_executable, boolean keep_runtime_options);
extern boolean save(char *filename, lispobj initfun, boolean prepend_runtime, boolean keep_runtime_options);

#endif
