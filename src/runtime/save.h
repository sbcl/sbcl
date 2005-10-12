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
extern boolean save_to_filehandle(FILE *file, char *filename, lispobj initfun);
extern boolean save(char *filename, lispobj initfun);

#endif
