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

#ifndef _PRINT_H_
#define _PRINT_H_

#include "sbcl.h"
#include "runtime.h"

extern void print(lispobj obj);
extern void brief_print(lispobj obj);
extern void reset_printer(void);
#include "genesis/vector.h"
#include <stdio.h>
extern void safely_show_lstring(struct vector*, int, FILE*);

#endif
