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

#ifndef _ALLOC_H_
#define _ALLOC_H_

#include "sbcl.h"
#include "runtime.h"
#include "gc-internal.h"
#include "genesis/sap.h"

#define DX_ALLOC_SAP(var_name, ptr)                                        \
lispobj var_name;                                                          \
struct sap _dx_##var_name __attribute__ ((aligned (N_WORD_BYTES * 2)));                  \
do {                                                                       \
    _dx_##var_name.header = (1 << 8) | SAP_WIDETAG;                        \
    _dx_##var_name.pointer = (char *)(ptr);                                \
    var_name = make_lispobj(&_dx_##var_name, OTHER_POINTER_LOWTAG); \
} while (0)

#endif /* _ALLOC_H_ */
