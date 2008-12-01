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

#ifdef LISP_FEATURE_GENCGC
extern lispobj *general_alloc(long bytes, int page_type_flag);
#endif

extern lispobj alloc_cons(lispobj car, lispobj cdr);
extern lispobj alloc_number(long n);
extern lispobj alloc_string(char *str);
extern lispobj alloc_sap(void *ptr);
extern lispobj alloc_base_string(char *str);
extern lispobj alloc_code_object(unsigned boxed, unsigned unboxed);

#endif /* _ALLOC_H_ */
