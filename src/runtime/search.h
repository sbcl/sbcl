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

#ifndef _SEARCH_H_
#define _SEARCH_H_

#include <stdbool.h>
lispobj get_package_by_id(int);
extern lispobj* find_symbol(char*, lispobj); // Find in a package
extern struct symbol* lisp_symbol_from_tls_index(lispobj tls_index);
// Find via heap scan
extern lispobj* search_for_symbol(char *name, lispobj start, lispobj end, bool);
lispobj *search_all_gc_spaces(void *pointer); // 'search.c' provides
lispobj *search_dynamic_space(void *pointer); // Provided by 'gencgc' or 'cheneygc'
lispobj *search_immobile_space(void *pointer);
int bsearch_greatereql_uword(uword_t item, uword_t* array, int nelements);
int bsearch_lesseql_uword(uword_t item, uword_t* array, int nelements);
#ifdef LISP_FEATURE_64_BIT
int bsearch_greatereql_uint32(uint32_t item, uint32_t* array, int nelements);
int bsearch_lesseql_uint32(uint32_t item, uint32_t* array, int nelements);
#endif

#endif
