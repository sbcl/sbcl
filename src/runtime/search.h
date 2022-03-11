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

extern lispobj find_package(char*);
extern lispobj* find_symbol(char*, lispobj, unsigned int*); // Find in a package
extern struct symbol* lisp_symbol_from_tls_index(lispobj tls_index);
// Find via heap scan
extern lispobj* search_for_symbol(char *name, lispobj start, lispobj end, boolean);
lispobj *search_all_gc_spaces(void *pointer); // 'search.c' provides
lispobj *search_dynamic_space(void *pointer); // Provided by 'gencgc' or 'cheneygc'
lispobj *search_immobile_space(void *pointer);

#endif
