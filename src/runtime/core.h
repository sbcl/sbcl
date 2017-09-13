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

#ifndef _CORE_H_
#define _CORE_H_

#include "sbcl.h"
#include "runtime.h"

#ifdef LISP_FEATURE_ALPHA
typedef u32 core_entry_elt_t;
#else
typedef sword_t core_entry_elt_t;
#endif

struct ndir_entry {
    core_entry_elt_t identifier;
    core_entry_elt_t nwords;
    core_entry_elt_t data_page;
    core_entry_elt_t address; /* expressed in units of KiB */
    core_entry_elt_t page_count;
};
#define NDIR_ENTRY_LENGTH (sizeof (struct ndir_entry)/sizeof (core_entry_elt_t))

/* Tri-state flag to determine whether we attempt to mark pages
 * as targets for virtual memory deduplication (ala MADV_MERGEABLE
 * on Linux).
 *
 * 1: Yes
 * 0: No
 * -1: default, yes for compressed cores, no otherwise.
 */
extern int merge_core_pages;

extern lispobj load_core_file(char *file, os_vm_offset_t offset);
extern os_vm_offset_t search_for_embedded_core(char *file);

/* arbitrary string identifying this build, embedded in .core files to
 * prevent people mismatching a runtime built e.g. with :SB-SHOW
 * against a .core built without :SB-SHOW (or against various grosser
 * mismatches, e.g. a .core built with an old version of the code
 * against a runtime with patches which add new C code) */
extern unsigned char build_id[];

#endif
