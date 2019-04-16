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
    core_entry_elt_t address;
    core_entry_elt_t page_count;
};
#define NDIR_ENTRY_LENGTH (sizeof (struct ndir_entry)/sizeof (core_entry_elt_t))

#define RUNTIME_OPTIONS_MAGIC 0x31EBF355
/* 1 for magic, 1 for core entry size in words, 3 for struct memsize_options fields
 * excluding the 'present_in_core' field */
#define RUNTIME_OPTIONS_WORDS (1 + 1 + 3)

struct memsize_options {
    os_vm_size_t dynamic_space_size;
    os_vm_size_t thread_control_stack_size;
    os_vm_size_t thread_tls_bytes;
    int present_in_core;
};

extern lispobj load_core_file(char *file, os_vm_offset_t file_offset,
                              int merge_core_pages);
extern os_vm_offset_t search_for_embedded_core(char *filename,
                                               struct memsize_options *memsize_options);

/* arbitrary string identifying this build, embedded in .core files to
 * prevent people mismatching a runtime built e.g. with :SB-SHOW
 * against a .core built without :SB-SHOW (or against various grosser
 * mismatches, e.g. a .core built with an old version of the code
 * against a runtime with patches which add new C code) */
extern unsigned char build_id[];

#endif
