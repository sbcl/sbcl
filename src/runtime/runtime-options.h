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

#ifndef _RUNTIME_OPTIONS_INCLUDED_
#define _RUNTIME_OPTIONS_INCLUDED_

#include "os.h"

#define RUNTIME_OPTIONS_MAGIC 0x31EBF355
/* 1 for magic, 1 for boolean, 2 for struct runtime_options fields */
#define RUNTIME_OPTIONS_WORDS (1 + 1 + 2)

struct runtime_options {
    os_vm_size_t dynamic_space_size;
    os_vm_size_t thread_control_stack_size;
};

/* saved runtime path computed from argv[0] */
extern char *saved_runtime_path;

#endif
