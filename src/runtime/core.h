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

#include "runtime.h"

#define CORE_END 3840
#define CORE_NDIRECTORY 3861
#define CORE_VALIDATE 3845
#define CORE_VERSION 3860
#define CORE_MACHINE_STATE 3862
#define CORE_INITIAL_FUNCTION 3863

#define DYNAMIC_SPACE_ID (1)
#define STATIC_SPACE_ID (2)
#define READ_ONLY_SPACE_ID (3)

struct ndir_entry {
#ifndef alpha
	long identifier;
	long nwords;
	long data_page;
	long address;
	long page_count;
#else
	u32 identifier;
	u32 nwords;
	u32 data_page;
	u32 address;
	u32 page_count;
#endif
};

extern lispobj load_core_file(char *file);

#endif
