/*
 * A saved SBCL system is a .core file; the code here helps us accept
 * such a file as input.
 */
  
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

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>

#ifdef irix
#include <fcntl.h>
#include <stdlib.h>
#endif

#include "os.h"
#include "runtime.h"
#include "globals.h"
#include "core.h"
#include "arch.h"
#include "interr.h"
#include "sbcl.h"

static void process_directory(int fd, long *ptr, int count)
{
    struct ndir_entry *entry;

    FSHOW((stderr, "process_directory(..), count=%d\n", count));
    
    for (entry = (struct ndir_entry *) ptr; --count>= 0; ++entry) {

	long id = entry->identifier;
	long offset = CORE_PAGESIZE * (1 + entry->data_page);
	os_vm_address_t addr =
	    (os_vm_address_t) (CORE_PAGESIZE * entry->address);
	lispobj *free_pointer = (lispobj *) addr + entry->nwords;
	long len = CORE_PAGESIZE * entry->page_count;
	
	if (len != 0) {
	    os_vm_address_t real_addr;
	    FSHOW((stderr, "mapping %ld bytes at 0x%lx\n", len, addr));
	    real_addr = os_map(fd, offset, addr, len);
	    if (real_addr != addr) {
		lose("file mapped in wrong place! "
		     "(0x%08x != 0x%08lx)",
		     real_addr,
		     addr);
	    }
	}

	FSHOW((stderr, "space id = %d, free pointer = 0x%08x\n",
	       id, free_pointer));

	switch (id) {
	case DYNAMIC_SPACE_ID:
	    if (addr != (os_vm_address_t)DYNAMIC_SPACE_START) {
		lose("core/runtime address mismatch: DYNAMIC_SPACE_START");
	    }
#if defined(ibmrt) || defined(__i386__)
	    SetSymbolValue(ALLOCATION_POINTER, (lispobj)free_pointer);
#else
	    dynamic_space_free_pointer = free_pointer;
#endif
	    break;
	case STATIC_SPACE_ID:
	    if (addr != (os_vm_address_t)STATIC_SPACE_START) {
		lose("core/runtime address mismatch: STATIC_SPACE_START");
	    }
	    break;
	case READ_ONLY_SPACE_ID:
	    if (addr != (os_vm_address_t)READ_ONLY_SPACE_START) {
		lose("core/runtime address mismatch: READ_ONLY_SPACE_START");
	    }
	    break;
	default:
	    lose("unknown space ID %ld", id);
	}
    }
}

lispobj load_core_file(char *file)
{
    int fd = open(file, O_RDONLY), count;

    /* KLUDGE: This kind of conditionalization everywhere that 32-bit
     * ints are used is really nasty. It would be much nicer to define
     * a typedef like addr_as_int once and for all in each
     * architecture file, then use that everywhere. -- WHN 19990904 */
#ifndef alpha
    long header[CORE_PAGESIZE / sizeof(long)], val, len, *ptr;
    long remaining_len;
#else
    u32 header[CORE_PAGESIZE / sizeof(u32)], val, len, *ptr;
    u32 remaining_len;
#endif

    lispobj initial_function = NIL;

    if (fd < 0) {
	fprintf(stderr, "could not open file \"%s\"\n", file);
	perror("open");
	exit(1);
    }

    count = read(fd, header, CORE_PAGESIZE);
    if (count < CORE_PAGESIZE) {
	lose("premature end of core file");
    }

    ptr = header;
    val = *ptr++;

    if (val != CORE_MAGIC) {
	lose("invalid magic number in core: 0x%lx should have been 0x%x.",
	     val,
	     CORE_MAGIC);
    }

    while (val != CORE_END) {
	val = *ptr++;
	len = *ptr++;
	remaining_len = len - 2; /* (-2 to cancel the two ++ operations) */

	switch (val) {

	case CORE_END:
	    break;

	case CORE_VERSION:
	    if (*ptr != SBCL_CORE_VERSION_INTEGER) {
		lose("core file version (%d) != runtime library version (%d)",
		     *ptr,
		     SBCL_CORE_VERSION_INTEGER);
	    }
	    break;

	case CORE_NDIRECTORY:
	    process_directory(fd,
			      ptr,
#ifndef alpha
			      remaining_len / (sizeof(struct ndir_entry) /
					       sizeof(long))
#else
			      remaining_len / (sizeof(struct ndir_entry) /
					       sizeof(u32))
#endif
			      );
	    break;

	case CORE_INITIAL_FUNCTION:
	    initial_function = (lispobj)*ptr;
	    break;

	default:
	    lose("unknown core file entry: %ld", val);
	}

	ptr += remaining_len;
    }

    return initial_function;
}
