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

static void
process_directory(int fd, long *ptr, int count)
{
    struct ndir_entry *entry;

    FSHOW((stderr, "/process_directory(..), count=%d\n", count));
    
    for (entry = (struct ndir_entry *) ptr; --count>= 0; ++entry) {

	long id = entry->identifier;
	long offset = os_vm_page_size * (1 + entry->data_page);
	os_vm_address_t addr =
	    (os_vm_address_t) (os_vm_page_size * entry->address);
	lispobj *free_pointer = (lispobj *) addr + entry->nwords;
	long len = os_vm_page_size * entry->page_count;
	
	if (len != 0) {
	    os_vm_address_t real_addr;
	    FSHOW((stderr, "/mapping %ld(0x%lx) bytes at 0x%lx\n",
		   (long)len, (long)len, addr));
	    real_addr = os_map(fd, offset, addr, len);
	    if (real_addr != addr) {
		lose("file mapped in wrong place! "
		     "(0x%08x != 0x%08lx)",
		     real_addr,
		     addr);
	    }
	}

	FSHOW((stderr, "/space id = %d, free pointer = 0x%08x\n",
	       id, (long)free_pointer));

	switch (id) {
	case DYNAMIC_SPACE_ID:
#ifdef GENCGC	  
	    if (addr != (os_vm_address_t)DYNAMIC_SPACE_START) {
	        fprintf(stderr, "in core: 0x%x - in runtime: 0x%x \n",
			addr, (os_vm_address_t)DYNAMIC_SPACE_START);
		lose("core/runtime address mismatch: DYNAMIC_SPACE_START");
	    }
#else
	    if ((addr != (os_vm_address_t)DYNAMIC_0_SPACE_START) &&
		(addr != (os_vm_address_t)DYNAMIC_1_SPACE_START)) {
		fprintf(stderr, "in core: 0x%x - in runtime: 0x%x or 0x%x\n",
			addr, (os_vm_address_t)DYNAMIC_0_SPACE_START,
			(os_vm_address_t)DYNAMIC_1_SPACE_START);
		lose("warning: core/runtime address mismatch: DYNAMIC_SPACE_START");
	    }
#endif
/* FIXME: Should the conditional here be reg_ALLOC instead of
 *   defined(__i386__)
 * ? */
#if defined(__i386__)
	    SetSymbolValue(ALLOCATION_POINTER, (lispobj)free_pointer);
#else
	    dynamic_space_free_pointer = free_pointer;
#endif
	    /* For stop-and-copy GC, this will be whatever the GC was
	     * using at the time. With GENCGC, this will always be
	     * space 0. (We checked above that for GENCGC,
	     * addr==DYNAMIC_SPACE_START.) */
	    current_dynamic_space = (lispobj *)addr;
	    break;
	case STATIC_SPACE_ID:
	    if (addr != (os_vm_address_t)STATIC_SPACE_START) {
		fprintf(stderr, "in core: 0x%p - in runtime: 0x%x\n",
			addr, (os_vm_address_t)STATIC_SPACE_START);
		lose("core/runtime address mismatch: STATIC_SPACE_START");
	    }
	    break;
	case READ_ONLY_SPACE_ID:
	    if (addr != (os_vm_address_t)READ_ONLY_SPACE_START) {
		fprintf(stderr, "in core: 0x%x - in runtime: 0x%x\n",
			addr, (os_vm_address_t)READ_ONLY_SPACE_START);
		lose("core/runtime address mismatch: READ_ONLY_SPACE_START");
	    }
	    break;
	default:
	    lose("unknown space ID %ld addr 0x%p", id);
	}
    }
}

lispobj
load_core_file(char *file)
{
    u32 *header, val, len, *ptr, remaining_len;
    int fd = open(file, O_RDONLY), count;

    lispobj initial_function = NIL;
    FSHOW((stderr, "/entering load_core_file(%s)\n", file));
    if (fd < 0) {
	fprintf(stderr, "could not open file \"%s\"\n", file);
	perror("open");
	exit(1);
    }

    header = calloc(os_vm_page_size / sizeof(u32), sizeof(u32));

    count = read(fd, header, os_vm_page_size);
    if (count < os_vm_page_size) {
	lose("premature end of core file");
    }
    SHOW("successfully read first page of core");

    ptr = header;
    val = *ptr++;

    if (val != CORE_MAGIC) {
	lose("invalid magic number in core: 0x%lx should have been 0x%x.",
	     val,
	     CORE_MAGIC);
    }
    SHOW("found CORE_MAGIC");

    while (val != CORE_END) {
	val = *ptr++;
	len = *ptr++;
	remaining_len = len - 2; /* (-2 to cancel the two ++ operations) */
	FSHOW((stderr, "/val=0x%ld, remaining_len=0x%ld\n",
	       (long)val, (long)remaining_len));

	switch (val) {

	case CORE_END:
	    SHOW("CORE_END case");
	    break;

	case CORE_VERSION:
	    SHOW("CORE_VERSION case");
	    if (*ptr != SBCL_CORE_VERSION_INTEGER) {
		lose("core file version (%d) != runtime library version (%d)",
		     *ptr,
		     SBCL_CORE_VERSION_INTEGER);
	    }
	    break;

	case CORE_NDIRECTORY:
	    SHOW("CORE_NDIRECTORY case");
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
	    SHOW("CORE_INITIAL_FUNCTION case");
	    initial_function = (lispobj)*ptr;
	    break;

	default:
	    lose("unknown core file entry: %ld", (long)val);
	}

	ptr += remaining_len;
	FSHOW((stderr, "/new ptr=%x\n", ptr));
    }
    SHOW("about to free(header)");
    free(header);
    SHOW("returning from load_core_file(..)");
    return initial_function;
}
