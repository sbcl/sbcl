#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/file.h>

#include <unistd.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/utsname.h>

#include "os.h"
#include "arch.h"
#include "interr.h"
#include "interrupt.h"
#include "globals.h"
#include "validate.h"
#include "sbcl.h"
#include "target-arch-os.h"

#define OS_VM_DEFAULT_PAGESIZE 8192

long os_vm_page_size=(-1);
static long os_real_page_size=(-1);

static os_vm_size_t real_page_size_difference=0;

/* So, this sucks. Versions of Solaris prior to 8 (SunOS releases
   earlier than 5.8) do not support MAP_ANON passed as a flag to
   mmap(). However, we would like SBCL compiled on SunOS 5.7 but
   running on 5.8 to use MAP_ANON, but because of C's lack of
   introspection at runtime, we can't grab the right value because
   it's stuffed in a header file somewhere. We can, however, hardcode
   it, and test at runtime for whether to use it... -- CSR, 2002-05-06 */
int KLUDGE_MAYBE_MAP_ANON = 0x0;

void os_init(void)
{
    struct utsname name;
    int major_version;
    int minor_version;
    
    uname(&name);
    major_version = atoi(name.release);
    if (major_version != 5) {
	lose("sunos major version=%d (which isn't 5!)", major_version);
    }
    minor_version = atoi(name.release+2);
    if (minor_version == 8) {
	KLUDGE_MAYBE_MAP_ANON = 0x100;
    }
    if (minor_version > 8) {
	FSHOW((stderr, "os_init: Solaris version greater than 8?\nUnknown MAP_ANON behaviour.\n"));
    }

    /* I do not understand this at all. FIXME. */
    os_vm_page_size = os_real_page_size = sysconf(_SC_PAGESIZE);

    if(os_vm_page_size>OS_VM_DEFAULT_PAGESIZE){
	fprintf(stderr,"os_init: Pagesize too large (%d > %d)\n",
		os_vm_page_size,OS_VM_DEFAULT_PAGESIZE);
	exit(1);
    } else {
	/*
	 * we do this because there are apparently dependencies on
	 * the pagesize being OS_VM_DEFAULT_PAGESIZE somewhere...
	 * but since the OS doesn't know we're using this restriction,
	 * we have to grovel around a bit to enforce it, thus anything
	 * that uses real_page_size_difference.
	 */
	/* FIXME: Is this still true? */
	real_page_size_difference=OS_VM_DEFAULT_PAGESIZE-os_vm_page_size;
	os_vm_page_size=OS_VM_DEFAULT_PAGESIZE;
    }
}

os_vm_address_t os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_NORESERVE | KLUDGE_MAYBE_MAP_ANON;
    
    if (addr) 
	flags |= MAP_FIXED;
    
    addr = mmap(addr, len, 
		OS_VM_PROT_ALL, 
		flags, 
		-1, 0);
    if (addr == MAP_FAILED) {
	perror("mmap");
	lose ("Error in mmap(..)");
    }
    
    return addr;
}

void os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if(munmap((void*) addr, len) == -1)
	perror("munmap");
}



os_vm_address_t 
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{

    addr = mmap(addr, len,
		OS_VM_PROT_ALL,
		MAP_PRIVATE | MAP_FIXED,
		fd, (off_t) offset);

    if (addr == MAP_FAILED) {
	perror("mmap");
	lose("Unexpedted mmap(..) failure");
    }
  
    return addr;
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if(mprotect((void*)address, length, prot) == -1) {
	perror("mprotect");
    }
}

static boolean in_range_p(os_vm_address_t a, lispobj sbeg, size_t slen)
{
    char* beg = (char*) sbeg;
    char* end = (char*) sbeg + slen;
    char* adr = (char*) a;
    return (adr >= beg && adr < end);
}

boolean is_valid_lisp_addr(os_vm_address_t addr)
{
    /* Old CMUCL comment:
       
       Just assume address is valid if it lies within one of the known
       spaces.  (Unlike sunos-os which keeps track of every valid page.) */
    return (   in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE)
	       || in_range_p(addr, STATIC_SPACE_START   , STATIC_SPACE_SIZE   )
	       || in_range_p(addr, DYNAMIC_0_SPACE_START, DYNAMIC_SPACE_SIZE  )
	       || in_range_p(addr, DYNAMIC_1_SPACE_START, DYNAMIC_SPACE_SIZE  )
	       || in_range_p(addr, CONTROL_STACK_START  , CONTROL_STACK_SIZE  )
	       || in_range_p(addr, BINDING_STACK_START  , BINDING_STACK_SIZE  ));
}



#if defined GENCGC

#error "GENCGC is not yet supported (presumably on x86 solaris?)"

#else

static void
sigsegv_handler(int signal, siginfo_t *info, void* void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    os_vm_address_t addr;

    addr = arch_get_bad_addr(signal, info, context);
	/* There's some complicated recovery code in linux-os.c here
	   that I'm currently too confused to understand. Fixme. */
    if(!interrupt_maybe_gc(signal, info, context)) {
	interrupt_handle_now(signal, info, context);
    }
}

#endif

void
os_install_interrupt_handlers()
{
    undoably_install_low_level_interrupt_handler(SIGSEGV,sigsegv_handler);
}
