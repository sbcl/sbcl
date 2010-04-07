/*
 * This file (along with os.h) exports an OS-independent interface to
 * the operating system VM facilities. Surprise surprise, this
 * interface looks a lot like the Mach interface (but simpler in some
 * places). For some operating systems, a subset of these functions
 * will have to be emulated.
 *
 * This is the OSF/1 version, based on the Linux version, itself based
 * on the OSF1 version from CMUCL by Sean Hallgren.  Now _there's_
 * a metacircularity for you ...
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
#include <sys/param.h>
#include <sys/file.h>
#include "sbcl.h"
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include <sys/socket.h>
#include <sys/utsname.h>
#include <errno.h>
#include <sys/sysinfo.h>
#include <sys/proc.h>
#include <sys/mman.h>
#include <machine/hal_sysinfo.h>

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#include "validate.h"
size_t os_vm_page_size;



void
os_init(char *argv[], char *envp[])
{
    os_vm_page_size = getpagesize();
}


os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE|MAP_ANONYMOUS;
    if (addr) flags |= MAP_FIXED;
    else  flags |= MAP_VARIABLE;

    if((addr=mmap(addr,len,OS_VM_PROT_ALL,flags,-1,0)) == (os_vm_address_t) -1)
        perror("mmap");

    return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (munmap(addr,len) == -1) {
        perror("munmap");
    }
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    addr = mmap(addr, len,
                OS_VM_PROT_ALL,
                MAP_PRIVATE | MAP_FILE | MAP_FIXED,
                fd, (off_t) offset);

    if (addr == MAP_FAILED) {
        perror("mmap");
        lose("unexpected mmap(..) failure\n");
    }

    return addr;
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect(address, length, prot) == -1) {
        perror("mprotect");
    }
}

boolean
is_valid_lisp_addr(os_vm_address_t addr)
{
    int ret;
    os_vm_address_t newaddr;
    newaddr=os_trunc_to_page(addr);
    if((ret=mvalid(newaddr,newaddr-addr+4,OS_VM_PROT_ALL)) == 0)
        return TRUE;
    else if(errno==EINVAL)
        perror("mvalid");
    return FALSE;
}

/*
 * any OS-dependent special low-level handling for signals
 */


static void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    os_vm_address_t addr = arch_get_bad_addr(signal,info,context);

    if (addr != NULL &&
        *os_context_register_addr(context,reg_ALLOC) & (1L<<63)){
        /* this is lifted from linux-os.c, so violates OOAO */
        *os_context_register_addr(context,reg_ALLOC) -= (1L<<63);
        interrupt_handle_pending(context);
    } else if (!cheneygc_handle_wp_violation(context, addr)) {
        if(!handle_guard_page_triggered(context,addr))
            lisp_memory_fault_error(context, addr);
    }
}


void
os_install_interrupt_handlers(void)
{
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
                                                 sigsegv_handler);
}

char *
os_get_runtime_executable_path(int external)
{
    return NULL;
}
