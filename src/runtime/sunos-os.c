#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/file.h>

#include <unistd.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/utsname.h>

#include "sbcl.h"
#include "os.h"
#include "arch.h"
#include "interr.h"
#include "interrupt.h"
#include "globals.h"
#include "validate.h"
#include "target-arch-os.h"

#ifdef LISP_FEATURE_X86
#include "genesis/static-symbols.h"
#include "genesis/fdefn.h"
#endif

#ifdef LISP_FEATURE_GENCGC
#include "gencgc-internal.h"
#endif

os_vm_size_t os_vm_page_size=0;

void
os_init(char *argv[], char *envp[])
{
    /*
     * historically, this used sysconf to select the runtime page size
     * per recent changes on other arches and discussion on sbcl-devel,
     * however, this is not necessary -- the VM page size need not match
     * the OS page size (and the default backend page size has been
     * ramped up accordingly for efficiency reasons).
     */
    os_vm_page_size = BACKEND_PAGE_BYTES;
}

os_vm_address_t os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_NORESERVE | MAP_ANON;
    if (addr)
        flags |= MAP_FIXED;

    addr = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);

    if (addr == MAP_FAILED) {
        perror("mmap");
        lose ("Error in mmap(..)\n");
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
        lose("Unexpedted mmap(..) failure\n");
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

    /* FIXME: this looks like a valid definition for all targets with
       cheney-gc; it may not be impressively smart (witness the
       comment above) but maybe associating these functions with the
       GC rather than the OS would be a maintainability win.  -- CSR,
       2003-04-04 */
    struct thread *th;
    if(in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE) ||
       in_range_p(addr, STATIC_SPACE_START   , STATIC_SPACE_SIZE) ||
#ifdef LISP_FEATURE_GENCGC
       in_range_p(addr, DYNAMIC_SPACE_START  , dynamic_space_size)
#else
       in_range_p(addr, DYNAMIC_0_SPACE_START, dynamic_space_size) ||
       in_range_p(addr, DYNAMIC_1_SPACE_START, dynamic_space_size)
#endif
       )
        return 1;
    for_each_thread(th) {
        if((th->control_stack_start <= addr) && (addr < th->control_stack_end))
            return 1;
        if(in_range_p(addr, th->binding_stack_start, BINDING_STACK_SIZE))
            return 1;
    }
    return 0;
}


#if defined LISP_FEATURE_GENCGC

void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    void* fault_addr = (void*)info->si_addr;

    if (!gencgc_handle_wp_violation(fault_addr))
        if(!handle_guard_page_triggered(context, fault_addr))
            lisp_memory_fault_error(context, fault_addr);
}

#else

static void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    os_vm_address_t addr = arch_get_bad_addr(signal, info, context);

    if (!cheneygc_handle_wp_violation(context, addr)) {
        if (!handle_guard_page_triggered(context,addr))
            lisp_memory_fault_error(context, addr);
    }
}

#endif

void
os_install_interrupt_handlers()
{
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
                                                 sigsegv_handler);

#ifdef LISP_FEATURE_SB_THREAD
    undoably_install_low_level_interrupt_handler(SIG_STOP_FOR_GC,
                                                 sig_stop_for_gc_handler);
#endif
}

char *
os_get_runtime_executable_path(int external)
{
    char path[] = "/proc/self/object/a.out";

    if (external || access(path, R_OK) == -1)
        return NULL;

    return copied_string(path);
}

