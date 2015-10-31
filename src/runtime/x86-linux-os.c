/*
 * The x86 Linux incarnation of arch-dependent OS-dependent routines.
 * See also "linux-os.c".
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
#include <stddef.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

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

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <asm/ldt.h>
#include <sys/syscall.h>
#include <sys/mman.h>
#include <linux/version.h>
#include "thread.h"             /* dynamic_values_bytes */

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
#define user_desc  modify_ldt_ldt_s
#endif

static inline int modify_ldt(int func, void *ptr, unsigned long bytecount)
{
  return syscall(SYS_modify_ldt, func, ptr, bytecount);
}

#include "validate.h"
size_t os_vm_page_size;

u32 local_ldt_copy[LDT_ENTRIES*LDT_ENTRY_SIZE/sizeof(u32)];

/* This is never actually called, but it's great for calling from gdb when
 * users have thread-related problems that maintainers can't duplicate */

void debug_get_ldt()
{
    int n=modify_ldt (0, local_ldt_copy, sizeof local_ldt_copy);
    printf("%d bytes in ldt: print/x local_ldt_copy\n", n);
}

#ifdef LISP_FEATURE_SB_THREAD
pthread_mutex_t modify_ldt_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

int arch_os_thread_init(struct thread *thread) {
    stack_t sigstack;
#ifdef LISP_FEATURE_SB_THREAD
    struct user_desc ldt_entry = {
        1, 0, 0, /* index, address, length filled in later */
        1, MODIFY_LDT_CONTENTS_DATA, 0, 0, 0, 1
    };
    int n;
    thread_mutex_lock(&modify_ldt_lock);
    n=modify_ldt(0,local_ldt_copy,sizeof local_ldt_copy);
    /* get next free ldt entry */

    if(n) {
        u32 *p;
        for(n=0,p=local_ldt_copy;*p;p+=LDT_ENTRY_SIZE/sizeof(u32))
            n++;
    }
    ldt_entry.entry_number=n;
    ldt_entry.base_addr=(unsigned long) thread;
    ldt_entry.limit=dynamic_values_bytes;
    ldt_entry.limit_in_pages=0;
    if (modify_ldt (1, &ldt_entry, sizeof (ldt_entry)) != 0) {
        thread_mutex_unlock(&modify_ldt_lock);
        /* modify_ldt call failed: something magical is not happening */
        return 0;
    }
    __asm__ __volatile__ ("movw %w0, %%fs" : : "q"
                          ((n << 3) /* selector number */
                           + (1 << 2) /* TI set = LDT */
                           + 3)); /* privilege level */
    thread->tls_cookie=n;
    pthread_mutex_unlock(&modify_ldt_lock);

    if(n<0) return 0;
#ifdef LISP_FEATURE_GCC_TLS
    current_thread = thread;
#else
    pthread_setspecific(specials,thread);
#endif
#endif
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp=((void *) thread)+dynamic_values_bytes;
    sigstack.ss_flags=0;
    sigstack.ss_size = 32*SIGSTKSZ;
    if(sigaltstack(&sigstack,0)<0)
        lose("Cannot sigaltstack: %s\n",strerror(errno));
#endif
    return 1;
}

struct thread *debug_get_fs() {
    register u32 fs;
    __asm__ __volatile__ ("movl %%fs,%0" : "=r" (fs)  : );
    return (struct thread *)fs;
}

/* free any arch/os-specific resources used by thread, which is now
 * defunct.  Not called on live threads
 */

int arch_os_thread_cleanup(struct thread *thread) {
    struct user_desc ldt_entry = {
        0, 0, 0,
        0, MODIFY_LDT_CONTENTS_DATA, 0, 0, 0, 0
    };
    int result;

    ldt_entry.entry_number=thread->tls_cookie;
    thread_mutex_lock(&modify_ldt_lock);
    result = modify_ldt(1, &ldt_entry, sizeof (ldt_entry));
    thread_mutex_unlock(&modify_ldt_lock);
    return result;
}



/* KLUDGE: As of kernel 2.2.14 on Red Hat 6.2, there's code in the
 * <sys/ucontext.h> file to define symbolic names for offsets into
 * gregs[], but it's conditional on __USE_GNU and not defined, so
 * we need to do this nasty absolute index magic number thing
 * instead. */
os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case reg_EAX: return &context->uc_mcontext.gregs[11];
    case reg_ECX: return &context->uc_mcontext.gregs[10];
    case reg_EDX: return &context->uc_mcontext.gregs[9];
    case reg_EBX: return &context->uc_mcontext.gregs[8];
    case reg_ESP: return &context->uc_mcontext.gregs[7];
    case reg_EBP: return &context->uc_mcontext.gregs[6];
    case reg_ESI: return &context->uc_mcontext.gregs[5];
    case reg_EDI: return &context->uc_mcontext.gregs[4];
    default: return 0;
    }
    return &context->uc_mcontext.gregs[offset];
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[14]; /*  REG_EIP */
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[17]; /* REG_UESP */
}

os_context_register_t *
os_context_fp_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[6]; /* REG_EBP */
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    return ((((context->uc_mcontext.fpregs->cw) & 0xffff) ^ 0x3f) |
            (((context->uc_mcontext.fpregs->sw) & 0xffff) << 16));
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

void
os_restore_fp_control(os_context_t *context)
{
    if (context->uc_mcontext.fpregs)
        asm ("fldcw %0" : : "m" (context->uc_mcontext.fpregs->cw));
}

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t*)&context->uc_mcontext.fpregs->_st[offset];
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}
