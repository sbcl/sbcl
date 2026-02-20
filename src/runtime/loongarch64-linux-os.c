/*
 * This is the LoongArch Linux incarnation of arch-dependent OS-dependent
 * routines. See also "linux-os.c".
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

#ifdef __linux__
#else
#include <sys/cachectl.h>
#endif
#include <sys/param.h>
#include <sys/file.h>
#include "genesis/sbcl.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "validate.h"

#include <stdint.h>
#include <stddef.h>
#include <sys/ucontext.h>   /* for ucontext_t / mcontext_t */
#include <asm/sigcontext.h> /* for sctx_info, FPU_CTX_MAGIC, struct fpu_context */


#define EXTCTX_SCAN_MAX_BYTES 16384

int
arch_os_thread_init(struct thread *thread)
{
    return 1;                  /* success */
}

int
arch_os_thread_cleanup(struct thread *thread)
{
    return 1;                  /* success */
}

os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t*)&(context->uc_mcontext.__gregs[offset]);
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return os_context_register_addr(context, reg_LIP);
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

void
os_restore_fp_control(os_context_t *context)
{
    /* FIXME: Implement. */
}

os_context_register_t *
os_context_float_register_addr(os_context_t *uc, int offset)
{
    mcontext_t *mc = &uc->uc_mcontext;

    uint8_t *p = (uint8_t *)&mc->__extcontext[0];
    size_t remaining = EXTCTX_SCAN_MAX_BYTES;

    while (remaining >= sizeof(struct sctx_info)) {
        struct sctx_info *hdr = (struct sctx_info *)p;

        if (hdr->size < sizeof(struct sctx_info)) break;

        if (hdr->magic == FPU_CTX_MAGIC) {
            if (hdr->size < (sizeof(struct sctx_info) + sizeof(struct fpu_context)))
                return NULL;

            uintptr_t base = ((uintptr_t)p + sizeof(struct sctx_info) + 15) & ~((uintptr_t)15);
            struct fpu_context *fpu = (struct fpu_context *)base;
            return (os_context_register_t *)&(fpu->regs[offset]);
        }

        size_t sz = hdr->size;
        if (sz > remaining) break;
        p += sz;
        remaining -= sz;
    }
    return NULL;
}


void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    os_vm_address_t end_address
        = (os_vm_address_t)(((uintptr_t) address) + length);
#ifndef LISP_FEATURE_OS_PROVIDES_FLUSH_ICACHE
        __builtin___clear_cache(address, end_address);
#endif
}

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
int _stat(const char *pathname, struct stat *sb) {return stat(pathname, sb); }
int _lstat(const char *pathname, struct stat *sb) { return lstat(pathname, sb); }
int _fstat(int fd, struct stat *sb) { return fstat(fd, sb); }
