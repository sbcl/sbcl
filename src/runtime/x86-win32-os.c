/*
 * The x86 Win32 incarnation of arch-dependent OS-dependent routines.
 * See also "win32-os.c".
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

#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "sbcl.h"

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include "thread.h"             /* dynamic_values_bytes */


#include "validate.h"
size_t os_vm_page_size;

int arch_os_thread_init(struct thread *thread) {
    {
        //unsigned long cur_stack_base;
        //unsigned long cur_stack_end;
        void *cur_stack_end;

        //asm volatile ("movl %%fs:8,%0": "=r" (cur_stack_base));
        //      asm volatile ("movl %%fs:4,%0": "=r" (cur_stack_end));

        asm volatile ("movl %%fs:0,%0": "=r" (cur_stack_end));

        //      fprintf(stderr, "#x%08lx #x%08lx.\n", cur_stack_base, cur_stack_end);

        //if (cur_stack_base > thread->control_stack_start) {
        //    cur_stack_base = thread->control_stack_start;
        //}

        //if (cur_stack_end < thread->control_stack_end) {
        //    cur_stack_end = thread->control_stack_end;
        //}

        //      fprintf(stderr, "#x%08lx #x%08lx.\n", cur_stack_base, cur_stack_end);
        //fflush(stderr);

        //getchar();

        //asm volatile ("movl %0,%%fs:8": : "r" (cur_stack_base));
        //asm volatile ("movl %0,%%fs:4": : "r" (cur_stack_end));

        thread->control_stack_end = cur_stack_end;
    }

#ifdef LISP_FEATURE_SB_THREAD
    /* this must be called from a function that has an exclusive lock
     * on all_threads
     */
    struct user_desc ldt_entry = {
        1, 0, 0, /* index, address, length filled in later */
        1, MODIFY_LDT_CONTENTS_DATA, 0, 0, 0, 1
    };
    int n;
    get_spinlock(&modify_ldt_lock,thread);
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
        modify_ldt_lock=0;
        /* modify_ldt call failed: something magical is not happening */
        return -1;
    }
    __asm__ __volatile__ ("movw %w0, %%fs" : : "q"
                          ((n << 3) /* selector number */
                           + (1 << 2) /* TI set = LDT */
                           + 3)); /* privilege level */
    thread->tls_cookie=n;
    modify_ldt_lock=0;

    if(n<0) return 0;
#endif

    return 1;
}

/* free any arch/os-specific resources used by thread, which is now
 * defunct.  Not called on live threads
 */

int arch_os_thread_cleanup(struct thread *thread) {
    return 0;
}

os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case reg_EAX: return &context->Eax;
    case reg_ECX: return &context->Ecx;
    case reg_EDX: return &context->Edx;
    case reg_EBX: return &context->Ebx;
    case reg_ESP: return &context->Esp;
    case reg_EBP: return &context->Ebp;
    case reg_ESI: return &context->Esi;
    case reg_EDI: return &context->Edi;
    default: return 0;
    }
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &context->Eip; /*  REG_EIP */
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return &context->Esp; /* REG_UESP */
}

os_context_register_t *
os_context_fp_addr(os_context_t *context)
{
    return &context->Ebp; /* REG_EBP */
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    return ((((context->FloatSave.ControlWord) & 0xffff) ^ 0x3f) |
            (((context->FloatSave.StatusWord) & 0xffff) << 16));
}

void
os_restore_fp_control(os_context_t *context)
{
    asm ("fldcw %0" : : "m" (context->FloatSave.ControlWord));
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}
