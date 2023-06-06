#ifdef LISP_FEATURE_SB_THREAD
#include <architecture/i386/table.h>
#include <i386/user_ldt.h>
#include <mach/mach_init.h>
#endif

#include "thread.h"
#include "validate.h"
#include "runtime.h"
#include "interrupt.h"
#include "x86-darwin-os.h"
#include "genesis/fdefn.h"
#include "gc.h" // for gencgc_handle_wp_violation

#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/mach_types.h>
#include <mach/sync_policy.h>
#include <mach/vm_region.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>
#include <sys/_types.h>
#include <sys/ucontext.h>
#include <pthread.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef LISP_FEATURE_SB_THREAD

pthread_mutex_t modify_ldt_lock = PTHREAD_MUTEX_INITIALIZER;

void set_data_desc_size(data_desc_t* desc, unsigned long size)
{
    desc->limit00 = (size - 1) & 0xffff;
    desc->limit16 = ((size - 1) >> 16) &0xf;
}

void set_data_desc_addr(data_desc_t* desc, void* addr)
{
    desc->base00 = (unsigned int)addr & 0xffff;
    desc->base16 = ((unsigned int)addr & 0xff0000) >> 16;
    desc->base24 = ((unsigned int)addr & 0xff000000) >> 24;
}

#endif

#ifdef LISP_FEATURE_SB_THREAD
void
arch_os_load_ldt(struct thread *thread)
{
    sel_t sel;

    sel.index = thread->tls_cookie;
    sel.rpl = USER_PRIV;
    sel.ti = SEL_LDT;

    __asm__ __volatile__ ("mov %0, %%fs" : : "r"(sel));
}
#endif

int arch_os_thread_init(struct thread *thread) {
#ifdef LISP_FEATURE_SB_THREAD
    int n;

    data_desc_t ldt_entry = { 0, 0, 0, DESC_DATA_WRITE,
                              3, 1, 0, DESC_DATA_32B, DESC_GRAN_BYTE, 0 };

    set_data_desc_addr(&ldt_entry, thread);
    set_data_desc_size(&ldt_entry, dynamic_values_bytes);

    ignore_value(mutex_acquire(&modify_ldt_lock));
    n = i386_set_ldt(LDT_AUTO_ALLOC, (union ldt_entry*) &ldt_entry, 1);

    if (n < 0) {
        perror("i386_set_ldt");
        lose("unexpected i386_set_ldt(..) failure");
    }
    ignore_value(mutex_release(&modify_ldt_lock));

    thread->tls_cookie=n;
    arch_os_load_ldt(thread);
#endif
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    stack_t sigstack;

    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp    = calc_altstack_base(thread);
    sigstack.ss_flags = 0;
    sigstack.ss_size  = calc_altstack_size(thread);
    sigaltstack(&sigstack,0);
#endif
    return 1;                  /* success */
}

int arch_os_thread_cleanup(struct thread *thread) {
#if defined(LISP_FEATURE_SB_THREAD)
    int n = thread->tls_cookie;

    /* Set the %%fs register back to 0 and free the ldt by setting it
     * to NULL.
     */
    __asm__ __volatile__ ("mov %0, %%fs" : : "r"(0));
    ignore_value(mutex_acquire(&modify_ldt_lock));
    i386_set_ldt(n, NULL, 1);
    ignore_value(mutex_release(&modify_ldt_lock));
#endif
    return 1;                  /* success */
}

void
os_restore_fp_control(os_context_t *context)
{
    /* KLUDGE: The x87 FPU control word is some nasty bitfield struct
     * thing.  Rather than deal with that, just grab it as a 16-bit
     * integer. */
    unsigned short fpu_control_word =
        *((unsigned short *)&context->uc_mcontext->FS.FPU_FCW);
    /* reset exception flags and restore control flags on x87 FPU */
    asm ("fldcw %0" : : "m" (fpu_control_word));
}
