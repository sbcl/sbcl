

#ifdef LISP_FEATURE_SB_THREAD
#include <architecture/i386/table.h>
#include <i386/user_ldt.h>
#include <mach/mach_init.h>
#endif

#include "thread.h"
#include "x86-darwin-os.h"

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

int arch_os_thread_init(struct thread *thread) {
#ifdef LISP_FEATURE_SB_THREAD
    int n;
    sel_t sel;

    data_desc_t ldt_entry = { 0, 0, 0, DESC_DATA_WRITE,
                              3, 1, 0, DESC_DATA_32B, DESC_GRAN_BYTE, 0 };

    set_data_desc_addr(&ldt_entry, (unsigned long) thread);
    set_data_desc_size(&ldt_entry, dynamic_values_bytes);

    thread_mutex_lock(&modify_ldt_lock);
    n = i386_set_ldt(LDT_AUTO_ALLOC, (union ldt_entry*) &ldt_entry, 1);

    if (n < 0) {
        perror("i386_set_ldt");
        lose("unexpected i386_set_ldt(..) failure\n");
    }
    thread_mutex_unlock(&modify_ldt_lock);

    FSHOW_SIGNAL((stderr, "/ TLS: Allocated LDT %x\n", n));
    sel.index = n;
    sel.rpl = USER_PRIV;
    sel.ti = SEL_LDT;

    __asm__ __volatile__ ("mov %0, %%fs" : : "r"(sel));

    thread->tls_cookie=n;
    pthread_setspecific(specials,thread);
#endif

#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    stack_t sigstack;

    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp=((void *) thread)+dynamic_values_bytes;
    sigstack.ss_flags=0;
    sigstack.ss_size = 32*SIGSTKSZ;
    sigaltstack(&sigstack,0);
#endif
    return 1;                  /* success */
}

int arch_os_thread_cleanup(struct thread *thread) {
#if defined(LISP_FEATURE_SB_THREAD)
    int n = thread->tls_cookie;

    /* Set the %%fs register back to 0 and free the the ldt
     * by setting it to NULL.
     */
    FSHOW_SIGNAL((stderr, "/ TLS: Freeing LDT %x\n", n));

    __asm__ __volatile__ ("mov %0, %%fs" : : "r"(0));
    thread_mutex_lock(&modify_ldt_lock);
    i386_set_ldt(n, NULL, 1);
    thread_mutex_unlock(&modify_ldt_lock);
#endif
    return 1;                  /* success */
}

