#include <signal.h>
#include "sbcl.h"
#include "runtime.h"
#include "thread.h"


#ifdef LISP_FEATURE_SB_THREAD
#ifdef LISP_FEATURE_DARWIN
#include <architecture/i386/table.h>
#include <i386/user_ldt.h>
#include <mach/mach_init.h>
#else
#include <machine/segments.h>
#include <machine/sysarch.h>
#endif /* LISP_FEATURE_DARWIN */
#endif

#if defined(LISP_FEATURE_FREEBSD)
#include "machine/npx.h"
#endif

/* KLUDGE: There is strong family resemblance in the signal context
 * stuff in FreeBSD and OpenBSD, but in detail they're different in
 * almost every line of code. It would be nice to find some way to
 * factor out the commonality better; failing that, it might be best
 * just to split this generic-BSD code into one variant for each BSD.
 *
 * KLUDGE II: this split has begun with the addition of the Darwin BSD
 * flavour, with the cross-architecture complications that this
 * entails; unfortunately, currently the situation is worse, not
 * better, than in the above paragraph. */

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(LISP_FEATURE_DARWIN)
int *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case  0:
        return CONTEXT_ADDR_FROM_STEM(eax);
    case  2:
        return CONTEXT_ADDR_FROM_STEM(ecx);
    case  4:
        return CONTEXT_ADDR_FROM_STEM(edx);
    case  6:
        return CONTEXT_ADDR_FROM_STEM(ebx);
    case  8:
        return CONTEXT_ADDR_FROM_STEM(esp);
    case 10:
        return CONTEXT_ADDR_FROM_STEM(ebp);
    case 12:
        return CONTEXT_ADDR_FROM_STEM(esi);
    case 14:
        return CONTEXT_ADDR_FROM_STEM(edi);
    default:
        return 0;
    }
}

int *
os_context_sp_addr(os_context_t *context)
{
    return CONTEXT_ADDR_FROM_STEM(esp);
}

#endif /* __FreeBSD__ || __OpenBSD__ */

#ifdef __NetBSD__
int *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case  0:
        return CONTEXT_ADDR_FROM_STEM(EAX);
    case  2:
        return CONTEXT_ADDR_FROM_STEM(ECX);
    case  4:
        return CONTEXT_ADDR_FROM_STEM(EDX);
    case  6:
        return CONTEXT_ADDR_FROM_STEM(EBX);
    case  8:
        return CONTEXT_ADDR_FROM_STEM(ESP);
    case 10:
        return CONTEXT_ADDR_FROM_STEM(EBP);
    case 12:
        return CONTEXT_ADDR_FROM_STEM(ESI);
    case 14:
        return CONTEXT_ADDR_FROM_STEM(EDI);
    case 16:
        return CONTEXT_ADDR_FROM_STEM(UESP);
    default:
        return 0;
    }
}

int *
os_context_sp_addr(os_context_t *context)
{
    return &(_UC_MACHINE_SP(context));
}

#endif  /* __NetBSD__ */


/* FIXME: If this can be a no-op on BSD/x86, then it
 * deserves a more precise name.
 *
 * (Perhaps os_prepare_data_area_to_be_executed()?) */
void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

/* Note: the Darwin versions of arch_os_thread_init found in
 * x86-darwin-os.c
*/
#if !defined(LISP_FEATURE_DARWIN)

#ifdef LISP_FEATURE_SB_THREAD

void set_data_desc_size(struct segment_descriptor* desc, unsigned long size)
{
    desc->sd_lolimit = (size - 1) & 0xffff;
    desc->sd_hilimit = ((size - 1) >> 16) &0xf;
}

void set_data_desc_addr(struct segment_descriptor* desc, void* addr)
{
    desc->sd_lobase = (unsigned int)addr & 0xffffff;
    desc->sd_hibase = ((unsigned int)addr & 0xff000000) >> 24;
}

#endif

int arch_os_thread_init(struct thread *thread) {

#ifdef LISP_FEATURE_SB_THREAD
    int n;
    int sel;

    struct segment_descriptor ldt_entry = { 0, 0, SDT_MEMRW, SEL_UPL, 1,
                                            0, 0, 1, 0, 0 };

    set_data_desc_addr(&ldt_entry, (unsigned long) thread);
    set_data_desc_size(&ldt_entry, dynamic_values_bytes);

    n = i386_set_ldt(LDT_AUTO_ALLOC, (union descriptor*) &ldt_entry, 1);
    if (n < 0) {
        perror("i386_set_ldt");
        lose("unexpected i386_set_ldt(..) failure\n");
    }
    FSHOW_SIGNAL((stderr, "/ TLS: Allocated LDT %x\n", n));
    sel =  LSEL(n, SEL_UPL);
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
    i386_set_ldt(n, NULL, 1);
#endif

    return 1;                  /* success */
}

#endif /* !LISP_FEATURE_DARWIN */

void
os_restore_fp_control(os_context_t *context)
{
    struct envxmm *ex = (struct envxmm*)(&context->uc_mcontext.mc_fpstate);
    asm ("fldcw %0" : : "m" (ex->en_cw));
}
