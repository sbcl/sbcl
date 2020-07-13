#include <signal.h>
#include <stdio.h>
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

#if defined(LISP_FEATURE_FREEBSD) || defined(LISP_FEATURE_DRAGONFLY)
#include "machine/npx.h"
#endif

#if defined(LISP_FEATURE_OPENBSD)
#include <machine/npx.h>
#include <stddef.h>
#include "openbsd-sigcontext.h"
#ifdef OS_OPENBSD_FPSTATE_IN_SIGFRAME
# include <machine/frame.h>
#endif
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

#if defined(LISP_FEATURE_FREEBSD) || defined(__OpenBSD__) || defined(LISP_FEATURE_DARWIN) || defined(__DragonFly__)
int *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case  0:
        return (int *)CONTEXT_ADDR_FROM_STEM(eax);
    case  2:
        return (int *)CONTEXT_ADDR_FROM_STEM(ecx);
    case  4:
        return (int *)CONTEXT_ADDR_FROM_STEM(edx);
    case  6:
        return (int *)CONTEXT_ADDR_FROM_STEM(ebx);
    case  8:
        return (int *)CONTEXT_ADDR_FROM_STEM(esp);
    case 10:
        return (int *)CONTEXT_ADDR_FROM_STEM(ebp);
    case 12:
        return (int *)CONTEXT_ADDR_FROM_STEM(esi);
    case 14:
        return (int *)CONTEXT_ADDR_FROM_STEM(edi);
    default:
        return 0;
    }
}

int *
os_context_sp_addr(os_context_t *context)
{
    return (int *)CONTEXT_ADDR_FROM_STEM(esp);
}

int *
os_context_fp_addr(os_context_t *context)
{
    return (int *)CONTEXT_ADDR_FROM_STEM(ebp);
}

#endif /* LISP_FEATURE_FREEBSD || __OpenBSD__ || __DragonFly__ */

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

int *os_context_pc_addr(os_context_t *context)
{
#if defined(LISP_FEATURE_FREEBSD) || defined(__DragonFly__)
    return CONTEXT_ADDR_FROM_STEM(eip);
#elif defined __OpenBSD__
    return CONTEXT_ADDR_FROM_STEM(pc);
#elif defined __NetBSD__
    return CONTEXT_ADDR_FROM_STEM(EIP);
#elif defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_X86)
    return (int *)CONTEXT_ADDR_FROM_STEM(eip);
#elif defined LISP_FEATURE_DARWIN
    return &context->uc_mcontext->ss.srr0;
#else
#error unsupported BSD variant
#endif
}

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

#ifdef LISP_FEATURE_SB_THREAD
void
arch_os_load_ldt(struct thread *thread)
{
    int sel = LSEL(thread->tls_cookie, SEL_UPL);
    unsigned int fs = rfs();

    /* Load FS only if it's necessary.  Modifying a selector
     * causes privilege checking and it takes long time. */
    if (fs != sel)
        load_fs(sel);
}
#endif

int arch_os_thread_init(struct thread *thread) {

#ifdef LISP_FEATURE_SB_THREAD
    int n;

    struct segment_descriptor ldt_entry = { 0, 0, SDT_MEMRW, SEL_UPL, 1,
                                            0, 0, 1, 0, 0 };

    set_data_desc_addr(&ldt_entry, thread);
    set_data_desc_size(&ldt_entry, dynamic_values_bytes);

    n = i386_set_ldt(LDT_AUTO_ALLOC, (union descriptor*) &ldt_entry, 1);
    if (n < 0) {
        perror("i386_set_ldt");
        lose("unexpected i386_set_ldt(..) failure");
    }
    FSHOW_SIGNAL((stderr, "/ TLS: Allocated LDT %x\n", n));
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
    FSHOW_SIGNAL((stderr, "/ TLS: Freeing LDT %x\n", n));

    __asm__ __volatile__ ("mov %0, %%fs" : : "r"(0));
    i386_set_ldt(n, NULL, 1);
#endif

    return 1;                  /* success */
}

#endif /* !LISP_FEATURE_DARWIN */

#if defined(LISP_FEATURE_FREEBSD)
#if defined(LISP_FEATURE_RESTORE_TLS_SEGMENT_REGISTER_FROM_CONTEXT)
void
os_restore_tls_segment_register(os_context_t *context)
{
    load_fs(context->uc_mcontext.mc_fs);
}
#endif

void
os_restore_fp_control(os_context_t *context)
{
    /* FPU state is saved per context on post-KSE systems.
     * On earlier systems, it is shared in a whole process.
     */
#if defined(__FreeBSD_version) && __FreeBSD_version >= 500040
    struct envxmm *ex = (struct envxmm *)(context->uc_mcontext.mc_fpstate);
    __asm__ __volatile__ ("fldcw %0" : : "m" (ex->en_cw));
#endif
#if defined(LISP_FEATURE_RESTORE_TLS_SEGMENT_REGISTER_FROM_CONTEXT)
    /* Calling this function here may not be good idea.  Or rename
     * function name os_restore_fp_control to os_restore_context or
     * so, to match the behavior?  */
    os_restore_tls_segment_register(context);
#endif
}
#endif

#if defined(LISP_FEATURE_DRAGONFLY)
void os_restore_fp_control (os_context_t *context)
{
    struct envxmm *ex = (struct envxmm *)(context->uc_mcontext.mc_fpregs);
    __asm__ __volatile__ ("fldcw %0" : : "m" (ex->en_cw));
}
#endif /* LISP_FEATURE_DRAGONFLY */

#if defined(LISP_FEATURE_OPENBSD)
void
os_restore_fp_control(os_context_t *context)
{
#ifdef OS_OPENBSD_FPSTATE_IN_SIGFRAME
    struct sigframe *frame = (struct sigframe *)((char*)context -
        offsetof(struct sigframe, sf_sc));
    union savefpu *fpu = frame->sf_fpstate;
#elif defined(OS_OPENBSD_FPSTATE_IN_SIGCONTEXT)
    union savefpu *fpu = context->sc_fpstate;
#endif

    if (openbsd_use_fxsave)
        __asm__ __volatile__ ("fldcw %0" : : "m" (fpu->sv_xmm.sv_env.en_cw));
    else
        __asm__ __volatile__ ("fldcw %0" : : "m" (fpu->sv_87.sv_env.en_cw));
}
#endif
