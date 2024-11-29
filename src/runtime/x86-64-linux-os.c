/*
 * The x86-64 Linux incarnation of arch-dependent OS-dependent
 * routines.  See also "linux-os.c".
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

#define _GNU_SOURCE /* for REG_RAX etc. from sys/ucontext */

#include <stdio.h>
#include <stddef.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include <sys/ucontext.h>

#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "genesis/sbcl.h"

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <linux/unistd.h>
#include <sys/mman.h>
#include <linux/version.h>
#include "thread.h"             /* dynamic_values_bytes */

#include "validate.h"

int arch_os_thread_init(struct thread *thread) {
    stack_t sigstack;
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp    = calc_altstack_base(thread);
    sigstack.ss_flags = 0;
    sigstack.ss_size  = calc_altstack_size(thread);
    if(sigaltstack(&sigstack,0)<0) {
        lose("Cannot sigaltstack: %s",strerror(errno));
    }
#endif
#ifdef MEMORY_SANITIZER
    extern __thread unsigned long __msan_param_tls[];
    ((lispobj*)thread)[THREAD_MSAN_PARAM_TLS_SLOT] = (uword_t)&__msan_param_tls[0];
#endif
    return 1;
}

/* free any arch/os-specific resources used by thread, which is now
 * defunct.  Not called on live threads
 */

int arch_os_thread_cleanup(struct thread __attribute__((unused)) *thread) {
    return 1;
}

void visit_context_registers(void (*p)(os_context_register_t,void*),
                             os_context_t *context, void* arg)
{
    mcontext_t* m = &context->uc_mcontext;
    p(m->gregs[REG_RIP], arg);
    // This is the order the registers appear in gregset_t (which makes no difference of course).
    // Not sure why the order is so kooky.
    p(m->gregs[REG_R8 ], arg); p(m->gregs[REG_R9 ], arg); p(m->gregs[REG_R10], arg);
    p(m->gregs[REG_R11], arg);
#ifndef LISP_FEATURE_SOFT_CARD_MARKS
    p(m->gregs[REG_R12], arg);  /* CARD_TABLE_REG */
#endif
#ifndef LISP_FEATURE_SB_THREAD
    p(m->gregs[REG_R13], arg);  /* THREAD_BASE_REG */
#endif
    p(m->gregs[REG_R14], arg); p(m->gregs[REG_R15], arg);
    p(m->gregs[REG_RDI], arg); p(m->gregs[REG_RSI], arg); p(m->gregs[REG_RBX], arg);
    p(m->gregs[REG_RDX], arg); p(m->gregs[REG_RAX], arg); p(m->gregs[REG_RCX], arg);
    // We could implicitly pin objects pointed to by parts of an XMM or YMM register here
}

// This array is indexed by the encoding of the register in an instruction.
static unsigned char regmap[16] = {
    REG_RAX, REG_RCX, REG_RDX, REG_RBX, REG_RSP, REG_RBP, REG_RSI, REG_RDI,
    REG_R8,  REG_R9,  REG_R10, REG_R11, REG_R12, REG_R13, REG_R14, REG_R15
};
// So is this one, to coincide with the one above
static char *gprnames[] = {
    "rax","rcx","rdx","rbx","rsp","rbp","rsi","rdi",
    "r8","r9","r10","r11","r12","r13","r14","r15"
};
static char *fprnames[] = {
    "xmm0","xmm1","xmm2","xmm3","xmm4","xmm5","xmm6","xmm7",
    "xmm8","xmm9","xmm10","xmm11","xmm12","xmm13","xmm14","xmm15"
};
static char *flagbits[] = {"CF",0,"PF",0,"AF",0,"ZF","SF","TF","IF","DF","OF"};

void sb_dump_mcontext(char *reason, ucontext_t* context)
{
    /* In case multiple threads receive SIGILL at the same time,
     * we want to try to avoid interleaving their output.
     * Using a single write() call usually does the trick */
    char obuf[2048], smallbuf[100];
    int bit, ptr = 0, i, j;
    smallbuf[0] = smallbuf[1] = 0;
    long unsigned int flags = context->uc_mcontext.gregs[REG_EFL];
    for (bit=11; bit>=0; --bit)
        if (flagbits[bit] && (flags & (1<<bit)))
          ptr += sprintf(smallbuf+ptr, " %s", flagbits[bit]);
#define REMAINING sizeof obuf - ptr
    ptr = 0;
    ptr = snprintf(obuf, REMAINING,
                   THREAD_ID_LABEL" CPU state (%s): PC=%lx Flags={%s}\n",
                   THREAD_ID_VALUE, reason,
                   (uword_t)context->uc_mcontext.gregs[REG_RIP],
                   smallbuf+1);
    // GPRs
    for(i=0; i<2; ++i) {
        for(j=0; j<8; ++j) ptr += snprintf(obuf+ptr, REMAINING, " %16s", gprnames[i*8+j]);
        if (REMAINING) obuf[ptr++] = '\n';
        for(j=0; j<8; ++j)
            ptr += snprintf(obuf+ptr, REMAINING, " %16lX",
                            (uword_t)context->uc_mcontext.gregs[regmap[i*8+j]]);
        if (REMAINING) obuf[ptr++] = '\n';
    }
    // FPRs
    for(i=0; i<4; ++i) {
        for(j=0; j<4; ++j) ptr += snprintf(obuf+ptr, REMAINING, "  %32s", fprnames[i*4+j]);
        if (REMAINING) obuf[ptr++] = '\n';
        for(j=0; j<4; ++j) {
            uint32_t* e = context->uc_mcontext.fpregs->_xmm[i*4+j].element;
            ptr += snprintf(obuf+ptr, REMAINING, "  %08X%08X%08X%08X", e[3], e[2], e[1], e[0]);
        }
        if (REMAINING) obuf[ptr++] = '\n';
    }
    sigset_tostring(&context->uc_sigmask, smallbuf, sizeof smallbuf);
    ptr += snprintf(obuf+ptr, REMAINING, "sigmask=%s\n", smallbuf);
    ignore_value(write(2, obuf, ptr));
}

os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    if (offset >= 0 && offset < 16)
        return (os_context_register_t*)&context->uc_mcontext.gregs[regmap[offset]];
    return 0;
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return (os_context_register_t*)&context->uc_mcontext.gregs[REG_RSP];
}

os_context_register_t *
os_context_fp_addr(os_context_t *context)
{
    return (os_context_register_t*)&context->uc_mcontext.gregs[REG_RBP];
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    return (uintptr_t)&context->uc_mcontext.gregs[REG_RSP];

}

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t*)&context->uc_mcontext.fpregs->_xmm[offset];
}

os_context_register_t *
os_context_ymm_register_addr(os_context_t *context, int offset)
{
#ifdef __USE_GNU
    struct _xstate *xstate = (void*)context->uc_mcontext.fpregs;
    return (os_context_register_t*)&(xstate->ymmh.ymmh_space[offset * 4]);
#else
    void *xstate = (void*)context->uc_mcontext.fpregs;
    return (os_context_register_t*)&((char*)xstate+0x240)[offset * 16];
#endif
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

void NO_SANITIZE_ADDRESS
os_restore_fp_control(os_context_t *context)
{
    if (context->uc_mcontext.fpregs) {
        /* reset exception flags and restore control flags on SSE2 FPU */
        unsigned int temp = (context->uc_mcontext.fpregs->mxcsr) & ~0x3F;
        asm ("ldmxcsr %0" : : "m" (temp));
        /* same for x87 FPU. */
        asm ("fldcw %0" : : "m" (context->uc_mcontext.fpregs->cwd));
    }
}

void
os_flush_icache(os_vm_address_t __attribute__((unused)) address,
                os_vm_size_t __attribute__((unused)) length)
{
}

// To avoid "Missing required foreign symbol" errors in os_link_runtime()
// the executable must actually depend on libm. It would not require libm,
// despite -lm in the link step, if there is no reference to a libm symbol
// observable to the linker. Any one symbol suffices to resolve all of them.
#include <math.h>
const long libm_anchor = (long)acos;

#ifdef LISP_FEATURE_SW_INT_AVOIDANCE
extern void sigtrap_handler();
extern char* vm_thread_name(struct thread*);
extern void sigset_tostring(const sigset_t*, char*, int);
void synchronous_trap(lispobj* sp_at_interrupt, char* savearea)
{
    os_context_t context;
    memset(&context, 0, sizeof context);

    // Create the signal context from the values pushed on the stack
    // by the lisp assembly routine.
    context.uc_mcontext.fpregs = &context.__fpregs_mem;
    if (sizeof context.uc_mcontext.fpregs->_xmm[0].element != 16) lose("sigcontext size bug");
    memcpy(context.uc_mcontext.fpregs->_xmm[0].element, savearea, 16*16);
    char* gprsave = savearea + 16*16;
    memcpy(context.uc_mcontext.gregs, gprsave, 15*8);

    context.uc_mcontext.gregs[REG_RSP] = (greg_t)sp_at_interrupt;
    // Take the return-PC to the user code which is 1 word down from exactly where
    // the stack-pointer was at the simulated INT3, then add 1 because a real INT
    // instructions leaves the PC pointing after it.
    long pc_at_interrupt = sp_at_interrupt[-1];
    context.uc_mcontext.gregs[REG_RIP] = 1 + pc_at_interrupt;
    // The first instruction of the asm routine was to push EFLAGS
    context.uc_mcontext.gregs[REG_EFL] = sp_at_interrupt[-2];
    // The next instruction was to push RBP
    context.uc_mcontext.gregs[REG_RBP] = sp_at_interrupt[-3];

    sigset_t curmask;
    thread_sigmask(SIG_UNBLOCK, 0, &curmask); // to read the mask
# define REAL_SIGSET_SIZE_BYTES ((NSIG/8))
    memcpy(&context.uc_sigmask, &curmask, REAL_SIGSET_SIZE_BYTES);
    thread_sigmask(SIG_BLOCK, &blockable_sigset, 0);
    sigset_t newmask;
    sigorset(&newmask, &blockable_sigset, &curmask);

    /* char newmask_string[100];
    sigset_tostring(&newmask, newmask_string, sizeof newmask_string);
    fprintf(stderr, "[%s]: trap: pc=%lx sp=%p savearea=%p newmask=%s\n",
            vm_thread_name(get_sb_vm_thread()),
            os_context_pc(&context), sp_at_interrupt, savearea,
            newmask_string); */

    sigtrap_handler(0, 0, &context);

    if (context.uc_mcontext.gregs[REG_RSP] != (greg_t)sp_at_interrupt ||
        context.uc_mcontext.gregs[REG_RBP] != (greg_t)sp_at_interrupt[-3])
        lose("don't know how return to a different frame\n");

    // Handler can alter the return PC which we need to stuff into
    // the return PC location that the assembly routine received.
    uword_t return_pc = context.uc_mcontext.gregs[REG_RIP];
    sp_at_interrupt[-1] = return_pc;

    // act like a return-from-signal by restoring the signal mask
    // Ideally this would be performed in the asm routine only after restoring
    // registers, but it doesn't matter too much.
    thread_sigmask(SIG_SETMASK, &context.uc_sigmask, 0);
}
int wrapped_pthread_sigmask(int how, const void* new, void* old)
{
     char new_string[80], old_string[80];
     sigset_tostring(new, new_string, sizeof new_string);
     int res = pthread_sigmask(how, new, old);
     sigset_tostring(old, old_string, sizeof old_string);
     fprintf(stderr, "[%s]: pthread_sigmask(%s,%s) -> %s\n",
             vm_thread_name(get_sb_vm_thread()),
             how==SIG_BLOCK?"BLOCK":how==SIG_UNBLOCK?"UNBLOCK":"SETMASK",
             new_string, old_string);
     return res;
}
#endif
