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

#include "sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "sbcl.h"
#include "arch.h"
#include "lispregs.h"
#include "signal.h"
#include "alloc.h"
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"
#include "thread.h"
#include "pseudo-atomic.h"
#include "unaligned.h"

#include "genesis/static-symbols.h"
#include "genesis/symbol.h"


#ifdef LISP_FEATURE_UD2_BREAKPOINTS
#define UD2_INST 0x0b0f         /* UD2 */
#define BREAKPOINT_WIDTH 2
#else
#ifdef LISP_FEATURE_INT4_BREAKPOINTS
# define BREAKPOINT_INST 0xce    /* INTO */
#else
# define BREAKPOINT_INST 0xcc    /* INT3 */
#endif
#define BREAKPOINT_WIDTH 1
#endif

unsigned int cpuid_fn1_ecx;
unsigned int avx_supported = 0;

static void cpuid(unsigned info, unsigned subinfo,
                  unsigned *eax, unsigned *ebx, unsigned *ecx, unsigned *edx)
{
#ifdef _MSC_VER
  int regs[4];
  __cpuid(regs, info);
  *eax = regs[0];
  *ebx = regs[1];
  *ecx = regs[2];
  *edx = regs[3];
#else
  __asm__("cpuid;"                                            /* assembly code */
          :"=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx) /* outputs */
          :"a" (info), "c" (subinfo)                          /* input: info into eax,
                                                                 subinfo to ecx */
           /* clobbers: none */
          );
#endif
}

static void xgetbv(unsigned *eax, unsigned *edx)
{
  __asm__("xgetbv;"
          :"=a" (*eax), "=d" (*edx)
          : "c" (0));
}

void arch_init(void)
{
  unsigned int eax, ebx, ecx, edx;

  cpuid(0, 0, &eax, &ebx, &ecx, &edx);
  if (eax >= 1) { // see if we can execute basic id function 1
      unsigned avx_mask = 0x18000000; // OXSAVE and AVX
      cpuid(1, 0, &eax, &ebx, &ecx, &edx);
      cpuid_fn1_ecx = ecx;
      if ((ecx & avx_mask) == avx_mask) {
          xgetbv(&eax, &edx);
          if ((eax & 0x06) == 0x06) // YMM and XMM
              avx_supported = 1;
      }
  }
}

#define FILL_VECTOR_T "FILL-VECTOR/T"

// Poke in a byte that changes an opcode to enable faster vector fill.
// Using fixed offsets and bytes is no worse than what we do elsewhere.
void tune_asm_routines_for_microarch(void)
{
    // I don't know if this works on Windows
#ifndef _MSC_VER
    unsigned int eax, ebx, ecx, edx;
    cpuid(0, 0, &eax, &ebx, &ecx, &edx);
    if (eax >= 7) {
        cpuid(7, 0, &eax, &ebx, &ecx, &edx);
        if (ebx & (1<<9)) // Enhanced Repeat Movs/Stos
          asm_routine_poke(FILL_VECTOR_T, 0x12, 0x7C); // Change JMP to JL
    }
#endif
}

/* Undo code patches so that the core file applies to the most generic
   microarchitecture on startup. As it happens, FILL-VECTOR/T is fine
   either way, but in general this might not be true for code using
   instructions that don't exist on some cpu family members */
void untune_asm_routines_for_microarch(void)
{
    asm_routine_poke(FILL_VECTOR_T, 0x12, 0xEB); // Change JL to JMP
}

#ifndef _WIN64
os_vm_address_t
arch_get_bad_addr(int sig, siginfo_t *code, os_context_t *context)
{
    return (os_vm_address_t)code->si_addr;
}
#endif


/*
 * hacking signal contexts
 *
 * (This depends both on architecture, which determines what we might
 * want to get to, and on OS, which determines how we get to it.)
 */

os_context_register_t *
context_eflags_addr(os_context_t *context)
{
#if defined __linux__ || defined __sun
    /* KLUDGE: As of kernel 2.2.14 on Red Hat 6.2, there's code in the
     * <sys/ucontext.h> file to define symbolic names for offsets into
     * gregs[], but it's conditional on __USE_GNU and not defined, so
     * we need to do this nasty absolute index magic number thing
     * instead. */
    return (os_context_register_t*)&context->uc_mcontext.gregs[17];
#elif defined LISP_FEATURE_FREEBSD || defined(__DragonFly__)
    return &context->uc_mcontext.mc_rflags;
#elif defined LISP_FEATURE_DARWIN
    return CONTEXT_ADDR_FROM_STEM(rflags);
#elif defined __OpenBSD__
    return &context->sc_rflags;
#elif defined __NetBSD__
    return CONTEXT_ADDR_FROM_STEM(RFLAGS);
#elif defined _WIN64
    return (os_context_register_t*)&context->win32_context->EFlags;
#else
#error unsupported OS
#endif
}

void arch_skip_instruction(os_context_t *context)
{
    /* Assuming we get here via an INT3 xxx instruction, the PC now
     * points to the interrupt code (a Lisp value) so we just move
     * past it. Skip the code; after that, if the code is an
     * error-trap or cerror-trap then skip the data bytes that follow. */

    long code;

    /* Get and skip the Lisp interrupt code. */
    code = *(char*)(*os_context_pc_addr(context))++;
    switch (code)
        {
        case trap_Error:
        case trap_Cerror:
            skip_internal_error(context);

            break;

        case trap_Breakpoint:           /* not tested */
        case trap_FunEndBreakpoint: /* not tested */
            break;

#ifdef LISP_FEATURE_SB_SAFEPOINT
        case trap_GlobalSafepoint:
        case trap_CspSafepoint:
#endif
        case trap_PendingInterrupt:
        case trap_Halt:
        case trap_SingleStepAround:
        case trap_SingleStepBefore:
        case trap_InvalidArgCount:
            /* only needed to skip the Code */
            break;

        default:
            fprintf(stderr,"[arch_skip_inst invalid code %ld\n]\n",code);
            break;
        }

    FSHOW((stderr,
           "/[arch_skip_inst resuming at %x]\n",
           *os_context_pc_addr(context)));
}

unsigned char *
arch_internal_error_arguments(os_context_t *context)
{
    return 1 + (unsigned char *)(*os_context_pc_addr(context));
}

boolean
arch_pseudo_atomic_atomic(os_context_t *context)
{
    return get_pseudo_atomic_atomic(arch_os_get_current_thread());
}

void
arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    struct thread *thread = arch_os_get_current_thread();
    set_pseudo_atomic_interrupted(thread);
}

void
arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
    struct thread *thread = arch_os_get_current_thread();
    clear_pseudo_atomic_interrupted(thread);
}

/*
 * This stuff seems to get called for TRACE and debug activity.
 */

unsigned int
arch_install_breakpoint(void *pc)
{
    unsigned int result = *(unsigned int*)pc;

#ifndef LISP_FEATURE_UD2_BREAKPOINTS
    *(char*)pc = BREAKPOINT_INST;               /* x86 INT3       */
    *((char*)pc+1) = trap_Breakpoint;           /* Lisp trap code */
#else
    *(char*)pc = UD2_INST & 0xff;
    *((char*)pc+1) = UD2_INST >> 8;
    *((char*)pc+2) = trap_Breakpoint;
#endif

    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    *((char *)pc) = orig_inst & 0xff;
    *((char *)pc + 1) = (orig_inst & 0xff00) >> 8;
#if BREAKPOINT_WIDTH > 1
    *((char *)pc + 2) = (orig_inst & 0xff0000) >> 16;
#endif
}

/* When single stepping, single_stepping holds the original instruction
 * PC location. */
unsigned int *single_stepping = NULL;
#ifdef CANNOT_GET_TO_SINGLE_STEP_FLAG
unsigned int  single_step_save1;
unsigned int  single_step_save2;
unsigned int  single_step_save3;
#endif

void
arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    unsigned int *pc = (unsigned int*)(*os_context_pc_addr(context));

    /* Put the original instruction back. */
    arch_remove_breakpoint(pc, orig_inst);

#ifdef CANNOT_GET_TO_SINGLE_STEP_FLAG
    /* Install helper instructions for the single step:
     * pushf; or [esp],0x100; popf. */
    single_step_save1 = *(pc-3);
    single_step_save2 = *(pc-2);
    single_step_save3 = *(pc-1);
    *(pc-3) = 0x9c909090;
    *(pc-2) = 0x00240c81;
    *(pc-1) = 0x9d000001;
#else
    *context_eflags_addr(context) |= 0x100;
#endif

    single_stepping = pc;

#ifdef CANNOT_GET_TO_SINGLE_STEP_FLAG
    *os_context_pc_addr(context) = (os_context_register_t)((char *)pc - 9);
#endif
}

void
arch_handle_breakpoint(os_context_t *context)
{
    *os_context_pc_addr(context) -= BREAKPOINT_WIDTH;
    handle_breakpoint(context);
}

void
arch_handle_fun_end_breakpoint(os_context_t *context)
{
    *os_context_pc_addr(context) -= BREAKPOINT_WIDTH;
    *os_context_pc_addr(context) =
        (uword_t)handle_fun_end_breakpoint(context);
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    arch_skip_instruction(context);
    /* On x86-64 the fdefn / function is always in RAX, so we pass
     * 0 as the register_offset. */
    handle_single_step_trap(context, trap, 0);
}


void
restore_breakpoint_from_single_step(os_context_t * context)
{
#ifdef CANNOT_GET_TO_SINGLE_STEP_FLAG
    /* Un-install single step helper instructions. */
    *(single_stepping-3) = single_step_save1;
    *(single_stepping-2) = single_step_save2;
    *(single_stepping-1) = single_step_save3;
#else
    *context_eflags_addr(context) &= ~0x100;
#endif
    /* Re-install the breakpoint if possible. */
    if (((char *)*os_context_pc_addr(context) >
         (char *)single_stepping) &&
        ((char *)*os_context_pc_addr(context) <=
         (char *)single_stepping + BREAKPOINT_WIDTH)) {
        fprintf(stderr, "warning: couldn't reinstall breakpoint\n");
    } else {
        arch_install_breakpoint(single_stepping);
    }

    single_stepping = NULL;
    return;
}

void
sigtrap_handler(int signal, siginfo_t *info, os_context_t *context)
{
    unsigned int trap;

    if (single_stepping) {
        restore_breakpoint_from_single_step(context);
        return;
    }

    /* This is just for info in case the monitor wants to print an
     * approximation. */
    access_control_stack_pointer(arch_os_get_current_thread()) =
        (lispobj *)*os_context_sp_addr(context);

    /* On entry %eip points just after the INT3 byte and aims at the
     * 'kind' value (eg trap_Cerror). For error-trap and Cerror-trap a
     * number of bytes will follow, the first is the length of the byte
     * arguments to follow. */
    trap = *(unsigned char *)(*os_context_pc_addr(context));

    handle_trap(context, trap);
}

void
sigill_handler(int signal, siginfo_t *siginfo, os_context_t *context) {
    /* Triggering SIGTRAP using int3 is unreliable on OS X/x86, so
     * we need to use illegal instructions for traps.
     */
#if defined(LISP_FEATURE_UD2_BREAKPOINTS) && !defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
    if (*((unsigned short *)*os_context_pc_addr(context)) == UD2_INST) {
        *os_context_pc_addr(context) += 2;
        return sigtrap_handler(signal, siginfo, context);
    }
#elif defined(LISP_FEATURE_INT4_BREAKPOINTS) && !defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
    if (*((unsigned char *)*os_context_pc_addr(context)) == BREAKPOINT_INST) {
        *os_context_pc_addr(context) += BREAKPOINT_WIDTH;
        return sigtrap_handler(signal, siginfo, context);
    }
#endif

    fake_foreign_function_call(context);
    lose("Unhandled SIGILL at %p.", *os_context_pc_addr(context));
}

#ifdef X86_64_SIGFPE_FIXUP
#define MXCSR_IE (0x01)         /* Invalid Operation */
#define MXCSR_DE (0x02)         /* Denormal */
#define MXCSR_ZE (0x04)         /* Devide-by-Zero */
#define MXCSR_OE (0x08)         /* Overflow */
#define MXCSR_UE (0x10)         /* Underflow */
#define MXCSR_PE (0x20)         /* Precision */

static inline int
mxcsr_to_code(unsigned int mxcsr)
{
    /* Extract unmasked exception bits. */
    mxcsr &= ~(mxcsr >> 7) & 0x3F;

    /* This order is defined at "Intel 64 and IA-32 Architectures
     * Software Developerfs Manual" Volume 1: "Basic Architecture",
     * 4.9.2 "Floating-Point Exception Priority". */
    if (mxcsr & MXCSR_IE)
        return FPE_FLTINV;
    else if (mxcsr & MXCSR_ZE)
        return FPE_FLTDIV;
    else if (mxcsr & MXCSR_DE)
        return FPE_FLTUND;
    else if (mxcsr & MXCSR_OE)
        return FPE_FLTOVF;
    else if (mxcsr & MXCSR_UE)
        return FPE_FLTUND;
    else if (mxcsr & MXCSR_PE)
        return FPE_FLTRES;

    return 0;
}

static void
sigfpe_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    unsigned int *mxcsr = arch_os_context_mxcsr_addr(context);

#ifndef LISP_FEATURE_DARWIN
    /* Darwin doesn't handle accrued bits right. */
    if (siginfo->si_code == 0)
#endif
    { /* XMM exception */
        siginfo->si_code = mxcsr_to_code(*mxcsr);

        /* Clear sticky exception flag. */
        *mxcsr &= ~0x3F;
    }

    interrupt_handle_now(signal, siginfo, context);
}
#endif

void
arch_install_interrupt_handlers()
{
    SHOW("entering arch_install_interrupt_handlers()");

    /* Note: The old CMU CL code here used sigtrap_handler() to handle
     * SIGILL as well as SIGTRAP. I couldn't see any reason to do
     * things that way. So, I changed to separate handlers when
     * debugging a problem on OpenBSD, where SBCL wasn't catching
     * SIGILL properly, but was instead letting the process be
     * terminated with an "Illegal instruction" output. If this change
     * turns out to break something (maybe breakpoint handling on some
     * OS I haven't tested on?) and we have to go back to the old CMU
     * CL way, I hope there will at least be a comment to explain
     * why.. -- WHN 2001-06-07 */
#if !defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER) && !defined(LISP_FEATURE_WIN32)
    undoably_install_low_level_interrupt_handler(SIGILL , sigill_handler);
    undoably_install_low_level_interrupt_handler(SIGTRAP, sigtrap_handler);
#endif

#if defined(X86_64_SIGFPE_FIXUP) && !defined(LISP_FEATURE_WIN32)
    undoably_install_low_level_interrupt_handler(SIGFPE, sigfpe_handler);
#endif

    SHOW("returning from arch_install_interrupt_handlers()");
}

#ifdef LISP_FEATURE_LINKAGE_TABLE
/* FIXME: It might be cleaner to generate these from the lisp side of
 * things.
 */

void
arch_write_linkage_table_jmp(char *reloc_addr, void *target_addr)
{
    reloc_addr[0] = 0xFF; /* Opcode for near jump to absolute reg/mem64. */
    reloc_addr[1] = 0x25; /* ModRM #b00 100 101, i.e. RIP-relative. */
    UNALIGNED_STORE32((reloc_addr+2), 2); /* 32-bit displacement field = 2 */
    reloc_addr[6] = 0x66; reloc_addr[7] = 0x90; /* 2-byte NOP */
    *(void**)(reloc_addr+8) = target_addr;
}

void
arch_write_linkage_table_ref(void *reloc_addr, void *target_addr)
{
    *(uword_t *)reloc_addr = (uword_t)target_addr;
}

#endif

/* These setup and check *both* the sse2 and x87 FPUs. While lisp code
   only uses the sse2 FPU, other code (such as libc) may use the x87 FPU.
 */

unsigned int
arch_get_fp_modes()
{
    unsigned int temp;
    unsigned int result;
    /* return the x87 exception flags ored in with the sse2
     * control+status flags */
    asm ("fnstsw %0" : "=m" (temp));
    result = temp;
    result &= 0x3F;
    asm ("stmxcsr %0" : "=m" (temp));
    result |= temp;
    /* flip exception mask bits */
    return result ^ (0x3F << 7);
}

struct fpenv
{
    unsigned short cw;
    unsigned short unused1;
    unsigned short sw;
    unsigned short unused2;
    unsigned int other_regs[5];
};

void
arch_set_fp_modes(unsigned int mxcsr)
{
    struct fpenv f_env;
    unsigned int temp;

    /* turn trap enable bits into exception mask */
    mxcsr ^= 0x3F << 7;

    /* set x87 modes */
    asm ("fnstenv %0" : "=m" (f_env));
    /* set control word: always long double precision
     * get traps and rounding from mxcsr word */
    f_env.cw = 0x300 | ((mxcsr >> 7) & 0x3F) | (((mxcsr >> 13) & 0x3) << 10);
    /* set status word: only override exception flags, from mxcsr */
    f_env.sw &= ~0x3F;
    f_env.sw |= (mxcsr & 0x3F);

    asm ("fldenv %0" : : "m" (f_env));

    /* now, simply, load up the mxcsr register */
    temp = mxcsr;
    asm ("ldmxcsr %0" : : "m" (temp));
}

#ifdef LISP_FEATURE_IMMOBILE_CODE
/// Return the Lisp object that fdefn's raw_addr slot jumps to.
/// This will either be:
/// (1) a simple-fun,
/// (2) a funcallable-instance with an embedded trampoline that makes
///     it resemble a simple-fun in terms of call convention, or
/// (3) a code-component with no simple-fun within it, that makes
///     closures and other funcallable-instances look like simple-funs.
lispobj fdefn_callee_lispobj(struct fdefn* fdefn) {
    extern unsigned asm_routines_end;
    if (((lispobj)fdefn->raw_addr & 0xFE) == 0xE8) {  // looks good
        int32_t offs = UNALIGNED_LOAD32((char*)&fdefn->raw_addr + 1);
        unsigned int raw_fun =
            (int)(long)&fdefn->raw_addr + 5 + offs; // 5 = length of "JMP rel32"
        switch (((unsigned char*)&fdefn->raw_addr)[5]) {
        case 0x00: // no closure/fin trampoline
          // If the target is an assembly routine, there is no simple-fun
          // that corresponds to the entry point. The code is kept live
          // by *ASSEMBLER-OBJECTS*. Otherwise, return the simple-fun.
          return raw_fun < asm_routines_end ? 0 : raw_fun - FUN_RAW_ADDR_OFFSET;
        case 0x48: // embedded funcallable instance trampoline
          return (raw_fun - (4<<WORD_SHIFT)) | FUN_POINTER_LOWTAG;
        case 0x90: // general closure/fin trampoline
          return (raw_fun - offsetof(struct code, constants)) | OTHER_POINTER_LOWTAG;
        }
    } else if (fdefn->raw_addr == 0)
        return 0;
    lose("Can't decode fdefn raw addr @ %p: %p\n", fdefn, fdefn->raw_addr);
}
#endif
