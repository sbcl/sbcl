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

#include "genesis/static-symbols.h"
#include "genesis/symbol.h"

#define BREAKPOINT_INST 0xcc    /* INT3 */

unsigned long fast_random_state = 1;

void arch_init(void)
{}

os_vm_address_t
arch_get_bad_addr(int sig, siginfo_t *code, os_context_t *context)
{
    return (os_vm_address_t)code->si_addr;
}


/*
 * hacking signal contexts
 *
 * (This depends both on architecture, which determines what we might
 * want to get to, and on OS, which determines how we get to it.)
 */

int *
context_eflags_addr(os_context_t *context)
{
#if defined __linux__
    /* KLUDGE: As of kernel 2.2.14 on Red Hat 6.2, there's code in the
     * <sys/ucontext.h> file to define symbolic names for offsets into
     * gregs[], but it's conditional on __USE_GNU and not defined, so
     * we need to do this nasty absolute index magic number thing
     * instead. */
    return &context->uc_mcontext.gregs[17];
#elif defined __FreeBSD__
    return &context->uc_mcontext.mc_eflags;
#elif defined __OpenBSD__
    return &context->sc_eflags;
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

    int vlen;
    long code;


    /* Get and skip the Lisp interrupt code. */
    code = *(char*)(*os_context_pc_addr(context))++;
    switch (code)
        {
        case trap_Error:
        case trap_Cerror:
            /* Lisp error arg vector length */
            vlen = *(char*)(*os_context_pc_addr(context))++;
            /* Skip Lisp error arg data bytes. */
            while (vlen-- > 0) {
                ++*os_context_pc_addr(context);
            }
            break;

        case trap_Breakpoint:           /* not tested */
        case trap_FunEndBreakpoint: /* not tested */
            break;

        case trap_PendingInterrupt:
        case trap_Halt:
            /* only needed to skip the Code */
            break;

        default:
            fprintf(stderr,"[arch_skip_inst invalid code %d\n]\n",code);
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
    return SymbolValue(PSEUDO_ATOMIC_ATOMIC,arch_os_get_current_thread());
}

void
arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(1),
                   arch_os_get_current_thread());
}

void
arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(0),
                   arch_os_get_current_thread());
}

/*
 * This stuff seems to get called for TRACE and debug activity.
 */

unsigned int
arch_install_breakpoint(void *pc)
{
    unsigned int result = *(unsigned int*)pc;

    *(char*)pc = BREAKPOINT_INST;               /* x86 INT3       */
    *((char*)pc+1) = trap_Breakpoint;           /* Lisp trap code */

    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    *((char *)pc) = orig_inst & 0xff;
    *((char *)pc + 1) = (orig_inst & 0xff00) >> 8;
}

/* When single stepping, single_stepping holds the original instruction
 * PC location. */
unsigned int *single_stepping = NULL;

void
arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    unsigned int *pc = (unsigned int*)(*os_context_pc_addr(context));

    /* Put the original instruction back. */
    *((char *)pc) = orig_inst & 0xff;
    *((char *)pc + 1) = (orig_inst & 0xff00) >> 8;

    *context_eflags_addr(context) |= 0x100;

    single_stepping = pc;
}


void
sigtrap_handler(int signal, siginfo_t *info, void *void_context)
{
    int code = info->si_code;
    os_context_t *context = (os_context_t*)void_context;
    unsigned int trap;

    if (single_stepping && (signal==SIGTRAP))
    {
        *context_eflags_addr(context) ^= 0x100;

        /* Re-install the breakpoint if possible. */
        if (*os_context_pc_addr(context) == (int)single_stepping + 1) {
            fprintf(stderr, "warning: couldn't reinstall breakpoint\n");
        } else {
            *((char *)single_stepping) = BREAKPOINT_INST;       /* x86 INT3 */
            *((char *)single_stepping+1) = trap_Breakpoint;
        }

        single_stepping = NULL;
        return;
    }

    /* This is just for info in case the monitor wants to print an
     * approximation. */
    current_control_stack_pointer =
        (lispobj *)*os_context_sp_addr(context);

    /* FIXME: CMUCL puts the float control restoration code here.
       Thus, it seems to me that single-stepping won't restore the
       float control.  Since SBCL currently doesn't support
       single-stepping (as far as I can tell) this is somewhat moot,
       but it might be worth either moving this code up or deleting
       the single-stepping code entirely.  -- CSR, 2002-07-15 */
#ifdef LISP_FEATURE_LINUX
    os_restore_fp_control(context);
#endif

    /* On entry %eip points just after the INT3 byte and aims at the
     * 'kind' value (eg trap_Cerror). For error-trap and Cerror-trap a
     * number of bytes will follow, the first is the length of the byte
     * arguments to follow. */
    trap = *(unsigned char *)(*os_context_pc_addr(context));
    switch (trap) {

    case trap_PendingInterrupt:
        FSHOW((stderr, "/<trap pending interrupt>\n"));
        arch_skip_instruction(context);
        interrupt_handle_pending(context);
        break;

    case trap_Halt:
        /* Note: the old CMU CL code tried to save FPU state
         * here, and restore it after we do our thing, but there
         * seems to be no point in doing that, since we're just
         * going to lose(..) anyway. */
        fake_foreign_function_call(context);
        lose("%%PRIMITIVE HALT called; the party is over.\n");

    case trap_Error:
    case trap_Cerror:
        FSHOW((stderr, "<trap error/cerror %d>\n", code));
        interrupt_internal_error(signal, info, context, code==trap_Cerror);
        break;

    case trap_Breakpoint:
        --*os_context_pc_addr(context);
        handle_breakpoint(signal, info, context);
        break;

    case trap_FunEndBreakpoint:
        --*os_context_pc_addr(context);
        *os_context_pc_addr(context) =
            (unsigned long)handle_fun_end_breakpoint(signal, info, context);
        break;

    default:
        FSHOW((stderr,"/[C--trap default %d %d %x]\n",
               signal, code, context));
        interrupt_handle_now(signal, info, context);
        break;
    }
}

static void
sigill_handler(int signal, siginfo_t *siginfo, void *void_context) {
    os_context_t *context = (os_context_t*)void_context;
    fake_foreign_function_call(context);
    lose("fake_foreign_function_call fell through");
}

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
    undoably_install_low_level_interrupt_handler(SIGILL , sigill_handler);
    undoably_install_low_level_interrupt_handler(SIGTRAP, sigtrap_handler);

    SHOW("returning from arch_install_interrupt_handlers()");
}

/* This is implemented in assembly language and called from C: */
extern lispobj
call_into_lisp(lispobj fun, lispobj *args, int nargs);

/* These functions are an interface to the Lisp call-in facility.
 * Since this is C we can know nothing about the calling environment.
 * The control stack might be the C stack if called from the monitor
 * or the Lisp stack if called as a result of an interrupt or maybe
 * even a separate stack. The args are most likely on that stack but
 * could be in registers depending on what the compiler likes. So we
 * copy the args into a portable vector and let the assembly language
 * call-in function figure it out. */

lispobj
funcall0(lispobj function)
{
    lispobj *args = NULL;

    FSHOW((stderr, "/entering funcall0(0x%lx)\n", (long)function));
    return call_into_lisp(function, args, 0);
}
lispobj
funcall1(lispobj function, lispobj arg0)
{
    lispobj args[1];
    args[0] = arg0;
    return call_into_lisp(function, args, 1);
}
lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj args[2];
    args[0] = arg0;
    args[1] = arg1;
    return call_into_lisp(function, args, 2);
}
lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj args[3];
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;
    return call_into_lisp(function, args, 3);
}


#ifdef LISP_FEATURE_LINKAGE_TABLE
/* FIXME: It might be cleaner to generate these from the lisp side of
 * things.
 */

void
arch_write_linkage_table_jmp(char * reloc, void * fun)
{
    unsigned long addr = (unsigned long) fun;
    int i;

    *reloc++ = 0xFF; /* Opcode for near jump to absolute reg/mem64. */
    *reloc++ = 0x25; /* ModRM #b00 100 101, i.e. RIP-relative. */
    *reloc++ = 0x00; /* 32-bit displacement field = 0 */
    *reloc++ = 0x00; /* ... */
    *reloc++ = 0x00; /* ... */
    *reloc++ = 0x00; /* ... */

    for (i = 0; i < 8; i++) {
        *reloc++ = addr & 0xff;
        addr >>= 8;
    }

    /* write a nop for good measure. */
    *reloc = 0x90;
}

void
arch_write_linkage_table_ref(void * reloc, void * data)
{
    *(unsigned long *)reloc = (unsigned long)data;
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
