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

#define BREAKPOINT_INST 0xcc	/* INT3 */

unsigned long fast_random_state = 1;

void arch_init(void)
{}

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
    return &context->uc_mcontext.gregs[16];
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
    int code;

    FSHOW((stderr, "[arch_skip_inst at %x]\n", *os_context_pc_addr(context)));

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
		( (char*)(*os_context_pc_addr(context)) )++;
	    }
	    break;

	case trap_Breakpoint:		/* not tested */
	case trap_FunctionEndBreakpoint: /* not tested */
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
	   "[arch_skip_inst resuming at %x]\n",
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
    return SymbolValue(PSEUDO_ATOMIC_ATOMIC);
}

void
arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(1));
}

/*
 * This stuff seems to get called for TRACE and debug activity.
 */

unsigned long
arch_install_breakpoint(void *pc)
{
    unsigned long result = *(unsigned long*)pc;

    *(char*)pc = BREAKPOINT_INST;		/* x86 INT3       */
    *((char*)pc+1) = trap_Breakpoint;		/* Lisp trap code */

    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    *((char *)pc) = orig_inst & 0xff;
    *((char *)pc + 1) = (orig_inst & 0xff00) >> 8;
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
arch_do_displaced_inst(os_context_t *context, unsigned long orig_inst)
{
    unsigned int *pc = (unsigned int*)(*os_context_pc_addr(context));

    /* Put the original instruction back. */
    *((char *)pc) = orig_inst & 0xff;
    *((char *)pc + 1) = (orig_inst & 0xff00) >> 8;

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

    single_stepping = (unsigned int*)pc;

#ifdef CANNOT_GET_TO_SINGLE_STEP_FLAG
    *os_context_pc_addr(context) = (char *)pc - 9;
#endif
}

void
sigtrap_handler(int signal, siginfo_t *info, void *void_context)
{
    int code = info->si_code;
    os_context_t *context = (os_context_t*)void_context;
    unsigned int trap;

    if (single_stepping && (signal==SIGTRAP))
    {
	/* fprintf(stderr,"* single step trap %x\n", single_stepping); */

#ifdef CANNOT_GET_TO_SINGLE_STEP_FLAG
	/* Un-install single step helper instructions. */
	*(single_stepping-3) = single_step_save1;
	*(single_stepping-2) = single_step_save2;
	*(single_stepping-1) = single_step_save3;
#else
	*context_eflags_addr(context) ^= 0x100;
#endif
	/* Re-install the breakpoint if possible. */
	if (*os_context_pc_addr(context) == (int)single_stepping + 1) {
	    fprintf(stderr, "warning: couldn't reinstall breakpoint\n");
	} else {
	    *((char *)single_stepping) = BREAKPOINT_INST;	/* x86 INT3 */
	    *((char *)single_stepping+1) = trap_Breakpoint;
	}

	single_stepping = NULL;
	return;
    }

    /* This is just for info in case the monitor wants to print an
     * approximation. */
    current_control_stack_pointer =
	(lispobj *)*os_context_sp_addr(context);

    /* On entry %eip points just after the INT3 byte and aims at the
     * 'kind' value (eg trap_Cerror). For error-trap and Cerror-trap a
     * number of bytes will follow, the first is the length of the byte
     * arguments to follow. */
    trap = *(unsigned char *)(*os_context_pc_addr(context));
    switch (trap) {

    case trap_PendingInterrupt:
	FSHOW((stderr, "<trap pending interrupt>\n"));
	arch_skip_instruction(context);
	interrupt_handle_pending(context);
	break;

    case trap_Halt:
	/* Note: the old CMU CL code tried to save FPU state
	 * here, and restore it after we do our thing, but there
	 * seems to be no point in doing that, since we're just
	 * going to lose(..) anyway. */
	fake_foreign_function_call(context);
	lose("%%PRIMITIVE HALT called; the party is over.");

    case trap_Error:
    case trap_Cerror:
	FSHOW((stderr, "<trap error/cerror %d>\n", code));
	interrupt_internal_error(signal, info, context, code==trap_Cerror);
	break;

    case trap_Breakpoint:
	(char*)(*os_context_pc_addr(context)) -= 1;
	handle_breakpoint(signal, info, context);
	break;

    case trap_FunctionEndBreakpoint:
	(char*)(*os_context_pc_addr(context)) -= 1;
	*os_context_pc_addr(context) =
	    (int)handle_function_end_breakpoint(signal, info, context);
	break;

    default:
	FSHOW((stderr,"[C--trap default %d %d %x]\n",
	       signal, code, context));
	interrupt_handle_now(signal, info, context);
	break;
    }
}

void
arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGILL , sigtrap_handler);
    interrupt_install_low_level_handler(SIGTRAP, sigtrap_handler);
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
