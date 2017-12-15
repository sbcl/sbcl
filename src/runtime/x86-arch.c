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
#include "arch.h"
#include "lispregs.h"
#include "alloc.h"
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"
#include "thread.h"
#include "pseudo-atomic.h"
#include "forwarding-ptr.h"
#include "var-io.h"

#include "genesis/static-symbols.h"
#include "genesis/symbol.h"
#include "genesis/vector.h"

#define BREAKPOINT_INST 0xcc    /* INT3 */
#define UD2_INST 0x0b0f         /* UD2 */

#ifndef LISP_FEATURE_UD2_BREAKPOINTS
#define BREAKPOINT_WIDTH 1
#else
#define BREAKPOINT_WIDTH 2
#endif

/* Should we check code objects for fixup errors after they are transported? */
boolean check_code_fixups = 0;

void arch_init(void)
{}

#ifndef LISP_FEATURE_WIN32
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

int *
context_eflags_addr(os_context_t *context)
{
#if defined __linux__ || defined __sun
    /* KLUDGE: As of kernel 2.2.14 on Red Hat 6.2, there's code in the
     * <sys/ucontext.h> file to define symbolic names for offsets into
     * gregs[], but it's conditional on __USE_GNU and not defined, so
     * we need to do this nasty absolute index magic number thing
     * instead. */
    return &context->uc_mcontext.gregs[16];
#elif defined(LISP_FEATURE_FREEBSD) || defined(__DragonFly__)
    return &context->uc_mcontext.mc_eflags;
#elif defined __OpenBSD__
    return &context->sc_eflags;
#elif defined LISP_FEATURE_DARWIN
    return (int *)(&context->uc_mcontext->SS.EFLAGS);
#elif defined __NetBSD__
    return &(context->uc_mcontext.__gregs[_REG_EFL]);
#elif defined LISP_FEATURE_WIN32
    return (int *)&context->win32_context->EFlags;
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

    int code;

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
restore_breakpoint_from_single_step(os_context_t * context)
{
    /* fprintf(stderr,"* single step trap %x\n", single_stepping); */
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
        (int)handle_fun_end_breakpoint(context);
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    arch_skip_instruction(context);
    /* On x86 the fdefn / function is always in EAX, so we pass 0
     * as the register_offset. */
    handle_single_step_trap(context, trap, 0);
}

#ifndef LISP_FEATURE_WIN32
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

#ifdef LISP_FEATURE_SUNOS
    /* For some reason the breakpoints that :ENCAPSULATE NIL tracing sets up
     * cause a trace trap (i.e. processor single-stepping trap) on the following
     * instruction on Solaris 10/x86. -- JES, 2006-04-07
     */
    if (info->si_code == TRAP_TRACE) {
        lose("foo");
        return;
    }
#endif

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
#endif
    fake_foreign_function_call(context);
    lose("Unhandled SIGILL at %p.", *os_context_pc_addr(context));
}
#endif /* not LISP_FEATURE_WIN32 */

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
#if !defined(LISP_FEATURE_WIN32) && !defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
    undoably_install_low_level_interrupt_handler(SIGILL , sigill_handler);
    undoably_install_low_level_interrupt_handler(SIGTRAP, sigtrap_handler);
#endif
    SHOW("returning from arch_install_interrupt_handlers()");
}


/* Scan a x86 compiled code object, looking for possible fixups that
 * have been missed after a move.
 *
 * Two types of fixups are needed:
 * 1. Absolute fixups to within the code object.
 * 2. Relative fixups to outside the code object.
 *
 * Currently only absolute fixups to the constant vector, or to the
 * code area are checked. */
void
sniff_code_object(struct code *code, os_vm_size_t displacement)
{
    sword_t nheader_words, ncode_words, nwords;
    os_vm_address_t constants_start_addr = NULL, constants_end_addr, p;
    os_vm_address_t code_start_addr, code_end_addr;
    os_vm_address_t code_addr = (os_vm_address_t)code;
    int fixup_found = 0;

    if (!check_code_fixups)
        return;

    FSHOW((stderr, "/sniffing code: %p, %lu\n", code, displacement));

    ncode_words = code_instruction_words(code->code_size);
    nheader_words = code_header_words(*(lispobj *)code);
    nwords = ncode_words + nheader_words;

    constants_start_addr = code_addr + 5*N_WORD_BYTES;
    constants_end_addr = code_addr + nheader_words*N_WORD_BYTES;
    code_start_addr = code_addr + nheader_words*N_WORD_BYTES;
    code_end_addr = code_addr + nwords*N_WORD_BYTES;

    /* Work through the unboxed code. */
    for (p = code_start_addr; p < code_end_addr; p++) {
        void *data = *(void **)p;
        unsigned d1 = *((unsigned char *)p - 1);
        unsigned d2 = *((unsigned char *)p - 2);
        unsigned d3 = *((unsigned char *)p - 3);
        unsigned d4 = *((unsigned char *)p - 4);
#if QSHOW
        unsigned d5 = *((unsigned char *)p - 5);
        unsigned d6 = *((unsigned char *)p - 6);
#endif

        /* Check for code references. */
        /* Check for a 32 bit word that looks like an absolute
           reference to within the code adea of the code object. */
        if ((data >= (void*)(code_start_addr-displacement))
            && (data < (void*)(code_end_addr-displacement))) {
            /* function header */
            if ((d4 == 0x5e)
                && (((unsigned)p - 4 - 4*HeaderValue(*((unsigned *)p-1))) ==
                    (unsigned)code)) {
                /* Skip the function header */
                p += 6*4 - 4 - 1;
                continue;
            }
            /* the case of PUSH imm32 */
            if (d1 == 0x68) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/code ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                FSHOW((stderr, "/PUSH $0x%.8x\n", data));
            }
            /* the case of MOV [reg-8],imm32 */
            if ((d3 == 0xc7)
                && (d2==0x40 || d2==0x41 || d2==0x42 || d2==0x43
                    || d2==0x45 || d2==0x46 || d2==0x47)
                && (d1 == 0xf8)) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/code ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                FSHOW((stderr, "/MOV [reg-8],$0x%.8x\n", data));
            }
            /* the case of LEA reg,[disp32] */
            if ((d2 == 0x8d) && ((d1 & 0xc7) == 5)) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/code ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                FSHOW((stderr,"/LEA reg,[$0x%.8x]\n", data));
            }
        }

        /* Check for constant references. */
        /* Check for a 32 bit word that looks like an absolute
           reference to within the constant vector. Constant references
           will be aligned. */
        if ((data >= (void*)(constants_start_addr-displacement))
            && (data < (void*)(constants_end_addr-displacement))
            && (((unsigned)data & 0x3) == 0)) {
            /*  Mov eax,m32 */
            if (d1 == 0xa1) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                FSHOW((stderr,"/MOV eax,0x%.8x\n", data));
            }

            /*  the case of MOV m32,EAX */
            if (d1 == 0xa3) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                FSHOW((stderr, "/MOV 0x%.8x,eax\n", data));
            }

            /* the case of CMP m32,imm32 */
            if ((d1 == 0x3d) && (d2 == 0x81)) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                /* XX Check this */
                FSHOW((stderr, "/CMP 0x%.8x,immed32\n", data));
            }

            /* Check for a mod=00, r/m=101 byte. */
            if ((d1 & 0xc7) == 5) {
                /* Cmp m32,reg */
                if (d2 == 0x39) {
                    fixup_found = 1;
                    FSHOW((stderr,
                           "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                           p, d6, d5, d4, d3, d2, d1, data));
                    FSHOW((stderr,"/CMP 0x%.8x,reg\n", data));
                }
                /* the case of CMP reg32,m32 */
                if (d2 == 0x3b) {
                    fixup_found = 1;
                    FSHOW((stderr,
                           "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                           p, d6, d5, d4, d3, d2, d1, data));
                    FSHOW((stderr, "/CMP reg32,0x%.8x\n", data));
                }
                /* the case of MOV m32,reg32 */
                if (d2 == 0x89) {
                    fixup_found = 1;
                    FSHOW((stderr,
                           "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                           p, d6, d5, d4, d3, d2, d1, data));
                    FSHOW((stderr, "/MOV 0x%.8x,reg32\n", data));
                }
                /* the case of MOV reg32,m32 */
                if (d2 == 0x8b) {
                    fixup_found = 1;
                    FSHOW((stderr,
                           "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                           p, d6, d5, d4, d3, d2, d1, data));
                    FSHOW((stderr, "/MOV reg32,0x%.8x\n", data));
                }
                /* the case of LEA reg32,m32 */
                if (d2 == 0x8d) {
                    fixup_found = 1;
                    FSHOW((stderr,
                           "abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                           p, d6, d5, d4, d3, d2, d1, data));
                    FSHOW((stderr, "/LEA reg32,0x%.8x\n", data));
                }
            }
        }
    }

    /* If anything was found, print some information on the code
     * object. */
    if (fixup_found) {
        FSHOW((stderr,
               "/compiled code object at %x: header words = %d, code words = %d\n",
               code, nheader_words, ncode_words));
        FSHOW((stderr,
               "/const start = %x, end = %x\n",
               constants_start_addr, constants_end_addr));
        FSHOW((stderr,
               "/code start = %x, end = %x\n",
               code_start_addr, code_end_addr));
    }
}

void
gencgc_apply_code_fixups(struct code *old_code, struct code *new_code)
{
    sword_t nheader_words, ncode_words, nwords;
    os_vm_address_t __attribute__((unused)) constants_start_addr, constants_end_addr;
    os_vm_address_t __attribute__((unused)) code_start_addr, code_end_addr;
    os_vm_address_t code_addr = (os_vm_address_t)new_code;
    os_vm_address_t old_addr = (os_vm_address_t)old_code;
    os_vm_size_t displacement = code_addr - old_addr;

    ncode_words = code_instruction_words(new_code->code_size);
    nheader_words = code_header_words(*(lispobj *)new_code);
    nwords = ncode_words + nheader_words;
    /* FSHOW((stderr,
             "/compiled code object at %x: header words = %d, code words = %d\n",
             new_code, nheader_words, ncode_words)); */
    constants_start_addr = code_addr + 5*N_WORD_BYTES;
    constants_end_addr = code_addr + nheader_words*N_WORD_BYTES;
    code_start_addr = code_addr + nheader_words*N_WORD_BYTES;
    code_end_addr = code_addr + nwords*N_WORD_BYTES;
    /*
    FSHOW((stderr,
           "/const start = %x, end = %x\n",
           constants_start_addr,constants_end_addr));
    FSHOW((stderr,
           "/code start = %x; end = %x\n",
           code_start_addr,code_end_addr));
    */

    lispobj fixups = new_code->fixups;
    /* It will be a nonzero integer if valid, or 0 if there are no fixups */
    if (fixups == 0) {
        /* Check for possible errors. */
        if (check_code_fixups)
            sniff_code_object(new_code, displacement);

        return;
    }

    /* Could be pointing to a forwarding pointer. */
    /* This is extremely unlikely, because the only referent of the fixups
       is usually the code itself; so scavenging the vector won't occur
       until after the code object is known to be live. As we're just now
       enlivening the code, the fixups shouldn't have been forwarded.
       Maybe the vector is on the special binding stack though ... */
    if (is_lisp_pointer(fixups) &&
        forwarding_pointer_p(native_pointer(fixups)))  {
        /* If so, then follow it. */
        /*SHOW("following pointer to a forwarding pointer");*/
        fixups = forwarding_pointer_value(native_pointer(fixups));
    }

    /*SHOW("got fixups");*/

    if (fixnump(fixups) ||
        (lowtag_of(fixups) == OTHER_POINTER_LOWTAG
         && widetag_of(*native_pointer(fixups)) == BIGNUM_WIDETAG)) {
        /* Got the fixups for the code block. Now work through the vector,
           and apply a fixup at each address. */
        struct varint_unpacker unpacker;
        varint_unpacker_init(&unpacker, fixups);
        int prev_offset = 0, offset;
        while (varint_unpack(&unpacker, &offset) && offset != 0) {
            // For extra compactness, each offset is relative to the prior,
            // so that the magnitudes are smaller.
            offset += prev_offset;
            prev_offset = offset;
            /* Now check the current value of offset. */
            os_vm_address_t old_value = *(os_vm_address_t *)(code_start_addr + offset);

            /* If it's within the old_code object then it must be an
             * absolute fixup (relative ones are not saved) */
            if ((old_value >= old_addr)
                && (old_value < (old_addr + nwords*N_WORD_BYTES)))
                /* So add the dispacement. */
                *(os_vm_address_t *)(code_start_addr + offset) =
                    old_value + displacement;
            else
                /* It is outside the old code object so it must be a
                 * relative fixup (absolute fixups are not saved). So
                 * subtract the displacement. */
                *(os_vm_address_t *)(code_start_addr + offset) =
                    old_value - displacement;
        }
    } else {
        /* This used to just print a note to stderr, but a bogus fixup seems to
         * indicate real heap corruption, so a hard hailure is in order. */
        lose("fixup vector %p has a bad widetag: %d\n",
             fixups, widetag_of(*native_pointer(fixups)));
    }

    /* Check for possible errors. */
    if (check_code_fixups) {
        sniff_code_object(new_code,displacement);
    }
}

#ifdef LISP_FEATURE_LINKAGE_TABLE
/* FIXME: It might be cleaner to generate these from the lisp side of
 * things.
 */

void
arch_write_linkage_table_jmp(char *reloc_addr, void *target_addr)
{
    /* Make JMP to function entry. JMP offset is calculated from next
     * instruction.
     */
    long offset = (char *)target_addr - (reloc_addr + 5);
    int i;

    *reloc_addr++ = 0xe9;       /* opcode for JMP rel32 */
    for (i = 0; i < 4; i++) {
        *reloc_addr++ = offset & 0xff;
        offset >>= 8;
    }

    /* write a nop for good measure. */
    *reloc_addr = 0x90;
}

void
arch_write_linkage_table_ref(void *reloc_addr, void *target_addr)
{
    *(unsigned long *)reloc_addr = (unsigned long)target_addr;
}

#endif
