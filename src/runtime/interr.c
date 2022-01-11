/*
 * stuff to handle internal errors
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
#include <stdarg.h>
#include <stdlib.h>

#include "sbcl.h"
#include "arch.h"
#include "signal.h"

#include "runtime.h"
#include "interr.h"
#include "print.h"
#include "lispregs.h"
#include "genesis/static-symbols.h"
#include "genesis/vector.h"
#include "code.h"
#include "thread.h"
#include "monitor.h"
#include "breakpoint.h"
#include "var-io.h"
#include "sc-offset.h"
#include "gc.h"

/* the way that we shut down the system on a fatal error */
void lisp_backtrace(int frames);

static void
default_lossage_handler(void)
{
    static int backtrace_invoked = 0;
    if (!backtrace_invoked) {
        backtrace_invoked = 1;
        // This may not be exactly the right condition for determining
        // whether it might be possible to backtrace, but at least it prevents
        // lose() from itself losing early in startup.
        if (get_sb_vm_thread()) lisp_backtrace(100);
    }
    exit(1);
}
static void (*lossage_handler)(void) = default_lossage_handler;

#if QSHOW
static void
configurable_lossage_handler()
{

    if (dyndebug_config.dyndebug_backtrace_when_lost) {
        fprintf(stderr, "lose: backtrace follows as requested\n");
        lisp_backtrace(100);
    }

    if (dyndebug_config.dyndebug_sleep_when_lost) {
        fprintf(stderr,
"The system is too badly corrupted or confused to continue at the Lisp.\n"
"level.  The monitor was enabled, but you requested `sleep_when_lost'\n"
"behaviour though dyndebug.  To help with your debugging effort, this\n"
"thread will not enter the monitor, and instead proceed immediately to an\n"
"infinite sleep call, maximizing your chances that the thread's current\n"
"state can be preserved until you attach an external debugger. Good luck!\n");
        for (;;)
#         ifdef LISP_FEATURE_WIN32
            Sleep(10000);
#         else
            sleep(10);
#         endif
    }

    monitor_or_something();
}
#endif

void enable_lossage_handler(void)
{
#if QSHOW
    lossage_handler = configurable_lossage_handler;
#else
    lossage_handler = monitor_or_something;
#endif
}
void disable_lossage_handler(void)
{
    lossage_handler = default_lossage_handler;
}

static
void print_message(char *fmt, va_list ap)
{
    fprintf(stderr, " in SBCL pid %d" THREAD_ID_LABEL, getpid(), THREAD_ID_VALUE);
    if (fmt) {
        fprintf(stderr, ":\n");
        vfprintf(stderr, fmt, ap);
    }
    fprintf(stderr, "\n");
}

static inline void
call_lossage_handler() never_returns;

static inline void
call_lossage_handler()
{
    lossage_handler();
    fprintf(stderr, "Argh! lossage_handler() returned, total confusion..\n");
    exit(1);
}

void
lose(char *fmt, ...)
{
    va_list ap;
    /* Block signals to prevent other threads, timers and such from
     * interfering. If only all threads could be stopped somehow. */

#ifdef LISP_FEATURE_WIN32
    /* pthread_sigmask is emulated on windows, if lose() happens very early
       it may not be ready to work yet. */
    if (get_sb_vm_thread())
#endif
      block_blockable_signals(0);

    fprintf(stderr, "fatal error encountered");
    va_start(ap, fmt);
    print_message(fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");
    fflush(stderr);
    call_lossage_handler();
}

#if 0
/// thread printf. This was used to produce the 2-column output
/// at the bottom of "src/code/final". The main thread'd os_kernel_tid
/// must be assigned a constant in main_thread_trampoline().
void tprintf(char *fmt, ...)
{
    va_list ap;
    char buf[200];
    char *ptr;
    const char spaces[] = "                                           ";
    struct thread*th = get_sb_vm_thread();
    buf[0] = ';'; buf[1] = ' ';
    ptr = buf+2;
    if (th->os_kernel_tid == 'A') {
        strcpy(ptr, spaces);
        ptr += (sizeof spaces)-1;
    }
    va_start(ap, fmt);
    int n = vsprintf(ptr, fmt, ap);
    va_end(ap);
    ptr += n;
    *ptr++ = '\n';
    write(2, buf, ptr-buf);
}
#endif

boolean lose_on_corruption_p = 0;

void
corruption_warning_and_maybe_lose(char *fmt, ...)
{
    va_list ap;
#ifndef LISP_FEATURE_WIN32
    sigset_t oldset;
    block_blockable_signals(&oldset);
#endif
    fprintf(stderr, "CORRUPTION WARNING");
    va_start(ap, fmt);
    print_message(fmt, ap);
    va_end(ap);
    fprintf(stderr, "The integrity of this image is possibly compromised.\n");
    if (lose_on_corruption_p || gc_active_p) {
        fprintf(stderr, "Exiting.\n");
        fflush(stderr);
        call_lossage_handler();
    }
    else {
        fprintf(stderr, "Continuing with fingers crossed.\n");
        fflush(stderr);
#ifndef LISP_FEATURE_WIN32
        thread_sigmask(SIG_SETMASK,&oldset,0);
#endif
    }
}

void print_constant(os_context_t *context, int offset) {
    lispobj code = find_code(context);
    if (code != NIL) {
        struct code *codeptr = (struct code *)native_pointer(code);
        putchar('\t');
        if (offset >= code_header_words(codeptr)) {
            printf("Constant offset %d out of bounds for the code object @ %p\n",
                   offset, codeptr);
        } else {
            brief_print(codeptr->constants[offset -
                                           (offsetof(struct code, constants) >> WORD_SHIFT)]);
        }
    }
}

#include "genesis/errnames.h"
char *internal_error_descriptions[] = {INTERNAL_ERROR_NAMES};
char internal_error_nargs[] = INTERNAL_ERROR_NARGS;

void skip_internal_error (os_context_t *context) {
    unsigned char *ptr = (unsigned char *)*os_context_pc_addr(context);

#ifdef LISP_FEATURE_ARM64
    uint32_t trap_instruction = *(uint32_t *)(ptr - 4);
#endif

    unsigned char code = *ptr;
    ptr++; // skip the byte indicating the kind of trap

    if (code > sizeof(internal_error_nargs)) {
        printf("Unknown error code %d at %p\n", code, (void*)*os_context_pc_addr(context));
    }
    int nargs = internal_error_nargs[code];

#ifdef LISP_FEATURE_ARM64
    /* See SB-VM::EMIT-ERROR-BREAK for the scheme */
    unsigned char first_arg = trap_instruction >> 13 & 0xFF;
    if (first_arg != 31 && nargs) {
        nargs--;
    }
#endif

    int nbytes = 0;
    while (nargs--) read_var_integer(ptr, &nbytes);
    ptr += nbytes;
#ifdef LISP_FEATURE_ARM64
    ptr=PTR_ALIGN_UP(ptr, 4);
#endif
    *((unsigned char **)os_context_pc_addr(context)) = ptr;

}

/* internal error handler for when the Lisp error system doesn't exist
 *
 * FIXME: Shouldn't error output go to stderr instead of stdout? (Alas,
 * this'd require changes in a number of things like brief_print(..),
 * or I'd have changed it immediately.) */
void describe_error_arg(os_context_t *context, int sc_number, int offset) {
{
    int ch;

    printf("    SC: %d, Offset: %d", sc_number, offset);
    switch (sc_number) {
    case sc_AnyReg:
    case sc_DescriptorReg:
        putchar('\t');
        brief_print(*os_context_register_addr(context, offset));
        break;

    case sc_CharacterReg:
        ch = *os_context_register_addr(context, offset);
#ifdef LISP_FEATURE_X86
        if (offset&1)
            ch = ch>>8;
        ch = ch & 0xff;
#endif
        switch (ch) {
        case '\n': printf("\t'\\n'\n"); break;
        case '\b': printf("\t'\\b'\n"); break;
        case '\t': printf("\t'\\t'\n"); break;
        case '\r': printf("\t'\\r'\n"); break;
        default:
            if (ch < 32 || ch > 127)
                printf("\\%03o", ch);
            else
                printf("\t'%c'\n", ch);
            break;
        }
        break;
    case sc_SapReg:
#ifdef sc_WordPointerReg
    case sc_WordPointerReg:
#endif
        printf("\t0x%08lx\n", (unsigned long) *os_context_register_addr(context, offset));
        break;
    case sc_SignedReg:
        printf("\t%ld\n", (long) *os_context_register_addr(context, offset));
        break;
    case sc_UnsignedReg:
        printf("\t%lu\n", (unsigned long) *os_context_register_addr(context, offset));
        break;
#ifdef sc_SingleFloatReg
    case sc_SingleFloatReg:
        printf("\t%g\n", *(float *)&context->sc_fpregs[offset]);
        break;
#endif
#ifdef sc_DoubleFloatReg
    case sc_DoubleFloatReg:
        printf("\t%g\n", *(double *)&context->sc_fpregs[offset]);
        break;
#endif
    case sc_Constant:
        print_constant(context, offset);
        break;
    default:
        printf("\t???\n");
        break;
    }
}
};
void
describe_internal_error(os_context_t *context)
{
    unsigned char *ptr = arch_internal_error_arguments(context);
    char count;
    int position;
    void * pc = (void*)*os_context_pc_addr(context);
    unsigned char code;

#ifdef LISP_FEATURE_ARM64
    /* See SB-VM::EMIT-ERROR-BREAK for the scheme */
    uint32_t trap_instruction = *(uint32_t *)ptr;
    unsigned char trap = trap_instruction >> 5 & 0xFF;
    ptr += 4;
#else
    unsigned char trap = *(ptr-1);
#endif

    if (trap >= trap_Error) {
        code = trap - trap_Error;
    } else {
        code = *ptr;
        ptr++;
    }
    if (code > sizeof(internal_error_nargs)) {
        printf("Unknown error code %d at %p\n", code, pc);
    }
    printf("Internal error #%d \"%s\" at %p\n", code, internal_error_descriptions[code], pc);
    count = internal_error_nargs[code];

#ifdef LISP_FEATURE_ARM64
    unsigned char first_arg = trap_instruction >> 13 & 0xFF;
    if (first_arg != 31) {
        describe_error_arg(context, 0, first_arg);
        count--;
    }
#endif

    for (position = 0; count > 0; --count) {
        int sc_and_offset = read_var_integer(ptr, &position);
        describe_error_arg(context, sc_and_offset_sc_number(sc_and_offset),
                           sc_and_offset_offset(sc_and_offset));
    }
}
