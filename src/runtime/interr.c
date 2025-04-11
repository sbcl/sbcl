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

#include "genesis/sbcl.h"
#include "arch.h"
#include <signal.h>

#include "runtime.h"
#include "interr.h"
#include "print.h"
#include "lispregs.h"
#include "genesis/static-symbols.h"
#include "genesis/vector.h"
#include "code.h"
#include "thread.h"
#include "breakpoint.h"
#include "var-io.h"
#include "sc-offset.h"
#include "gc.h"

/* the way that we shut down the system on a fatal error */
extern void ldb_monitor(void);

static void
default_lossage_handler(void)
{
    static int backtrace_invoked = 0;
    if (!backtrace_invoked) {
        backtrace_invoked = 1;
        // This may not be exactly the right condition for determining
        // whether it might be possible to backtrace, but at least it prevents
        // lose() from itself losing early in startup.
        if (get_sb_vm_thread()) print_lisp_backtrace(100, stderr);
    }
    exit(1);
}
static void (*lossage_handler)(void) = default_lossage_handler;

#ifdef LISP_FEATURE_WIN32
static void
configurable_lossage_handler()
{

    if (dyndebug_config.dyndebug_backtrace_when_lost) {
        fprintf(stderr, "lose: backtrace follows as requested\n");
        print_lisp_backtrace(100, stderr);
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
            Sleep(10000);
    }

    ldb_monitor();
}
#endif

void enable_lossage_handler(void)
{
#ifdef LISP_FEATURE_WIN32
    lossage_handler = configurable_lossage_handler;
#else
    lossage_handler = ldb_monitor;
#endif
}
void disable_lossage_handler(void)
{
    lossage_handler = default_lossage_handler;
}
void set_lossage_handler(void (*handler)(void))
{
    lossage_handler = handler ? handler : &default_lossage_handler;
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

static inline void never_returns
call_lossage_handler()
{
    lossage_handler();
    fprintf(stderr, "Argh! lossage_handler() returned, total confusion..\n");
    exit(1);
}

#include <setjmp.h>
void
lose(char *fmt, ...)
{
    va_list ap;
#ifdef STANDALONE_LDB
    extern jmp_buf ldb_toplevel;
    va_start(ap, fmt);
    print_message(fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");
    fflush(stderr);
    longjmp(ldb_toplevel, 1);
#else
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
#endif
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

int lose_on_corruption_p = 0; // DO NOT CHANGE THIS TO 'bool'. (Naughty users think it's 4 bytes)

void
corruption_warning(char *fmt, ...)
{
    va_list ap;
    fprintf(stderr, "CORRUPTION WARNING");
    va_start(ap, fmt);
    print_message(fmt, ap);
    va_end(ap);
    fprintf(stderr, "The integrity of this image is possibly compromised.\n");
    fflush(stderr);

}

void
maybe_lose()
{
#ifndef LISP_FEATURE_WIN32
    sigset_t oldset;
    block_blockable_signals(&oldset);
#endif
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


void
corruption_warning_and_maybe_lose(char *fmt, ...)
{
    va_list ap;
    fprintf(stderr, "CORRUPTION WARNING");
    va_start(ap, fmt);
    print_message(fmt, ap);
    va_end(ap);
    maybe_lose();
}

static void print_constant(os_context_t *context, int offset, FILE* ostream) {
    lispobj code = find_code(context);
    if (code != NIL) {
        struct code *codeptr = (struct code *)native_pointer(code);
        putc('\t', ostream);
        if (offset >= code_header_words(codeptr)) {
            fprintf(ostream,
                    "Constant offset %d out of bounds for the code object @ %p\n",
                    offset, codeptr);
        } else {
            struct iochannel io = {ostream, stdin};
            brief_print(codeptr->constants[offset -
                                           (offsetof(struct code, constants) >> WORD_SHIFT)],
                        &io);
        }
    }
}

#include "genesis/errnames.h"
char *internal_error_descriptions[] = {INTERNAL_ERROR_NAMES};
char internal_error_nargs[] = INTERNAL_ERROR_NARGS;

void skip_internal_error (os_context_t *context) {
    unsigned char *ptr = (unsigned char *)os_context_pc(context);

#ifdef LISP_FEATURE_ARM64
    uint32_t trap_instruction = *(uint32_t *)(ptr - 4);
#endif

    unsigned char code = *ptr;
    ptr++; // skip the byte indicating the kind of trap

    if (code > sizeof(internal_error_nargs)) {
        printf("Unknown error code %d at %p\n", code, (void*)os_context_pc(context));
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
    set_os_context_pc(context, (os_context_register_t)ptr);

}

/* internal error handler for when the Lisp error system doesn't exist */
void describe_error_arg(os_context_t *context, int sc_number, int offset, FILE* f)
{
    int ch;

    fprintf(f, "    SC: %d, Offset: %d", sc_number, offset);
    switch (sc_number) {
    case sc_AnyReg:
    case sc_DescriptorReg:
        putc('\t', f);
        struct iochannel io = {f, stdin};
        brief_print(*os_context_register_addr(context, offset), &io);
        break;

    case sc_CharacterReg:
        ch = *os_context_register_addr(context, offset);
#ifdef LISP_FEATURE_X86
        if (offset&1)
            ch = ch>>8;
        ch = ch & 0xff;
#endif
        switch (ch) {
        case '\n': fprintf(f, "\t'\\n'\n"); break;
        case '\b': fprintf(f, "\t'\\b'\n"); break;
        case '\t': fprintf(f, "\t'\\t'\n"); break;
        case '\r': fprintf(f, "\t'\\r'\n"); break;
        default:
            if (ch < 32 || ch > 127)
                fprintf(f, "\\%03o", ch);
            else
                fprintf(f, "\t'%c'\n", ch);
            break;
        }
        break;
    case sc_SapReg:
        fprintf(f, "\t0x%08lx\n", (unsigned long) *os_context_register_addr(context, offset));
        break;
    case sc_SignedReg:
        fprintf(f, "\t%ld\n", (long) *os_context_register_addr(context, offset));
        break;
    case sc_UnsignedReg:
        fprintf(f, "\t%lu\n", (unsigned long) *os_context_register_addr(context, offset));
        break;
#ifdef sc_SingleFloatReg
    case sc_SingleFloatReg:
        fprintf(f, "\t%g\n", *(float *)&context->sc_fpregs[offset]);
        break;
#endif
#ifdef sc_DoubleFloatReg
    case sc_DoubleFloatReg:
        fprintf(f, "\t%g\n", *(double *)&context->sc_fpregs[offset]);
        break;
#endif
    case sc_Constant:
        print_constant(context, offset, f);
        break;
    default:
        fprintf(f, "\t???\n");
        break;
    }
}

void
describe_internal_error(os_context_t *context)
{
    unsigned char *ptr = arch_internal_error_arguments(context);
    char count;
    int position;
    void * pc = (void*)os_context_pc(context);
    unsigned char code;

#ifdef LISP_FEATURE_ARM64
    /* See SB-VM::EMIT-ERROR-BREAK for the scheme */
    uint32_t trap_instruction = *(uint32_t *)ptr;
    unsigned char trap = trap_instruction >> 5 & 0xFF;
    ptr += 4;
#elif defined(LISP_FEATURE_PPC64) && defined(LISP_FEATURE_LITTLE_ENDIAN)
    unsigned char trap = *(ptr-4);
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
        fprintf(stderr, "Unknown error code %d at %p\n", code, pc);
    }
    fprintf(stderr,
            "Internal error #%d \"%s\" at %p\n", code, internal_error_descriptions[code], pc);
    count = internal_error_nargs[code];

#ifdef LISP_FEATURE_ARM64
    unsigned char first_arg = trap_instruction >> 13 & 0xFF;
    unsigned char first_offset = first_arg & 0x1F;
    unsigned char first_sc = first_arg >> 5;
    if (first_arg != 31) {
        char sc = 0;
        switch (first_sc) {
        case 1:
            sc = sc_UnsignedReg;
            break;
        case 2:
            sc = sc_SignedReg;
            break;
        default:
            sc = sc_DescriptorReg;
        }
        describe_error_arg(context, sc, first_offset, stderr);
        count--;
    }
#endif

    for (position = 0; count > 0; --count) {
        int sc_and_offset = read_var_integer(ptr, &position);
        describe_error_arg(context, sc_and_offset_sc_number(sc_and_offset),
                           sc_and_offset_offset(sc_and_offset), stderr);
    }
}
