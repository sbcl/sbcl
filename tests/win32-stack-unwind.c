/* Compiled and loaded by win32-foreign-stack-unwind.impure.lisp
 *
 * establish_return_frame(callback_ptr) establishes an SEH frame
 * that will cause an unwind to itself followed by a return on
 * any exception, and then calls the callback_ptr.
 *
 * perform_test_unwind() does an unwind to the SEH frame
 * established by establish_return_frame().
 *
 * The name of the game for the tests is to establish a callback
 * that establishes something with a dynamic contour and
 * possibly a control transfer semantic (such as a binding or an
 * unwind-protect) and then call perform_test_unwind() or cause
 * an exception that should be handled by SBCL and see what
 * breaks.
 */

/* This software is part of the SBCL system. See the README file for
 * more information.
 *
 * While most of SBCL is derived from the CMU CL system, the test
 * files (like this one) were written from scratch after the fork
 * from CMU CL.
 *
 * This software is in the public domain and is provided with
 * absolutely no warranty. See the COPYING and CREDITS files for
 * more information.
 */

#include <stdio.h>
#include <stdlib.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <excpt.h>

#include <setjmp.h>

#ifndef EH_UNWINDING
#define EH_UNWINDING 0x02
#define EH_EXIT_UNWIND 0x04
#endif

/* The "public" API */

typedef void (*callback_ptr)(void);

void establish_return_frame(callback_ptr callback);
void perform_test_unwind(void);


/* The implementation */

static
void **saved_exception_frame;

static
DWORD saved_ebp;

static void *get_seh_frame(void)
{
    void* retval;
    asm volatile ("movl %%fs:0,%0": "=r" (retval));
    return retval;
}

static void set_seh_frame(void *frame)
{
    asm volatile ("movl %0,%%fs:0": : "r" (frame));
}


static
EXCEPTION_DISPOSITION handle_exception(EXCEPTION_RECORD *exception_record,
                                       void **exception_frame,
                                       CONTEXT *context,
                                       void *dc)
{
    /* If an exception occurs and is passed to us to handle, just
     * unwind.  One or more test cases check for SBCL handling
     * breakpoint exceptions properly.  This makes sure that it
     * doesn't unless a new exception frame is estabished when a
     * callback occurs. */
    if (!(exception_record->ExceptionFlags
          & (EH_UNWINDING | EH_EXIT_UNWIND))) {
        perform_test_unwind();
    }

    return ExceptionContinueSearch;
}

static void
  __attribute__((returns_twice))
  invoke_callback(callback_ptr callback, DWORD *unwind_token);

asm("_invoke_callback:"
    "pushl %ebp; movl %esp, %ebp;"
    "movl 12(%ebp), %eax;"
    "movl %ebp, (%eax);"
    "call *8(%ebp);"
    "movl %ebp, %esp; popl %ebp; ret");

static void do_unwind(void *target_frame, DWORD unwind_token);
asm("_do_unwind:"
    "pushl $target; pushl %ebp; movl %esp, %ebp;"
    "pushl $0xcafe; pushl $0; pushl $-1; pushl 12(%ebp); call _RtlUnwind@16;"
    "target:"
    "movl 16(%ebp), %esp; popl %ebp; ret");


void establish_return_frame(callback_ptr callback)
{
    void *exception_frame[2];

    saved_exception_frame = exception_frame;
    exception_frame[0] = get_seh_frame();
    exception_frame[1] = handle_exception;
    set_seh_frame(exception_frame);

    /* Do a setjmp just to explicitly spill callee-saved registers, i.e.
     * the portable equivalent of:
     *   asm("" : : : "%esi", "%edi", "%ebx");
     * which is needed because otherwise those registers would be trashed
     * following the stack unwind, and leave us with a likely crash upon
     * return to call_into_c.
     *
     * The returns_twice attribute on invoke_callback should take care
     * of this already, but does not seem to take effect, at least with
     * the version of gcc I am using.
     *
     * Note: __builtin_setjmp, not setjmp, because only the former has
     * the desired effect on the immediate call site. */
    jmp_buf env;
    __builtin_setjmp(env);

    invoke_callback(callback, &saved_ebp);

    if (exception_frame != get_seh_frame()) {
        /* It is never good for this to happen. */
        printf("exception frame mismatch on callback return.\n");
    }

    set_seh_frame(exception_frame[0]);
}

void perform_test_unwind(void)
{
    do_unwind(saved_exception_frame, saved_ebp);
}

/* EOF */
