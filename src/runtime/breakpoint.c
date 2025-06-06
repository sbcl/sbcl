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
#include <stdlib.h>
#include <signal.h>

#include "genesis/sbcl.h"
#include "genesis/sap.h"
#include "runtime.h"
#include "os.h"
#include "interrupt.h"
#include "arch.h"
#include "gc.h"
#include "lispregs.h"
#include "globals.h"
#include "breakpoint.h"
#include "thread.h"
#include "code.h"
#include "genesis/symbol.h"

#define REAL_LRA_SLOT 0
#ifdef reg_LRA
#define KNOWN_RETURN_P_SLOT 1
#endif

static void *compute_pc(lispobj code_obj, int pc_offset)
{
    return code_text_start((struct code*)native_pointer(code_obj)) + pc_offset;
}

unsigned int breakpoint_install(lispobj code_obj, int pc_offset)
{
    return arch_install_breakpoint(compute_pc(code_obj, pc_offset));
}

void breakpoint_remove(lispobj code_obj, int pc_offset,
                       unsigned int orig_inst)
{
    arch_remove_breakpoint(compute_pc(code_obj, pc_offset), orig_inst);
}

void breakpoint_do_displaced_inst(os_context_t* context,
                                  unsigned int orig_inst)
{
    /* on platforms with sigreturn(), we go directly back from
     * arch_do_displaced_inst() to lisp code, so we need to clean up
     * our bindings now.  (side note: I'd love to know in exactly what
     * scenario the speed of breakpoint handling is critical enough to
     * justify this maintenance mess)
     *
     * -dan 2001.08.09 */

#if (defined(LISP_FEATURE_SPARC) && defined (solaris))
    undo_fake_foreign_function_call(context);
#endif
    arch_do_displaced_inst(context, orig_inst);
}

lispobj find_code(os_context_t *context)
{
#if (defined(reg_LRA) && !defined(LISP_FEATURE_PPC) && !defined(LISP_FEATURE_PPC64))
    lispobj code = *os_context_register_addr(context, reg_CODE);
    lispobj header;

    if (lowtag_of(code) != OTHER_POINTER_LOWTAG)
        return NIL;

    header = *(lispobj *)(code-OTHER_POINTER_LOWTAG);

    if (header_widetag(header) == CODE_HEADER_WIDETAG)
        return code;
    else
        return code - HeaderValue(header)*sizeof(lispobj);
#else
    lispobj codeptr =
        (lispobj)component_ptr_from_pc((char *)os_context_pc(context));

    if (codeptr == 0)
        return NIL;
    else
        return codeptr + OTHER_POINTER_LOWTAG;
#endif
}

static long compute_offset(os_context_t *context, lispobj code)
{
  if (code != NIL) {
        uword_t pc = os_context_pc(context);
        struct code *codeptr = (struct code *)native_pointer(code);
        uword_t code_start = (uword_t)code_text_start(codeptr);
        int offset;
        if (pc >= code_start &&
            ((offset = pc - code_start) < code_text_size(codeptr)))
            return make_fixnum(offset);
    }
    return 0;
}

void handle_breakpoint(os_context_t *context)
{
    lispobj code;
    DX_ALLOC_SAP(context_sap, context);

    fake_foreign_function_call(context);

#if HAVE_GC_STW_SIGNAL
    unblock_gc_stop_signal();
#endif
    code = find_code(context);

#ifdef LISP_FEATURE_UNIX
    /* Don't disallow recursive breakpoint traps. Otherwise, we can't
     * use debugger breakpoints anywhere in here. */
    thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);
#endif

    funcall3(StaticSymbolFunction(HANDLE_BREAKPOINT),
             compute_offset(context, code),
             code,
             context_sap);

    undo_fake_foreign_function_call(context);
}

void *handle_fun_end_breakpoint(os_context_t *context)
{
    lispobj code, lra;
    struct code *codeptr;
    DX_ALLOC_SAP(context_sap, context);

    fake_foreign_function_call(context);

#if HAVE_GC_STW_SIGNAL
    unblock_gc_stop_signal();
#endif

    code = find_code(context);
    codeptr = (struct code *)native_pointer(code);

#ifdef LISP_FEATURE_UNIX
    /* Don't disallow recursive breakpoint traps. Otherwise, we can't
     * use debugger breakpoints anywhere in here. */
    thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);
#endif

    funcall3(StaticSymbolFunction(HANDLE_BREAKPOINT),
             compute_offset(context, code),
             code,
             context_sap);

    lra = codeptr->constants[REAL_LRA_SLOT];

#if defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64
    /* PPC now passes LRA objects in reg_LRA during return.  Other
     * platforms should as well, but haven't been fixed yet. */
    *os_context_register_addr(context, reg_LRA) = lra;
#else
#ifdef reg_LRA
    /*
     * With the known-return convention, we definitely do NOT want to
     * mangle the CODE register because it isn't pointing to the bogus
     * LRA but to the actual routine.
     */
    if (codeptr->constants[KNOWN_RETURN_P_SLOT] == NIL)
        *os_context_register_addr(context, reg_CODE) = lra;
#endif
#endif

    undo_fake_foreign_function_call(context);

#ifdef reg_LRA
    return (void *)(lra-OTHER_POINTER_LOWTAG+sizeof(lispobj));
#else
    return compute_pc(lra, fixnum_value(codeptr->constants[REAL_LRA_SLOT+1]));
#endif
}

void
handle_single_step_trap (os_context_t *context, int kind, int register_offset)
{
    fake_foreign_function_call(context);

#ifndef LISP_FEATURE_WIN32
    thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);
#endif

    funcall2(StaticSymbolFunction(HANDLE_SINGLE_STEP_TRAP),
             make_fixnum(kind),
             make_fixnum(register_offset));

    undo_fake_foreign_function_call(context); /* blocks signals again */
}
