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
#include <signal.h>

#include "runtime.h"
#include "os.h"
#include "sbcl.h"
#include "interrupt.h"
#include "arch.h"
#include "lispregs.h"
#include "globals.h"
#include "alloc.h"
#include "breakpoint.h"
#include "genesis/code.h"
#include "genesis/fdefn.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"

#define REAL_LRA_SLOT 0
#ifndef __i386__
#define KNOWN_RETURN_P_SLOT 1
#define BOGUS_LRA_CONSTANTS 2
#else
#define KNOWN_RETURN_P_SLOT 2
#define BOGUS_LRA_CONSTANTS 3
#endif

static void *compute_pc(lispobj code_obj, int pc_offset)
{
    struct code *code;

    code = (struct code *)native_pointer(code_obj);
    return (void *)((char *)code + HeaderValue(code->header)*sizeof(lispobj)
		    + pc_offset);
}

unsigned long breakpoint_install(lispobj code_obj, int pc_offset)
{
    return arch_install_breakpoint(compute_pc(code_obj, pc_offset));
}

void breakpoint_remove(lispobj code_obj, int pc_offset,
		       unsigned long orig_inst)
{
    arch_remove_breakpoint(compute_pc(code_obj, pc_offset), orig_inst);
}

void breakpoint_do_displaced_inst(os_context_t* context,
				  unsigned long orig_inst)
{
    /* on platforms with sigreturn(), we go directly back from
     * arch_do_displaced_inst() to lisp code, so we need to clean up
     * our bindings now.  (side note: I'd love to know in exactly what
     * scenario the speed of breakpoint handling is critical enough to
     * justify this maintenance mess)
     *
     * -dan 2001.08.09 */

#if (defined(sparc) && defined (solaris))
    undo_fake_foreign_function_call(context);
#endif
    arch_do_displaced_inst(context, orig_inst);
}

#ifndef __i386__
static lispobj find_code(os_context_t *context)
{
#ifdef reg_CODE
    lispobj code = *os_context_register_addr(context, reg_CODE);
    lispobj header;

    if (lowtag_of(code) != OTHER_POINTER_LOWTAG)
	return NIL;

    header = *(lispobj *)(code-OTHER_POINTER_LOWTAG);

    if (widetag_of(header) == CODE_HEADER_WIDETAG)
	return code;
    else
	return code - HeaderValue(header)*sizeof(lispobj);
#else
    return NIL;
#endif
}
#endif

#ifdef __i386__
static lispobj find_code(os_context_t *context)
{
    lispobj codeptr =
	(lispobj)component_ptr_from_pc((lispobj *)(*os_context_pc_addr(context)));

    if (codeptr == 0) {
	return NIL;
    } else {
	return codeptr + OTHER_POINTER_LOWTAG;
    }
}
#endif

static int compute_offset(os_context_t *context, lispobj code)
{
    if (code == NIL)
	return 0;
    else {
	unsigned long code_start;
	struct code *codeptr = (struct code *)native_pointer(code);
#ifdef parisc
	unsigned long pc = *os_context_pc_addr(context) & ~3;
#else
	unsigned long pc = *os_context_pc_addr(context);
#endif

	code_start = (unsigned long)codeptr
	    + HeaderValue(codeptr->header)*sizeof(lispobj);
	if (pc < code_start)
	    return 0;
	else {
	    int offset = pc - code_start;
	    if (offset >= codeptr->code_size)
		return 0;
	    else
		return make_fixnum(offset);
	}
    }
}
/* FIXME: I can see no really good reason these couldn't be merged, but haven't
 * tried.  The sigprocmask() call would work just as well on alpha as it
 * presumably does on x86   -dan 2001.08.10
 */
#ifndef __i386__
void handle_breakpoint(int signal, siginfo_t *info, os_context_t *context)
{
    lispobj code;

    fake_foreign_function_call(context);

    code = find_code(context);

    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(context, code),
	     code,
	     alloc_sap(context));

    undo_fake_foreign_function_call(context);
}
#else
void handle_breakpoint(int signal, siginfo_t* info, os_context_t *context)
{
    lispobj code, context_sap = alloc_sap(context);

    fake_foreign_function_call(context);

    code = find_code(context);

    /* Don't disallow recursive breakpoint traps. Otherwise, we can't
     * use debugger breakpoints anywhere in here. */
    sigprocmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);

    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(context, code),
	     code,
	     context_sap);

    undo_fake_foreign_function_call(context);
}
#endif

#ifndef __i386__
void *handle_fun_end_breakpoint(int signal, siginfo_t *info,
				os_context_t *context)
{
    lispobj code, lra;
    struct code *codeptr;

    fake_foreign_function_call(context);

    code = find_code(context);
    codeptr = (struct code *)native_pointer(code);

    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(context, code),
	     code,
	     alloc_sap(context));

    lra = codeptr->constants[REAL_LRA_SLOT];
#ifdef reg_CODE
    if (codeptr->constants[KNOWN_RETURN_P_SLOT] == NIL) {
	*os_context_register_addr(context, reg_CODE) = lra;
    }
#endif
    undo_fake_foreign_function_call(context);
    return (void *)(lra-OTHER_POINTER_LOWTAG+sizeof(lispobj));
}
#else
void *handle_fun_end_breakpoint(int signal, siginfo_t *info,
				os_context_t *context)
{
    lispobj code, context_sap = alloc_sap(context);
    struct code *codeptr;

    fake_foreign_function_call(context);

    code = find_code(context);
    codeptr = (struct code *)native_pointer(code);

    /* Don't disallow recursive breakpoint traps. Otherwise, we can't
     * use debugger breakpoints anywhere in here. */
    sigprocmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);

    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(context, code),
	     code,
	     context_sap);

    undo_fake_foreign_function_call(context);

    return compute_pc(codeptr->constants[REAL_LRA_SLOT],
		      fixnum_value(codeptr->constants[REAL_LRA_SLOT+1]));
}
#endif
