/* funcall0 -- funcall3: we can get by with just two sets of these:
 * for platforms where the control stack is the C-stack, and all others.
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

#include "genesis/sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "interrupt.h"

extern lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs)
#ifdef LISP_FEATURE_X86_64
    __attribute__((sysv_abi))
#endif
    ;

#if defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK) || defined(LISP_FEATURE_ARM64) \
    || defined(LISP_FEATURE_ARM)
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

#else

lispobj
funcall0(lispobj function)
{
    lispobj **stack_pointer
        = &access_control_stack_pointer(get_sb_vm_thread());
    lispobj *args = *stack_pointer;

    return call_into_lisp(function, args, 0);
}

lispobj
funcall1(lispobj function, lispobj arg0)
{
    lispobj **stack_pointer
        = &access_control_stack_pointer(get_sb_vm_thread());
    lispobj *args = *stack_pointer;

    *stack_pointer += 1;
    args[0] = arg0;

    return call_into_lisp(function, args, 1);
}

lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj **stack_pointer
        = &access_control_stack_pointer(get_sb_vm_thread());
    lispobj *args = *stack_pointer;

    *stack_pointer += 2;
    args[0] = arg0;
    args[1] = arg1;

    return call_into_lisp(function, args, 2);
}

lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj **stack_pointer
        = &access_control_stack_pointer(get_sb_vm_thread());
    lispobj *args = *stack_pointer;

    *stack_pointer += 3;
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;

    return call_into_lisp(function, args, 3);
}
#endif

#if (!defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_X86_64))
#if (defined(LISP_FEATURE_ARM64) || defined(LISP_FEATURE_ARM) || defined(LISP_FEATURE_X86))
lispobj
callback_wrapper_trampoline(lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj args[3];
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;
    return call_into_lisp(StaticSymbolFunction(ENTER_ALIEN_CALLBACK), args, 3);
}
#else
lispobj
callback_wrapper_trampoline(lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj **stack_pointer
        = &access_control_stack_pointer(get_sb_vm_thread());
    lispobj *args = *stack_pointer;

    *stack_pointer += 3;
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;

    return call_into_lisp(StaticSymbolFunction(ENTER_ALIEN_CALLBACK), args, 3);
}
#endif
#endif
