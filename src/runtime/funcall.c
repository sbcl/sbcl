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

#include "sbcl.h"
#include "runtime.h"

extern lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs)

#ifdef LISP_FEATURE_X86_64
    __attribute__((sysv_abi))
#endif
    ;

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

