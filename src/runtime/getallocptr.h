/*
 * macros for getting/setting the end of dynamic space
 * and manipulating pseudo-atomic flags (per thread, if applicable)
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

#ifndef GETALLOCPTR_H
#define GETALLOCPTR_H

#if defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64
#include "allocptr-x86.inc"
#elif defined LISP_FEATURE_ARM || defined LISP_FEATURE_ARM64 || defined LISP_FEATURE_RISCV
#include "allocptr-lisp-symbol.inc"
#else
#include "allocptr-c-symbol.inc"
#endif

#endif /* GETALLOCPTR_H */
