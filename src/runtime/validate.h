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

#if !defined(_INCLUDE_VALIDATE_H_)
#define _INCLUDE_VALIDATE_H_

/* constants derived from the fundamental constants in passed by GENESIS */
#define   BINDING_STACK_SIZE (  BINDING_STACK_END -   BINDING_STACK_START)
#define   CONTROL_STACK_SIZE (  CONTROL_STACK_END -   CONTROL_STACK_START)
#define   DYNAMIC_SPACE_SIZE (  DYNAMIC_SPACE_END -   DYNAMIC_SPACE_START)
#define READ_ONLY_SPACE_SIZE (READ_ONLY_SPACE_END - READ_ONLY_SPACE_START)
#define    STATIC_SPACE_SIZE (   STATIC_SPACE_END -    STATIC_SPACE_START)

#if !defined(LANGUAGE_ASSEMBLY)
extern void validate(void);
#endif

/* note for anyone trying to port an architecture's support files
 * from CMU CL to SBCL:
 *
 * CMU CL had architecture-dependent header files included here to
 * define memory map data:
 *   #ifdef __i386__
 *   #include "x86-validate.h"
 *   #endif
 * and so forth. In SBCL, the memory map data are defined at the Lisp
 * level and stuffed into the sbcl.h file created by GENESIS, so
 * there's no longer a need for an architecture-dependent header file
 * of memory map data. */

#endif
