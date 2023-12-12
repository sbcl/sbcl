#ifndef _X86_ARCH_H
#define _X86_ARCH_H

// DO NOT REMOVE THIS. DOING SO WILL CAUSE NO COMPILATION ERRORS,
// BUT WILL CAUSE RUNTIME FAILURE.
#include "interr.h"                     /* for declaration of lose() */

#define ARCH_HAS_STACK_POINTER

/* When single stepping, single_stepping holds the original instruction
 * PC location. */
extern unsigned int *single_stepping;

#endif /* _X86_ARCH_H */
