#ifdef __FreeBSD_kernel__

/* GNU/kFreeBSD <machine/_types.h> is patched to check for the GNU form
   (_SYS_CDEFS_H) instead of BSD form (_SYS_CDEFS_H_).  */
#if defined(_SYS_CDEFS_H_) && !defined(_SYS_CDEFS_H)
#define _SYS_CDEFS_H
#endif

#include_next <machine/_types.h>

#else

#ifndef _MACHINE__TYPES_H_
#define _MACHINE__TYPES_H_

typedef        __builtin_va_list        __va_list;        /* internally known to gcc */

#endif

#endif
