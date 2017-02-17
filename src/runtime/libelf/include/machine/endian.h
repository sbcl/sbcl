#if defined(__FreeBSD_kernel__) && !defined(__FREEBSD_GLUE_USE_EMBEDDED_HEADERS)
/* We have <machine/endian.h>.  Use it.  */
# include_next <machine/endian.h>
#else
# ifndef _MACHINE_ENDIAN_H_
#  define _MACHINE_ENDIAN_H_
#  include_next <endian.h>
#  define _LITTLE_ENDIAN        LITTLE_ENDIAN
#  define _BIG_ENDIAN                BIG_ENDIAN
#  define _BYTE_ORDER                BYTE_ORDER
# endif
#endif
