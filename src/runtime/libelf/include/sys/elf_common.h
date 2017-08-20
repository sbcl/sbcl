#if defined(__FreeBSD_kernel__) && !defined(__FREEBSD_GLUE_USE_EMBEDDED_HEADERS)
#include_next <sys/elf_common.h>
#else
#include <freebsd/embed/sys/elf_common.h>
#endif
