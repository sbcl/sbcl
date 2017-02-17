#if defined(__FreeBSD_kernel__) && !defined(__FREEBSD_GLUE_USE_EMBEDDED_HEADERS)
#include_next <machine/elf.h>
#else

#ifndef _MACHINE_ELF_H_
#define _MACHINE_ELF_H_ 1

#include <sys/elf32.h>
#include <sys/elf64.h>

#ifndef ELF_ARCH
#include <machine/__get_elf_arch.h>        /* ELF_ARCH */
#endif

#ifndef ELF_TARG_DATA
#include <endian.h>
#if __BYTE_ORDER == __LITTLE_ENDIAN
# define ELF_TARG_DATA        ELFDATA2LSB
#elif __BYTE_ORDER == __BIG_ENDIAN
# define ELF_TARG_DATA        ELFDATA2MSB
#else
# error wtf??
#endif
#endif

#ifndef ELF_TARG_CLASS
#include <link.h>
#if __ELF_NATIVE_CLASS == 32
# define ELF_TARG_CLASS        ELFCLASS32
#elif __ELF_NATIVE_CLASS == 64
# define ELF_TARG_CLASS        ELFCLASS64
#else
# error wtf??
#endif
#endif

#endif

#endif /* __FreeBSD_kernel__ */
