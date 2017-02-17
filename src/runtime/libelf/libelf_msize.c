/*-
 * Copyright (c) 2006 Joseph Koshy
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS `AS IS' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <sys/cdefs.h>
__FBSDID("$FreeBSD$");

#include <sys/types.h>
#include <sys/elf32.h>
#include <sys/elf64.h>

#include <assert.h>
#include <libelf.h>
#include <osreldate.h>
#include <string.h>

#include "_libelf.h"

/* WARNING: GENERATED FROM libelf/libelf_msize.m4. */

struct msize {
	size_t	msz32;
	size_t	msz64;
};



static struct msize msize[ELF_T_NUM] = {
#if	__FreeBSD_version >= 600102
    [ELF_T_ADDR] = { .msz32 = sizeof(Elf32_Addr), .msz64 = sizeof(Elf64_Addr) },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_BYTE] = { .msz32 = 1, .msz64 = 1 },
#endif
#if	__FreeBSD_version >= 700025
    [ELF_T_CAP] = { .msz32 = sizeof(Elf32_Cap), .msz64 = sizeof(Elf64_Cap) },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_DYN] = { .msz32 = sizeof(Elf32_Dyn), .msz64 = sizeof(Elf64_Dyn) },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_EHDR] = { .msz32 = sizeof(Elf32_Ehdr), .msz64 = sizeof(Elf64_Ehdr) },
#endif
#if	__FreeBSD_version >= 800062
    [ELF_T_GNUHASH] = { .msz32 = 1, .msz64 = 1 },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_HALF] = { .msz32 = sizeof(Elf32_Half), .msz64 = sizeof(Elf64_Half) },
#endif
#if	__FreeBSD_version >= 700025
    [ELF_T_LWORD] = { .msz32 = sizeof(Elf32_Lword), .msz64 = sizeof(Elf64_Lword) },
#endif
#if	__FreeBSD_version >= 700025
    [ELF_T_MOVE] = { .msz32 = sizeof(Elf32_Move), .msz64 = sizeof(Elf64_Move) },
#endif
#if	__FreeBSD_version >= 700025
    [ELF_T_MOVEP] = { .msz32 = 0, .msz64 = 0 },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_NOTE] = { .msz32 = 1, .msz64 = 1 },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_OFF] = { .msz32 = sizeof(Elf32_Off), .msz64 = sizeof(Elf64_Off) },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_PHDR] = { .msz32 = sizeof(Elf32_Phdr), .msz64 = sizeof(Elf64_Phdr) },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_REL] = { .msz32 = sizeof(Elf32_Rel), .msz64 = sizeof(Elf64_Rel) },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_RELA] = { .msz32 = sizeof(Elf32_Rela), .msz64 = sizeof(Elf64_Rela) },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_SHDR] = { .msz32 = sizeof(Elf32_Shdr), .msz64 = sizeof(Elf64_Shdr) },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_SWORD] = { .msz32 = sizeof(Elf32_Sword), .msz64 = sizeof(Elf64_Sword) },
#endif
#if	__FreeBSD_version >= 700009
    [ELF_T_SXWORD] = { .msz32 = 0, .msz64 = sizeof(Elf64_Sxword) },
#endif
#if	__FreeBSD_version >= 700025
    [ELF_T_SYMINFO] = { .msz32 = sizeof(Elf32_Syminfo), .msz64 = sizeof(Elf64_Syminfo) },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_SYM] = { .msz32 = sizeof(Elf32_Sym), .msz64 = sizeof(Elf64_Sym) },
#endif
#if	__FreeBSD_version >= 700009
    [ELF_T_VDEF] = { .msz32 = sizeof(Elf32_Verdef), .msz64 = sizeof(Elf64_Verdef) },
#endif
#if	__FreeBSD_version >= 700009
    [ELF_T_VNEED] = { .msz32 = sizeof(Elf32_Verneed), .msz64 = sizeof(Elf64_Verneed) },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_WORD] = { .msz32 = sizeof(Elf32_Word), .msz64 = sizeof(Elf64_Word) },
#endif
#if	__FreeBSD_version >= 700009
    [ELF_T_XWORD] = { .msz32 = 0, .msz64 = sizeof(Elf64_Xword) },
#endif

};

size_t
_libelf_msize(Elf_Type t, int elfclass, unsigned int version)
{
	size_t sz;

	assert(elfclass == ELFCLASS32 || elfclass == ELFCLASS64);
	assert((signed) t >= ELF_T_FIRST && t <= ELF_T_LAST);

	if (version != EV_CURRENT) {
		LIBELF_SET_ERROR(VERSION, 0);
		return (0);
	}

	sz = (elfclass == ELFCLASS32) ? msize[t].msz32 : msize[t].msz64;

	return (sz);
}
/*-
 * Copyright (c) 2006 Joseph Koshy
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS `AS IS' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $FreeBSD$
 */

/*
 * ELF types, defined in the "enum Elf_Type" API.
 *
 * The members of the list form a 3-tuple: (name, C-type-suffix, OSversion).
 * + name is an Elf_Type symbol without the ELF_T_ prefix.
 * + C-type-suffix is the suffix for Elf32_ and Elf64_ type names.
 * + version is the OS version the symbol first appeared in.
 *
 * OS revisions of note are:
 * 600102 - The earliest (6.0-STABLE) version supported by this code.
 * 700009 - Symbol versioning and ELF64 type changes.
 * 700025 - More ELF types and the introduction of libelf.
 */



/*
 *  *
 * Map a type name to its members.
 *
 * Each member-list element comprises of pairs of (field name, type),
 * in the sequence used in the file representation of NAME.
 *
 * Each member list element comprises a pair containing a field name
 * and a basic type.  Basic types include IDENT, HALF, WORD, LWORD,
 * ADDR{32,64}, OFF{32,64}, SWORD, XWORD, SXWORD.
 *
 * The last element of a member list is the null element: _,_.
 */






























