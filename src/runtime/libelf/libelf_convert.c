/*-
 * Copyright (c) 2006-2008 Joseph Koshy
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

/* WARNING: GENERATED FROM libelf/libelf_convert.m4. */

/*
 * Macros to swap various integral quantities.
 */

#define	SWAP_HALF(X) 	do {						\
		uint16_t _x = (uint16_t) (X);				\
		uint16_t _t = _x & 0xFF;				\
		_t <<= 8; _x >>= 8; _t |= _x & 0xFF;			\
		(X) = _t;						\
	} while (0)
#define	SWAP_WORD(X) 	do {						\
		uint32_t _x = (uint32_t) (X);				\
		uint32_t _t = _x & 0xFF;				\
		_t <<= 8; _x >>= 8; _t |= _x & 0xFF;			\
		_t <<= 8; _x >>= 8; _t |= _x & 0xFF;			\
		_t <<= 8; _x >>= 8; _t |= _x & 0xFF;			\
		(X) = _t;						\
	} while (0)
#define	SWAP_ADDR32(X)	SWAP_WORD(X)
#define	SWAP_OFF32(X)	SWAP_WORD(X)
#define	SWAP_SWORD(X)	SWAP_WORD(X)
#define	SWAP_WORD64(X)	do {						\
		uint64_t _x = (uint64_t) (X);				\
		uint64_t _t = _x & 0xFF;				\
		_t <<= 8; _x >>= 8; _t |= _x & 0xFF;			\
		_t <<= 8; _x >>= 8; _t |= _x & 0xFF;			\
		_t <<= 8; _x >>= 8; _t |= _x & 0xFF;			\
		_t <<= 8; _x >>= 8; _t |= _x & 0xFF;			\
		_t <<= 8; _x >>= 8; _t |= _x & 0xFF;			\
		_t <<= 8; _x >>= 8; _t |= _x & 0xFF;			\
		_t <<= 8; _x >>= 8; _t |= _x & 0xFF;			\
		(X) = _t;						\
	} while (0)
#define	SWAP_ADDR64(X)	SWAP_WORD64(X)
#define	SWAP_LWORD(X)	SWAP_WORD64(X)
#define	SWAP_OFF64(X)	SWAP_WORD64(X)
#define	SWAP_SXWORD(X)	SWAP_WORD64(X)
#define	SWAP_XWORD(X)	SWAP_WORD64(X)

/*
 * Write out various integral values.  The destination pointer could
 * be unaligned.  Values are written out in native byte order.  The
 * destination pointer is incremented after the write.
 */
#define	WRITE_BYTE(P,X) do {						\
		char *const _p = (char *) (P);	\
		_p[0]		= (char) (X);			\
		(P)		= _p + 1;				\
	} while (0)
#define	WRITE_HALF(P,X)	do {						\
		uint16_t _t	= (X);					\
		char *const _p	= (char *) (P);	\
		const char *const _q = (char *) &_t;	\
		_p[0]		= _q[0];				\
		_p[1]		= _q[1];				\
		(P) 		= _p + 2;				\
	} while (0)
#define	WRITE_WORD(P,X)	do {						\
		uint32_t _t	= (X);					\
		char *const _p	= (char *) (P);	\
		const char *const _q = (char *) &_t;	\
		_p[0]		= _q[0];				\
		_p[1]		= _q[1];				\
		_p[2]		= _q[2];				\
		_p[3]		= _q[3];				\
		(P)		= _p + 4;				\
	} while (0)
#define	WRITE_ADDR32(P,X)	WRITE_WORD(P,X)
#define	WRITE_OFF32(P,X)	WRITE_WORD(P,X)
#define	WRITE_SWORD(P,X)	WRITE_WORD(P,X)
#define	WRITE_WORD64(P,X)	do {					\
		uint64_t _t	= (X);					\
		char *const _p	= (char *) (P);	\
		const char *const _q = (char *) &_t;	\
		_p[0]		= _q[0];				\
		_p[1]		= _q[1];				\
		_p[2]		= _q[2];				\
		_p[3]		= _q[3];				\
		_p[4]		= _q[4];				\
		_p[5]		= _q[5];				\
		_p[6]		= _q[6];				\
		_p[7]		= _q[7];				\
		(P)		= _p + 8;				\
	} while (0)
#define	WRITE_ADDR64(P,X)	WRITE_WORD64(P,X)
#define	WRITE_LWORD(P,X)	WRITE_WORD64(P,X)
#define	WRITE_OFF64(P,X)	WRITE_WORD64(P,X)
#define	WRITE_SXWORD(P,X)	WRITE_WORD64(P,X)
#define	WRITE_XWORD(P,X)	WRITE_WORD64(P,X)
#define	WRITE_IDENT(P,X)	do {					\
		(void) memcpy((P), (X), sizeof((X)));			\
		(P)		= (P) + EI_NIDENT;			\
	} while (0)

/*
 * Read in various integral values.  The source pointer could be
 * unaligned.  Values are read in native byte order.  The source
 * pointer is incremented appropriately.
 */

#define	READ_BYTE(P,X)	do {						\
		const char *const _p =				\
			(const char *) (P);			\
		(X)		= _p[0];				\
		(P)		= (P) + 1;				\
	} while (0)
#define	READ_HALF(P,X)	do {						\
		uint16_t _t;						\
		char *const _q = (char *) &_t;	\
		const char *const _p =				\
			(const char *) (P);			\
		_q[0]		= _p[0];				\
		_q[1]		= _p[1];				\
		(P)		= (P) + 2;				\
		(X)		= _t;					\
	} while (0)
#define	READ_WORD(P,X)	do {						\
		uint32_t _t;						\
		char *const _q = (char *) &_t;	\
		const char *const _p =				\
			(const char *) (P);			\
		_q[0]		= _p[0];				\
		_q[1]		= _p[1];				\
		_q[2]		= _p[2];				\
		_q[3]		= _p[3];				\
		(P)		= (P) + 4;				\
		(X)		= _t;					\
	} while (0)
#define	READ_ADDR32(P,X)	READ_WORD(P,X)
#define	READ_OFF32(P,X)		READ_WORD(P,X)
#define	READ_SWORD(P,X)		READ_WORD(P,X)
#define	READ_WORD64(P,X)	do {					\
		uint64_t _t;						\
		char *const _q = (char *) &_t;	\
		const char *const _p =				\
			(const char *) (P);			\
		_q[0]		= _p[0];				\
		_q[1]		= _p[1];				\
		_q[2]		= _p[2];				\
		_q[3]		= _p[3];				\
		_q[4]		= _p[4];				\
		_q[5]		= _p[5];				\
		_q[6]		= _p[6];				\
		_q[7]		= _p[7];				\
		(P)		= (P) + 8;				\
		(X)		= _t;					\
	} while (0)
#define	READ_ADDR64(P,X)	READ_WORD64(P,X)
#define	READ_LWORD(P,X)		READ_WORD64(P,X)
#define	READ_OFF64(P,X)		READ_WORD64(P,X)
#define	READ_SXWORD(P,X)	READ_WORD64(P,X)
#define	READ_XWORD(P,X)		READ_WORD64(P,X)
#define	READ_IDENT(P,X)		do {					\
		(void) memcpy((X), (P), sizeof((X)));			\
		(P)		= (P) + EI_NIDENT;			\
	} while (0)

#define	ROUNDUP2(V,N)	(V) = ((((V) + (N) - 1)) & ~((N) - 1))



/*
 * Sections of type ELF_T_BYTE are never byteswapped, consequently a
 * simple memcpy suffices for both directions of conversion.
 */

static int
libelf_cvt_BYTE_tox(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	(void) byteswap;
	if (dsz < count)
		return (0);
	if (dst != src)
		(void) memcpy(dst, src, count);
	return (1);
}

#if	__FreeBSD_version >= 600102 /* ADDR */

static int
libelf_cvt_ADDR32_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Addr t, *s = (Elf32_Addr *) (uintptr_t) src;
	size_t c;

	(void) dsz;

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*s));
		return (1);
	}

	for (c = 0; c < count; c++) {
		t = *s++;
		SWAP_ADDR32(t);
		WRITE_ADDR32(dst,t);
	}

	return (1);
}
       
static int
libelf_cvt_ADDR32_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Addr t, *d = (Elf32_Addr *) (uintptr_t) dst;
	size_t c;

	if (dsz < count * sizeof(Elf32_Addr))
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*d));
		return (1);
	}

	for (c = 0; c < count; c++) {
		READ_ADDR32(src,t);
		SWAP_ADDR32(t);
		*d++ = t;
	}

	return (1);
}
       
static int
libelf_cvt_ADDR64_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Addr t, *s = (Elf64_Addr *) (uintptr_t) src;
	size_t c;

	(void) dsz;

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*s));
		return (1);
	}

	for (c = 0; c < count; c++) {
		t = *s++;
		SWAP_ADDR64(t);
		WRITE_ADDR64(dst,t);
	}

	return (1);
}
       
static int
libelf_cvt_ADDR64_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Addr t, *d = (Elf64_Addr *) (uintptr_t) dst;
	size_t c;

	if (dsz < count * sizeof(Elf64_Addr))
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*d));
		return (1);
	}

	for (c = 0; c < count; c++) {
		READ_ADDR64(src,t);
		SWAP_ADDR64(t);
		*d++ = t;
	}

	return (1);
}

#endif /* ADDR */
#if	__FreeBSD_version >= 600102 /* BYTE */

#endif /* BYTE */
#if	__FreeBSD_version >= 700025 /* CAP */

static int
libelf_cvt32_CAP_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Cap	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Cap *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Cap */
			SWAP_WORD(t.c_tag);
			SWAP_WORD(t.c_un.c_val);
			/**/
		}
		/* Write an Elf32_Cap */
		WRITE_WORD(dst,t.c_tag);
		WRITE_WORD(dst,t.c_un.c_val);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_CAP_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Cap	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Cap *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Cap */
			SWAP_XWORD(t.c_tag);
			SWAP_XWORD(t.c_un.c_val);
			/**/
		}
		/* Write an Elf64_Cap */
		WRITE_XWORD(dst,t.c_tag);
		WRITE_XWORD(dst,t.c_un.c_val);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_CAP_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Cap	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_CAP, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Cap *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Cap))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Cap */
		READ_WORD(s,t.c_tag);
		READ_WORD(s,t.c_un.c_val);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Cap */
			SWAP_WORD(t.c_tag);
			SWAP_WORD(t.c_un.c_val);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_CAP_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Cap	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_CAP, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Cap *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Cap))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Cap */
		READ_XWORD(s,t.c_tag);
		READ_XWORD(s,t.c_un.c_val);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Cap */
			SWAP_XWORD(t.c_tag);
			SWAP_XWORD(t.c_un.c_val);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* CAP */
#if	__FreeBSD_version >= 600102 /* DYN */

static int
libelf_cvt32_DYN_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Dyn	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Dyn *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Dyn */
			SWAP_SWORD(t.d_tag);
			SWAP_WORD(t.d_un.d_ptr);
			/**/
		}
		/* Write an Elf32_Dyn */
		WRITE_SWORD(dst,t.d_tag);
		WRITE_WORD(dst,t.d_un.d_ptr);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_DYN_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Dyn	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Dyn *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Dyn */
			SWAP_SXWORD(t.d_tag);
			SWAP_XWORD(t.d_un.d_ptr);
			/**/
		}
		/* Write an Elf64_Dyn */
		WRITE_SXWORD(dst,t.d_tag);
		WRITE_XWORD(dst,t.d_un.d_ptr);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_DYN_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Dyn	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_DYN, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Dyn *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Dyn))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Dyn */
		READ_SWORD(s,t.d_tag);
		READ_WORD(s,t.d_un.d_ptr);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Dyn */
			SWAP_SWORD(t.d_tag);
			SWAP_WORD(t.d_un.d_ptr);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_DYN_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Dyn	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_DYN, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Dyn *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Dyn))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Dyn */
		READ_SXWORD(s,t.d_tag);
		READ_XWORD(s,t.d_un.d_ptr);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Dyn */
			SWAP_SXWORD(t.d_tag);
			SWAP_XWORD(t.d_un.d_ptr);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* DYN */
#if	__FreeBSD_version >= 600102 /* EHDR */

static int
libelf_cvt32_EHDR_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Ehdr	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Ehdr *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Ehdr */
			SWAP_HALF(t.e_type);
			SWAP_HALF(t.e_machine);
			SWAP_WORD(t.e_version);
			SWAP_ADDR32(t.e_entry);
			SWAP_OFF32(t.e_phoff);
			SWAP_OFF32(t.e_shoff);
			SWAP_WORD(t.e_flags);
			SWAP_HALF(t.e_ehsize);
			SWAP_HALF(t.e_phentsize);
			SWAP_HALF(t.e_phnum);
			SWAP_HALF(t.e_shentsize);
			SWAP_HALF(t.e_shnum);
			SWAP_HALF(t.e_shstrndx);
			/**/
		}
		/* Write an Elf32_Ehdr */
		WRITE_IDENT(dst,t.e_ident);
		WRITE_HALF(dst,t.e_type);
		WRITE_HALF(dst,t.e_machine);
		WRITE_WORD(dst,t.e_version);
		WRITE_ADDR32(dst,t.e_entry);
		WRITE_OFF32(dst,t.e_phoff);
		WRITE_OFF32(dst,t.e_shoff);
		WRITE_WORD(dst,t.e_flags);
		WRITE_HALF(dst,t.e_ehsize);
		WRITE_HALF(dst,t.e_phentsize);
		WRITE_HALF(dst,t.e_phnum);
		WRITE_HALF(dst,t.e_shentsize);
		WRITE_HALF(dst,t.e_shnum);
		WRITE_HALF(dst,t.e_shstrndx);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_EHDR_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Ehdr	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Ehdr *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Ehdr */
			SWAP_HALF(t.e_type);
			SWAP_HALF(t.e_machine);
			SWAP_WORD(t.e_version);
			SWAP_ADDR64(t.e_entry);
			SWAP_OFF64(t.e_phoff);
			SWAP_OFF64(t.e_shoff);
			SWAP_WORD(t.e_flags);
			SWAP_HALF(t.e_ehsize);
			SWAP_HALF(t.e_phentsize);
			SWAP_HALF(t.e_phnum);
			SWAP_HALF(t.e_shentsize);
			SWAP_HALF(t.e_shnum);
			SWAP_HALF(t.e_shstrndx);
			/**/
		}
		/* Write an Elf64_Ehdr */
		WRITE_IDENT(dst,t.e_ident);
		WRITE_HALF(dst,t.e_type);
		WRITE_HALF(dst,t.e_machine);
		WRITE_WORD(dst,t.e_version);
		WRITE_ADDR64(dst,t.e_entry);
		WRITE_OFF64(dst,t.e_phoff);
		WRITE_OFF64(dst,t.e_shoff);
		WRITE_WORD(dst,t.e_flags);
		WRITE_HALF(dst,t.e_ehsize);
		WRITE_HALF(dst,t.e_phentsize);
		WRITE_HALF(dst,t.e_phnum);
		WRITE_HALF(dst,t.e_shentsize);
		WRITE_HALF(dst,t.e_shnum);
		WRITE_HALF(dst,t.e_shstrndx);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_EHDR_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Ehdr	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_EHDR, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Ehdr *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Ehdr))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Ehdr */
		READ_IDENT(s,t.e_ident);
		READ_HALF(s,t.e_type);
		READ_HALF(s,t.e_machine);
		READ_WORD(s,t.e_version);
		READ_ADDR32(s,t.e_entry);
		READ_OFF32(s,t.e_phoff);
		READ_OFF32(s,t.e_shoff);
		READ_WORD(s,t.e_flags);
		READ_HALF(s,t.e_ehsize);
		READ_HALF(s,t.e_phentsize);
		READ_HALF(s,t.e_phnum);
		READ_HALF(s,t.e_shentsize);
		READ_HALF(s,t.e_shnum);
		READ_HALF(s,t.e_shstrndx);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Ehdr */
			SWAP_HALF(t.e_type);
			SWAP_HALF(t.e_machine);
			SWAP_WORD(t.e_version);
			SWAP_ADDR32(t.e_entry);
			SWAP_OFF32(t.e_phoff);
			SWAP_OFF32(t.e_shoff);
			SWAP_WORD(t.e_flags);
			SWAP_HALF(t.e_ehsize);
			SWAP_HALF(t.e_phentsize);
			SWAP_HALF(t.e_phnum);
			SWAP_HALF(t.e_shentsize);
			SWAP_HALF(t.e_shnum);
			SWAP_HALF(t.e_shstrndx);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_EHDR_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Ehdr	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_EHDR, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Ehdr *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Ehdr))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Ehdr */
		READ_IDENT(s,t.e_ident);
		READ_HALF(s,t.e_type);
		READ_HALF(s,t.e_machine);
		READ_WORD(s,t.e_version);
		READ_ADDR64(s,t.e_entry);
		READ_OFF64(s,t.e_phoff);
		READ_OFF64(s,t.e_shoff);
		READ_WORD(s,t.e_flags);
		READ_HALF(s,t.e_ehsize);
		READ_HALF(s,t.e_phentsize);
		READ_HALF(s,t.e_phnum);
		READ_HALF(s,t.e_shentsize);
		READ_HALF(s,t.e_shnum);
		READ_HALF(s,t.e_shstrndx);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Ehdr */
			SWAP_HALF(t.e_type);
			SWAP_HALF(t.e_machine);
			SWAP_WORD(t.e_version);
			SWAP_ADDR64(t.e_entry);
			SWAP_OFF64(t.e_phoff);
			SWAP_OFF64(t.e_shoff);
			SWAP_WORD(t.e_flags);
			SWAP_HALF(t.e_ehsize);
			SWAP_HALF(t.e_phentsize);
			SWAP_HALF(t.e_phnum);
			SWAP_HALF(t.e_shentsize);
			SWAP_HALF(t.e_shnum);
			SWAP_HALF(t.e_shstrndx);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* EHDR */
#if	__FreeBSD_version >= 800062 /* GNUHASH */
                     
#endif /* GNUHASH */
#if	__FreeBSD_version >= 600102 /* HALF */

static int
libelf_cvt_HALF_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Half t, *s = (Elf64_Half *) (uintptr_t) src;
	size_t c;

	(void) dsz;

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*s));
		return (1);
	}

	for (c = 0; c < count; c++) {
		t = *s++;
		SWAP_HALF(t);
		WRITE_HALF(dst,t);
	}

	return (1);
}

       
static int
libelf_cvt_HALF_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Half t, *d = (Elf64_Half *) (uintptr_t) dst;
	size_t c;

	if (dsz < count * sizeof(Elf64_Half))
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*d));
		return (1);
	}

	for (c = 0; c < count; c++) {
		READ_HALF(src,t);
		SWAP_HALF(t);
		*d++ = t;
	}

	return (1);
}

#endif /* HALF */
#if	__FreeBSD_version >= 700025 /* LWORD */

static int
libelf_cvt_LWORD_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Lword t, *s = (Elf64_Lword *) (uintptr_t) src;
	size_t c;

	(void) dsz;

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*s));
		return (1);
	}

	for (c = 0; c < count; c++) {
		t = *s++;
		SWAP_LWORD(t);
		WRITE_LWORD(dst,t);
	}

	return (1);
}

       
static int
libelf_cvt_LWORD_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Lword t, *d = (Elf64_Lword *) (uintptr_t) dst;
	size_t c;

	if (dsz < count * sizeof(Elf64_Lword))
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*d));
		return (1);
	}

	for (c = 0; c < count; c++) {
		READ_LWORD(src,t);
		SWAP_LWORD(t);
		*d++ = t;
	}

	return (1);
}

#endif /* LWORD */
#if	__FreeBSD_version >= 700025 /* MOVE */

static int
libelf_cvt32_MOVE_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Move	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Move *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Move */
			SWAP_LWORD(t.m_value);
			SWAP_WORD(t.m_info);
			SWAP_WORD(t.m_poffset);
			SWAP_HALF(t.m_repeat);
			SWAP_HALF(t.m_stride);
			/**/
		}
		/* Write an Elf32_Move */
		WRITE_LWORD(dst,t.m_value);
		WRITE_WORD(dst,t.m_info);
		WRITE_WORD(dst,t.m_poffset);
		WRITE_HALF(dst,t.m_repeat);
		WRITE_HALF(dst,t.m_stride);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_MOVE_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Move	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Move *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Move */
			SWAP_LWORD(t.m_value);
			SWAP_XWORD(t.m_info);
			SWAP_XWORD(t.m_poffset);
			SWAP_HALF(t.m_repeat);
			SWAP_HALF(t.m_stride);
			/**/
		}
		/* Write an Elf64_Move */
		WRITE_LWORD(dst,t.m_value);
		WRITE_XWORD(dst,t.m_info);
		WRITE_XWORD(dst,t.m_poffset);
		WRITE_HALF(dst,t.m_repeat);
		WRITE_HALF(dst,t.m_stride);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_MOVE_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Move	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_MOVE, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Move *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Move))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Move */
		READ_LWORD(s,t.m_value);
		READ_WORD(s,t.m_info);
		READ_WORD(s,t.m_poffset);
		READ_HALF(s,t.m_repeat);
		READ_HALF(s,t.m_stride);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Move */
			SWAP_LWORD(t.m_value);
			SWAP_WORD(t.m_info);
			SWAP_WORD(t.m_poffset);
			SWAP_HALF(t.m_repeat);
			SWAP_HALF(t.m_stride);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_MOVE_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Move	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_MOVE, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Move *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Move))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Move */
		READ_LWORD(s,t.m_value);
		READ_XWORD(s,t.m_info);
		READ_XWORD(s,t.m_poffset);
		READ_HALF(s,t.m_repeat);
		READ_HALF(s,t.m_stride);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Move */
			SWAP_LWORD(t.m_value);
			SWAP_XWORD(t.m_info);
			SWAP_XWORD(t.m_poffset);
			SWAP_HALF(t.m_repeat);
			SWAP_HALF(t.m_stride);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* MOVE */
#if	__FreeBSD_version >= 700025 /* MOVEP */
                     
#endif /* MOVEP */
#if	__FreeBSD_version >= 600102 /* NOTE */

#endif /* NOTE */
#if	__FreeBSD_version >= 600102 /* OFF */

static int
libelf_cvt_OFF32_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Off t, *s = (Elf32_Off *) (uintptr_t) src;
	size_t c;

	(void) dsz;

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*s));
		return (1);
	}

	for (c = 0; c < count; c++) {
		t = *s++;
		SWAP_OFF32(t);
		WRITE_OFF32(dst,t);
	}

	return (1);
}
       
static int
libelf_cvt_OFF32_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Off t, *d = (Elf32_Off *) (uintptr_t) dst;
	size_t c;

	if (dsz < count * sizeof(Elf32_Off))
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*d));
		return (1);
	}

	for (c = 0; c < count; c++) {
		READ_OFF32(src,t);
		SWAP_OFF32(t);
		*d++ = t;
	}

	return (1);
}
       
static int
libelf_cvt_OFF64_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Off t, *s = (Elf64_Off *) (uintptr_t) src;
	size_t c;

	(void) dsz;

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*s));
		return (1);
	}

	for (c = 0; c < count; c++) {
		t = *s++;
		SWAP_OFF64(t);
		WRITE_OFF64(dst,t);
	}

	return (1);
}
       
static int
libelf_cvt_OFF64_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Off t, *d = (Elf64_Off *) (uintptr_t) dst;
	size_t c;

	if (dsz < count * sizeof(Elf64_Off))
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*d));
		return (1);
	}

	for (c = 0; c < count; c++) {
		READ_OFF64(src,t);
		SWAP_OFF64(t);
		*d++ = t;
	}

	return (1);
}

#endif /* OFF */
#if	__FreeBSD_version >= 600102 /* PHDR */

static int
libelf_cvt32_PHDR_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Phdr	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Phdr *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Phdr */
			SWAP_WORD(t.p_type);
			SWAP_OFF32(t.p_offset);
			SWAP_ADDR32(t.p_vaddr);
			SWAP_ADDR32(t.p_paddr);
			SWAP_WORD(t.p_filesz);
			SWAP_WORD(t.p_memsz);
			SWAP_WORD(t.p_flags);
			SWAP_WORD(t.p_align);
			/**/
		}
		/* Write an Elf32_Phdr */
		WRITE_WORD(dst,t.p_type);
		WRITE_OFF32(dst,t.p_offset);
		WRITE_ADDR32(dst,t.p_vaddr);
		WRITE_ADDR32(dst,t.p_paddr);
		WRITE_WORD(dst,t.p_filesz);
		WRITE_WORD(dst,t.p_memsz);
		WRITE_WORD(dst,t.p_flags);
		WRITE_WORD(dst,t.p_align);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_PHDR_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Phdr	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Phdr *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Phdr */
			SWAP_WORD(t.p_type);
			SWAP_WORD(t.p_flags);
			SWAP_OFF64(t.p_offset);
			SWAP_ADDR64(t.p_vaddr);
			SWAP_ADDR64(t.p_paddr);
			SWAP_XWORD(t.p_filesz);
			SWAP_XWORD(t.p_memsz);
			SWAP_XWORD(t.p_align);
			/**/
		}
		/* Write an Elf64_Phdr */
		WRITE_WORD(dst,t.p_type);
		WRITE_WORD(dst,t.p_flags);
		WRITE_OFF64(dst,t.p_offset);
		WRITE_ADDR64(dst,t.p_vaddr);
		WRITE_ADDR64(dst,t.p_paddr);
		WRITE_XWORD(dst,t.p_filesz);
		WRITE_XWORD(dst,t.p_memsz);
		WRITE_XWORD(dst,t.p_align);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_PHDR_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Phdr	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_PHDR, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Phdr *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Phdr))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Phdr */
		READ_WORD(s,t.p_type);
		READ_OFF32(s,t.p_offset);
		READ_ADDR32(s,t.p_vaddr);
		READ_ADDR32(s,t.p_paddr);
		READ_WORD(s,t.p_filesz);
		READ_WORD(s,t.p_memsz);
		READ_WORD(s,t.p_flags);
		READ_WORD(s,t.p_align);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Phdr */
			SWAP_WORD(t.p_type);
			SWAP_OFF32(t.p_offset);
			SWAP_ADDR32(t.p_vaddr);
			SWAP_ADDR32(t.p_paddr);
			SWAP_WORD(t.p_filesz);
			SWAP_WORD(t.p_memsz);
			SWAP_WORD(t.p_flags);
			SWAP_WORD(t.p_align);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_PHDR_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Phdr	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_PHDR, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Phdr *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Phdr))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Phdr */
		READ_WORD(s,t.p_type);
		READ_WORD(s,t.p_flags);
		READ_OFF64(s,t.p_offset);
		READ_ADDR64(s,t.p_vaddr);
		READ_ADDR64(s,t.p_paddr);
		READ_XWORD(s,t.p_filesz);
		READ_XWORD(s,t.p_memsz);
		READ_XWORD(s,t.p_align);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Phdr */
			SWAP_WORD(t.p_type);
			SWAP_WORD(t.p_flags);
			SWAP_OFF64(t.p_offset);
			SWAP_ADDR64(t.p_vaddr);
			SWAP_ADDR64(t.p_paddr);
			SWAP_XWORD(t.p_filesz);
			SWAP_XWORD(t.p_memsz);
			SWAP_XWORD(t.p_align);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* PHDR */
#if	__FreeBSD_version >= 600102 /* REL */

static int
libelf_cvt32_REL_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Rel	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Rel *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Rel */
			SWAP_ADDR32(t.r_offset);
			SWAP_WORD(t.r_info);
			/**/
		}
		/* Write an Elf32_Rel */
		WRITE_ADDR32(dst,t.r_offset);
		WRITE_WORD(dst,t.r_info);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_REL_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Rel	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Rel *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Rel */
			SWAP_ADDR64(t.r_offset);
			SWAP_XWORD(t.r_info);
			/**/
		}
		/* Write an Elf64_Rel */
		WRITE_ADDR64(dst,t.r_offset);
		WRITE_XWORD(dst,t.r_info);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_REL_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Rel	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_REL, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Rel *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Rel))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Rel */
		READ_ADDR32(s,t.r_offset);
		READ_WORD(s,t.r_info);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Rel */
			SWAP_ADDR32(t.r_offset);
			SWAP_WORD(t.r_info);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_REL_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Rel	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_REL, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Rel *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Rel))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Rel */
		READ_ADDR64(s,t.r_offset);
		READ_XWORD(s,t.r_info);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Rel */
			SWAP_ADDR64(t.r_offset);
			SWAP_XWORD(t.r_info);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* REL */
#if	__FreeBSD_version >= 600102 /* RELA */

static int
libelf_cvt32_RELA_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Rela	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Rela *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Rela */
			SWAP_ADDR32(t.r_offset);
			SWAP_WORD(t.r_info);
			SWAP_SWORD(t.r_addend);
			/**/
		}
		/* Write an Elf32_Rela */
		WRITE_ADDR32(dst,t.r_offset);
		WRITE_WORD(dst,t.r_info);
		WRITE_SWORD(dst,t.r_addend);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_RELA_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Rela	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Rela *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Rela */
			SWAP_ADDR64(t.r_offset);
			SWAP_XWORD(t.r_info);
			SWAP_SXWORD(t.r_addend);
			/**/
		}
		/* Write an Elf64_Rela */
		WRITE_ADDR64(dst,t.r_offset);
		WRITE_XWORD(dst,t.r_info);
		WRITE_SXWORD(dst,t.r_addend);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_RELA_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Rela	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_RELA, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Rela *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Rela))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Rela */
		READ_ADDR32(s,t.r_offset);
		READ_WORD(s,t.r_info);
		READ_SWORD(s,t.r_addend);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Rela */
			SWAP_ADDR32(t.r_offset);
			SWAP_WORD(t.r_info);
			SWAP_SWORD(t.r_addend);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_RELA_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Rela	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_RELA, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Rela *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Rela))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Rela */
		READ_ADDR64(s,t.r_offset);
		READ_XWORD(s,t.r_info);
		READ_SXWORD(s,t.r_addend);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Rela */
			SWAP_ADDR64(t.r_offset);
			SWAP_XWORD(t.r_info);
			SWAP_SXWORD(t.r_addend);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* RELA */
#if	__FreeBSD_version >= 600102 /* SHDR */

static int
libelf_cvt32_SHDR_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Shdr	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Shdr *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Shdr */
			SWAP_WORD(t.sh_name);
			SWAP_WORD(t.sh_type);
			SWAP_WORD(t.sh_flags);
			SWAP_ADDR32(t.sh_addr);
			SWAP_OFF32(t.sh_offset);
			SWAP_WORD(t.sh_size);
			SWAP_WORD(t.sh_link);
			SWAP_WORD(t.sh_info);
			SWAP_WORD(t.sh_addralign);
			SWAP_WORD(t.sh_entsize);
			/**/
		}
		/* Write an Elf32_Shdr */
		WRITE_WORD(dst,t.sh_name);
		WRITE_WORD(dst,t.sh_type);
		WRITE_WORD(dst,t.sh_flags);
		WRITE_ADDR32(dst,t.sh_addr);
		WRITE_OFF32(dst,t.sh_offset);
		WRITE_WORD(dst,t.sh_size);
		WRITE_WORD(dst,t.sh_link);
		WRITE_WORD(dst,t.sh_info);
		WRITE_WORD(dst,t.sh_addralign);
		WRITE_WORD(dst,t.sh_entsize);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_SHDR_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Shdr	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Shdr *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Shdr */
			SWAP_WORD(t.sh_name);
			SWAP_WORD(t.sh_type);
			SWAP_XWORD(t.sh_flags);
			SWAP_ADDR64(t.sh_addr);
			SWAP_OFF64(t.sh_offset);
			SWAP_XWORD(t.sh_size);
			SWAP_WORD(t.sh_link);
			SWAP_WORD(t.sh_info);
			SWAP_XWORD(t.sh_addralign);
			SWAP_XWORD(t.sh_entsize);
			/**/
		}
		/* Write an Elf64_Shdr */
		WRITE_WORD(dst,t.sh_name);
		WRITE_WORD(dst,t.sh_type);
		WRITE_XWORD(dst,t.sh_flags);
		WRITE_ADDR64(dst,t.sh_addr);
		WRITE_OFF64(dst,t.sh_offset);
		WRITE_XWORD(dst,t.sh_size);
		WRITE_WORD(dst,t.sh_link);
		WRITE_WORD(dst,t.sh_info);
		WRITE_XWORD(dst,t.sh_addralign);
		WRITE_XWORD(dst,t.sh_entsize);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_SHDR_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Shdr	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_SHDR, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Shdr *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Shdr))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Shdr */
		READ_WORD(s,t.sh_name);
		READ_WORD(s,t.sh_type);
		READ_WORD(s,t.sh_flags);
		READ_ADDR32(s,t.sh_addr);
		READ_OFF32(s,t.sh_offset);
		READ_WORD(s,t.sh_size);
		READ_WORD(s,t.sh_link);
		READ_WORD(s,t.sh_info);
		READ_WORD(s,t.sh_addralign);
		READ_WORD(s,t.sh_entsize);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Shdr */
			SWAP_WORD(t.sh_name);
			SWAP_WORD(t.sh_type);
			SWAP_WORD(t.sh_flags);
			SWAP_ADDR32(t.sh_addr);
			SWAP_OFF32(t.sh_offset);
			SWAP_WORD(t.sh_size);
			SWAP_WORD(t.sh_link);
			SWAP_WORD(t.sh_info);
			SWAP_WORD(t.sh_addralign);
			SWAP_WORD(t.sh_entsize);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_SHDR_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Shdr	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_SHDR, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Shdr *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Shdr))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Shdr */
		READ_WORD(s,t.sh_name);
		READ_WORD(s,t.sh_type);
		READ_XWORD(s,t.sh_flags);
		READ_ADDR64(s,t.sh_addr);
		READ_OFF64(s,t.sh_offset);
		READ_XWORD(s,t.sh_size);
		READ_WORD(s,t.sh_link);
		READ_WORD(s,t.sh_info);
		READ_XWORD(s,t.sh_addralign);
		READ_XWORD(s,t.sh_entsize);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Shdr */
			SWAP_WORD(t.sh_name);
			SWAP_WORD(t.sh_type);
			SWAP_XWORD(t.sh_flags);
			SWAP_ADDR64(t.sh_addr);
			SWAP_OFF64(t.sh_offset);
			SWAP_XWORD(t.sh_size);
			SWAP_WORD(t.sh_link);
			SWAP_WORD(t.sh_info);
			SWAP_XWORD(t.sh_addralign);
			SWAP_XWORD(t.sh_entsize);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* SHDR */
#if	__FreeBSD_version >= 600102 /* SWORD */

static int
libelf_cvt_SWORD_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Sword t, *s = (Elf64_Sword *) (uintptr_t) src;
	size_t c;

	(void) dsz;

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*s));
		return (1);
	}

	for (c = 0; c < count; c++) {
		t = *s++;
		SWAP_SWORD(t);
		WRITE_SWORD(dst,t);
	}

	return (1);
}

       
static int
libelf_cvt_SWORD_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Sword t, *d = (Elf64_Sword *) (uintptr_t) dst;
	size_t c;

	if (dsz < count * sizeof(Elf64_Sword))
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*d));
		return (1);
	}

	for (c = 0; c < count; c++) {
		READ_SWORD(src,t);
		SWAP_SWORD(t);
		*d++ = t;
	}

	return (1);
}

#endif /* SWORD */
#if	__FreeBSD_version >= 700009 /* SXWORD */

static int
libelf_cvt_SXWORD_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Sxword t, *s = (Elf64_Sxword *) (uintptr_t) src;
	size_t c;

	(void) dsz;

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*s));
		return (1);
	}

	for (c = 0; c < count; c++) {
		t = *s++;
		SWAP_SXWORD(t);
		WRITE_SXWORD(dst,t);
	}

	return (1);
}

       
static int
libelf_cvt_SXWORD_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Sxword t, *d = (Elf64_Sxword *) (uintptr_t) dst;
	size_t c;

	if (dsz < count * sizeof(Elf64_Sxword))
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*d));
		return (1);
	}

	for (c = 0; c < count; c++) {
		READ_SXWORD(src,t);
		SWAP_SXWORD(t);
		*d++ = t;
	}

	return (1);
}

#endif /* SXWORD */
#if	__FreeBSD_version >= 700025 /* SYMINFO */

static int
libelf_cvt32_SYMINFO_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Syminfo	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Syminfo *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Syminfo */
			SWAP_HALF(t.si_boundto);
			SWAP_HALF(t.si_flags);
			/**/
		}
		/* Write an Elf32_Syminfo */
		WRITE_HALF(dst,t.si_boundto);
		WRITE_HALF(dst,t.si_flags);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_SYMINFO_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Syminfo	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Syminfo *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Syminfo */
			SWAP_HALF(t.si_boundto);
			SWAP_HALF(t.si_flags);
			/**/
		}
		/* Write an Elf64_Syminfo */
		WRITE_HALF(dst,t.si_boundto);
		WRITE_HALF(dst,t.si_flags);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_SYMINFO_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Syminfo	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_SYMINFO, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Syminfo *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Syminfo))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Syminfo */
		READ_HALF(s,t.si_boundto);
		READ_HALF(s,t.si_flags);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Syminfo */
			SWAP_HALF(t.si_boundto);
			SWAP_HALF(t.si_flags);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_SYMINFO_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Syminfo	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_SYMINFO, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Syminfo *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Syminfo))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Syminfo */
		READ_HALF(s,t.si_boundto);
		READ_HALF(s,t.si_flags);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Syminfo */
			SWAP_HALF(t.si_boundto);
			SWAP_HALF(t.si_flags);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* SYMINFO */
#if	__FreeBSD_version >= 600102 /* SYM */

static int
libelf_cvt32_SYM_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Sym	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Sym *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Sym */
			SWAP_WORD(t.st_name);
			SWAP_ADDR32(t.st_value);
			SWAP_WORD(t.st_size);
			SWAP_HALF(t.st_shndx);
			/**/
		}
		/* Write an Elf32_Sym */
		WRITE_WORD(dst,t.st_name);
		WRITE_ADDR32(dst,t.st_value);
		WRITE_WORD(dst,t.st_size);
		WRITE_BYTE(dst,t.st_info);
		WRITE_BYTE(dst,t.st_other);
		WRITE_HALF(dst,t.st_shndx);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_SYM_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Sym	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Sym *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Sym */
			SWAP_WORD(t.st_name);
			SWAP_HALF(t.st_shndx);
			SWAP_ADDR64(t.st_value);
			SWAP_XWORD(t.st_size);
			/**/
		}
		/* Write an Elf64_Sym */
		WRITE_WORD(dst,t.st_name);
		WRITE_BYTE(dst,t.st_info);
		WRITE_BYTE(dst,t.st_other);
		WRITE_HALF(dst,t.st_shndx);
		WRITE_ADDR64(dst,t.st_value);
		WRITE_XWORD(dst,t.st_size);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_SYM_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Sym	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_SYM, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Sym *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Sym))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Sym */
		READ_WORD(s,t.st_name);
		READ_ADDR32(s,t.st_value);
		READ_WORD(s,t.st_size);
		READ_BYTE(s,t.st_info);
		READ_BYTE(s,t.st_other);
		READ_HALF(s,t.st_shndx);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Sym */
			SWAP_WORD(t.st_name);
			SWAP_ADDR32(t.st_value);
			SWAP_WORD(t.st_size);
			SWAP_HALF(t.st_shndx);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_SYM_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Sym	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_SYM, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Sym *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Sym))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Sym */
		READ_WORD(s,t.st_name);
		READ_BYTE(s,t.st_info);
		READ_BYTE(s,t.st_other);
		READ_HALF(s,t.st_shndx);
		READ_ADDR64(s,t.st_value);
		READ_XWORD(s,t.st_size);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Sym */
			SWAP_WORD(t.st_name);
			SWAP_HALF(t.st_shndx);
			SWAP_ADDR64(t.st_value);
			SWAP_XWORD(t.st_size);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* SYM */
#if	__FreeBSD_version >= 700009 /* VDEF */

static int
libelf_cvt32_VDEF_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Verdef	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Verdef *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Verdef */
			SWAP_HALF(t.vd_version);
			SWAP_HALF(t.vd_flags);
			SWAP_HALF(t.vd_ndx);
			SWAP_HALF(t.vd_cnt);
			SWAP_WORD(t.vd_hash);
			SWAP_WORD(t.vd_aux);
			SWAP_WORD(t.vd_next);
			/**/
		}
		/* Write an Elf32_Verdef */
		WRITE_HALF(dst,t.vd_version);
		WRITE_HALF(dst,t.vd_flags);
		WRITE_HALF(dst,t.vd_ndx);
		WRITE_HALF(dst,t.vd_cnt);
		WRITE_WORD(dst,t.vd_hash);
		WRITE_WORD(dst,t.vd_aux);
		WRITE_WORD(dst,t.vd_next);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_VDEF_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Verdef	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Verdef *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Verdef */
			SWAP_HALF(t.vd_version);
			SWAP_HALF(t.vd_flags);
			SWAP_HALF(t.vd_ndx);
			SWAP_HALF(t.vd_cnt);
			SWAP_WORD(t.vd_hash);
			SWAP_WORD(t.vd_aux);
			SWAP_WORD(t.vd_next);
			/**/
		}
		/* Write an Elf64_Verdef */
		WRITE_HALF(dst,t.vd_version);
		WRITE_HALF(dst,t.vd_flags);
		WRITE_HALF(dst,t.vd_ndx);
		WRITE_HALF(dst,t.vd_cnt);
		WRITE_WORD(dst,t.vd_hash);
		WRITE_WORD(dst,t.vd_aux);
		WRITE_WORD(dst,t.vd_next);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_VDEF_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Verdef	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_VDEF, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Verdef *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Verdef))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Verdef */
		READ_HALF(s,t.vd_version);
		READ_HALF(s,t.vd_flags);
		READ_HALF(s,t.vd_ndx);
		READ_HALF(s,t.vd_cnt);
		READ_WORD(s,t.vd_hash);
		READ_WORD(s,t.vd_aux);
		READ_WORD(s,t.vd_next);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Verdef */
			SWAP_HALF(t.vd_version);
			SWAP_HALF(t.vd_flags);
			SWAP_HALF(t.vd_ndx);
			SWAP_HALF(t.vd_cnt);
			SWAP_WORD(t.vd_hash);
			SWAP_WORD(t.vd_aux);
			SWAP_WORD(t.vd_next);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_VDEF_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Verdef	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_VDEF, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Verdef *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Verdef))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Verdef */
		READ_HALF(s,t.vd_version);
		READ_HALF(s,t.vd_flags);
		READ_HALF(s,t.vd_ndx);
		READ_HALF(s,t.vd_cnt);
		READ_WORD(s,t.vd_hash);
		READ_WORD(s,t.vd_aux);
		READ_WORD(s,t.vd_next);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Verdef */
			SWAP_HALF(t.vd_version);
			SWAP_HALF(t.vd_flags);
			SWAP_HALF(t.vd_ndx);
			SWAP_HALF(t.vd_cnt);
			SWAP_WORD(t.vd_hash);
			SWAP_WORD(t.vd_aux);
			SWAP_WORD(t.vd_next);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* VDEF */
#if	__FreeBSD_version >= 700009 /* VNEED */

static int
libelf_cvt32_VNEED_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Verneed	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf32_Verneed *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf32_Verneed */
			SWAP_HALF(t.vn_version);
			SWAP_HALF(t.vn_cnt);
			SWAP_WORD(t.vn_file);
			SWAP_WORD(t.vn_aux);
			SWAP_WORD(t.vn_next);
			/**/
		}
		/* Write an Elf32_Verneed */
		WRITE_HALF(dst,t.vn_version);
		WRITE_HALF(dst,t.vn_cnt);
		WRITE_WORD(dst,t.vn_file);
		WRITE_WORD(dst,t.vn_aux);
		WRITE_WORD(dst,t.vn_next);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt64_VNEED_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Verneed	t, *s;
	size_t c;

	(void) dsz;

	s = (Elf64_Verneed *) (uintptr_t) src;
	for (c = 0; c < count; c++) {
		t = *s++;
		if (byteswap) {
			/* Swap an Elf64_Verneed */
			SWAP_HALF(t.vn_version);
			SWAP_HALF(t.vn_cnt);
			SWAP_WORD(t.vn_file);
			SWAP_WORD(t.vn_aux);
			SWAP_WORD(t.vn_next);
			/**/
		}
		/* Write an Elf64_Verneed */
		WRITE_HALF(dst,t.vn_version);
		WRITE_HALF(dst,t.vn_cnt);
		WRITE_WORD(dst,t.vn_file);
		WRITE_WORD(dst,t.vn_aux);
		WRITE_WORD(dst,t.vn_next);
		/**/
	}

	return (1);
}
       
static int
libelf_cvt32_VNEED_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf32_Verneed	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf32_fsize(ELF_T_VNEED, (size_t) 1, EV_CURRENT);
	d   = ((Elf32_Verneed *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf32_Verneed))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf32_Verneed */
		READ_HALF(s,t.vn_version);
		READ_HALF(s,t.vn_cnt);
		READ_WORD(s,t.vn_file);
		READ_WORD(s,t.vn_aux);
		READ_WORD(s,t.vn_next);
		/**/
		if (byteswap) {
			/* Swap an Elf32_Verneed */
			SWAP_HALF(t.vn_version);
			SWAP_HALF(t.vn_cnt);
			SWAP_WORD(t.vn_file);
			SWAP_WORD(t.vn_aux);
			SWAP_WORD(t.vn_next);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}
       
static int
libelf_cvt64_VNEED_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Verneed	 t, *d;
	char		*s,*s0;
	size_t		fsz;

	fsz = elf64_fsize(ELF_T_VNEED, (size_t) 1, EV_CURRENT);
	d   = ((Elf64_Verneed *) (uintptr_t) dst) + (count - 1);
	s0  = (char *) src + (count - 1) * fsz;

	if (dsz < count * sizeof(Elf64_Verneed))
		return (0);

	while (count--) {
		s = s0;
		/* Read an Elf64_Verneed */
		READ_HALF(s,t.vn_version);
		READ_HALF(s,t.vn_cnt);
		READ_WORD(s,t.vn_file);
		READ_WORD(s,t.vn_aux);
		READ_WORD(s,t.vn_next);
		/**/
		if (byteswap) {
			/* Swap an Elf64_Verneed */
			SWAP_HALF(t.vn_version);
			SWAP_HALF(t.vn_cnt);
			SWAP_WORD(t.vn_file);
			SWAP_WORD(t.vn_aux);
			SWAP_WORD(t.vn_next);
			/**/
		}
		*d-- = t; s0 -= fsz;
	}

	return (1);
}

#endif /* VNEED */
#if	__FreeBSD_version >= 600102 /* WORD */

static int
libelf_cvt_WORD_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Word t, *s = (Elf64_Word *) (uintptr_t) src;
	size_t c;

	(void) dsz;

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*s));
		return (1);
	}

	for (c = 0; c < count; c++) {
		t = *s++;
		SWAP_WORD(t);
		WRITE_WORD(dst,t);
	}

	return (1);
}

       
static int
libelf_cvt_WORD_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Word t, *d = (Elf64_Word *) (uintptr_t) dst;
	size_t c;

	if (dsz < count * sizeof(Elf64_Word))
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*d));
		return (1);
	}

	for (c = 0; c < count; c++) {
		READ_WORD(src,t);
		SWAP_WORD(t);
		*d++ = t;
	}

	return (1);
}

#endif /* WORD */
#if	__FreeBSD_version >= 700009 /* XWORD */

static int
libelf_cvt_XWORD_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Xword t, *s = (Elf64_Xword *) (uintptr_t) src;
	size_t c;

	(void) dsz;

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*s));
		return (1);
	}

	for (c = 0; c < count; c++) {
		t = *s++;
		SWAP_XWORD(t);
		WRITE_XWORD(dst,t);
	}

	return (1);
}

       
static int
libelf_cvt_XWORD_tom(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	Elf64_Xword t, *d = (Elf64_Xword *) (uintptr_t) dst;
	size_t c;

	if (dsz < count * sizeof(Elf64_Xword))
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count * sizeof(*d));
		return (1);
	}

	for (c = 0; c < count; c++) {
		READ_XWORD(src,t);
		SWAP_XWORD(t);
		*d++ = t;
	}

	return (1);
}

#endif /* XWORD */


#if	__FreeBSD_version >= 800062
/*
 * Sections of type ELF_T_GNUHASH start with a header containing 4 32-bit
 * words.  Bloom filter data comes next, followed by hash buckets and the
 * hash chain.
 *
 * Bloom filter words are 64 bit wide on ELFCLASS64 objects and are 32 bit
 * wide on ELFCLASS32 objects.  The other objects in this section are 32
 * bits wide.
 *
 * Argument srcsz denotes the number of bytes to be converted.  In the
 * 32-bit case we need to translate srcsz to a count of 32-bit words.
 */

static int
libelf_cvt32_GNUHASH_tom(char *dst, size_t dsz, char *src, size_t srcsz,
    int byteswap)
{
	return (libelf_cvt_WORD_tom(dst, dsz, src, srcsz / sizeof(uint32_t),
	        byteswap));
}

static int
libelf_cvt32_GNUHASH_tof(char *dst, size_t dsz, char *src, size_t srcsz,
    int byteswap)
{
	return (libelf_cvt_WORD_tof(dst, dsz, src, srcsz / sizeof(uint32_t),
	        byteswap));
}

static int
libelf_cvt64_GNUHASH_tom(char *dst, size_t dsz, char *src, size_t srcsz,
    int byteswap)
{
	size_t sz;
	uint64_t t64, *bloom64;
	Elf_GNU_Hash_Header *gh;
	uint32_t n, nbuckets, nchains, maskwords, shift2, symndx, t32;
	uint32_t *buckets, *chains;

	sz = 4 * sizeof(uint32_t);	/* File header is 4 words long. */
	if (dsz < sizeof(Elf_GNU_Hash_Header) || srcsz < sz)
		return (0);

	/* Read in the section header and byteswap if needed. */
	READ_WORD(src, nbuckets);
	READ_WORD(src, symndx);
	READ_WORD(src, maskwords);
	READ_WORD(src, shift2);

	srcsz -= sz;

	if (byteswap) {
		SWAP_WORD(nbuckets);
		SWAP_WORD(symndx);
		SWAP_WORD(maskwords);
		SWAP_WORD(shift2);
	}

	/* Check source buffer and destination buffer sizes. */
	sz = nbuckets * sizeof(uint32_t) + maskwords * sizeof(uint64_t);
	if (srcsz < sz || dsz < sz + sizeof(Elf_GNU_Hash_Header))
		return (0);

	gh = (Elf_GNU_Hash_Header *) (uintptr_t) dst;
	gh->gh_nbuckets  = nbuckets;
	gh->gh_symndx    = symndx;
	gh->gh_maskwords = maskwords;
	gh->gh_shift2    = shift2;
	
	dsz -= sizeof(Elf_GNU_Hash_Header);
	dst += sizeof(Elf_GNU_Hash_Header);

	bloom64 = (uint64_t *) (uintptr_t) dst;

	/* Copy bloom filter data. */
	for (n = 0; n < maskwords; n++) {
		READ_XWORD(src, t64);
		if (byteswap)
			SWAP_XWORD(t64);
		bloom64[n] = t64;
	}

	/* The hash buckets follows the bloom filter. */
	dst += maskwords * sizeof(uint64_t);
	buckets = (uint32_t *) (uintptr_t) dst;

	for (n = 0; n < nbuckets; n++) {
		READ_WORD(src, t32);
		if (byteswap)
			SWAP_WORD(t32);
		buckets[n] = t32;
	}

	dst += nbuckets * sizeof(uint32_t);

	/* The hash chain follows the hash buckets. */
	dsz -= sz;
	srcsz -= sz;

	if (dsz < srcsz)	/* Destination lacks space. */
	        return (0);

	nchains = srcsz / sizeof(uint32_t);
	chains = (uint32_t *) (uintptr_t) dst;

	for (n = 0; n < nchains; n++) {
		READ_WORD(src, t32);
		if (byteswap)
			SWAP_WORD(t32);
		*chains++ = t32;
	}

	return (1);
}

static int
libelf_cvt64_GNUHASH_tof(char *dst, size_t dsz, char *src, size_t srcsz,
    int byteswap)
{
	uint32_t *s32;
	size_t sz, hdrsz;
	uint64_t *s64, t64;
	Elf_GNU_Hash_Header *gh;
	uint32_t maskwords, n, nbuckets, nchains, t0, t1, t2, t3, t32;

	hdrsz = 4 * sizeof(uint32_t);	/* Header is 4x32 bits. */
	if (dsz < hdrsz || srcsz < sizeof(Elf_GNU_Hash_Header))
		return (0);

	gh = (Elf_GNU_Hash_Header *) (uintptr_t) src;

	t0 = nbuckets = gh->gh_nbuckets;
	t1 = gh->gh_symndx;
	t2 = maskwords = gh->gh_maskwords;
	t3 = gh->gh_shift2;

	src   += sizeof(Elf_GNU_Hash_Header);
	srcsz -= sizeof(Elf_GNU_Hash_Header);
	dsz   -= hdrsz;

	sz = gh->gh_nbuckets * sizeof(uint32_t) + gh->gh_maskwords *
	    sizeof(uint64_t);

	if (srcsz < sz || dsz < sz)
		return (0);

 	/* Write out the header. */
	if (byteswap) {
		SWAP_WORD(t0);
		SWAP_WORD(t1);
		SWAP_WORD(t2);
		SWAP_WORD(t3);
	}

	WRITE_WORD(dst, t0);
	WRITE_WORD(dst, t1);
	WRITE_WORD(dst, t2);
	WRITE_WORD(dst, t3);

	/* Copy the bloom filter and the hash table. */
	s64 = (uint64_t *) (uintptr_t) src;
	for (n = 0; n < maskwords; n++) {
		t64 = *s64++;
		if (byteswap)
			SWAP_XWORD(t64);
		WRITE_WORD64(dst, t64);
	}

	s32 = (uint32_t *) s64;
	for (n = 0; n < nbuckets; n++) {
		t32 = *s32++;
		if (byteswap)
			SWAP_WORD(t32);
		WRITE_WORD(dst, t32);
	}

	srcsz -= sz;
	dsz   -= sz;

	/* Copy out the hash chains. */
	if (dsz < srcsz)
		return (0);

	nchains = srcsz / sizeof(uint32_t);
	for (n = 0; n < nchains; n++) {
		t32 = *s32++;
		if (byteswap)
			SWAP_WORD(t32);
		WRITE_WORD(dst, t32);
	}

	return (1);
}
#endif

/*
 * Elf_Note structures comprise a fixed size header followed by variable
 * length strings.  The fixed size header needs to be byte swapped, but
 * not the strings.
 *
 * Argument count denotes the total number of bytes to be converted.
 * The destination buffer needs to be at least count bytes in size.
 */
static int
libelf_cvt_NOTE_tom(char *dst, size_t dsz, char *src, size_t count, 
    int byteswap)
{
	uint32_t namesz, descsz, type;
	Elf_Note *en;
	size_t sz, hdrsz;

	if (dsz < count)	/* Destination buffer is too small. */
		return (0);

	hdrsz = 3 * sizeof(uint32_t);
	if (count < hdrsz)		/* Source too small. */
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count);
		return (1);
	}

	/* Process all notes in the section. */
	while (count > hdrsz) {
		/* Read the note header. */
		READ_WORD(src, namesz);
		READ_WORD(src, descsz);
		READ_WORD(src, type);

		/* Translate. */
		SWAP_WORD(namesz);
		SWAP_WORD(descsz);
		SWAP_WORD(type);

		/* Copy out the translated note header. */
		en = (Elf_Note *) (uintptr_t) dst;
		en->n_namesz = namesz;
		en->n_descsz = descsz;
		en->n_type = type;

		dsz -= sizeof(Elf_Note);
		dst += sizeof(Elf_Note);
		count -= hdrsz;

		ROUNDUP2(namesz, 4);
		ROUNDUP2(descsz, 4);

		sz = namesz + descsz;

		if (count < sz || dsz < sz)	/* Buffers are too small. */
			return (0);

		(void) memcpy(dst, src, sz);

		src += sz;
		dst += sz;

		count -= sz;
		dsz -= sz;
	}

	return (1);
}

static int
libelf_cvt_NOTE_tof(char *dst, size_t dsz, char *src, size_t count,
    int byteswap)
{
	uint32_t namesz, descsz, type;
	Elf_Note *en;
	size_t sz;

	if (dsz < count)
		return (0);

	if (!byteswap) {
		(void) memcpy(dst, src, count);
		return (1);
	}

	while (count > sizeof(Elf_Note)) {

		en = (Elf_Note *) (uintptr_t) src;
		namesz = en->n_namesz;
		descsz = en->n_descsz;
		type = en->n_type;

		SWAP_WORD(namesz);
		SWAP_WORD(descsz);
		SWAP_WORD(type);

		WRITE_WORD(dst, namesz);
		WRITE_WORD(dst, descsz);
		WRITE_WORD(dst, type);

		src += sizeof(Elf_Note);

		ROUNDUP2(namesz, 4);
		ROUNDUP2(descsz, 4);

		sz = namesz + descsz;

		if (count < sz)
			sz = count;

		(void) memcpy(dst, src, sz);

		src += sz;
		dst += sz;
		count -= sz;
	}

	return (1);
}

struct converters {
	int	(*tof32)(char *dst, size_t dsz, char *src, size_t cnt,
		    int byteswap);
	int	(*tom32)(char *dst, size_t dsz, char *src, size_t cnt,
		    int byteswap);
	int	(*tof64)(char *dst, size_t dsz, char *src, size_t cnt,
		    int byteswap);
	int	(*tom64)(char *dst, size_t dsz, char *src, size_t cnt,
		    int byteswap);
};



static struct converters cvt[ELF_T_NUM] = {
#if	__FreeBSD_version >= 600102
    [ELF_T_ADDR] = {
        .tof32 = libelf_cvt_ADDR32_tof, .tom32 = libelf_cvt_ADDR32_tom,
        .tof64 = libelf_cvt_ADDR64_tof, .tom64 = libelf_cvt_ADDR64_tom },
#endif
#if	__FreeBSD_version >= 700025
    [ELF_T_CAP] = {
        .tof32 = libelf_cvt32_CAP_tof, .tom32 = libelf_cvt32_CAP_tom,
        .tof64 = libelf_cvt64_CAP_tof, .tom64 = libelf_cvt64_CAP_tom },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_DYN] = {
        .tof32 = libelf_cvt32_DYN_tof, .tom32 = libelf_cvt32_DYN_tom,
        .tof64 = libelf_cvt64_DYN_tof, .tom64 = libelf_cvt64_DYN_tom },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_EHDR] = {
        .tof32 = libelf_cvt32_EHDR_tof, .tom32 = libelf_cvt32_EHDR_tom,
        .tof64 = libelf_cvt64_EHDR_tof, .tom64 = libelf_cvt64_EHDR_tom },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_HALF] = {
        .tof32 = libelf_cvt_HALF_tof, .tom32 = libelf_cvt_HALF_tom,
        .tof64 = libelf_cvt_HALF_tof, .tom64 = libelf_cvt_HALF_tom },
#endif
#if	__FreeBSD_version >= 700025
    [ELF_T_LWORD] = {
        .tof32 = libelf_cvt_LWORD_tof, .tom32 = libelf_cvt_LWORD_tom,
        .tof64 = libelf_cvt_LWORD_tof, .tom64 = libelf_cvt_LWORD_tom },
#endif
#if	__FreeBSD_version >= 700025
    [ELF_T_MOVE] = {
        .tof32 = libelf_cvt32_MOVE_tof, .tom32 = libelf_cvt32_MOVE_tom,
        .tof64 = libelf_cvt64_MOVE_tof, .tom64 = libelf_cvt64_MOVE_tom },
#endif
#if	__FreeBSD_version >= 700025
    [ELF_T_MOVEP] = {
        .tof32 = NULL, .tom32 = NULL,
        .tof64 = NULL, .tom64 = NULL },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_OFF] = {
        .tof32 = libelf_cvt_OFF32_tof, .tom32 = libelf_cvt_OFF32_tom,
        .tof64 = libelf_cvt_OFF64_tof, .tom64 = libelf_cvt_OFF64_tom },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_PHDR] = {
        .tof32 = libelf_cvt32_PHDR_tof, .tom32 = libelf_cvt32_PHDR_tom,
        .tof64 = libelf_cvt64_PHDR_tof, .tom64 = libelf_cvt64_PHDR_tom },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_REL] = {
        .tof32 = libelf_cvt32_REL_tof, .tom32 = libelf_cvt32_REL_tom,
        .tof64 = libelf_cvt64_REL_tof, .tom64 = libelf_cvt64_REL_tom },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_RELA] = {
        .tof32 = libelf_cvt32_RELA_tof, .tom32 = libelf_cvt32_RELA_tom,
        .tof64 = libelf_cvt64_RELA_tof, .tom64 = libelf_cvt64_RELA_tom },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_SHDR] = {
        .tof32 = libelf_cvt32_SHDR_tof, .tom32 = libelf_cvt32_SHDR_tom,
        .tof64 = libelf_cvt64_SHDR_tof, .tom64 = libelf_cvt64_SHDR_tom },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_SWORD] = {
        .tof32 = libelf_cvt_SWORD_tof, .tom32 = libelf_cvt_SWORD_tom,
        .tof64 = libelf_cvt_SWORD_tof, .tom64 = libelf_cvt_SWORD_tom },
#endif
#if	__FreeBSD_version >= 700009
    [ELF_T_SXWORD] = {
        .tof32 = NULL, .tom32 = NULL,
        .tof64 = libelf_cvt_SXWORD_tof, .tom64 = libelf_cvt_SXWORD_tom },
#endif
#if	__FreeBSD_version >= 700025
    [ELF_T_SYMINFO] = {
        .tof32 = libelf_cvt32_SYMINFO_tof, .tom32 = libelf_cvt32_SYMINFO_tom,
        .tof64 = libelf_cvt64_SYMINFO_tof, .tom64 = libelf_cvt64_SYMINFO_tom },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_SYM] = {
        .tof32 = libelf_cvt32_SYM_tof, .tom32 = libelf_cvt32_SYM_tom,
        .tof64 = libelf_cvt64_SYM_tof, .tom64 = libelf_cvt64_SYM_tom },
#endif
#if	__FreeBSD_version >= 700009
    [ELF_T_VDEF] = {
        .tof32 = libelf_cvt32_VDEF_tof, .tom32 = libelf_cvt32_VDEF_tom,
        .tof64 = libelf_cvt64_VDEF_tof, .tom64 = libelf_cvt64_VDEF_tom },
#endif
#if	__FreeBSD_version >= 700009
    [ELF_T_VNEED] = {
        .tof32 = libelf_cvt32_VNEED_tof, .tom32 = libelf_cvt32_VNEED_tom,
        .tof64 = libelf_cvt64_VNEED_tof, .tom64 = libelf_cvt64_VNEED_tom },
#endif
#if	__FreeBSD_version >= 600102
    [ELF_T_WORD] = {
        .tof32 = libelf_cvt_WORD_tof, .tom32 = libelf_cvt_WORD_tom,
        .tof64 = libelf_cvt_WORD_tof, .tom64 = libelf_cvt_WORD_tom },
#endif
#if	__FreeBSD_version >= 700009
    [ELF_T_XWORD] = {
        .tof32 = NULL, .tom32 = NULL,
        .tof64 = libelf_cvt_XWORD_tof, .tom64 = libelf_cvt_XWORD_tom },
#endif


	/*
	 * Types that needs hand-coded converters follow.
	 */

	[ELF_T_BYTE] = {
		.tof32 = libelf_cvt_BYTE_tox,
		.tom32 = libelf_cvt_BYTE_tox,
		.tof64 = libelf_cvt_BYTE_tox,
		.tom64 = libelf_cvt_BYTE_tox
	},

#if	__FreeBSD_version >= 800062
	[ELF_T_GNUHASH] = {
		.tof32 = libelf_cvt32_GNUHASH_tof,
		.tom32 = libelf_cvt32_GNUHASH_tom,
		.tof64 = libelf_cvt64_GNUHASH_tof,
		.tom64 = libelf_cvt64_GNUHASH_tom
	},
#endif

	[ELF_T_NOTE] = {
		.tof32 = libelf_cvt_NOTE_tof,
		.tom32 = libelf_cvt_NOTE_tom,
		.tof64 = libelf_cvt_NOTE_tof,
		.tom64 = libelf_cvt_NOTE_tom
	}
};

int (*_libelf_get_translator(Elf_Type t, int direction, int elfclass))
 (char *_dst, size_t dsz, char *_src, size_t _cnt, int _byteswap)
{
	assert(elfclass == ELFCLASS32 || elfclass == ELFCLASS64);
	assert(direction == ELF_TOFILE || direction == ELF_TOMEMORY);

	if (t >= ELF_T_NUM ||
	    (elfclass != ELFCLASS32 && elfclass != ELFCLASS64) ||
	    (direction != ELF_TOFILE && direction != ELF_TOMEMORY))
		return (NULL);

	return ((elfclass == ELFCLASS32) ?
	    (direction == ELF_TOFILE ? cvt[t].tof32 : cvt[t].tom32) :
	    (direction == ELF_TOFILE ? cvt[t].tof64 : cvt[t].tom64));
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






























