#include_next <sys/cdefs.h>

/*-
 * Copyright (c) 1991, 1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Berkeley Software Design, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      @(#)cdefs.h     8.8 (Berkeley) 1/9/95
 * $FreeBSD$
 */

#ifndef        _SYS_CDEFS_H_
#define        _SYS_CDEFS_H_

#define        __dead2                __attribute__((__noreturn__))
#define        __pure2                __attribute__((__const__))
#ifndef __DO_NOT_DEFINE_UNUSED        /* See <netdb.h> et al */
#define        __unused        __attribute__((__unused__))
#endif
#define        __used                __attribute__((__used__))
#define        __packed        __attribute__((__packed__))
#define        __aligned(x)        __attribute__((__aligned__(x)))
#define        __section(x)        __attribute__((__section__(x)))

#define __offsetof(type, field)        __builtin_offsetof(type, field)
#define __printflike(fmtarg, firstvararg) \
        __attribute__((__format__ (__printf__, fmtarg, firstvararg)))

#define        __containerof(x, s, m) ({                                        \
        const volatile __typeof(((s *)0)->m) *__x = (x);                \
        __DEQUALIFY(s *, (const volatile char *)__x - __offsetof(s, m));\
})

/* Requires freebsd-gcc extensions */
#define __printf0like(fmtarg, firstvararg)

#define __FBSDID(s)                struct __hack
#define __RCSID(s)                struct __hack
#define __RCSID_SOURCE(s)        struct __hack
#define __SCCSID(s)                struct __hack
#define __COPYRIGHT(s)                struct __hack

#ifndef __DECONST
#define __DECONST(type, var)        ((type)(__uintptr_t)(const void *)(var))
#endif

#ifndef __DEVOLATILE
#define __DEVOLATILE(type, var)        ((type)(__uintptr_t)(volatile void *)(var))
#endif

#ifndef __DEQUALIFY
#define __DEQUALIFY(type, var)        ((type)(__uintptr_t)(const volatile void *)(var))
#endif

/* Fix for nested __CONCAT as used in <sys/elf.h>.  */
#undef __CONCAT
#define __CONCAT1(x,y)        x ## y
#define __CONCAT(x,y)        __CONCAT1(x,y)

#endif
