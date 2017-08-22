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
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
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

#include <ar.h>
#include <assert.h>
#include <libelf.h>

#include "_libelf.h"

Elf_Cmd
elf_next(Elf *e)
{
        off_t next;
        Elf *parent;

        if (e == NULL)
                return (ELF_C_NULL);

         if ((parent = e->e_parent) == NULL) {
                 LIBELF_SET_ERROR(ARGUMENT, 0);
                 return (ELF_C_NULL);
         }

        assert (parent->e_kind == ELF_K_AR);
        assert (parent->e_cmd == ELF_C_READ);
        assert((uintptr_t) e->e_rawfile % 2 == 0);
        assert(e->e_rawfile > parent->e_rawfile);

        next = e->e_rawfile - parent->e_rawfile + e->e_rawsize;
        next = (next + 1) & ~1;        /* round up to an even boundary */

        parent->e_u.e_ar.e_next = (next >= (off_t) parent->e_rawsize) ? (off_t) 0 : next;

        return (ELF_C_READ);
}
