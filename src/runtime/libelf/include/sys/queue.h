/*-
 * Copyright (c) 1991, 1993
 *        The Regents of the University of California.  All rights reserved.
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
 *        @(#)queue.h        8.5 (Berkeley) 8/20/94
 * $FreeBSD$
 */

#if defined(__FreeBSD_kernel__) && !defined(__FREEBSD_GLUE_USE_EMBEDDED_HEADERS)
#include_next <sys/queue.h>        /* Supply disabled macros using the Glibc version */
#include <sys/kern/queue.h>
#else
#include_next <sys/queue.h>
#include <sys/cdefs.h>

/* Currently, Debian eglibc only defines these on kfreebsd-* platforms.  */

#ifndef LIST_FOREACH_SAFE
#define        LIST_FOREACH_SAFE(var, head, field, tvar)                        \
        for ((var) = ((head)->lh_first);                                \
            (var) && ((tvar) = ((var)->field.le_next), 1);                \
            (var) = (tvar))
#endif

#ifndef SLIST_FOREACH_SAFE
#define        SLIST_FOREACH_SAFE(var, head, field, tvar)                        \
        for ((var) = SLIST_FIRST((head));                                \
            (var) && ((tvar) = SLIST_NEXT((var), field), 1);                \
            (var) = (tvar))
#endif

#ifndef SLIST_FOREACH_PREVPTR
#define        SLIST_FOREACH_PREVPTR(var, varp, head, field)                        \
        for ((varp) = &SLIST_FIRST((head));                                \
            ((var) = *(varp)) != NULL;                                        \
            (varp) = &SLIST_NEXT((var), field))
#endif

#ifndef STAILQ_REMOVE_HEAD_UNTIL
#define        STAILQ_REMOVE_HEAD_UNTIL(head, elm, field) do {                        \
        if (((head)->stqh_first = ((elm)->field.stqe_next)) == NULL)        \
                (head)->stqh_last = &((head)->stqh_first);                \
} while (/*CONSTCOND*/0)
#endif

#ifndef STAILQ_FOREACH_SAFE
#define        STAILQ_FOREACH_SAFE(var, head, field, tvar)                        \
        for ((var) = ((head)->stqh_first);                                \
            (var) && ((tvar) = ((var)->field.stqe_next), 1);                \
            (var) = (tvar))
#endif

#ifndef STAILQ_LAST
#define        STAILQ_LAST(head, type, field)                                        \
        (STAILQ_EMPTY((head)) ?                                                \
                NULL :                                                        \
                ((struct type *)(void *)                                \
                ((char *)((head)->stqh_last) - __offsetof(struct type, field))))
#endif

#ifndef TAILQ_FOREACH_SAFE
#define        TAILQ_FOREACH_SAFE(var, head, field, tvar)                        \
        for ((var) = ((head)->tqh_first);                                \
            (var) && ((tvar) = ((var)->field.tqe_next), 1);                \
            (var) = (tvar))
#endif

#endif


#ifdef TAILQ_FOREACH_REVERSE_SAFE
#undef TAILQ_FOREACH_REVERSE_SAFE
#define        TAILQ_FOREACH_REVERSE_SAFE(var, head, headname, field, tvar)        \
        for ((var) = (*(((struct headname *)((head)->tqh_last))->tqh_last));        \
            (var) && ((tvar) = (*(((struct headname *)((var)->field.tqe_prev))->tqh_last)), 1);        \
            (var) = (tvar))
#endif
