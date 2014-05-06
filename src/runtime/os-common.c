/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */
# define _GNU_SOURCE /* needed for RTLD_DEFAULT from dlfcn.h */
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "sbcl.h"
#include "globals.h"
#include "runtime.h"
#include "genesis/config.h"
#include "genesis/constants.h"
#include "genesis/cons.h"
#include "genesis/vector.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include "thread.h"
#include "sbcl.h"
#include "os.h"
#include "interr.h"
#if defined(LISP_FEATURE_OS_PROVIDES_DLOPEN) && !defined(LISP_FEATURE_WIN32)
# include <dlfcn.h>
#endif


/* Except for os_zero, these routines are only called by Lisp code.
 * These routines may also be replaced by os-dependent versions
 * instead. See hpux-os.c for some useful restrictions on actual
 * usage. */

void
os_zero(os_vm_address_t addr, os_vm_size_t length)
{
    os_vm_address_t block_start;
    os_vm_size_t block_size;

#ifdef DEBUG
    fprintf(stderr,";;; os_zero: addr: 0x%08x, len: 0x%08x\n",addr,length);
#endif

    block_start = os_round_up_to_page(addr);

    length -= block_start-addr;
    block_size = os_trunc_size_to_page(length);

    if (block_start > addr)
        bzero((char *)addr, block_start-addr);
    if (block_size < length)
        bzero((char *)block_start+block_size, length-block_size);

    if (block_size != 0) {
        /* Now deallocate and allocate the block so that it faults in
         * zero-filled. */

        os_invalidate(block_start, block_size);
        addr = os_validate(block_start, block_size);

        if (addr == NULL || addr != block_start)
            lose("os_zero: block moved! 0x%08x ==> 0x%08x\n",
                 block_start,
                 addr);
    }
}

os_vm_address_t
os_allocate(os_vm_size_t len)
{
    return os_validate((os_vm_address_t)NULL, len);
}

void
os_deallocate(os_vm_address_t addr, os_vm_size_t len)
{
    os_invalidate(addr,len);
}

int
os_get_errno(void)
{
    return errno;
}


#if defined(LISP_FEATURE_SB_THREAD) && (!defined(CANNOT_USE_POSIX_SEM_T) || defined(LISP_FEATURE_WIN32))

void
os_sem_init(os_sem_t *sem, unsigned int value)
{
    if (-1==sem_init(sem, 0, value))
        lose("os_sem_init(%p, %u): %s", sem, value, strerror(errno));
    FSHOW((stderr, "os_sem_init(%p, %u)\n", sem, value));
}

void
os_sem_wait(os_sem_t *sem, char *what)
{
    FSHOW((stderr, "%s: os_sem_wait(%p) ...\n", what, sem));
    while (-1 == sem_wait(sem))
        if (EINTR!=errno)
            lose("%s: os_sem_wait(%p): %s", what, sem, strerror(errno));
    FSHOW((stderr, "%s: os_sem_wait(%p) => ok\n", what, sem));
}

void
os_sem_post(sem_t *sem, char *what)
{
    if (-1 == sem_post(sem))
        lose("%s: os_sem_post(%p): %s", what, sem, strerror(errno));
    FSHOW((stderr, "%s: os_sem_post(%p)\n", what, sem));
}

void
os_sem_destroy(os_sem_t *sem)
{
    if (-1==sem_destroy(sem))
        lose("os_sem_destroy(%p): %s", sem, strerror(errno));
}

#endif

#if defined(LISP_FEATURE_OS_PROVIDES_DLOPEN) && !defined(LISP_FEATURE_WIN32)
void* os_dlopen(char* name, int flags) {
    return dlopen(name,flags);
}
#endif

#if defined(LISP_FEATURE_SB_DYNAMIC_CORE)
/* When this feature is enabled, the special category of /static/ foreign
 * symbols disappears. Foreign fixups are resolved to linkage table locations
 * during genesis, and for each of them a record is added to
 * REQUIRED_RUNTIME_C_SYMBOLS list, of the form (cons name datap).
 *
 * Name is a base-string of a symbol name, and non-nil datap marks data
 * references.
 *
 * Before any code in lisp image can be called, we have to resolve all
 * references to runtime foreign symbols that used to be static, adding linkage
 * table entry for each element of REQUIRED_RUNTIME_C_SYMBOLS.
 */

/* We start with a little greenspunning to make car, cdr and base-string data
 * accessible. */

/* Object tagged? (dereference (cast (untag (obj)))) */
#define FOLLOW(obj,lowtagtype,ctype)            \
    (*(struct ctype*)(obj - lowtagtype##_LOWTAG))

/* For all types sharing OTHER_POINTER_LOWTAG: */
#define FOTHERPTR(obj,ctype)                    \
    FOLLOW(obj,OTHER_POINTER,ctype)

static inline lispobj car(lispobj conscell)
{
    return FOLLOW(conscell,LIST_POINTER,cons).car;
}

static inline lispobj cdr(lispobj conscell)
{
    return FOLLOW(conscell,LIST_POINTER,cons).cdr;
}

#ifndef LISP_FEATURE_WIN32
void *
os_dlsym_default(char *name)
{
    void *frob = dlsym(RTLD_DEFAULT, name);
    odxprint(misc, "%p", frob);
    return frob;
}
#endif

void os_link_runtime()
{
    lispobj head;
    void *link_target = (void*)(intptr_t)LINKAGE_TABLE_SPACE_START;
    void *validated_end = link_target;
    lispobj symbol_name;
    char *namechars;
    boolean datap;
    void* result;
    int strict /* If in a cold core, fail early and often. */
      = (SymbolValue(GC_INHIBIT, 0) & WIDETAG_MASK) == UNBOUND_MARKER_WIDETAG;
    int n = 0, m = 0;

    for (head = SymbolValue(REQUIRED_RUNTIME_C_SYMBOLS,0);
         head!=NIL; head = cdr(head), n++)
    {
        lispobj item = car(head);
        symbol_name = car(item);
        datap = (NIL!=(cdr(item)));
        namechars = (void*)(intptr_t)FOTHERPTR(symbol_name,vector).data;
        result = os_dlsym_default(namechars);
        odxprint(runtime_link, "linking %s => %p", namechars, result);

        if (link_target == validated_end) {
            validated_end += os_vm_page_size;
#ifdef LISP_FEATURE_WIN32
            os_validate_recommit(link_target,os_vm_page_size);
#endif
        }
        if (result) {
            if (datap)
                arch_write_linkage_table_ref(link_target,result);
            else
                arch_write_linkage_table_jmp(link_target,result);
        } else {
            m++;
            if (strict)
                fprintf(stderr,
                        "undefined foreign symbol in cold init: %s\n",
                        namechars);
        }

        link_target = (void*)(((uintptr_t)link_target)+LINKAGE_TABLE_ENTRY_SIZE);
    }
    odxprint(runtime_link, "%d total symbols linked, %d undefined",
             n, m);
    if (strict && m)
        /* We could proceed, but rather than run into improperly
         * displayed internal errors, let's make ourselves heard right
         * here and now. */
        lose("Undefined aliens in cold init.");
}
#endif  /* sb-dynamic-core */
