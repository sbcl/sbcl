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
#ifdef MEMORY_SANITIZER
#include <sanitizer/msan_interface.h>
#endif

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
#include "arch.h"
#include "interr.h"
#if defined(LISP_FEATURE_OS_PROVIDES_DLOPEN) && !defined(LISP_FEATURE_WIN32)
# include <dlfcn.h>
#endif

/* Expose to Lisp the value of the preprocessor define. Don't touch! */
int install_sig_memory_fault_handler = INSTALL_SIG_MEMORY_FAULT_HANDLER;

/* Except for os_zero, these routines are only called by Lisp code.
 * These routines may also be replaced by os-dependent versions
 * instead. See hpux-os.c for some useful restrictions on actual
 * usage. */

#ifdef LISP_FEATURE_CHENEYGC
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
        addr = os_validate(NOT_MOVABLE, block_start, block_size);

        if (addr == NULL || addr != block_start)
            lose("os_zero: block moved! 0x%08x ==> 0x%08x\n",
                 block_start,
                 addr);
    }
}
#endif

os_vm_address_t
os_allocate(os_vm_size_t len)
{
    return os_validate(MOVABLE, (os_vm_address_t)NULL, len);
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

/* When :SB-DYNAMIC-CORE is enabled, the special category of /static/ foreign
 * symbols disappears. Foreign fixups are resolved to linkage table locations
 * during genesis, and for each of them a record is added to
 * REQUIRED_FOREIGN_SYMBOLS vector, of the form "name" for a function reference,
 * or ("name") for a data reference. "name" is a base-string.
 *
 * Before any code in lisp image can be called, we have to resolve all
 * references to runtime foreign symbols that used to be static, adding linkage
 * table entry for each element of REQUIRED_FOREIGN_SYMBOLS.
 */

#if defined(LISP_FEATURE_SB_DYNAMIC_CORE) && !defined(LISP_FEATURE_WIN32)
void *
os_dlsym_default(char *name)
{
    void *frob = dlsym(RTLD_DEFAULT, name);
    odxprint(misc, "%p", frob);
    return frob;
}
#endif

#ifdef MEMORY_SANITIZER
/* Unless the Lisp compiler annotates every (SETF SAP-REF-n) to update
 * shadow memory indicating non-poison state byte-for-byte,
 * we need to unpoison all malloc() results,
 * otherwise the memory can't be read by sanitized code.
 */
static void* malloc_unpoisoned(size_t size)
{
    void* result = malloc(size);
    if (result)
        __msan_unpoison(result, size);
    return result;
}
#endif

void os_link_runtime()
{
    extern void write_protect_immobile_space();

#ifdef LISP_FEATURE_SB_DYNAMIC_CORE
    char *link_target = (char*)(intptr_t)LINKAGE_TABLE_SPACE_START;
    void *validated_end = link_target;
    lispobj symbol_name;
    char *namechars;
    boolean datap;
    void* result;
    int n = 0, m = 0, j;

    struct vector* symbols = VECTOR(SymbolValue(REQUIRED_FOREIGN_SYMBOLS,0));
    n = fixnum_value(symbols->length);
    for (j = 0 ; j < n ; ++j)
    {
        lispobj item = symbols->data[j];
        datap = lowtag_of(item) == LIST_POINTER_LOWTAG;
        symbol_name = datap ? CONS(item)->car : item;
        namechars = (void*)(intptr_t)(VECTOR(symbol_name)->data);
#ifdef MEMORY_SANITIZER
        if (!strcmp(namechars,"malloc"))
            result = malloc_unpoisoned;
        else
#endif
            result = os_dlsym_default(namechars);
        odxprint(runtime_link, "linking %s => %p", namechars, result);

        if (link_target == validated_end) {
            validated_end = (char*)validated_end + os_vm_page_size;
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
        }

        link_target += LINKAGE_TABLE_ENTRY_SIZE;
    }
    odxprint(runtime_link, "%d total symbols linked, %d undefined",
             n, m);
#endif /* LISP_FEATURE_SB_DYNAMIC_CORE */

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    /* Delayed until after dynamic space has been mapped, fixups made,
     * and/or immobile-space linkage entries written,
     * since it was too soon earlier to handle write faults. */
    write_protect_immobile_space();
#endif
}

#ifndef LISP_FEATURE_WIN32

/* Remap a part of an already existing mapping to a file */
void os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    os_vm_address_t actual;

    actual = mmap(addr, len, OS_VM_PROT_ALL, MAP_PRIVATE | MAP_FIXED,
                  fd, (off_t) offset);
    if (actual == MAP_FAILED || (addr && (addr != actual))) {
        perror("mmap");
        lose("unexpected mmap(%d, %d, ...) failure\n", addr, len);
    }
}

boolean
gc_managed_addr_p(lispobj ad)
{
    struct thread *th;

    if ((READ_ONLY_SPACE_START <= ad && ad < READ_ONLY_SPACE_END)
        || (STATIC_SPACE_START <= ad && ad < STATIC_SPACE_END)
#if defined LISP_FEATURE_GENCGC
        || (DYNAMIC_SPACE_START <= ad &&
            ad < (DYNAMIC_SPACE_START + dynamic_space_size))
        || immobile_space_p(ad)
#else
        || (DYNAMIC_0_SPACE_START <= ad && ad < DYNAMIC_0_SPACE_END)
        || (DYNAMIC_1_SPACE_START <= ad && ad < DYNAMIC_1_SPACE_END)
#endif
        )
        return 1;
    for_each_thread(th) {
        if(th->control_stack_start <= (lispobj*)ad
           && (lispobj*)ad < th->control_stack_end)
            return 1;
        if(th->binding_stack_start <= (lispobj*)ad
           && (lispobj*)ad < th->binding_stack_start + BINDING_STACK_SIZE)
            return 1;
    }
    return 0;
}

#endif
