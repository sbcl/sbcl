/*
 * memory validation
 */

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

#include <stdio.h>
#include <stdlib.h>

#include "sbcl.h"
#include "runtime.h"
#include "os.h"
#include "globals.h"
#include "interr.h"
#include "validate.h"
#include "interr.h"                     /* for declaration of lose */

#if defined(LISP_FEATURE_RELOCATABLE_HEAP)
#ifdef LISP_FEATURE_CHENEYGC
uword_t DYNAMIC_0_SPACE_START, DYNAMIC_1_SPACE_START;
#else
uword_t DYNAMIC_SPACE_START;
#endif
#endif

uword_t asm_routines_start, asm_routines_end;

static void
ensure_space(uword_t start, uword_t size)
{
    if (os_validate(NOT_MOVABLE, (os_vm_address_t)start, (os_vm_size_t)size)==NULL) {
        fprintf(stderr,
                "ensure_space: failed to allocate %lu bytes at %p\n",
                (long unsigned)size, (void*)start);
        fprintf(stderr,
                "(hint: Try \"ulimit -a\"; maybe you should increase memory limits.)\n");
        exit(1);
    }
}

os_vm_address_t undefined_alien_address = 0;

static void
ensure_undefined_alien(void) {
    os_vm_address_t start = os_allocate(os_vm_page_size);
    if (start) {
        os_protect(start, os_vm_page_size, OS_VM_PROT_NONE);
        undefined_alien_address = start;
    } else {
        lose("could not allocate guard page for undefined alien\n");
    }
}

boolean allocate_hardwired_spaces(boolean hard_failp)
{
#ifdef PRINTNOISE
    printf("allocating memory ...");
    fflush(stdout);
#endif
    struct {
        uword_t start;
        unsigned size;
    } preinit_spaces[] = {
      { READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE },
      { STATIC_SPACE_START, STATIC_SPACE_SIZE },
#ifdef LISP_FEATURE_LINKAGE_TABLE
      { LINKAGE_TABLE_SPACE_START, LINKAGE_TABLE_SPACE_SIZE },
#endif
    };
    int i;
    int n_spaces = sizeof preinit_spaces / sizeof preinit_spaces[0];
    boolean success = 1;
    for (i = 0; i< n_spaces; ++i) {
        if (hard_failp)
            ensure_space(preinit_spaces[i].start, preinit_spaces[i].size);
        else if (!os_validate(NOT_MOVABLE,
                              (os_vm_address_t)preinit_spaces[i].start,
                              preinit_spaces[i].size)) {
            success = 0;
            break;
        }
    }
#ifdef PRINTNOISE
    printf(" done.\n");
#endif
    return success;
}

void
allocate_spaces(boolean did_preinit)
{
#ifndef LISP_FEATURE_RELOCATABLE_HEAP
    // Allocate the largest space(s) first,
    // since if that fails, it's game over.
#ifdef LISP_FEATURE_GENCGC
    ensure_space(DYNAMIC_SPACE_START  , dynamic_space_size);
#else
    ensure_space(DYNAMIC_0_SPACE_START, dynamic_space_size);
    ensure_space(DYNAMIC_1_SPACE_START, dynamic_space_size);
#endif
#endif

    if (!did_preinit)
      allocate_hardwired_spaces(1);

#ifdef LISP_FEATURE_OS_PROVIDES_DLOPEN
    ensure_undefined_alien();
#endif
}

static inline void
protect_page(void *page, int protect_p, os_vm_prot_t flags) {
    os_protect(page, os_vm_page_size, protect_p ?
               flags : OS_VM_PROT_ALL);
}

#define DEF_PROTECT_PAGE(name,page_name,flags)                          \
    void                                                                \
    protect_##name(int protect_p, struct thread *thread) {              \
        if (!thread)                                                    \
            thread = arch_os_get_current_thread();                      \
        protect_page(page_name(thread), protect_p, flags);              \
    }

DEF_PROTECT_PAGE(control_stack_hard_guard_page,
                 CONTROL_STACK_HARD_GUARD_PAGE,
                 OS_VM_PROT_NONE)
DEF_PROTECT_PAGE(control_stack_guard_page,
                 CONTROL_STACK_GUARD_PAGE,
                 OS_VM_PROT_READ|OS_VM_PROT_EXECUTE)
DEF_PROTECT_PAGE(control_stack_return_guard_page,
                 CONTROL_STACK_RETURN_GUARD_PAGE,
                 OS_VM_PROT_READ|OS_VM_PROT_EXECUTE)

DEF_PROTECT_PAGE(binding_stack_hard_guard_page,
                 BINDING_STACK_HARD_GUARD_PAGE,
                 OS_VM_PROT_NONE)
DEF_PROTECT_PAGE(binding_stack_guard_page,
                 BINDING_STACK_GUARD_PAGE,
                 OS_VM_PROT_NONE)
DEF_PROTECT_PAGE(binding_stack_return_guard_page,
                 BINDING_STACK_RETURN_GUARD_PAGE,
                 OS_VM_PROT_NONE)

DEF_PROTECT_PAGE(alien_stack_hard_guard_page,
                 ALIEN_STACK_HARD_GUARD_PAGE,
                 OS_VM_PROT_NONE)
DEF_PROTECT_PAGE(alien_stack_guard_page,
                 ALIEN_STACK_GUARD_PAGE,
                 OS_VM_PROT_NONE)
DEF_PROTECT_PAGE(alien_stack_return_guard_page,
                 ALIEN_STACK_RETURN_GUARD_PAGE,
                 OS_VM_PROT_NONE)
