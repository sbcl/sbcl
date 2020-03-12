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

#ifdef LISP_FEATURE_CHENEYGC
uword_t DYNAMIC_0_SPACE_START, DYNAMIC_1_SPACE_START;
#else
uword_t DYNAMIC_SPACE_START;
#endif

uword_t asm_routines_start, asm_routines_end;

// Return the ALLOCATE_LOW flag or 0 for the hardwired spaces
// depending on the backend.  Why specify the ALLOCATE_LOW on a non-relocatable
// mapping? To make the OS tell us an address that it would have been ok with,
// as well as our code being ok with. Otherwise, we see unhelpful output:
//  "mmap: wanted 1048576 bytes at 0x50000000, actually mapped at 0x7f75b1f6b000"
// which could never work as the base of static space on x86-64.
// Care is needed because not all backends put the small spaces below 2GB.
// In particular, arm64 has #xF0000000 which is above 2GB but below 4GB.
// The ALLOCATE_LOW flag means that the limit is 2GB.
// (See MAP_32BIT in http://man7.org/linux/man-pages/man2/mmap.2.html)
static const int should_allocate_low =
#ifdef LISP_FEATURE_X86_64
    ALLOCATE_LOW;
#else
    0;
#endif

static void
ensure_space(int attributes, uword_t start, uword_t size)
{
    if (os_validate(attributes, (os_vm_address_t)start, (os_vm_size_t)size)==NULL) {
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
    os_vm_address_t start = os_validate(MOVABLE|IS_GUARD_PAGE, NULL,
#ifndef LISP_FEATURE_WIN32
    /* We can/should disregard our 'os_vm_page_size' constant which tends to be
     * larger than the granularity that the OS will allow you to manipulate via
     * mprotect(). e.g. on x86-64-linux we use a page size of 32K but in reality
     * the protection granularity is 4K.
     * Moreover, since the memory protection is not changed after allocation,
     * the granuarity that mprotect() operates on is immaterial. As such, it
     * probably would work to put N_WORD_BYTES here since that's all we need. */
                                        getpagesize()
#else // Use the same value as does contrib/sb-posix/interface.lisp
                                        4096
#endif
                                        );
    if (start) {
        undefined_alien_address = start;
    } else {
        lose("could not allocate guard page for undefined alien");
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
      { LINKAGE_TABLE_SPACE_START, LINKAGE_TABLE_SPACE_SIZE },
      { STATIC_SPACE_START, STATIC_SPACE_SIZE },
    };
    int i;
    int n_spaces = sizeof preinit_spaces / sizeof preinit_spaces[0];
    boolean success = 1;
    for (i = 0; i< n_spaces; ++i) {
        if (!preinit_spaces[i].size) continue;
        if (hard_failp)
            ensure_space(NOT_MOVABLE | should_allocate_low,
                         preinit_spaces[i].start, preinit_spaces[i].size);
        else if (!os_validate(NOT_MOVABLE | should_allocate_low,
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
allocate_lisp_dynamic_space(boolean did_preinit)
{
    // Small spaces can be allocated after large spaces are.
    // The above code is only utilized when heap relocation is disabled.
    // And when so, failure to allocate dynamic space is fatal.
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
