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

#include "genesis/sbcl.h"
#include "runtime.h"
#include "os.h"
#include "globals.h"
#include "interr.h"
#include "validate.h"

/* Using madvise to define guard pages is better than using mprotect and PROT_NONE.
 * With madvise, it does not split the VMA into pieces based on the protection.
 * The kernel knows that a page fault occurring on a non-present page indicated
 * as a guard page should cause delivery of SIGSEGV instead of bringing in a
 * physical page. To illustrate how this creates fewer VMAs, notice what happens
 * when ranges are broken up by different protections with mprotect -
 *
 * |      <-- control || bindings ->     ||         <- alien |
 * |prot_none|   prot_rw        |     prot_none    | prot_rw |  = 4 VMAs
 * | guards  | usable || usable | guards || guards |  usable |
 *
 * This picture is for downward control stack growth. If the growth direction is upward,
 * the kernel can not merge the usable range of the control stack with the usable range
 * of the binding stack, so there is actually 1 more VMA.
 * With madvise() the entire thread memory area is backed by a single VMA.
 * See https://github.com/torvalds/linux/commit/662df3e5c376
 */

static inline void
protect_guard_page(void *page, int protect_p, os_vm_prot_t flags) {
#ifdef MADV_GUARD_INSTALL
    if (flags == OS_VM_PROT_NONE &&
        !madvise(page, os_vm_page_size,
                 protect_p ? MADV_GUARD_INSTALL : MADV_GUARD_REMOVE))
        return;
#endif
    os_protect(page, os_vm_page_size, protect_p ?
               flags : OS_VM_PROT_READ | OS_VM_PROT_WRITE);
}

#define DEF_PROTECT_PAGE(name,page_name,flags)                          \
    void                                                                \
    protect_##name(int protect_p, struct thread *thread) {              \
        protect_guard_page(page_name(thread), protect_p, flags);        \
    }

#if !(defined LISP_FEATURE_WIN32 && defined LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
DEF_PROTECT_PAGE(control_stack_hard_guard_page,
                 CONTROL_STACK_HARD_GUARD_PAGE,
                 OS_VM_PROT_NONE)
DEF_PROTECT_PAGE(control_stack_guard_page,
                 CONTROL_STACK_GUARD_PAGE, OS_VM_PROT_NONE)
DEF_PROTECT_PAGE(control_stack_return_guard_page,
                 CONTROL_STACK_RETURN_GUARD_PAGE, OS_VM_PROT_READ)
#else
void protect_control_stack_return_guard_page (__attribute__((unused)) int protect_p,
                                              __attribute__((unused)) struct thread *thread) {
}
#endif

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
