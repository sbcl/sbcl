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

/*
 * $Header$
 */

#ifndef __ARCH_H__
#define __ARCH_H__

#include "os.h"
#include "signal.h"

extern void arch_init(void);
extern void arch_skip_instruction(os_context_t*);
extern boolean arch_pseudo_atomic_atomic(os_context_t*);
extern void arch_set_pseudo_atomic_interrupted(os_context_t*);
extern os_vm_address_t arch_get_bad_addr(int, siginfo_t*, os_context_t*);
extern unsigned char *arch_internal_error_arguments(os_context_t*);
extern unsigned long arch_install_breakpoint(void *pc);
extern void arch_remove_breakpoint(void *pc, unsigned long orig_inst);
extern void arch_install_interrupt_handlers(void);
extern void arch_do_displaced_inst(os_context_t *context,
				   unsigned long orig_inst);
extern lispobj funcall0(lispobj function);
extern lispobj funcall1(lispobj function, lispobj arg0);
extern lispobj funcall2(lispobj function, lispobj arg0, lispobj arg1);
extern lispobj funcall3(lispobj function, lispobj arg0, lispobj arg1,
			lispobj arg2);

#endif /* __ARCH_H__ */
