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

#ifndef __ARCH_H__
#define __ARCH_H__

#include "os.h"
#include "signal.h"
#include "thread.h"

/* Do anything we need to do when starting up the runtime environment
 * on this architecture. */
extern void arch_init(void);

/* FIXME: It would be good to document these too! */
extern void arch_skip_instruction(os_context_t*);
extern boolean arch_pseudo_atomic_atomic(os_context_t*);
extern void arch_set_pseudo_atomic_interrupted(os_context_t*);
extern void arch_clear_pseudo_atomic_interrupted(os_context_t*);
extern os_vm_address_t arch_get_bad_addr(int, siginfo_t*, os_context_t*);
extern unsigned char *arch_internal_error_arguments(os_context_t*);
extern unsigned int arch_install_breakpoint(void *pc);
extern void arch_remove_breakpoint(void *pc, unsigned int orig_inst);
extern void arch_install_interrupt_handlers(void);
extern void arch_do_displaced_inst(os_context_t *context,
                                   unsigned int orig_inst);

extern int arch_os_thread_init(struct thread *thread);
extern int arch_os_thread_cleanup(struct thread *thread);

extern lispobj funcall0(lispobj function);
extern lispobj funcall1(lispobj function, lispobj arg0);
extern lispobj funcall2(lispobj function, lispobj arg0, lispobj arg1);
extern lispobj funcall3(lispobj function, lispobj arg0, lispobj arg1,
                        lispobj arg2);
extern lispobj *component_ptr_from_pc(lispobj *pc);

extern void fpu_save(void *);
extern void fpu_restore(void *);

#ifdef LISP_FEATURE_X86
extern unsigned int * single_stepping;
extern void restore_breakpoint_from_single_step(os_context_t * context);
#endif

extern void arch_handle_breakpoint(os_context_t* context);
extern void arch_handle_fun_end_breakpoint(os_context_t *context);
#ifdef trap_AfterBreakpoint
extern void arch_handle_after_breakpoint(os_context_t *context);
#endif
#ifdef trap_SingleStepAround
extern void arch_handle_single_step_trap(os_context_t *context, int trap);
#endif

#endif /* __ARCH_H__ */
