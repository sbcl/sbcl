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
extern void arch_init(void); // Most architectures don't have this one
extern void tune_asm_routines_for_microarch(void);
extern void untune_asm_routines_for_microarch(void);
extern void asm_routine_poke(const char*, int, char);

/* FIXME: It would be good to document these too! */
extern void arch_skip_instruction(os_context_t*);
extern void arch_handle_allocation_trap(os_context_t*);
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
#if defined(LISP_FEATURE_X86) && defined(LISP_FEATURE_SB_THREAD)
extern void arch_os_load_ldt(struct thread *thread);
#endif
extern int arch_os_thread_cleanup(struct thread *thread);

extern lispobj funcall0(lispobj function);
extern lispobj funcall1(lispobj function, lispobj arg0);
extern lispobj funcall2(lispobj function, lispobj arg0, lispobj arg1);
extern lispobj funcall3(lispobj function, lispobj arg0, lispobj arg1,
                        lispobj arg2);
extern lispobj *component_ptr_from_pc(char *pc);
extern lispobj *dynamic_space_code_from_pc(char *pc);

#if defined(LISP_FEATURE_X86)||defined(LISP_FEATURE_X86_64)
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

extern void arch_write_linkage_table_entry(int index, void *target_addr, int datap);

#endif /* __ARCH_H__ */
