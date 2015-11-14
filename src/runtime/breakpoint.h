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

#ifndef _BREAKPOINT_H_
#define _BREAKPOINT_H_

extern unsigned int breakpoint_install(lispobj code_obj, int pc_offset);
extern void breakpoint_remove(lispobj code_obj,
                              int pc_offset,
                              unsigned int orig_inst);
extern void breakpoint_do_displaced_inst(os_context_t *context,
                                         unsigned int orig_inst);
extern void handle_breakpoint(os_context_t *context);
extern void *handle_fun_end_breakpoint(os_context_t *context);
extern void handle_single_step_trap (os_context_t *context, int kind,
                                     int register_offset);

extern void handle_single_step_trap(os_context_t *context, int kind,
                                    int register_offset);
extern lispobj find_code(os_context_t *context);

#endif
