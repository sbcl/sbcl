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

extern unsigned long breakpoint_install(lispobj code_obj, int pc_offset);
extern void breakpoint_remove(lispobj code_obj,
                              int pc_offset,
                              unsigned long orig_inst);
extern void breakpoint_do_displaced_inst(os_context_t *context,
                                         unsigned long orig_inst);
extern void handle_breakpoint(int signal, siginfo_t *info,
                              os_context_t *context);
extern void *handle_fun_end_breakpoint(int signal, siginfo_t *info,
                                       os_context_t *context);

#endif
