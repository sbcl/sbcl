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

#ifndef _INTERR_H_
#define _INTERR_H_

extern void lose(char *fmt, ...) never_returns;
extern boolean lose_on_corruption_p;
extern void corruption_warning_and_maybe_lose(char *fmt, ...);
extern void enable_lossage_handler(void);
extern void disable_lossage_handler(void);
extern void describe_internal_error(os_context_t *context);

extern lispobj debug_print(lispobj string);

#endif
