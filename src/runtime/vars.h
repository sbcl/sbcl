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

#include <stdbool.h>

// These interfaces are used by src/runtime/monitor to assign a numeric
// label to each value printed so that you can refer to a previously
// printed value using $N.
extern void flush_vars(void);
extern struct var *lookup_by_name(char *name);
extern struct var *lookup_by_obj(lispobj obj);
extern struct var *define_var(char *name, lispobj obj, bool perm);

extern char *var_name(struct var *var);
extern lispobj var_value(struct var *var);
extern sword_t var_clock(struct var *var);
extern void var_setclock(struct var *var, sword_t value);
