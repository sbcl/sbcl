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
#include <stdio.h>
extern bool more_p(char **ptr);
extern char *parse_token(char **ptr);
extern int parse_lispobj(char **ptr, lispobj *output);
extern int parse_addr(char **ptr, bool safely, char **output, FILE*);
extern int parse_number(char **ptr, int *output);
