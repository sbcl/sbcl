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

/* All parse routines take a char ** as their only argument */
extern boolean more_p(char **ptr);
extern char *parse_token(char **ptr);
extern lispobj parse_lispobj(char **ptr);
extern char *parse_addr(char **ptr);
extern long parse_number(char **ptr);
