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

#ifndef PPC_DARWIN_LANGINFO_H
#define PPC_DARWIN_LANGINFO_H

#define CODESET 49

typedef int nl_item;
char *nl_langinfo (nl_item);

#endif /* PPC_DARWIN_LANGINFO_H */
