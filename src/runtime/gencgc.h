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

#ifndef __GENCGC_H__
#define __GENCGC_H__

#if defined(LUTEX_WIDETAG)
#include "genesis/lutex.h"

extern void gencgc_register_lutex (struct lutex *lutex);
extern void gencgc_unregister_lutex (struct lutex *lutex);
#endif

#endif /* __GENCGC_H__ */
