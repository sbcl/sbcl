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

#ifndef __PTHREAD_LUTEX_H__
#define __PTHREAD_LUTEX_H__

#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_SB_LUTEX)

typedef unsigned long tagged_lutex_t;

extern int lutex_init (tagged_lutex_t tagged_lutex);
extern int lutex_destroy (tagged_lutex_t tagged_lutex);

#endif

#endif /* __PTHREAD_LUTEX_H__ */
