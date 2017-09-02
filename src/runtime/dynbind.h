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

#ifndef _DYNBIND_H_
#define _DYNBIND_H_

#ifdef LISP_FEATURE_SB_THREAD
#define bind_variable(sym, val, th) bind_tls_cell(sym##_tlsindex, val, th)
extern void bind_tls_cell(unsigned index, lispobj value, void *thread);
#else
extern void bind_variable(lispobj symbol, lispobj value,void *thread);
#endif
extern void unbind(void *thread);
extern void unbind_to_here(lispobj *bsp,void *thread);

#endif
