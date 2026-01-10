#ifndef _PPC_BSD_OS_H
#define _PPC_BSD_OS_H

#ifdef __LP64__
typedef long os_context_register_t;
#else
typedef int os_context_register_t;
#endif
typedef ucontext_t os_context_t;

#ifdef LISP_FEATURE_NETBSD
#  define OS_CONTEXT_PC(context) _UC_MACHINE_PC(context)
#elif defined LISP_FEATURE_OPENBSD
#  define OS_CONTEXT_PC(context) context->sc_frame.srr0
#elif defined(LISP_FEATURE_FREEBSD)
#  define OS_CONTEXT_PC(context) context->uc_mcontext.mc_srr0
#else
#  error "Need a definition of OS_CONTEXT_PC"
#endif

#endif /* _PPC_BSD_OS_H */
