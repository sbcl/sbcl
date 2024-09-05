#ifndef _PPC_BSD_OS_H
#define _PPC_BSD_OS_H

typedef int os_context_register_t;

#ifdef LISP_FEATURE_NETBSD
#  define OS_CONTEXT_PC(context) _UC_MACHINE_PC(context)
#elif defined LISP_FEATURE_OPENBSD
#  define OS_CONTEXT_PC(context) context->sc_frame.srr0
#else
#  error "Need a definition of OS_CONTEXT_PC"
#endif

#endif /* _PPC_BSD_OS_H */
