#ifndef _X86_64_LINUX_OS_H
#define _X86_64_LINUX_OS_H

typedef struct ucontext os_context_t;
typedef long os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context) {
    return (os_context_t *) *void_context;
}

unsigned long os_context_fp_control(os_context_t *context);
void os_restore_fp_control(os_context_t *context);

#ifdef LISP_FEATURE_SB_THREAD
static inline int kill_thread(os_thread_t tid,int signal) 
{
    return pthread_kill((os_thread_t) tid,signal);
}
static inline os_thread_t thread_self() 
{
    return (os_thread_t) pthread_self();
}
#endif

#endif /* _X86_64_LINUX_OS_H */
