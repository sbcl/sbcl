#ifndef _X86_LINUX_OS_H
#define _X86_LINUX_OS_H

typedef struct ucontext os_context_t;
typedef long os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context) {
    return (os_context_t *) *void_context;
}

extern struct thread *arch_os_get_current_thread();
unsigned long os_context_fp_control(os_context_t *context);
void os_restore_fp_control(os_context_t *context);

#endif /* _X86_LINUX_OS_H */
