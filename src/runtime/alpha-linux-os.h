#ifndef _ALPHA_LINUX_OS_H
#define _ALPHA_LINUX_OS_H

typedef struct ucontext os_context_t;

static inline os_context_t *arch_os_get_context(void **void_context) {
  return (os_context_t *) *void_context;
}

#endif /* _ALPHA_LINUX_OS_H */
