#ifndef _X86_LINUX_OS_H
#define _X86_LINUX_OS_H

static inline os_context_t *arch_os_get_context(void **void_context) {
  return (os_context_t *) *void_context;
}

#endif /* _X86_LINUX_OS_H */
