#ifndef _X86_64_WIN32_OS_H
#define _X86_64_WIN32_OS_H

typedef struct os_context_t {
  CONTEXT* win32_context;
  sigset_t sigmask;
} os_context_t;

typedef intptr_t os_context_register_t;

static inline DWORD NT_GetLastError() {
    return GetLastError();
}

unsigned long os_context_fp_control(os_context_t *context);
void os_restore_fp_control(os_context_t *context);
os_context_register_t * os_context_fp_addr(os_context_t *context);

#define OS_CONTEXT_PC(context) context->win32_context->Rip

#endif /* _X86_64_WIN32_OS_H */
