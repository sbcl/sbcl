#ifndef _ARM64_WIN32_OS_H
#define _ARM64_WIN32_OS_H

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
os_context_register_t * os_context_sp_addr(os_context_t *context);
os_context_register_t * os_context_lr_addr(os_context_t *context);
os_context_register_t * os_context_flags_addr(os_context_t *context);
os_context_register_t * os_context_register_addr(os_context_t *context, int offset);
os_context_register_t * os_context_float_register_addr(os_context_t *context, int offset);

#define OS_CONTEXT_PC(context) context->win32_context->Pc

#endif /* _ARM64_WIN32_OS_H */
