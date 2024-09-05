#ifndef _X86_WIN32_OS_H
#define _X86_WIN32_OS_H

typedef struct os_context_t {
  CONTEXT* win32_context;
#if defined(LISP_FEATURE_SB_THREAD)
  sigset_t sigmask;
#endif
} os_context_t;

typedef long os_context_register_t;

static inline DWORD NT_GetLastError() {
    DWORD result;
    asm("movl %%fs:0x0D,%0":"=r"(result));
    return result;
}

unsigned long os_context_fp_control(os_context_t *context);
void os_restore_fp_control(os_context_t *context);

os_context_register_t * os_context_fp_addr(os_context_t *context);

#define OS_CONTEXT_PC(context) context->win32_context->Eip

#endif /* _X86_WIN32_OS_H */
