#ifndef _X86_DARWIN_OS_H
#define _X86_DARWIN_OS_H

#include <architecture/i386/table.h>
#include <i386/user_ldt.h>

#include "darwin-os.h"

typedef int os_context_register_t;

void set_data_desc_size(data_desc_t* desc, unsigned long size);
void set_data_desc_addr(data_desc_t* desc, void* addr);

/* On OS X 10.5, the field names for the thread state have changed and
 * now are prepended with __. Use some #define hackery to deal with
 * this.
 */
#if __DARWIN_UNIX03

#define CONTEXT_ADDR_FROM_STEM(stem) (os_context_register_t*)&context->uc_mcontext->__ss.__##stem
#define OS_CONTEXT_PC(context) context->uc_mcontext->__ss.__eip

#define EIP __eip
#define ESP __esp
#define EBP __ebp
#define EAX __eax
#define EBX __ebx
#define ECX __ecx
#define EDX __edx
#define ESI __esi
#define EDI __edi
#define EFLAGS __eflags
#define CS __cs
#define DS __ds
#define ES __es
#define FS __fs
#define SS __ss
#define GS __gs

#define FPU_FCW __fpu_fcw

#else

#define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext->ss.stem
#define OS_CONTEXT_PC(context) context->uc_mcontext->ss.eip

#define EIP eip
#define ESP esp
#define EBP ebp
#define EAX eax
#define EBX ebx
#define ECX ecx
#define EDX edx
#define ESI esi
#define EDI edi
#define EFLAGS eflags
#define CS cs
#define DS ds
#define ES es
#define FS fs
#define SS ss
#define GS gs

#define FPU_FCW fpu_fcw

#endif /* __DARWIN_UNIX03 */

#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);

#endif /* _X86_DARWIN_OS_H */
