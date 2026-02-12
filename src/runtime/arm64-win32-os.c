/*
 * The ARM64 Win32 incarnation of arch-dependent OS-dependent routines.
 * See also "win32-os.c".
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#include <stdio.h>
#include <stddef.h>
#include <string.h> // For memset
#include <sys/param.h>
#include <sys/file.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "genesis/sbcl.h"

#include <sys/types.h>
#include "runtime.h"
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include "thread.h"             /* dynamic_values_bytes */
#include "align.h"

#include "validate.h"

#include <windows.h> // For VirtualQuery, GetLastError, FlushInstructionCache

int arch_os_thread_init(struct thread *thread)
{
    // On ARM64 Windows, we use the allocated Lisp control stack (from alloc_thread_struct),
    // not the OS thread's C stack. Unlike x86-64 which uses C_STACK_IS_CONTROL_STACK,
    // ARM64 has a separate Lisp control stack.
    // Therefore, we do NOT call VirtualQuery here to overwrite control_stack_start/end.
    // They have already been set correctly by alloc_thread_struct().

    // CRITICAL: Set the TLS value so that get_sb_vm_thread() returns the correct thread!
    // Without this, funcall0-3 will get the wrong thread from TlsGetValue.
    extern DWORD OUR_TLS_INDEX;
    TlsSetValue(OUR_TLS_INDEX, thread);

    extern void win32_set_stack_guarantee(void);
    win32_set_stack_guarantee();

    return 1;
}

/* free any arch/os-specific resources used by thread, which is now
 * defunct.  Not called on live threads
 */
int arch_os_thread_cleanup(struct thread *thread) {
    return 0;
}

sigset_t *os_context_sigmask_addr(os_context_t *context)
{
  return &context->sigmask;
}

void visit_context_registers(void (*proc)(os_context_register_t,void*),
                             os_context_t *context, void* arg)
{
    // ARM64 general purpose registers X0-X30 and program counter Pc
    proc(context->win32_context->X0,  arg);
    proc(context->win32_context->X1,  arg);
    proc(context->win32_context->X2,  arg);
    proc(context->win32_context->X3,  arg);
    proc(context->win32_context->X4,  arg);
    proc(context->win32_context->X5,  arg);
    proc(context->win32_context->X6,  arg);
    proc(context->win32_context->X7,  arg);
    proc(context->win32_context->X8,  arg);
    proc(context->win32_context->X9,  arg);
    proc(context->win32_context->X10, arg);
    proc(context->win32_context->X11, arg);
    proc(context->win32_context->X12, arg);
    proc(context->win32_context->X13, arg);
    proc(context->win32_context->X14, arg);
    proc(context->win32_context->X15, arg);
    proc(context->win32_context->X16, arg);
    proc(context->win32_context->X17, arg);
    proc(context->win32_context->X18, arg);
    proc(context->win32_context->X19, arg);
    proc(context->win32_context->X20, arg);
    proc(context->win32_context->X21, arg);
    proc(context->win32_context->X22, arg);
    proc(context->win32_context->X23, arg);
    proc(context->win32_context->X24, arg);
    proc(context->win32_context->X25, arg);
    proc(context->win32_context->X26, arg);
    proc(context->win32_context->X27, arg);
    proc(context->win32_context->X28, arg);
    proc(context->win32_context->Fp, arg);  // X29 Frame Pointer
    proc(context->win32_context->Lr, arg);  // X30 Link Register
    proc(context->win32_context->Pc, arg);  // Program Counter
}

#include "lispregs.h" // This will pull in arm64-lispregs.h via target-lispregs.h

// ... (rest of the file remains the same until os_context_register_addr) ...

os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    if (!context) {
        return NULL;
    }
    if (!context->win32_context) {
        return NULL;
    }

    // Map physical ARM64 register number (0-31) to CONTEXT structure fields
    // The offset parameter is the physical register number from arm64-lispregs.h

    switch (offset) {
        case 0: return (os_context_register_t*)&context->win32_context->X0;
        case 1: return (os_context_register_t*)&context->win32_context->X1;
        case 2: return (os_context_register_t*)&context->win32_context->X2;
        case 3: return (os_context_register_t*)&context->win32_context->X3;
        case 4: return (os_context_register_t*)&context->win32_context->X4;
        case 5: return (os_context_register_t*)&context->win32_context->X5;
        case 6: return (os_context_register_t*)&context->win32_context->X6;
        case 7: return (os_context_register_t*)&context->win32_context->X7;
        case 8: return (os_context_register_t*)&context->win32_context->X8;
        case 9: return (os_context_register_t*)&context->win32_context->X9;
        case 10: return (os_context_register_t*)&context->win32_context->X10;
        case 11: return (os_context_register_t*)&context->win32_context->X11;
        case 12: return (os_context_register_t*)&context->win32_context->X12;
        case 13: return (os_context_register_t*)&context->win32_context->X13;
        case 14: return (os_context_register_t*)&context->win32_context->X14;
        case 15: return (os_context_register_t*)&context->win32_context->X15;
        case 16: return (os_context_register_t*)&context->win32_context->X16;
        case 17: return (os_context_register_t*)&context->win32_context->X17;
        case 18: return (os_context_register_t*)&context->win32_context->X18;
        case 19: return (os_context_register_t*)&context->win32_context->X19;
        case 20: return (os_context_register_t*)&context->win32_context->X20;
        case 21: return (os_context_register_t*)&context->win32_context->X21;
        case 22: return (os_context_register_t*)&context->win32_context->X22;
        case 23: return (os_context_register_t*)&context->win32_context->X23;
        case 24: return (os_context_register_t*)&context->win32_context->X24;
        case 25: return (os_context_register_t*)&context->win32_context->X25;
        case 26: return (os_context_register_t*)&context->win32_context->X26;
        case 27: return (os_context_register_t*)&context->win32_context->X27;
        case 28: return (os_context_register_t*)&context->win32_context->X28;
        case 29: return (os_context_register_t*)&context->win32_context->Fp; // X29
        case 30: return (os_context_register_t*)&context->win32_context->Lr;   // X30
        case 31: return (os_context_register_t*)&context->win32_context->Sp;   // X31 (SP)
        default:
            return NULL;
    }
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return (os_context_register_t*)&context->win32_context->Sp;
}

os_context_register_t *
os_context_fp_addr(os_context_t *context)
{
    return (os_context_register_t*)&context->win32_context->Fp;
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return (os_context_register_t*)&context->win32_context->Lr;
}

os_context_register_t *
os_context_flags_addr(os_context_t *context)
{
    // On ARM64 Windows, the CPSR is typically available as a separate field
    // within the CONTEXT structure or can be accessed via an appropriate
    // field for flags. Assuming `Cpsr` is the field name.
    return (os_context_register_t*)&context->win32_context->Cpsr;
}


unsigned long
os_context_fp_control(os_context_t *context)
{
    return context->win32_context->Fpsr | context->win32_context->Fpcr;
}

void
os_restore_fp_control(os_context_t *context)
{
    /* No-op on ARM64, same as Linux and BSD. FPCR is preserved across exceptions. */
}

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    if (!context) {
        return NULL;
    }
    if (!context->win32_context) {
        return NULL;
    }

    // ARM64 Windows CONTEXT structure has V[0-31] array for SIMD/FP registers
    // Each V register is 128 bits (16 bytes)
    // The offset parameter is the physical register number (0-31)

    if (offset >= 0 && offset < 32) {
        return (os_context_register_t*)&context->win32_context->V[offset];
    }

    return NULL;
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    // Use the Windows API function for flushing instruction cache.
    // The first argument is a process handle. For the current process, GetCurrentProcess() can be used.
    if (!FlushInstructionCache(GetCurrentProcess(), address, length)) {
        // Log an error or handle failure if necessary.
        fprintf(stderr, "FlushInstructionCache failed: 0x%lx.\n", GetLastError());
    }
}
