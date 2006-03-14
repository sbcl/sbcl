/*
 * the Win32 incarnation of OS-dependent routines.  See also
 * $(sbcl_arch)-win32-os.c
 *
 * This file (along with os.h) exports an OS-independent interface to
 * the operating system VM facilities. Surprise surprise, this
 * interface looks a lot like the Mach interface (but simpler in some
 * places). For some operating systems, a subset of these functions
 * will have to be emulated.
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

/*
 * This file was copied from the Linux version of the same, and
 * likely still has some linuxisms in it have haven't been elimiated
 * yet.
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include "sbcl.h"
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "sbcl.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "runtime.h"
#include "monitor.h"
#include "alloc.h"
#include "genesis/primitive-objects.h"

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#include <excpt.h>

#include "validate.h"
#include "thread.h"
size_t os_vm_page_size;


#include "gc.h"
#include "gencgc-internal.h"

#if 0
int linux_sparc_siginfo_bug = 0;
int linux_supports_futex=0;
#endif

/* The exception handling function looks like this: */
EXCEPTION_DISPOSITION handle_exception(EXCEPTION_RECORD *,
                                       struct lisp_exception_frame *,
                                       CONTEXT *,
                                       void *);

void *base_seh_frame;

static void *get_seh_frame(void)
{
    void* retval;
    asm volatile ("movl %%fs:0,%0": "=r" (retval));
    return retval;
}

static void set_seh_frame(void *frame)
{
    asm volatile ("movl %0,%%fs:0": : "r" (frame));
}

static struct lisp_exception_frame *find_our_seh_frame(void)
{
    struct lisp_exception_frame *frame = get_seh_frame();

    while (frame->handler != handle_exception)
        frame = frame->next_frame;

    return frame;
}

#if 0
inline static void *get_stack_frame(void)
{
    void* retval;
    asm volatile ("movl %%ebp,%0": "=r" (retval));
    return retval;
}
#endif

void os_init(char *argv[], char *envp[])
{
    SYSTEM_INFO system_info;

    GetSystemInfo(&system_info);
    os_vm_page_size = system_info.dwPageSize;

    base_seh_frame = get_seh_frame();
}


/*
 * So we have three fun scenarios here.
 *
 * First, we could be being called to reserve the memory areas
 * during initialization (prior to loading the core file).
 *
 * Second, we could be being called by the GC to commit a page
 * that has just been decommitted (for easy zero-fill).
 *
 * Third, we could be being called by create_thread_struct()
 * in order to create the sundry and various stacks.
 *
 * The third case is easy to pick out because it passes an
 * addr of 0.
 *
 * The second case is easy to pick out because it will be for
 * a range of memory that is MEM_RESERVE rather than MEM_FREE.
 *
 * The second case is also an easy implement, because we leave
 * the memory as reserved (since we do lazy commits).
 */

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    MEMORY_BASIC_INFORMATION mem_info;

    if (!addr) {
        /* the simple case first */
        os_vm_address_t real_addr;
        if (!(real_addr = VirtualAlloc(addr, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE))) {
            perror("VirtualAlloc");
            return 0;
        }

        return real_addr;
    }

    if (!VirtualQuery(addr, &mem_info, sizeof mem_info)) {
        perror("VirtualQuery");
        return 0;
    }

    if ((mem_info.State == MEM_RESERVE) && (mem_info.RegionSize >=len)) return addr;

    if (mem_info.State == MEM_RESERVE) {
        fprintf(stderr, "validation of reserved space too short.\n");
        fflush(stderr);
    }

    if (!VirtualAlloc(addr, len, (mem_info.State == MEM_RESERVE)? MEM_COMMIT: MEM_RESERVE, PAGE_EXECUTE_READWRITE)) {
        perror("VirtualAlloc");
        return 0;
    }

    return addr;
}

/*
 * For os_invalidate(), we merely decommit the memory rather than
 * freeing the address space. This loses when freeing per-thread
 * data and related memory since it leaks address space. It's not
 * too lossy, however, since the two scenarios I'm aware of are
 * fd-stream buffers, which are pooled rather than torched, and
 * thread information, which I hope to pool (since windows creates
 * threads at its own whim, and we probably want to be able to
 * have them callback without funky magic on the part of the user,
 * and full-on thread allocation is fairly heavyweight). Someone
 * will probably shoot me down on this with some pithy comment on
 * the use of (setf symbol-value) on a special variable. I'm happy
 * for them.
 */

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (!VirtualFree(addr, len, MEM_DECOMMIT)) {
        perror("VirtualFree");
    }
}

/*
 * os_map() is called to map a chunk of the core file into memory.
 *
 * Unfortunately, Windows semantics completely screws this up, so
 * we just add backing store from the swapfile to where the chunk
 * goes and read it up like a normal file. We could consider using
 * a lazy read (demand page) setup, but that would mean keeping an
 * open file pointer for the core indefinately (and be one more
 * thing to maintain).
 */

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    os_vm_size_t count;

    fprintf(stderr, "os_map: %d, 0x%x, %p, 0x%x.\n", fd, offset, addr, len);
    fflush(stderr);

    if (!VirtualAlloc(addr, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE)) {
        perror("VirtualAlloc");
        lose("os_map: VirtualAlloc failure");
    }

    if (lseek(fd, offset, SEEK_SET) == -1) {
        lose("os_map: Seek failure.");
    }

    count = read(fd, addr, len);
    if (count != len) {
        fprintf(stderr, "expected 0x%x, read 0x%x.\n", len, count);
        lose("os_map: Failed to read enough bytes.");
    }

    return addr;
}

static DWORD os_protect_modes[8] = {
    PAGE_NOACCESS,
    PAGE_READONLY,
    PAGE_READWRITE,
    PAGE_READWRITE,
    PAGE_EXECUTE,
    PAGE_EXECUTE_READ,
    PAGE_EXECUTE_READWRITE,
    PAGE_EXECUTE_READWRITE,
};

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    DWORD old_prot;

    if (!VirtualProtect(address, length, os_protect_modes[prot], &old_prot)) {
        fprintf(stderr, "VirtualProtect failed, code 0x%lx.\n", GetLastError());
        fflush(stderr);
    }
}

/* FIXME: Now that FOO_END, rather than FOO_SIZE, is the fundamental
 * description of a space, we could probably punt this and just do
 * (FOO_START <= x && x < FOO_END) everywhere it's called. */
static boolean
in_range_p(os_vm_address_t a, lispobj sbeg, size_t slen)
{
    char* beg = (char*)((long)sbeg);
    char* end = (char*)((long)sbeg) + slen;
    char* adr = (char*)a;
    return (adr >= beg && adr < end);
}

boolean
is_valid_lisp_addr(os_vm_address_t addr)
{
    struct thread *th;
    if(in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE) ||
       in_range_p(addr, STATIC_SPACE_START   , STATIC_SPACE_SIZE) ||
       in_range_p(addr, DYNAMIC_SPACE_START  , DYNAMIC_SPACE_SIZE))
        return 1;
    for_each_thread(th) {
        if(((os_vm_address_t)th->control_stack_start <= addr) && (addr < (os_vm_address_t)th->control_stack_end))
            return 1;
        if(in_range_p(addr, (unsigned long)th->binding_stack_start, BINDING_STACK_SIZE))
            return 1;
    }
    return 0;
}

/*
 * any OS-dependent special low-level handling for signals
 */

/* A tiny bit of interrupt.c state we want our paws on. */
extern boolean internal_errors_enabled;

/*
 * FIXME: There is a potential problem with foreign code here.
 * If we are running foreign code instead of lisp code and an
 * exception occurs we arrange a call into Lisp. If the
 * foreign code has installed an exception handler, we run the
 * very great risk of throwing through their exception handler
 * without asking it to unwind. This is more a problem with
 * non-sigtrap (EXCEPTION_BREAKPOINT) exceptions, as they could
 * reasonably be expected to happen in foreign code. We need to
 * figure out the exception handler unwind semantics and adhere
 * to them (probably by abusing the Lisp unwind-protect system)
 * if we are going to handle this scenario correctly.
 *
 * A good explanation of the exception handling semantics is
 * http://win32assembly.online.fr/Exceptionhandling.html .
 * We will also need to handle this ourselves when foreign
 * code tries to unwind -us-.
 *
 * When unwinding through foreign code we should unwind the
 * Lisp stack to the entry from foreign code, then unwind the
 * foreign code stack to the entry from Lisp, then resume
 * unwinding in Lisp.
 */

EXCEPTION_DISPOSITION sigtrap_emulator(CONTEXT *context,
                                       struct lisp_exception_frame *exception_frame)
{
    if (*((char *)context->Eip + 1) == trap_ContextRestore) {
        /*
         * This is the cleanup for what is immediately below, and
         * for the generic exception handling further below. We
         * have to memcpy() the original context (emulated sigtrap
         * or normal exception) over our context and resume it.
         */
        memcpy(context, &exception_frame->context, sizeof(CONTEXT));
        return ExceptionContinueExecution;

    } else { /* Not a trap_ContextRestore, must be a sigtrap. */
        /* sigtrap_trampoline is defined in x86-assem.S. */
        extern void sigtrap_trampoline;

        /*
         * Unlike some other operating systems, Win32 leaves EIP
         * pointing to the breakpoint instruction.
         */
        context->Eip++;

        /*
         * We're not on an alternate stack like we would be in some
         * other operating systems, and we don't want to risk leaking
         * any important resources if we throw out of the sigtrap
         * handler, so we need to copy off our context to a "safe"
         * place and then monkey with the return EIP to point to a
         * trampoline which calls another function which copies the
         * context out to a really-safe place and then calls the real
         * sigtrap handler. When the real sigtrap handler returns, the
         * trampoline then contains another breakpoint with a code of
         * trap_ContextRestore (see above). Essentially the same
         * mechanism is used by the generic exception path. There is
         * a small window of opportunity between us copying the
         * context to the "safe" place and the sigtrap wrapper copying
         * it to the really-safe place (allocated in its stack frame)
         * during which the context can be smashed. The only scenario
         * I can come up with for this, however, involves a stack
         * overflow occuring at just the wrong time (which makes one
         * wonder how stack overflow exceptions even happen, given
         * that we don't switch stacks for exception processing...)
         */
        memcpy(&exception_frame->context, context, sizeof(CONTEXT));
        context->Eax = context->Eip;
        context->Eip = (unsigned long)&sigtrap_trampoline;

        /* and return */
        return ExceptionContinueExecution;
    }
}

void sigtrap_wrapper(void)
{
    /*
     * This is the wrapper around the sigtrap handler called from
     * the trampoline returned to from the function above.
     *
     * There actually is a point to some of the commented-out code
     * in this function, although it really belongs to the callback
     * wrappers. Once it is installed there, it can probably be
     * removed from here.
     */

    extern void sigtrap_handler(int signal, siginfo_t *info, void *context);

/*     volatile struct { */
/*      void *handler[2]; */
    CONTEXT context;
/*     } handler; */

    struct lisp_exception_frame *frame = find_our_seh_frame();

/*     wos_install_interrupt_handlers(handler); */
/*     handler.handler[0] = get_seh_frame(); */
/*     handler.handler[1] = &handle_exception; */
/*     set_seh_frame(&handler); */

    memcpy(&context, &frame->context, sizeof(CONTEXT));
    sigtrap_handler(0, NULL, &context);
    memcpy(&frame->context, &context, sizeof(CONTEXT));

/*     set_seh_frame(handler.handler[0]); */
}

EXCEPTION_DISPOSITION handle_exception(EXCEPTION_RECORD *exception_record,
                                       struct lisp_exception_frame *exception_frame,
                                       CONTEXT *context,
                                       void *dc) /* FIXME: What's dc again? */
{

    /* For EXCEPTION_ACCESS_VIOLATION only. */
    void *fault_address = (void *)exception_record->ExceptionInformation[1];

    if (exception_record->ExceptionCode == EXCEPTION_BREAKPOINT) {
        /* Pick off sigtrap case first. */
        return sigtrap_emulator(context, exception_frame);

    } else if (exception_record->ExceptionCode == EXCEPTION_ACCESS_VIOLATION &&
               (is_valid_lisp_addr(fault_address) ||
                /* the linkage table does not contain valid lisp
                 * objects, but is also committed on-demand here
                 */
                in_range_p(fault_address, LINKAGE_TABLE_SPACE_START,
                           LINKAGE_TABLE_SPACE_END))) {
        /* Pick off GC-related memory fault next. */
        MEMORY_BASIC_INFORMATION mem_info;

        if (!VirtualQuery(fault_address, &mem_info, sizeof mem_info)) {
            fprintf(stderr, "VirtualQuery: 0x%lx.\n", GetLastError());
            lose("handle_exception: VirtualQuery failure");
        }

        if (mem_info.State == MEM_RESERVE) {
            /* First use new page, lets get some memory for it. */
            if (!VirtualAlloc(mem_info.BaseAddress, os_vm_page_size,
                              MEM_COMMIT, PAGE_EXECUTE_READWRITE)) {
                fprintf(stderr, "VirtualAlloc: 0x%lx.\n", GetLastError());
                lose("handle_exception: VirtualAlloc failure");

            } else {
                /*
                 * Now, if the page is supposedly write-protected and this
                 * is a write, tell the gc that it's been hit.
                 *
                 * FIXME: Are we supposed to fall-through to the Lisp
                 * exception handler if the gc doesn't take the wp violation?
                 */
                if (exception_record->ExceptionInformation[0]) {
                    int index = find_page_index(fault_address);
                    if ((index != -1) && (page_table[index].write_protected)) {
                        gencgc_handle_wp_violation(fault_address);
                    }
                }
                return ExceptionContinueExecution;
            }

        } else if (gencgc_handle_wp_violation(fault_address)) {
            /* gc accepts the wp violation, so resume where we left off. */
            return ExceptionContinueExecution;
        }

        /* All else failed, drop through to the lisp-side exception handler. */
    }

    /*
     * If we fall through to here then we need to either forward
     * the exception to the lisp-side exception handler if it's
     * set up, or drop to LDB.
     */

    if (internal_errors_enabled) {
        /* exception_trampoline is defined in x86-assem.S. */
        extern void exception_trampoline;

        /*
         * We're making the somewhat arbitrary decision that
         * having internal errors enabled means that lisp has
         * sufficient marbles to be able to handle exceptions.
         *
         * Exceptions aren't supposed to happen during cold
         * init or reinit anyway.
         */

        /*
         * We use the same mechanism as the sigtrap emulator above
         * with just a couple changes. We obviously use a different
         * trampoline and wrapper function, we kill out any live
         * floating point exceptions, and we save off the exception
         * record as well as the context.
         */

        /* Save off context and exception information */
        memcpy(&exception_frame->context, context, sizeof(CONTEXT));
        memcpy(&exception_frame->exception, exception_record, sizeof(EXCEPTION_RECORD));

        /* Set up to activate trampoline when we return */
        context->Eax = context->Eip;
        context->Eip = (unsigned long)&exception_trampoline;

        /* Make sure a floating-point trap doesn't kill us */
        context->FloatSave.StatusWord &= ~0x3f;

        /* And return */
        return ExceptionContinueExecution;
    }

    fprintf(stderr, "Exception Code: 0x%lx.\n", exception_record->ExceptionCode);
    fprintf(stderr, "Faulting IP: 0x%lx.\n", (DWORD)exception_record->ExceptionAddress);
    if (exception_record->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
        MEMORY_BASIC_INFORMATION mem_info;

        if (VirtualQuery(fault_address, &mem_info, sizeof mem_info)) {
            fprintf(stderr, "page status: 0x%lx.\n", mem_info.State);
        }

        fprintf(stderr, "Was writing: %ld, where: 0x%lx.\n",
                exception_record->ExceptionInformation[0],
                (DWORD)fault_address);
    }

    fflush(stderr);

    fake_foreign_function_call(context);
    monitor_or_something();

    return ExceptionContinueSearch;
}

void handle_win32_exception_wrapper(void)
{
    struct lisp_exception_frame *frame = find_our_seh_frame();
    CONTEXT context;
    EXCEPTION_RECORD exception_record;
    lispobj context_sap;
    lispobj exception_record_sap;

    memcpy(&context, &frame->context, sizeof(CONTEXT));
    memcpy(&exception_record, &frame->exception, sizeof(EXCEPTION_RECORD));

    fake_foreign_function_call(&context);

    /* Allocate the SAP objects while the "interrupts" are still
     * disabled. */
    context_sap = alloc_sap(&context);
    exception_record_sap = alloc_sap(&exception_record);

    funcall2(SymbolFunction(HANDLE_WIN32_EXCEPTION), context_sap,
             exception_record_sap);

    undo_fake_foreign_function_call(&context);

    memcpy(&frame->context, &context, sizeof(CONTEXT));
}

void
wos_install_interrupt_handlers(struct lisp_exception_frame *handler)
{
    handler->next_frame = get_seh_frame();
    handler->handler = &handle_exception;
    set_seh_frame(handler);
}

void bcopy(const void *src, void *dest, size_t n)
{
    MoveMemory(dest, src, n);
}

/*
 * The stubs below are replacements for the windows versions,
 * which can -fail- when used in our memory spaces because they
 * validate the memory spaces they are passed in a way that
 * denies our exception handler a chance to run.
 */

void *memmove(void *dest, const void *src, size_t n)
{
    if (dest < src) {
        int i;
        for (i = 0; i < n; i++) *(((char *)dest)+i) = *(((char *)src)+i);
    } else {
        while (n--) *(((char *)dest)+n) = *(((char *)src)+n);
    }
    return dest;
}

void *memcpy(void *dest, const void *src, size_t n)
{
    while (n--) *(((char *)dest)+n) = *(((char *)src)+n);
    return dest;
}

char *dirname(char *path)
{
    static char buf[PATH_MAX + 1];
    size_t pathlen = strlen(path);
    int i;

    if (pathlen >= sizeof(buf)) {
        lose("Pathname too long in dirname.\n");
        return NULL;
    }

    strcpy(buf, path);
    for (i = pathlen; i >= 0; --i) {
        if (buf[i] == '/' || buf[i] == '\\') {
            buf[i] = '\0';
            break;
        }
    }

    return buf;
}

/* This is a manually-maintained version of ldso_stubs.S. */

void scratch(void)
{
    strerror(42);
    asin(0);
    acos(0);
    sinh(0);
    cosh(0);
    hypot(0, 0);
    write(0, 0, 0);
    close(0);
    rename(0,0);
    getcwd(0,0);
    dup(0);
    LoadLibrary(0);
    GetProcAddress(0, 0);
    FreeLibrary(0);
    mkdir(0);
    isatty(0);
    access(0,0);
    GetLastError();
    FormatMessageA(0, 0, 0, 0, 0, 0, 0);
    _get_osfhandle(0);
    ReadFile(0, 0, 0, 0, 0);
    WriteFile(0, 0, 0, 0, 0);
    PeekNamedPipe(0, 0, 0, 0, 0, 0);
    FlushConsoleInputBuffer(0);
    PeekConsoleInput(0, 0, 0, 0);
    Sleep(0);
}

char *
os_get_runtime_executable_path()
{
    char path[MAX_PATH + 1];
    DWORD bufsize = sizeof(path);
    DWORD size;

    if ((size = GetModuleFileNameA(NULL, path, bufsize)) == 0)
        return NULL;
    else if (size == bufsize && GetLastError() == ERROR_INSUFFICIENT_BUFFER)
        return NULL;

    return copied_string(path);
}

/* EOF */
