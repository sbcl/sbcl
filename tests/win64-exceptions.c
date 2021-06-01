/* Compiled and loaded by win64-exceptions.impure.lisp
 */

/* This software is part of the SBCL system. See the README file for
 * more information.
 *
 * While most of SBCL is derived from the CMU CL system, the test
 * files (like this one) were written from scratch after the fork
 * from CMU CL.
 *
 * This software is in the public domain and is provided with
 * absolutely no warranty. See the COPYING and CREDITS files for
 * more information.
 */

#include <windows.h>
#include <stdio.h>
#include <stdint.h>

extern int
unsafe_div(int dividend, int divisor)
{
    return dividend / divisor;
}

extern void
raise_exception(int code)
{
    RaiseException(code, EXCEPTION_NONCONTINUABLE, 0, NULL);
}

struct division {
    int dividend;
    int divisor;
    int result;
};

static DWORD WINAPI
thread_fn(LPVOID param)
{
    struct division *x = param;
    x->result = x->dividend / x->divisor;
    return 0;
}

/* Only used in self-tests, since SBCL can't (and probably shouldn't) handle
 * exceptions in foreign threads. */
extern int
unsafe_div_in_foreign_thread(int dividend, int divisor)
{
    struct division x = { dividend, divisor, -1 };
    DWORD tid;
    HANDLE thread = CreateThread(NULL, 0, thread_fn, &x, 0, &tid);
    WaitForMultipleObjects(1, &thread, TRUE, INFINITE);
    CloseHandle(thread);
    return x.result;
}

typedef struct _UNWIND_INFO {
  uint8_t Version : 3;
  uint8_t Flags : 5;
  uint8_t SizeOfProlog;
  uint8_t CountOfCodes;
  uint8_t FrameRegister : 4;
  uint8_t FrameOffset : 4;
  ULONG ExceptionHandler;
  ULONG ExceptionData[1];
} UNWIND_INFO;

struct fn {
    uint8_t code[8];
    uint8_t eh_trampoline[16];
    UNWIND_INFO ui; // needs to be DWORD-aligned
    RUNTIME_FUNCTION rt;
};

static EXCEPTION_DISPOSITION
div_exception_handler(PEXCEPTION_RECORD ExceptionRecord,
                      ULONG64 EstablisherFrame,
                      PCONTEXT ContextRecord,
                      PDISPATCHER_CONTEXT DispatcherContext)
{
    printf("div_exception_handler() got exception 0x%lx; continuing\n",
           ExceptionRecord->ExceptionCode);
    fflush(stdout);
    ContextRecord->Rip += 2; // skip over DIV instruction
    return ExceptionContinueExecution;
}

static EXCEPTION_DISPOSITION
mov_exception_handler(PEXCEPTION_RECORD ExceptionRecord,
                      ULONG64 EstablisherFrame,
                      PCONTEXT ContextRecord,
                      PDISPATCHER_CONTEXT DispatcherContext)
{
    printf("mov_exception_handler() got exception 0x%lx; continuing\n",
           ExceptionRecord->ExceptionCode);
    fflush(stdout);
    ContextRecord->Rip += 7; // skip over 7-byte MOV instruction
    return ExceptionContinueExecution;
}


static BOOLEAN
set_up_exception_handler(struct fn *fn, void *handler)
{
    DWORD64 base = (DWORD64) fn;

    uint8_t *tramp = fn->eh_trampoline;
    tramp[0] = 0xFF; // jmp qword ptr [rip+2]
    tramp[1] = 0x25;
    tramp[2] = 0x02;
    tramp[3] = 0x00;
    tramp[4] = 0x00;
    tramp[5] = 0x00;
    tramp[6] = 0x66; // 2-byte nop
    tramp[7] = 0x90;
    *(void **)(tramp+8) = handler;

    UNWIND_INFO *ui = &fn->ui;
    ui->Version = 1;
    ui->Flags = UNW_FLAG_EHANDLER;
    ui->SizeOfProlog = 0;
    ui->CountOfCodes = 0;
    ui->FrameRegister = 0;
    ui->FrameOffset = 0;
    ui->ExceptionHandler = (DWORD64) tramp - base;
    ui->ExceptionData[0] = 0;

    RUNTIME_FUNCTION *rt = &fn->rt;
    rt->BeginAddress = 0;
    rt->EndAddress = 8;
    rt->UnwindData = (DWORD64) ui - base;

    return RtlAddFunctionTable(rt, 1, base);
}

/* Sadly, MINGW's __try1/__except don't compile with -shared, so we have to do
 * exception handling the hard way, again. */
extern int
raise_int_divide_by_zero(int handle)
{
    struct fn *fn = VirtualAlloc(NULL, sizeof(struct fn), MEM_COMMIT,
                                 PAGE_EXECUTE_READWRITE);

    uint8_t *code = fn->code;
    code[0] = 0x31; // xor ecx, ecx
    code[1] = 0xC9;
    code[2] = 0xF7; // div ecx
    code[3] = 0xF1;
    code[4] = 0xC3; // ret
    code[5] = code[6] = code[7] = 0x90; // nop

    if (handle && !set_up_exception_handler(fn, div_exception_handler))
        return 0;

    // will raise EXCEPTION_INT_DIVIDE_BY_ZERO
    (*((void (*)()) code))();

    return 1;
}

extern int *
allocate_readonly_int()
{
    return VirtualAlloc(NULL, sizeof(int), MEM_COMMIT, PAGE_READONLY);
}

extern int
free_readonly_int(int *n)
{
    return VirtualFree(n, sizeof(int), MEM_RELEASE);
}

extern int
raise_access_violation(int handle)
{
    struct fn *fn = VirtualAlloc(NULL, sizeof(struct fn), MEM_COMMIT,
                                 PAGE_EXECUTE_READWRITE);

    uint8_t *code = fn->code;
    code[0] = 0x48; // mov qword ptr [rcx], 42
    code[1] = 0xC7;
    code[2] = 0x01;
    code[3] = 0x2A;
    code[4] = 0x00;
    code[5] = 0x00;
    code[6] = 0x00;
    code[7] = 0xC3; // ret

    if (handle && !set_up_exception_handler(fn, mov_exception_handler))
        return 0;

    int *n = allocate_readonly_int();

    // will raise EXCEPTION_ACCESS_VIOLATION
    (*((void (*)(int *)) code))(n);

    free_readonly_int(n);

    return 1;
}

/*
 * Self-tests. As of GCC 9.2.0, __try1/__except1 is very finicky. -O1 or higher
 * fails to compile, they can only appear once per file (therefore we need to
 * enable each test case individually), and there are some restrictions on what
 * can happen within the __try1 block (e.g., no returns).
 *
 * Run with:
 *   for n in {1..6}; do gcc -DTEST_CASE=$n win64-exceptions.c && ./a.exe; done
 */

#ifndef TEST_CASE
#define TEST_CASE 0
#endif

#if TEST_CASE > 0
static long CALLBACK
eh(EXCEPTION_POINTERS *ep)
{
    printf("raised exception 0x%lx ", ep->ExceptionRecord->ExceptionCode);
    fflush(stdout);
    return EXCEPTION_EXECUTE_HANDLER;
}

int main(void)
{
    printf("\n>>> TEST_CASE=%d\n", TEST_CASE); fflush(stdout);

# if TEST_CASE == 1
    printf("unsafe_div(12, 4) = %d\n", unsafe_div(12, 4)); fflush(stdout);

    __try1(eh) {
        printf("unsafe_div(12, 0) = "); fflush(stdout);
        printf("%d (unexpectedly)\n", unsafe_div(12, 0)); fflush(stdout);
    } __except1 {
        printf("which was handled gracefully\n"); fflush(stdout);
    }
# endif

# if TEST_CASE == 2
    __try1(eh) {
        printf("raise_exception(42) ... "); fflush(stdout);
        raise_exception(42);
    } __except1 {
        printf("which was handled gracefully\n"); fflush(stdout);
    }
# endif

# if TEST_CASE == 3
    printf("unsafe_div_in_foreign_thread(12, 4) = %d\n",
           unsafe_div_in_foreign_thread(12, 4));
    fflush(stdout);

    printf("unsafe_div_in_foreign_thread(12, 0) ... should kill the process\n");
    fflush(stdout);
    printf("unexpectedly, it returned %d\n",
           unsafe_div_in_foreign_thread(12, 0));
    fflush(stdout);
# endif

# if TEST_CASE == 4
    printf("raise_int_divide_by_zero(1) = %d\n",
           raise_int_divide_by_zero(1)); fflush(stdout);

    __try1(eh) {
        printf("raise_int_divide_by_zero(0) = "); fflush(stdout);
        printf("%d (unexpectedly)\n", raise_int_divide_by_zero(0));
        fflush(stdout);
    } __except1 {
        printf("which was handled gracefully\n"); fflush(stdout);
    }
# endif

# if TEST_CASE == 5
    printf("raise_access_violation(1) = %d\n",
           raise_access_violation(1)); fflush(stdout);

    __try1(eh) {
        printf("raise_access_violation(0) = "); fflush(stdout);
        printf("%d (unexpectedly)\n", raise_access_violation(0));
        fflush(stdout);
    } __except1 {
        printf("which was handled gracefully\n"); fflush(stdout);
    }
# endif

# if TEST_CASE == 6
    int *n = allocate_readonly_int();
    __try1(eh) {
        printf("writing into read-only memory "); fflush(stdout);
        printf("yielded %d (unexpectedly)\n", *n = 42); fflush(stdout);
    } __except1 {
        printf("which was handled gracefully\n"); fflush(stdout);
        free_readonly_int(n);
    }
# endif
    return EXIT_SUCCESS;
}
#endif
