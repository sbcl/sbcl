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
 * likely still has some linuxisms in it have haven't been eliminated
 * yet.
 */

#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/file.h>
#include <io.h>
#include "genesis/sbcl.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "runtime.h"
#include "genesis/primitive-objects.h"

#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#include <math.h>
#include <float.h>

#include <excpt.h>
#include <errno.h>

#include "validate.h"
#include "thread.h"
#include "align.h"

#include "gc.h"
#include <wincrypt.h>
#include <stdarg.h>
#include <string.h>
#include "print.h"
#include <shlobj.h>

/* missing definitions for modern mingws */
#ifndef EH_UNWINDING
#define EH_UNWINDING 0x02
#endif
#ifndef EH_EXIT_UNWIND
#define EH_EXIT_UNWIND 0x04
#endif

/* Documented limit for ReadConsole/WriteConsole is 64K bytes.
   Real limit observed on W2K-SP3 is somewhere in between 32KiB and 64Kib...
*/
#define MAX_CONSOLE_TCHARS 16384

#ifdef LISP_FEATURE_SB_UNICODE
typedef WCHAR console_char;
#else
typedef CHAR console_char;
#endif

/* The exception handling function looks like this: */
EXCEPTION_DISPOSITION handle_exception(EXCEPTION_RECORD *,
                                       struct lisp_exception_frame *,
                                       CONTEXT *,
                                       void *);
/* handle_exception is defined further in this file, but since SBCL
 * 1.0.1.24, it doesn't get registered as SEH handler directly anymore,
 * not even by wos_install_interrupt_handlers. Instead, x86-assem.S
 * provides exception_handler_wrapper; we install it here, and each
 * exception frame on nested funcall()s also points to it.
 */

HMODULE runtime_module_handle = 0u;

#ifdef LISP_FEATURE_X86
static void *get_seh_frame(void)
{
    void* retval;
    asm volatile ("mov %%fs:0,%0": "=r" (retval));
    return retval;
}

static void set_seh_frame(void *frame)
{
    asm volatile ("mov %0,%%fs:0": : "r" (frame));
}
#endif

void alloc_gc_page()
{
#ifndef LISP_FEATURE_64_BIT // 64-bit uses the page below the card mark table
    gc_assert(VirtualAlloc(GC_SAFEPOINT_PAGE_ADDR, BACKEND_PAGE_BYTES,
                           MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE));
#endif
}

/* Permit loads from GC_SAFEPOINT_PAGE_ADDR (NB page state change is
 * "synchronized" with the memory region content/availability --
 * e.g. you won't see other CPU flushing buffered writes after WP --
 * but there is some window when other thread _seem_ to trap AFTER
 * access is granted. You may think of it something like "OS enters
 * SEH handler too slowly" -- what's important is there's no implicit
 * synchronization between VirtualProtect caller and other thread's
 * SEH handler, hence no ordering of events. VirtualProtect is
 * implicitly synchronized with protected memory contents (only).
 *
 * The last fact may be potentially used with many benefits e.g. for
 * foreign call speed, but we don't use it for now: almost the only
 * fact relevant to the current signalling protocol is "sooner or
 * later everyone will trap [everyone will stop trapping]".
 *
 * An interesting source on page-protection-based inter-thread
 * communication is a well-known paper by Dave Dice, Hui Huang,
 * Mingyao Yang: ``Asymmetric Dekker Synchronization''. Last time
 * I checked it was available at
 * http://home.comcast.net/~pjbishop/Dave/Asymmetric-Dekker-Synchronization.txt
 */
void map_gc_page()
{
    DWORD oldProt;
    gc_assert(VirtualProtect((void*) GC_SAFEPOINT_PAGE_ADDR, BACKEND_PAGE_BYTES,
                             PAGE_READWRITE, &oldProt));
}

void unmap_gc_page()
{
    DWORD oldProt;
    gc_assert(VirtualProtect((void*) GC_SAFEPOINT_PAGE_ADDR, BACKEND_PAGE_BYTES,
                             PAGE_NOACCESS, &oldProt));
}

/* This feature has already saved me more development time than it
 * took to implement.  In its current state, ``dynamic RT<->core
 * linking'' is a protocol of initialization of C runtime and Lisp
 * core, populating SBCL linkage table with entries for runtime
 * "foreign" symbols that were referenced in cross-compiled code.
 *
 * How it works: a sketch
 *
 * Last Genesis (resulting in cold-sbcl.core) binds foreign fixups in
 * x-compiled lisp-objs to sequential addresses from the beginning of
 * linkage-table space; that's how it ``resolves'' foreign references.
 * Obviously, this process doesn't require pre-built runtime presence.
 *
 * When the runtime loads the core (cold-sbcl.core initially,
 * sbcl.core later), runtime should do its part of the protocol by (1)
 * traversing a list of ``runtime symbols'' prepared by Genesis and
 * dumped as a static symbol value, (2) resolving each name from this
 * list to an address (stubbing unresolved ones with
 * undefined_alien_address or undefined_alien_function), (3) adding an
 * entry for each symbol somewhere near the beginning of linkage table
 * space (location is provided by the core).
 *
 * The implementation of the part described in the last paragraph
 * follows. C side is currently more ``hackish'' and less clear than
 * the Lisp code; OTOH, related Lisp changes are scattered, and some
 * of them play part in complex interrelations -- beautiful but taking
 * much time to understand --- but my subset of PE-i386 parser below
 * is in one place (here) and doesn't have _any_ non-trivial coupling
 * with the rest of the Runtime.
 *
 * What do we gain with this feature, after all?
 *
 * One things that I have to do rather frequently: recompile and
 * replace runtime without rebuilding the core. Doubtlessly, slam.sh
 * was a great time-saver here, but relinking ``cold'' core and bake a
 * ``warm'' one takes, as it seems, more than 10x times of bare
 * SBCL.EXE build time -- even if everything is recompiled, which is
 * now unnecessary. Today, if I have a new idea for the runtime,
 * getting from C-x C-s M-x ``compile'' to fully loaded SBCL
 * installation takes 5-15 seconds.
 *
 * Another thing (that I'm not currently using, but obviously
 * possible) is delivering software patches to remote system on
 * customer site. As you are doing minor additions or corrections in
 * Lisp code, it doesn't take much effort to prepare a tiny ``FASL
 * bundle'' that rolls up your patch, redumps and -- presto -- 100MiB
 * program is fixed by sending and loading a 50KiB thingie.
 *
 * However, until LISP_FEATURE_ALIEN_LINKAGE_TABLE, if your bug were fixed
 * by modifying two lines of _C_ sources, a customer described above
 * had to be ready to receive and reinstall a new 100MiB
 * executable. With the aid of code below, deploying such a fix
 * requires only sending ~300KiB (when stripped) of SBCL.EXE.
 *
 * But there is more to it: as the common linkage-table is used for
 * DLLs and core, its entries may be overridden almost without a look
 * into SBCL internals. Therefore, ``patching'' C runtime _without_
 * restarting target systems is also possible in many situations
 * (it's not as trivial as loading FASLs into a running daemon, but
 * easy enough to be a viable alternative if any downtime is highly
 * undesirable).
 *
 * During my (rather limited) commercial Lisp development experience
 * I've already been through a couple of situations where such
 * ``deployment'' issues were important; from my _total_ programming
 * experience I know -- _sometimes_ they are a two orders of magnitude
 * more important than those I observed.
 *
 * The possibility of entire runtime ``hot-swapping'' in running
 * process is not purely theoretical, as it could seem. There are 2-3
 * problems whose solution is not obvious (call stack patching, for
 * instance), but it's literally _nothing_ if compared with
 * e.g. LISP_FEATURE_SB_AUTO_FPU_SWITCH.  By the way, one of the
 * problems with ``hot-swapping'', that could become a major one in
 * many other environments, is nonexistent in SBCL: we already have a
 * ``global quiesce point'' that is generally required for this kind
 * of worldwide revolution -- around collect_garbage.
 *
 * If we look at the majority of the ``new style'' code units, it's a
 * common thing to observe how #+-ifdeffery _vanishes_ instead of
 * multiplying: #-sb-xc, #+sb-xc-host and #-sb-xc-host end up
 * needing the same code. Runtime checks of static v. dynamic symbol
 * disappear even faster. STDCALL mangling and leading underscores go
 * out of scope (and GCed, hopefully) instead of surfacing here and
 * there as a ``special case for core static symbols''. What I like
 * the most about CL development in general is a frequency of solving
 * problems and fixing bugs by simplifying code and dropping special
 * cases.
 *
 * Last important thing about the following code: besides resolving
 * symbols provided by the core itself, it detects runtime's own
 * build-time prerequisite DLLs. Any symbol that is unresolved against
 * the core is looked up in those DLLs (normally kernel32, msvcrt,
 * ws2_32... I could forget something). This action (1) resembles
 * implementation of foreign symbol lookup in SBCL itself, (2)
 * emulates shared library d.l. facilities of OSes that use flat
 * dynamic symbol namespace (or default to it). Anyone concerned with
 * portability problems of this PE-i386 stuff below will be glad to
 * hear that it could be ported to most modern Unices _by deletion_:
 * raw dlsym() with null handle usually does the same thing that i'm
 * trying to squeeze out of MS Windows by the brute force.
 *
 * My reason for _desiring_ flat symbol namespace, populated from
 * link-time dependencies, is avoiding any kind of ``requested-by-Lisp
 * symbol lists to be linked statically'', providing core v. runtime
 * independence in both directions. Minimizing future maintenance
 * effort is very important; I had gone for it consistently, starting
 * by turning "CloseHandle@4" into a simple "CloseHandle", continuing
 * by adding intermediate Genesis resulting in autogenerated symbol
 * list (farewell, void scratch(); good riddance), going to take
 * another great step for core/runtime independence... and _without_
 * flat namespace emulation, the ghosts and spirits exiled at the
 * first steps would come and take revenge: well, here are the symbols
 * that are really in msvcrt.dll.. hmm, let's link statically against
 * them, so the entry is pulled from the import library.. and those
 * entry has mangled names that we have to map.. ENOUGH, I though
 * here: fed up with stuff like that.
 *
 * Now here we are, without import libraries, without mangled symbols,
 * and without nm-generated symbol tables. Every symbol exported by
 * the runtime is added to SBCL.EXE export directory; every symbol
 * requested by the core is looked up by GetProcAddress for SBCL.EXE,
 * falling back to GetProcAddress for MSVCRT.dll, etc etc.. All ties
 * between SBCL's foreign symbols with object file symbol tables,
 * import libraries and other pre-linking symbol-resolving entities
 * _having no representation in SBCL.EXE_ were teared.
 *
 * This simplistic approach proved to work well; there is only one
 * problem introduced by it, and rather minor: in real MSVCRT.dll,
 * what's used to be available as open() is now called _open();
 * similar thing happened to many other `lowio' functions, though not
 * every one, so it's not a kind of name mangling but rather someone's
 * evil creative mind in action.
 *
 * When we look up any of those poor `uglified' functions in CRT
 * reference on MSDN, we can see a notice resembling this one:
 *
 * `unixishname()' is obsolete and provided for backward
 * compatibility; new standard-compliant function, `_unixishname()',
 * should be used instead.  Sentences of that kind were there for
 * several years, probably even for a decade or more (a propos,
 * MSVCRT.dll, as the name to link against, predates year 2000, so
 * it's actually possible). Reasoning behing it (what MS people had in
 * mind) always seemed strange to me: if everyone uses open() and that
 * `everyone' is important to you, why rename the function?  If no one
 * uses open(), why provide or retain _open() at all? <kidding>After
 * all, names like _open() are entirely non-informative and just plain
 * ugly; compare that with CreateFileW() or InitCommonControlsEx(),
 * the real examples of beauty and clarity.</kidding>
 *
 * Anyway, if the /standard/ name on Windows is _open() (I start to
 * recall, vaguely, that it's because of _underscore names being
 * `reserved to system' and all other ones `available for user', per
 * ANSI/ISO C89) -- well, if the /standard/ name is _open, SBCL should
 * use it when it uses MSVCRT and not some ``backward-compatible''
 * stuff. Deciding this way, I added a hack to SBCL's syscall macros,
 * so "[_]open" as a syscall name is interpreted as a request to link
 * agains "_open" on win32 and "open" on every other system.
 *
 * Of course, this name-parsing trick lacks conceptual clarity; we're
 * going to get rid of it eventually. */

uint32_t os_get_build_time_shared_libraries(uint32_t excl_maximum,
                                       void* opt_root,
                                       void** opt_store_handles,
                                       const char *opt_store_names[])
{
    char* base = opt_root ? opt_root : (void*)runtime_module_handle;
    /* base defaults to 0x400000 with GCC/mingw32. If you dereference
     * that location, you'll see 'MZ' bytes */
    char* base_magic_location =
        base + ((IMAGE_DOS_HEADER*)base)->e_lfanew;

    /* dos header provided the offset from `base' to
     * IMAGE_FILE_HEADER where PE-i386 really starts */

    void* check_duplicates[excl_maximum];

    if ((*(uint32_t*)base_magic_location)!=0x4550) {
        /* We don't need this DLL thingie _that_ much. If the world
         * has changed to a degree where PE magic isn't found, let's
         * silently return `no libraries detected'. */
        return 0;
    } else {
        /* We traverse PE-i386 structures of SBCL.EXE in memory (not
         * in the file). File and memory layout _surely_ differ in
         * some places and _may_ differ in some other places, but
         * fortunately, those places are irrelevant to the task at
         * hand. */

        IMAGE_FILE_HEADER* image_file_header = (void*)(base_magic_location + 4);
        IMAGE_OPTIONAL_HEADER* image_optional_header =
            (void*)(image_file_header + 1);
        IMAGE_DATA_DIRECTORY* image_import_direntry =
            &image_optional_header->DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
        IMAGE_IMPORT_DESCRIPTOR* image_import_descriptor =
            (void*)(base + image_import_direntry->VirtualAddress);
        uint32_t nlibrary, j;

        for (nlibrary=0u; nlibrary < excl_maximum
                          && image_import_descriptor->FirstThunk;
             ++image_import_descriptor)
        {
            HMODULE hmodule;
            odxprint(runtime_link, "Now should know DLL: %s",
                     (char*)(base + image_import_descriptor->Name));
            /* Code using image thunk data to get its handle was here, with a
             * number of platform-specific tricks (like using VirtualQuery for
             * old OSes lacking GetModuleHandleEx).
             *
             * It's now replaced with requesting handle by name, which is
             * theoretically unreliable (with SxS, multiple modules with same
             * name are quite possible), but good enough to find the
             * link-time dependencies of our executable or DLL. */

            hmodule = (HMODULE)
                GetModuleHandle(base + image_import_descriptor->Name);

            if (hmodule)
            {
                /* We may encouncer some module more than once while
                   traversing import descriptors (it's usually a
                   result of non-trivial linking process, like doing
                   ld -r on some groups of files before linking
                   everything together.

                   Anyway: using a module handle more than once will
                   do no harm, but it slows down the startup (even
                   now, our startup time is not a pleasant topic to
                   discuss when it comes to :linkage-table; there is
                   an obvious direction to go for speed, though --
                   instead of resolving symbols one-by-one, locate PE
                   export directories -- they are sorted by symbol
                   name -- and merge them, at one pass, with sorted
                   list of required symbols (the best time to sort the
                   latter list is during Genesis -- that's why I don't
                   proceed with memory copying, qsort() and merge
                   right here)). */

                for (j=0; j<nlibrary; ++j)
                {
                    if(check_duplicates[j] == hmodule)
                        break;
                }
                if (j<nlibrary) continue; /* duplicate => skip it in
                                           * outer loop */

                check_duplicates[nlibrary] = hmodule;
                if (opt_store_handles) {
                    opt_store_handles[nlibrary] = hmodule;
                }
                if (opt_store_names) {
                    opt_store_names[nlibrary] = (const char *)
                        (base + image_import_descriptor->Name);
                }
                odxprint(runtime_link, "DLL detection: %u, base %p: %s",
                         nlibrary, hmodule,
                         (char*)(base + image_import_descriptor->Name));
                ++ nlibrary;
            }
        }
        return nlibrary;
    }
}

static uint32_t buildTimeImageCount = 0;
static void* buildTimeImages[16];

/* Resolve symbols against the executable and its build-time dependencies */
void* os_dlsym_default(char* name)
{
    unsigned int i;
    void* result = 0;
    buildTimeImages[0] = (void*)runtime_module_handle;
    if (buildTimeImageCount == 0) {
        buildTimeImageCount =
            1 + os_get_build_time_shared_libraries(15u,
            NULL, 1+(void**)buildTimeImages, NULL);
    }
    for (i = 0; i<buildTimeImageCount && (!result); ++i) {
        result = GetProcAddress(buildTimeImages[i], name);
    }
    return result;
}

/* We want to get a slot in TIB that (1) is available at constant
   offset, (2) is our private property, so libraries wouldn't legally
   override it, (3) contains something predefined for threads created
   out of our sight.

   Low 64 TLS slots are adressable directly, starting with
   FS:[#xE10]. When SBCL runtime is initialized, some of the low slots
   may be already in use by its prerequisite DLLs, as DllMain()s and
   TLS callbacks have been called already. But slot 63 is unlikely to
   be reached at this point: one slot per DLL that needs it is the
   common practice, and many system DLLs use predefined TIB-based
   areas outside conventional TLS storage and don't need TLS slots.
   With our current dependencies, even slot 2 is observed to be free
   (as of WinXP and wine).

   Now we'll call TlsAlloc() repeatedly until slot 63 is officially
   assigned to us, then TlsFree() all other slots for normal use. TLS
   slot 63, alias FS:[#.(+ #xE10 (* 4 63))], now belongs to us.

   To summarize, let's list the assumptions we make:

   - TIB, which is FS segment base, contains first 64 TLS slots at the
   offset #xE10 (i.e. TIB layout compatibility);
   - TLS slots are allocated from lower to higher ones;
   - All libraries together with CRT startup have not requested 64
   slots yet.

   All these assumptions together don't seem to be less warranted than
   the availability of TIB arbitrary data slot for our use. There are
   some more reasons to prefer slot 63 over TIB arbitrary data: (1) if
   our assumptions for slot 63 are violated, it will be detected at
   startup instead of causing some system-specific unreproducible
   problems afterwards, depending on OS and loaded foreign libraries;
   (2) if getting slot 63 reliably with our current approach will
   become impossible for some future Windows version, we can add TLS
   callback directory to SBCL binary; main image TLS callback is
   started before _any_ TLS slot is allocated by libraries, and
   some C compiler vendors rely on this fact. */

extern CRITICAL_SECTION code_allocator_lock, alloc_profiler_lock;
static CRITICAL_SECTION interrupt_io_lock;

struct {
    console_char buffer[MAX_CONSOLE_TCHARS];
    DWORD head, tail;
    CRITICAL_SECTION lock;
    CONDITION_VARIABLE cond_has_data;
    CONDITION_VARIABLE cond_has_client;
    HANDLE thread;
    bool initialized;
    HANDLE handle;
    bool in_progress;
} ttyinput;

#ifdef LISP_FEATURE_64_BIT
// 32-bit uses a hardwired value (63) as this index because codegen
// usese %FS:[a_constant] to get CURRENT-THREAD-SAP, etc.
DWORD sbcl_thread_tls_index;
#endif

int os_preinit(char *argv[], char *envp[])
{
#ifdef LISP_FEATURE_SB_FUTEX
    if (GetModuleHandle("API-MS-Win-Core-Synch-l1-2-0") == NULL) {
        // calling lose() here loses. Such irony.
        fprintf(stderr, "This binary was compiled for Windows 8 or later but you appear to be"
" using 7 or earlier.\nRecompiling SBCL from source on the older release will"
" probably work. See also\n"
                "https://support.microsoft.com/en-us/help/4057281/windows-7-support-ended-on-january-14-2020\n");
        fflush(stderr);
        exit(1);
    }
#endif
    InitializeCriticalSection(&code_allocator_lock);
    InitializeCriticalSection(&alloc_profiler_lock);
    InitializeCriticalSection(&interrupt_io_lock);
    InitializeCriticalSection(&ttyinput.lock);
#ifdef LISP_FEATURE_X86
    DWORD slots[TLS_MINIMUM_AVAILABLE];
    DWORD key;
    int n_slots = 0, i;
    for (i=0; i<TLS_MINIMUM_AVAILABLE; ++i) {
        key = TlsAlloc();
        if (key == OUR_TLS_INDEX) {
            if (TlsGetValue(key)!=NULL)
                lose("TLS slot assertion failed: fresh slot value is not NULL");
            TlsSetValue(OUR_TLS_INDEX, (void*)(intptr_t)0xFEEDBAC4);
            if ((intptr_t)(void*)get_sb_vm_thread()!=(intptr_t)0xFEEDBAC4)
                lose("TLS slot assertion failed: TIB layout change detected");
            TlsSetValue(OUR_TLS_INDEX, NULL);
            break;
        }
        slots[n_slots++]=key;
    }
    for (i=0; i<n_slots; ++i) {
        TlsFree(slots[i]);
    }
    if (key!=OUR_TLS_INDEX) {
        lose("TLS slot assertion failed: slot 63 is unavailable "
             "(last TlsAlloc() returned %u)",key);
    }
#else
    OUR_TLS_INDEX = TlsAlloc();
#endif
    return 0;
}

int os_number_of_processors = 1;

BOOL (*ptr_CancelIoEx)(HANDLE /*handle*/, LPOVERLAPPED /*overlapped*/);
BOOL (*ptr_CancelSynchronousIo)(HANDLE /*threadHandle*/);

#define RESOLVE(hmodule,fn)                     \
    do {                                        \
        ptr_##fn = (typeof(ptr_##fn)) (void *)  \
            GetProcAddress(hmodule,#fn);        \
    } while (0)

static void resolve_optional_imports()
{
    HMODULE kernel32 = GetModuleHandleA("kernel32");
    if (kernel32) {
        RESOLVE(kernel32,CancelIoEx);
        RESOLVE(kernel32,CancelSynchronousIo);
    }
}

#undef RESOLVE

intptr_t win32_get_module_handle_by_address(void* addr)
{
    HMODULE result = 0;
    /* So apparently we could use VirtualQuery instead of
     * GetModuleHandleEx if we wanted to support pre-XP, pre-2003
     * versions of Windows (i.e. Windows 2000).  I've opted against such
     * special-casing. :-).  --DFL */
    return (intptr_t)(GetModuleHandleEx(
                          GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
                          GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                          (LPCSTR)addr, &result)
                      ? result : 0);
}

/*
 * x86-64 exception handling around foreign function calls.
 *
 * On x86-64, when an exception is raised, the Windows kernel looks up the RIP
 * to retrieve an associated UNWIND_INFO object. This object informs the kernel
 * whether there's an exception handler and how to find the start of the current
 * frame. If there's no exception handler or it has declined to handle the
 * exception, the frame's return address is looked up and the process is
 * repeated, walking the stack until an exception handler handles the exception.
 * If the exception goes unhandled, the process is usually terminated. Our goal,
 * then, is to associate an appropriate UNWIND_INFO object with return addresses
 * that point to Lisp code. We don't need to inform the kernel how to walk a
 * Lisp frame, since our exception handler will always handle the exception.
 *
 * UNWIND_INFO objects are mapped onto their respective address ranges by
 * RUNTIME_FUNCTION objects. These objects are usually stored in the .pdata
 * section of an executable file, but can also be mapped dynamically via
 * RtlAddFunctionTable().
 *
 * A major constraint imposed by UNWIND_INFO and RUNTIME_FUNCTION is that the
 * code's address range is specified by 32-bit relative addresses, which means
 * we can only establish a frame-based handler for a 4 GB region at most. This
 * is problematic because most Lisp code lives in dynamic space which can be
 * much larger than 4 GB. Furthermore, the exception handler is also specified
 * as a 32-bit relative address, so we can't directly reference our
 * handle_exception() function.
 *
 * We work around this constraint by allocating these data structures in a fixed
 * memory region at WIN64_SEH_DATA_ADDR and map an UNWIND_INFO to this region
 * using RtlAddFunctionTable(). Foreign calls (see EMIT-C-CALL) place the target
 * address in RBX and call a thunk within this region. The thunk swaps out the
 * return address with the thunk's return address thereby establishing a
 * frame-based exception handler. The original address (pointing into Lisp code)
 * is stored in a non-volatile, callee-saved register, allowing the thunk to
 * jump back to Lisp. The exception handler is a trampoline to
 * handle_exception().
 *
 * In the future, should we segregate code objects into dedicated code areas
 * smaller than 4 GB, these thunks could be removed, and UNWIND_INFO could be
 * associated directly with such areas.
 */

#ifdef LISP_FEATURE_X86_64
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

struct win64_seh_data {
    uint8_t direct_thunk[8];
    uint8_t indirect_thunk[8];
    uint8_t handler_trampoline[16];
    UNWIND_INFO ui; // needs to be DWORD-aligned
    RUNTIME_FUNCTION rt;
};

static void
set_up_win64_seh_thunk(size_t page_size)
{
    if (page_size < sizeof(struct win64_seh_data))
        lose("Not enough space to allocate struct win64_seh_data");

    gc_assert(VirtualAlloc(WIN64_SEH_DATA_ADDR, page_size,
                           MEM_RESERVE | MEM_COMMIT, PAGE_EXECUTE_READWRITE));

    struct win64_seh_data *seh_data = (void *) WIN64_SEH_DATA_ADDR;
    DWORD64 base = (DWORD64) seh_data;

    // 'volatile' works around "warning: writing 1 byte into a region of size 0 [-Wstringop-overflow=]"
    volatile uint8_t *dthunk = seh_data->direct_thunk;
    dthunk[0] = 0x41; // pop r15
    dthunk[1] = 0x5F;
    dthunk[2] = 0xFF; // call rbx
    dthunk[3] = 0xD3;
    dthunk[4] = 0x41; // push r15
    dthunk[5] = 0x57;
    dthunk[6] = 0xC3; // ret
    dthunk[7] = 0x90; // nop (padding)

    volatile uint8_t *ithunk = seh_data->indirect_thunk;
    ithunk[0] = 0x41; // pop r15
    ithunk[1] = 0x5F;
    ithunk[2] = 0xFF; // call qword ptr [rbx]
    ithunk[3] = 0x13;
    ithunk[4] = 0x41; // push r15
    ithunk[5] = 0x57;
    ithunk[6] = 0xC3; // ret
    ithunk[7] = 0x90; // nop (padding)

    volatile uint8_t *tramp = seh_data->handler_trampoline;
    tramp[0] = 0xFF; // jmp qword ptr [rip+2]
    tramp[1] = 0x25;
    UNALIGNED_STORE32((void*volatile)(tramp+2), 2);
    tramp[6] = 0x66; // 2-byte nop
    tramp[7] = 0x90;
    *(void **)(tramp+8) = handle_exception;

    UNWIND_INFO *ui = &seh_data->ui;
    ui->Version = 1;
    ui->Flags = UNW_FLAG_EHANDLER;
    ui->SizeOfProlog = 0;
    ui->CountOfCodes = 0;
    ui->FrameRegister = 0;
    ui->FrameOffset = 0;
    ui->ExceptionHandler = (DWORD64) tramp - base;
    ui->ExceptionData[0] = 0;

    RUNTIME_FUNCTION *rt = &seh_data->rt;
    rt->BeginAddress = 0;
    rt->EndAddress = 16;
    rt->UnwindData = (DWORD64) ui - base;

    gc_assert(RtlAddFunctionTable(rt, 1, base));
}
#endif

static LARGE_INTEGER lisp_init_time;
static double qpcMultiplier;
DWORD win32_page_size = 0;
ULONG win32_stack_guarantee = 0;

void
win32_set_stack_guarantee()
{
    /* 64 KB appears to be enough to run the SWANK debugger + (FACT 1000). 32 KB
     * is surprisingly not enough for the debugger. */
    ULONG request = 64*1024;
    if (!SetThreadStackGuarantee(&request)) {
        fprintf(stderr, "ERROR: SetThreadStackGuarantee failed: 0x%lx.\n",
                GetLastError());
        fflush(stderr);
    }

    /* Store stack guarantee size the first time around so that
     * win32_reset_stack_overflow_guard_page() may restore the stack guard. */
    if (!win32_stack_guarantee)
        SetThreadStackGuarantee(&win32_stack_guarantee);
}

/*
 * The usual way to do this would be to invoke _resetstkoflw(). However, it
 * refuses to re-establish the stack overflow guard page when the stack
 * pointer is still within the stack area reserved by SetThreadStackGuarantee().
 *
 * This is invoked when SB-KERNEL:HANDLE-WIN32-EXCEPTION is unwound after
 * handling a STACK_OVERFLOW_EXCEPTION.
 */
void
win32_reset_stack_overflow_guard_page() {
    struct thread *self = get_sb_vm_thread();

    /* this is similar to CONTROL_STACK_RETURN_GUARD_PAGE on other platforms,
     * but Windows handles the page faults on its own.
     *
     * From experimentation, it seems that as long as set up a guard region
     * somewhere below the stack guarantee, Windows will manage to raise a
     * STACK_OVERFLOW_EXCEPTION appropriately next time we exhaust the stack.
     * Furthermore, if we reprotect sufficiently far away from the stack
     * guarantee, user code can get away with modifying the stack while
     * unwinding from a stack overflow condition without retriggering the guard
     * page. See test (:EXHAUST :WRITE-TO-STACK-ON-UNWIND). */
#define WIN32_STACK_GUARD_SLACK (2*win32_stack_guarantee + win32_page_size)
    char *stack_guard_start = CONTROL_STACK_GUARD_PAGE(self);
    fprintf(stderr, "INFO: Reprotecting control stack guard (0x%p+0x%lx)\n",
            stack_guard_start, WIN32_STACK_GUARD_SLACK);
    fflush(stderr);

    DWORD oldprot;
    VirtualProtect(stack_guard_start + WIN32_STACK_GUARD_SLACK,
                   /* a single page would probably be enough, but let's cargo
                    * cult Windows and use guarantee size + 1 page. */
                   win32_stack_guarantee + win32_page_size,
                   PAGE_READWRITE | PAGE_GUARD,
                   &oldprot);

    self->state_word.control_stack_guard_page_protected = 1;
}

void os_init()
{
#ifdef LISP_FEATURE_64_BIT
    LARGE_INTEGER qpcFrequency;
    if (!QueryPerformanceCounter(&lisp_init_time) ||
        !QueryPerformanceFrequency(&qpcFrequency))
        lose("Can't use monotonic realtime clock. Please report this as an SBCL bug");
    // Convert performance counter ticks per second into INTERNAL-TIME-UNITS-PER-SECOND
    // (microseconds). In theory the performance counter can increment faster than
    // or slower than I-T-U. Fwiw, on my machine the frequency is 10x I-T-U-P-S,
    // so a single integer division by 10 would work, but I don't know in general
    // what the ratio is.
    qpcMultiplier = (double)1000000 / qpcFrequency.QuadPart;
#endif

    SYSTEM_INFO system_info;
    GetSystemInfo(&system_info);
    win32_page_size = system_info.dwPageSize;
    os_vm_page_size = system_info.dwPageSize > BACKEND_PAGE_BYTES?
        system_info.dwPageSize : BACKEND_PAGE_BYTES;
    os_number_of_processors = system_info.dwNumberOfProcessors;

#ifdef LISP_FEATURE_X86_64
    set_up_win64_seh_thunk(os_vm_page_size);
#endif

    resolve_optional_imports();
    runtime_module_handle = (HMODULE)win32_get_module_handle_by_address(&runtime_module_handle);
}

#ifdef LISP_FEATURE_64_BIT
uword_t get_monotonic_time()
{
    LARGE_INTEGER t;
    QueryPerformanceCounter(&t);
    t.QuadPart -= lisp_init_time.QuadPart;
    return (uword_t)((double)t.QuadPart * qpcMultiplier);
}
#endif

os_vm_address_t
os_alloc_gc_space(int space_id, int attributes, os_vm_address_t addr, os_vm_size_t len)
{
    if (!addr) {
        int protection = attributes & IS_GUARD_PAGE ? PAGE_NOACCESS : PAGE_EXECUTE_READWRITE;
        os_vm_address_t actual = VirtualAlloc(addr, len, MEM_RESERVE|MEM_COMMIT, protection);
        gc_assert(actual);
        return actual;
    }

    // Reserving the dynamic space doesn't commit it.
    DWORD commit =
      (space_id == DYNAMIC_CORE_SPACE_ID && (attributes & MOVABLE)) ? 0 : MEM_COMMIT;
    os_vm_address_t actual = VirtualAlloc(addr, len, MEM_RESERVE|commit, PAGE_EXECUTE_READWRITE);

    if (!actual) {
        if (!(attributes & MOVABLE)) {
            fprintf(stderr,
                    "VirtualAlloc: wanted %lu bytes at %p, actually mapped at %p\n",
                    (unsigned long) len, addr, actual);
            fflush(stderr);
            return 0;
        }

        actual = VirtualAlloc(NULL, len, MEM_RESERVE|commit, PAGE_EXECUTE_READWRITE);
        gc_assert(actual);
    }

    return actual;
}

void os_commit_memory(os_vm_address_t addr, os_vm_size_t len)
{
    if (len) {
        gc_assert(addr);
        gc_assert(VirtualAlloc(addr, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE));
    }
}

/*
 * load_core_bytes() is called to load a chunk of the core file into memory.
 *
 * Unfortunately, Windows semantics completely screws this up, so
 * we just add backing store from the swapfile to where the chunk
 * goes and read it up like a normal file. We could consider using
 * a lazy read (demand page) setup, but that would mean keeping an
 * open file pointer for the core indefinately (and be one more
 * thing to maintain).
 * FIXME: I would bet that we can use PAGE_EXECUTE_WRITECOPY for this,
 * but I'll leave it to someone who actually cares.
 */

void* load_core_bytes(int fd, os_vm_offset_t offset, os_vm_address_t addr, os_vm_size_t len,
                      int is_readonly_space)
{
    if (addr) {
        os_commit_memory(addr, len);
    } else {
        addr = VirtualAlloc(NULL, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
        gc_assert(addr);
    }
#ifdef LISP_FEATURE_64_BIT
    os_vm_offset_t res = _lseeki64(fd, offset, SEEK_SET);
#else
    os_vm_offset_t res = lseek(fd, offset, SEEK_SET);
#endif
    gc_assert(res == offset);
    int count;

    os_vm_address_t original_addr = addr;
    os_vm_size_t original_len = len;
    while (len) {
        unsigned to_read = len > INT_MAX ? INT_MAX : len;
        count = read(fd, addr, to_read);
        if (count == -1) {
            perror("read() failed"); fflush(stderr);
        }
        addr += count;
        len -= count;
        gc_assert(count == (int) to_read);
    }
    DWORD old;
    if (is_readonly_space) VirtualProtect(original_addr, original_len, PAGE_READONLY, &old);

    return original_addr;
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

    DWORD new_prot = os_protect_modes[prot];
    gc_assert(VirtualProtect(address, length, new_prot, &old_prot)||
              (VirtualAlloc(address, length, MEM_COMMIT, new_prot) &&
               VirtualProtect(address, length, new_prot, &old_prot)));
    odxprint(misc,"Protecting %p + %p vmaccess %d "
             "newprot %08x oldprot %08x",
             address,length,prot,new_prot,old_prot);
}

/* A tiny bit of interrupt.c state we want our paws on. */
extern int internal_errors_enabled;

extern void exception_handler_wrapper();

#ifdef LISP_FEATURE_X86
#define voidreg(ctxptr,name) ((void*)((ctxptr)->E##name))
#else
#define voidreg(ctxptr,name) ((void*)((ctxptr)->R##name))
#endif


static int
handle_single_step(os_context_t *ctx)
{
    if (!single_stepping)
        return -1;

    /* We are doing a displaced instruction. At least function
     * end breakpoints use this. */
    restore_breakpoint_from_single_step(ctx);

    return 0;
}

#ifdef LISP_FEATURE_UD2_BREAKPOINTS
#define SBCL_EXCEPTION_BREAKPOINT EXCEPTION_ILLEGAL_INSTRUCTION
#define TRAP_CODE_WIDTH 2
#else
#define SBCL_EXCEPTION_BREAKPOINT EXCEPTION_BREAKPOINT
#define TRAP_CODE_WIDTH 1
#endif

static int
handle_breakpoint_trap(os_context_t *ctx, struct thread* self)
{
#ifdef LISP_FEATURE_UD2_BREAKPOINTS
    if (((unsigned short *)OS_CONTEXT_PC(ctx))[0] != 0x0b0f)
        return -1;
#endif

    /* Unlike some other operating systems, Win32 leaves EIP
     * pointing to the breakpoint instruction. */
    OS_CONTEXT_PC(ctx) += TRAP_CODE_WIDTH;

    /* Now EIP points just after the INT3 byte and aims at the
     * 'kind' value (eg trap_Cerror). */
    unsigned trap = *(unsigned char *)OS_CONTEXT_PC(ctx);

    /* Before any other trap handler: gc_safepoint ensures that
       inner alloc_sap for passing the context won't trap on
       pseudo-atomic. */
    /* Now that there is no alloc_sap, I don't know what happens here. */
    if (trap == trap_PendingInterrupt) {
        /* Done everything needed for this trap, except EIP
           adjustment */
        arch_skip_instruction(ctx);
        thread_interrupted(ctx);
        return 0;
    }

    /* This is just for info in case the monitor wants to print an
     * approximation. */
    access_control_stack_pointer(self) =
        (lispobj *)*os_context_sp_addr(ctx);

    WITH_GC_AT_SAFEPOINTS_ONLY() {
        block_blockable_signals(&ctx->sigmask);
        handle_trap(ctx, trap);
        thread_sigmask(SIG_SETMASK,&ctx->sigmask,NULL);
    }

    /* Done, we're good to go! */
    return 0;
}

static int
handle_access_violation(os_context_t *ctx,
                        EXCEPTION_RECORD *exception_record,
                        void *fault_address,
                        struct thread* self)
{
    CONTEXT *win32_context = ctx->win32_context;

#if defined(LISP_FEATURE_X86)
    odxprint(pagefaults,
             "SEGV. ThSap %p, Eip %p, Esp %p, Esi %p, Edi %p, "
             "Addr %p Access %d\n",
             self,
             win32_context->Eip,
             win32_context->Esp,
             win32_context->Esi,
             win32_context->Edi,
             fault_address,
             exception_record->ExceptionInformation[0]);
#else
    odxprint(pagefaults,
             "SEGV. ThSap %p, Eip %p, Esp %p, Esi %p, Edi %p, "
             "Addr %p Access %d\n",
             self,
             win32_context->Rip,
             win32_context->Rsp,
             win32_context->Rsi,
             win32_context->Rdi,
             fault_address,
             exception_record->ExceptionInformation[0]);
#endif

    /* Safepoint pages */
    if (fault_address == (void *) GC_SAFEPOINT_TRAP_ADDR) {
        thread_in_lisp_raised(ctx);
        return 0;
    }

    if ((1+THREAD_HEADER_SLOTS)+(lispobj*)fault_address == (lispobj*)self) {
        thread_in_safety_transition(ctx);
        return 0;
    }

    /* dynamic space */
    page_index_t page = find_page_index(fault_address);
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    if (page >= 0) lose("should not get access violation in dynamic space");
#else
    if (page != -1 && !PAGE_WRITEPROTECTED_P(page)) {
        os_commit_memory(PTR_ALIGN_DOWN(fault_address, os_vm_page_size),
                         os_vm_page_size);
        return 0;
    }
    if (gencgc_handle_wp_violation(ctx, fault_address)) {
        return 0;
    }
#endif

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    extern int immobile_space_handle_wp_violation(void*);
    if (immobile_space_handle_wp_violation(fault_address)) {
        return 0;
    }
#endif
    if (handle_guard_page_triggered(ctx, fault_address)) {
        return 0;
    }

    return -1;
}

static void
signal_internal_error_or_lose(os_context_t *ctx,
                              EXCEPTION_RECORD *exception_record,
                              void *fault_address)
{
    /*
     * If we fall through to here then we need to either forward
     * the exception to the lisp-side exception handler if it's
     * set up, or drop to LDB.
     */

    if (internal_errors_enabled) {
        /* The exception system doesn't automatically clear pending
         * exceptions, so we lose as soon as we execute any FP
         * instruction unless we do this first. */
        asm("fnclex");
        /* We're making the somewhat arbitrary decision that having
         * internal errors enabled means that lisp has sufficient
         * marbles to be able to handle exceptions, but exceptions
         * aren't supposed to happen during cold init or reinit
         * anyway. */

        block_blockable_signals(&ctx->sigmask);
        fake_foreign_function_call(ctx);

        WITH_GC_AT_SAFEPOINTS_ONLY() {
            DX_ALLOC_SAP(context_sap, ctx);
            DX_ALLOC_SAP(exception_record_sap, exception_record);
            thread_sigmask(SIG_SETMASK, &ctx->sigmask, NULL);

#ifdef LISP_FEATURE_X86_64
            asm("fninit");
#endif

            /* Call into lisp to handle things. */
            funcall2(StaticSymbolFunction(HANDLE_WIN32_EXCEPTION),
                     context_sap,
                     exception_record_sap);
        }
        /* If Lisp doesn't nlx, we need to put things back. */
        undo_fake_foreign_function_call(ctx);
        thread_sigmask(SIG_SETMASK, &ctx->sigmask, NULL);
        /* FIXME: HANDLE-WIN32-EXCEPTION should be allowed to decline */
        return;
    }

    fprintf(stderr, "Exception Code: %p.\n",
            (void*)(intptr_t)exception_record->ExceptionCode);
    fprintf(stderr, "Faulting IP: %p.\n",
            (void*)(intptr_t)exception_record->ExceptionAddress);
    if ((long int)exception_record->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
        MEMORY_BASIC_INFORMATION mem_info;

        if (VirtualQuery(fault_address, &mem_info, sizeof mem_info)) {
            fprintf(stderr, "page status: 0x%lx.\n", mem_info.State);
        }

        fprintf(stderr, "Was writing: %p, where: %p.\n",
                (void*)exception_record->ExceptionInformation[0],
                fault_address);
    }

    fflush(stderr);

    fake_foreign_function_call(ctx);
    lose("Exception too early in cold init, cannot continue.");
}

static EXCEPTION_DISPOSITION
handle_exception_ex(EXCEPTION_RECORD *exception_record,
                    struct lisp_exception_frame *exception_frame,
                    CONTEXT *win32_context,
                    BOOL continue_search_on_unhandled_access_violation)
{
    if (!win32_context)
        /* Not certain why this should be possible, but let's be safe... */
        return ExceptionContinueSearch;

    if (exception_record->ExceptionFlags & (EH_UNWINDING | EH_EXIT_UNWIND)) {
        /* If we're being unwound, be graceful about it. */

        /* Undo any dynamic bindings. */
        unbind_to_here(exception_frame->bindstack_pointer,
                       get_sb_vm_thread());
        return ExceptionContinueSearch;
    }

    DWORD code = exception_record->ExceptionCode;

    if(code == 0x20474343 || /* GCC */
       code == 0x406D1388 || /* MS_VC_EXCEPTION */
       code == 0xE06D7363 || /* Emsc */
       code == 0xE0434352)   /* ECCR */
        /* Do not handle G++, VC++ and .NET exceptions */
        return ExceptionContinueSearch;

    DWORD lastError = GetLastError();
    DWORD lastErrno = errno;

    struct thread* self = get_sb_vm_thread();

    os_context_t context, *ctx = &context;
    context.win32_context = win32_context;
    context.sigmask = self ? thread_extra_data(self)->blocked_signal_set : 0;

    os_context_register_t oldbp = 0;
    if (self) {
        oldbp = self ? thread_extra_data(self)->carried_base_pointer : 0;
        thread_extra_data(self)->carried_base_pointer
            = (os_context_register_t) voidreg(win32_context, bp);
    }

    /* For EXCEPTION_ACCESS_VIOLATION only. */
    void *fault_address = (void *)exception_record->ExceptionInformation[1];

    odxprint(seh,
             "SEH: rec %p, ctxptr %p, rip %p, fault %p\n"
             "... code %p, rcx %p, fp-tags %p\n\n",
             exception_record,
             win32_context,
             voidreg(win32_context,ip),
             fault_address,
             (void*)(intptr_t)code,
             voidreg(win32_context,cx),
             win32_context->FloatSave.TagWord);

    /* This function had become unwieldy.  Let's cut it down into
     * pieces based on the different exception codes.  Each exception
     * code handler gets the chance to decline by returning non-zero if it
     * isn't happy: */

    int rc;
    EXCEPTION_DISPOSITION disp = ExceptionContinueExecution;
    switch (code) {
    case EXCEPTION_STACK_OVERFLOW:
    {
        void *sp = voidreg(win32_context, sp);
        fprintf(stderr, "INFO: Caught stack overflow exception (sp=0x%p); "
                        "proceed with caution.\n", sp);
        fflush(stderr);
        self->state_word.control_stack_guard_page_protected = 0;
        rc = -1;
        break;
    }

    case EXCEPTION_ACCESS_VIOLATION:
        rc = handle_access_violation(ctx, exception_record, fault_address, self);
        if (rc && continue_search_on_unhandled_access_violation) {
            rc = 0;
            disp = ExceptionContinueSearch;
        }
        break;

    case SBCL_EXCEPTION_BREAKPOINT:
        rc = handle_breakpoint_trap(ctx, self);
        break;

    case EXCEPTION_SINGLE_STEP:
        rc = handle_single_step(ctx);
        break;

    default:
        rc = -1;
    }

    if (rc)
        /* All else failed, drop through to the lisp-side exception handler. */
        signal_internal_error_or_lose(ctx, exception_record, fault_address);

    if (self)
        thread_extra_data(self)->carried_base_pointer = oldbp;

    errno = lastErrno;
    SetLastError(lastError);
    return disp;
}

/*
 * A good explanation of the exception handling semantics is
 *   https://web.archive.org/web/20120628151428/http://win32assembly.online.fr/Exceptionhandling.html
 *   (x86-specific)
 * or:
 *   https://docs.microsoft.com/en-us/windows/win32/debug/structured-exception-handling
 * or:
 *   James McNellis's CppCon 2018 talk: "Unwinding the Stack: Exploring How C++
 *   Exceptions Work on Windows"
 *
 * On x86, this function is always invoked by frame-based exception handlers,
 * either the one established by call_into_lisp in x86-assem.S or the top-level
 * handler established by wos_install_interrupt_handlers().
 *
 * On x86-64, this function may be invoked by the frame-based exception handler
 * established by set_up_win64_seh_thunk(). Our vectored exception handler,
 * veh(), invokes handle_exception_ex directly.
 */

EXCEPTION_DISPOSITION
handle_exception(EXCEPTION_RECORD *exception_record,
                 struct lisp_exception_frame *exception_frame,
                 CONTEXT *win32_context,
                 void __attribute__((__unused__)) *dispatcher_context)
{
    return handle_exception_ex(exception_record, exception_frame, win32_context, FALSE);
}

#ifdef LISP_FEATURE_X86_64

#define RESTORING_ERRNO()                                       \
    int sbcl__lastErrno = errno;                                \
    RUN_BODY_ONCE(restoring_errno, errno = sbcl__lastErrno)

/*
 * This vectored exception handler runs before frame-based handlers, so we only
 * want to handle exceptions triggered by Lisp code. Exceptions raised by
 * foreign function calls are handled by the frame-based handler established by
 * set_up_win64_seh_thunk().
 *
 * Access violation exceptions can be triggered by the runtime as well, and
 * that's neither Lisp code nor is it covered the SEH thunk, so we handle those
 * exceptions differently.
 */
LONG
veh(EXCEPTION_POINTERS *ep)
{
    EXCEPTION_DISPOSITION disp = ExceptionContinueSearch;

    RESTORING_ERRNO() {
        if (!get_sb_vm_thread())
            return EXCEPTION_CONTINUE_SEARCH;
    }

    DWORD64 rip = ep->ContextRecord->Rip;
    long int code = ep->ExceptionRecord->ExceptionCode;
    BOOL from_lisp =
        (rip >= DYNAMIC_SPACE_START && rip < DYNAMIC_SPACE_START+dynamic_space_size) ||
        (rip >= READ_ONLY_SPACE_START && rip < READ_ONLY_SPACE_END) ||
        immobile_space_p(rip) ||
        (rip >= STATIC_SPACE_START && rip < (uword_t)static_space_free_pointer);

    if (code == EXCEPTION_ACCESS_VIOLATION ||
        code == STATUS_HEAP_CORRUPTION ||
        from_lisp)
        disp = handle_exception_ex(ep->ExceptionRecord, 0, ep->ContextRecord,
                                   // continue search on unhandled memory faults
                                   // if not in Lisp code. This gives foreign
                                   // handlers a chance to handle the
                                   // exception. If nothing handles it, our
                                   // frame-based handler will.
                                   !from_lisp);

    switch (disp) {
    case ExceptionContinueExecution:
        return EXCEPTION_CONTINUE_EXECUTION;
    case ExceptionContinueSearch:
        return EXCEPTION_CONTINUE_SEARCH;
    default:
        fprintf(stderr, "Exception handler is mad\n"); fflush(stderr);
        ExitProcess(0);
    }
}
#endif

os_context_register_t
carry_frame_pointer(os_context_register_t default_value)
{
    struct thread* self = get_sb_vm_thread();
    os_context_register_t bp = thread_extra_data(self)->carried_base_pointer;
    return bp ? bp : default_value;
}

void
wos_install_interrupt_handlers
(struct lisp_exception_frame __attribute__((__unused__)) *handler)
{
#ifdef LISP_FEATURE_X86
    handler->next_frame = get_seh_frame();
    handler->handler = (void*)exception_handler_wrapper;
    set_seh_frame(handler);
#else
    static int once = 0;
    if (!once++)
        AddVectoredExceptionHandler(1,veh);
#endif
}

char *dirname(char *path)
{
    static char buf[PATH_MAX + 1];
    size_t pathlen = strlen(path);
    int i;

    if (pathlen >= sizeof(buf)) {
        lose("Pathname too long in dirname.");
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

// 0 - not a socket or other error, 1 - has input, 2 - has no input
int
socket_input_available(HANDLE socket, long time, long utime)
{
    int count = 0;
    int wsaErrno = GetLastError();
    TIMEVAL timeout = {time, utime};
    fd_set readfds, errfds;

    FD_ZERO(&readfds);
    FD_ZERO(&errfds);
    FD_SET((uword_t)socket, &readfds);
    FD_SET((uword_t)socket, &errfds);

    count = select(0, &readfds, NULL, &errfds, &timeout);
    SetLastError(wsaErrno);

    if (count == SOCKET_ERROR)
        /* This is an error in select() itself, not with the socket. */
        return 0;
    else if (count == 0)
        return 2;
    /* True if there is data, or the socket has been closed (i.e. a read
     * would retrieve an EOF error) */
    else if (FD_ISSET(socket, &readfds))
        return 1;
    else
        /* This is an error in the socket. The count was positive, and
         * it wasn't in the read set, so it must be in the error set. */
        return 0;
}

/* Atomically mark current thread as (probably) doing synchronous I/O
 * on handle, if no cancellation is requested yet (and return TRUE),
 * otherwise clear thread's I/O cancellation flag and return false.
 */
static bool io_begin_interruptible(HANDLE handle)
{
    /* No point in doing it unless OS supports cancellation from other
     * threads */
    if (!ptr_CancelIoEx)
        return 1;

    struct thread* this_thread = get_sb_vm_thread();
    if (!__sync_bool_compare_and_swap(&thread_extra_data(this_thread)->synchronous_io_handle_and_flag,
                                      0, handle)) {
        ResetEvent(thread_private_events(this_thread,0));
        thread_extra_data(this_thread)->synchronous_io_handle_and_flag = 0;
        return 0;
    }
    return 1;
}

/* Unmark current thread as (probably) doing synchronous I/O; if an
 * I/O cancellation was requested, postpone it until next
 * io_begin_interruptible */
static void
io_end_interruptible(HANDLE handle)
{
    if (!ptr_CancelIoEx)
        return;
    EnterCriticalSection(&interrupt_io_lock);
    __sync_bool_compare_and_swap(&thread_extra_data(get_sb_vm_thread())->synchronous_io_handle_and_flag,
                                 handle, 0);
    LeaveCriticalSection(&interrupt_io_lock);
}
#define WITH_INTERRUPTIBLE_IO(handle)      \
    if (!io_begin_interruptible(handle)) { \
      errno = EINTR;                       \
      return -1;                           \
    }                                      \
    RUN_BODY_ONCE(xx, io_end_interruptible(handle))

int console_handle_p(HANDLE handle)
{
    DWORD mode;
    return GetFileType(handle) == FILE_TYPE_CHAR &&
        GetConsoleMode(handle, &mode);
}

/*
 * (AK writes:)
 *
 * It may be unobvious, but (probably) the most straightforward way of
 * providing some sane CL:LISTEN semantics for line-mode console
 * channel requires _dedicated input thread_.
 *
 * LISTEN should return true iff the next (READ-CHAR) won't have to
 * wait. As our console may be shared with another process, entirely
 * out of our control, looking at the events in PeekConsoleEvent
 * result (and searching for #\Return) doesn't cut it.
 *
 * We decided that console input thread must do something smarter than
 * a bare loop of continuous ReadConsoleW(). On Unix, user experience
 * with the terminal is entirely unaffected by the fact that some
 * process does (or doesn't) call read(); the situation on MS Windows
 * is different.
 *
 * Echo output and line editing present on MS Windows while some
 * process is waiting in ReadConsole(); otherwise all input events are
 * buffered. If our thread were calling ReadConsole() all the time, it
 * would feel like Unix cooked mode.
 *
 * But we don't write a Unix emulator here, even if it sometimes feels
 * like that; therefore preserving this aspect of console I/O seems a
 * good thing to us.
 *
 * LISTEN itself becomes trivial with dedicated input thread, but the
 * goal stated above -- provide `native' user experience with blocked
 * console -- don't play well with this trivial implementation.
 *
 * What's currently implemented is a compromise, looking as something
 * in between Unix cooked mode and Win32 line mode.
 *
 * 1. As long as no console I/O function is called (incl. CL:LISTEN),
 * console looks `blocked': no echo, no line editing.
 *
 * 2. (READ-CHAR), (READ-SEQUENCE) and other functions doing real
 * input result in the ReadConsole request (in a dedicated thread);
 *
 * 3. Once ReadConsole is called, it is not cancelled in the
 * middle. In line mode, it returns when <Enter> key is hit (or
 * something like that happens). Therefore, if line editing and echo
 * output had a chance to happen, console won't look `blocked' until
 * the line is entered (even if line input was triggered by
 * (READ-CHAR)).
 *
 * 4. LISTEN may request ReadConsole too (if no other thread is
 * reading the console and no data are queued). It's the only case
 * when the console becomes `unblocked' without any actual input
 * requested by Lisp code.  LISTEN check if there is at least one
 * input event in PeekConsole queue; unless there is such an event,
 * ReadConsole is not triggered by LISTEN.
 *
 * 5. Console-reading Lisp thread now may be interrupted immediately;
 * ReadConsole call itself, however, continues until the line is
 * entered.
 */

static __stdcall unsigned int tty_read_line_server(LPVOID arg)
{
    EnterCriticalSection(&ttyinput.lock);
    while (ttyinput.handle) {
        DWORD nchars;
        BOOL ok;

        while (!ttyinput.in_progress)
          SleepConditionVariableCS(&ttyinput.cond_has_client,&ttyinput.lock,INFINITE);

        LeaveCriticalSection(&ttyinput.lock);
#ifdef LISP_FEATURE_SB_UNICODE
        ok = ReadConsoleW(ttyinput.handle,
                          &ttyinput.buffer[ttyinput.tail],
                          MAX_CONSOLE_TCHARS-ttyinput.tail,
                          &nchars,NULL);
#else
        ok = ReadConsole(ttyinput.handle,
                         &ttyinput.buffer[ttyinput.tail],
                         MAX_CONSOLE_TCHARS-ttyinput.tail,
                         &nchars,NULL);
#endif

        EnterCriticalSection(&ttyinput.lock);

        if (ok) {
            ttyinput.tail += nchars;
            WakeAllConditionVariable(&ttyinput.cond_has_data);
        }
        ttyinput.in_progress = 0;
    }
    LeaveCriticalSection(&ttyinput.lock);
    return 0;
}

static bool tty_maybe_initialize_unlocked(HANDLE handle)
{
    if (!ttyinput.initialized) {
        if (!DuplicateHandle(GetCurrentProcess(),handle,
                             GetCurrentProcess(),&ttyinput.handle,
                             0,FALSE,DUPLICATE_SAME_ACCESS)) {
            return 0;
        }
        InitializeConditionVariable(&ttyinput.cond_has_data);
        InitializeConditionVariable(&ttyinput.cond_has_client);
        ttyinput.thread =
          (HANDLE)_beginthreadex(NULL,
                                 0, // stack size = default
                                 tty_read_line_server, 0,
                                 0, 0);
        ttyinput.initialized = 1;
    }
    return 1;
}

bool win32_tty_listen(HANDLE handle)
{
    bool result = 0;
    INPUT_RECORD ir;
    DWORD nevents;
    EnterCriticalSection(&ttyinput.lock);
    if (!tty_maybe_initialize_unlocked(handle))
        result = 0;

    if (ttyinput.in_progress) {
        result = 0;
    } else {
        if (ttyinput.head != ttyinput.tail) {
            result = 1;
        } else {
            if (PeekConsoleInput(ttyinput.handle,&ir,1,&nevents) && nevents) {
                ttyinput.in_progress = 1;
                WakeAllConditionVariable(&ttyinput.cond_has_client);
            }
        }
    }
    LeaveCriticalSection(&ttyinput.lock);
    return result;
}

static int win32_read_console(HANDLE handle, void* buf, int count)
{
    int result = 0;
    int nchars = count / sizeof(console_char);

    if (!nchars)
        return 0;
    if (nchars>MAX_CONSOLE_TCHARS)
        nchars=MAX_CONSOLE_TCHARS;

    count = nchars*sizeof(console_char);

    EnterCriticalSection(&ttyinput.lock);

    if (!tty_maybe_initialize_unlocked(handle)) {
        result = -1;
        errno = EIO;
        goto unlock;
    }

    while (!result) {
        while (ttyinput.head == ttyinput.tail) {
            if (!io_begin_interruptible(ttyinput.handle)) {
                ttyinput.in_progress = 0;
                result = -1;
                errno = EINTR;
                goto unlock;
            } else {
                if (!ttyinput.in_progress) {
                    /* We are to wait */
                    ttyinput.in_progress=1;
                    /* wake console reader */
                    WakeAllConditionVariable(&ttyinput.cond_has_client);
                }
                SleepConditionVariableCS(&ttyinput.cond_has_data, &ttyinput.lock, INFINITE);
                io_end_interruptible(ttyinput.handle);
            }
        }
        result = sizeof(console_char)*(ttyinput.tail-ttyinput.head);
        if (result > count) {
            result = count;
        }
        if (result) {
            if (result > 0) {
                DWORD nch,offset = 0;
                LPWSTR ubuf = buf;

                memcpy(buf,&ttyinput.buffer[ttyinput.head],count);
                ttyinput.head += (result / sizeof(console_char));
                if (ttyinput.head == ttyinput.tail)
                    ttyinput.head = ttyinput.tail = 0;

                for (nch=0;nch<result/sizeof(console_char);++nch) {
                    if (ubuf[nch]==13) {
                        ++offset;
                    } else {
                        ubuf[nch-offset]=ubuf[nch];
                    }
                }
                result-=offset*sizeof(console_char);

            }
        } else {
            result = -1;
            ttyinput.head = ttyinput.tail = 0;
            errno = EIO;
        }
    }
unlock:
    LeaveCriticalSection(&ttyinput.lock);
    return result;
}

bool win32_maybe_interrupt_io(void* thread)
{
    struct thread *th = thread;
    bool done = 0;

#ifdef LISP_FEATURE_SB_FUTEX
    if (thread_extra_data(th)->waiting_on_address)
        WakeByAddressAll(thread_extra_data(th)->waiting_on_address);
#endif

    if (ptr_CancelIoEx) {
        EnterCriticalSection(&interrupt_io_lock);
        HANDLE h = (HANDLE)
            InterlockedExchangePointer((volatile LPVOID *)
                                       &thread_extra_data(th)->synchronous_io_handle_and_flag,
                                       (LPVOID)INVALID_HANDLE_VALUE);

        if (h && (h!=INVALID_HANDLE_VALUE)) {
            if (console_handle_p(h)) {
                EnterCriticalSection(&ttyinput.lock);
                WakeAllConditionVariable(&ttyinput.cond_has_data);
                LeaveCriticalSection(&ttyinput.lock);
                done = 1;
                goto unlock;
            }
            if (ptr_CancelSynchronousIo) {
                done = !!ptr_CancelSynchronousIo((HANDLE)th->os_thread);
            }
            done |= !!ptr_CancelIoEx(h,NULL);
        }
    unlock:
        LeaveCriticalSection(&interrupt_io_lock);
    }
    return done;
}

static const LARGE_INTEGER zero_large_offset = {.QuadPart = 0LL};

int
win32_write_console(HANDLE handle, void * buf, int count)
{
    DWORD written = 0;
    DWORD nchars = count / sizeof(console_char);
    BOOL result;

    if (nchars>MAX_CONSOLE_TCHARS) nchars = MAX_CONSOLE_TCHARS;

    WITH_INTERRUPTIBLE_IO(handle) {
#ifdef LISP_FEATURE_SB_UNICODE
        result = WriteConsoleW(handle, buf, nchars, &written, NULL);
#else
        result = WriteConsole(handle, buf, nchars, &written, NULL);
#endif
    }

    if (result) {
        if (!written) {
            errno = EINTR;
            return -1;
        } else {
            return written * sizeof(console_char);

        }
    } else {
        DWORD err = GetLastError();
        odxprint(io,"WriteConsole fails => %u\n", err);
        errno = (err==ERROR_OPERATION_ABORTED ? EINTR : EIO);
        return -1;
    }
}

ssize_t
win32_unix_write(HANDLE handle, void * buf, size_t requested_count)
{
    int count = requested_count > INT_MAX ? INT_MAX : requested_count;
    DWORD written_bytes;
    OVERLAPPED overlapped;
    struct thread * self = get_sb_vm_thread();
    BOOL waitInGOR;
    LARGE_INTEGER file_position;
    BOOL seekable;
    BOOL ok;
    DWORD errorCode;

    if (console_handle_p(handle)) {
        return win32_write_console(handle,buf,count);
    }

    overlapped.hEvent = thread_private_events(self,0);
    seekable = SetFilePointerEx(handle,
                                zero_large_offset,
                                &file_position,
                                FILE_CURRENT);
    if (seekable) {
        overlapped.Offset = file_position.LowPart;
        overlapped.OffsetHigh = file_position.HighPart;
    } else {
        overlapped.Offset = 0;
        overlapped.OffsetHigh = 0;
    }

    WITH_INTERRUPTIBLE_IO(handle) {
        ok = WriteFile(handle, buf, count, &written_bytes, &overlapped);
        if (!ok)
            errorCode = GetLastError();
    }

    if (ok) {
        goto done_something;
    } else {
        if (errorCode==ERROR_OPERATION_ABORTED) {
            GetOverlappedResult(handle,&overlapped,&written_bytes,FALSE);
            errno = EINTR;
            return -1;
        }
        if (errorCode!=ERROR_IO_PENDING) {
            errno = errorCode;
            return -1;
        } else {
            if(WaitForMultipleObjects(2,thread_extra_data(self)->private_events,
                                      FALSE,INFINITE) != WAIT_OBJECT_0) {
                CancelIo(handle);
                waitInGOR = TRUE;
            } else {
                waitInGOR = FALSE;
            }
            if (!GetOverlappedResult(handle,&overlapped,&written_bytes,waitInGOR)) {
                if ((errorCode = GetLastError())==ERROR_OPERATION_ABORTED) {
                    errno = EINTR;
                } else {
                    errno = errorCode;
                }
                return -1;
            } else {
                goto done_something;
            }
        }
    }
  done_something:
    if (seekable) {
        file_position.QuadPart += written_bytes;
        SetFilePointerEx(handle,file_position,NULL,FILE_BEGIN);
    }
    return written_bytes;
}


ssize_t
win32_unix_read(HANDLE handle, void * buf, size_t requested_count)
{
    int count = requested_count > INT_MAX ? INT_MAX : requested_count;
    OVERLAPPED overlapped = {.Internal=0};
    DWORD read_bytes = 0;
    struct thread * self = get_sb_vm_thread();
    DWORD errorCode = 0;
    BOOL waitInGOR = FALSE;
    BOOL ok = FALSE;
    LARGE_INTEGER file_position;
    BOOL seekable;

    if (console_handle_p(handle)) {
        return win32_read_console(handle, buf, count);
    }

    overlapped.hEvent = thread_private_events(self,0);
    /* If it has a position, we won't try overlapped */
    seekable = SetFilePointerEx(handle,
                                zero_large_offset,
                                &file_position,
                                FILE_CURRENT);
    if (seekable) {
        overlapped.Offset = file_position.LowPart;
        overlapped.OffsetHigh = file_position.HighPart;
    } else {
        overlapped.Offset = 0;
        overlapped.OffsetHigh = 0;
    }

    WITH_INTERRUPTIBLE_IO(handle) {
        ok = ReadFile(handle,buf,count,&read_bytes, &overlapped);
        if (!ok)
            errorCode = GetLastError();
    }

    if (ok) {
        /* immediately */
        goto done_something;
    } else {
        if (errorCode == ERROR_HANDLE_EOF ||
            errorCode == ERROR_BROKEN_PIPE ||
            errorCode == ERROR_NETNAME_DELETED) {

            read_bytes = 0;
            goto done_something;
        }
        if (errorCode==ERROR_OPERATION_ABORTED) {
            GetOverlappedResult(handle,&overlapped,&read_bytes,FALSE);
            errno = EINTR;
            return -1;
        }
        if (errorCode!=ERROR_IO_PENDING) {
            /* is it some _real_ error? */
            errno = EIO;
            return -1;
        } else {
            int ret;
            if( (ret = WaitForMultipleObjects(2,thread_extra_data(self)->private_events,
                                              FALSE,INFINITE)) != WAIT_OBJECT_0) {
                CancelIo(handle);
                waitInGOR = TRUE;
                /* Waiting for IO only */
            } else {
                waitInGOR = FALSE;
            }
            ok = GetOverlappedResult(handle,&overlapped,&read_bytes,waitInGOR);
            if (!ok) {
                errorCode = GetLastError();
                if (errorCode == ERROR_HANDLE_EOF ||
                    errorCode == ERROR_BROKEN_PIPE ||
                    errorCode == ERROR_NETNAME_DELETED) {
                    read_bytes = 0;
                    goto done_something;
                } else {
                    if (errorCode == ERROR_OPERATION_ABORTED)
                        errno = EINTR;      /* that's it. */
                    else
                        errno = EIO;        /* something unspecific */
                    return -1;
                }
            } else
                goto done_something;
        }
    }
  done_something:
    if (seekable) {
        file_position.QuadPart += read_bytes;
        SetFilePointerEx(handle,file_position,NULL,FILE_BEGIN);
    }
    return read_bytes;
}

/* We used to have a scratch() function listing all symbols needed by
 * Lisp.  Much rejoicing commenced upon its removal.  However, I would
 * like cold init to fail aggressively when encountering unused symbols.
 * That poses a problem, however, since our C code no longer includes
 * any references to symbols in ws2_32.dll, and hence the linker
 * completely ignores our request to reference it (--no-as-needed does
 * not work).  Warm init would later load the DLLs explicitly, but then
 * it's too late for an early sanity check.  In the unfortunate spirit
 * of scratch(), continue to reference some required DLLs explicitly by
 * means of one scratch symbol per DLL.
 */
void scratch(void)
{
    /* a function from ws2_32.dll */
    shutdown(0, 0);

    /* a function from shell32.dll */
    SHGetFolderPathA(0, 0, 0, 0, 0);

    /* from advapi32.dll */
    CryptGenRandom(0, 0, 0);
}

char *os_get_runtime_executable_path()
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


DWORD
win32_wait_object_or_signal(HANDLE waitFor)
{
    struct thread *self = get_sb_vm_thread();
    HANDLE handles[] = {waitFor, thread_private_events(self,1)};
    return
        WaitForMultipleObjects(2,handles, FALSE, INFINITE);
}

DWORD
win32_wait_for_multiple_objects_or_signal(HANDLE *handles, DWORD count)
{
    struct thread *self = get_sb_vm_thread();
    handles[count] = thread_private_events(self,1);
    return
        WaitForMultipleObjects(count + 1, handles, FALSE, INFINITE);
}

/*
 * Portability glue for win32 waitable timers.
 *
 * One may ask: Why is there a wrapper in C when the calls are so
 * obvious that Lisp could do them directly (as it did on Windows)?
 *
 * But the answer is that on POSIX platforms, we now emulate the win32
 * calls and hide that emulation behind this os_* abstraction.
 */
HANDLE
os_create_wtimer()
{
    return CreateWaitableTimer(0, 0, 0);
}

int
os_wait_for_wtimer(HANDLE handle)
{
    return win32_wait_object_or_signal(handle);
}

void
os_close_wtimer(HANDLE handle)
{
    CloseHandle(handle);
}

void
os_set_wtimer(HANDLE handle, int sec, int nsec)
{
    /* in units of 100ns, i.e. 0.1us. Negative for relative semantics. */
    long long dueTime
        = -(((long long) sec) * 10000000
            + ((long long) nsec + 99) / 100);
    SetWaitableTimer(handle, (LARGE_INTEGER*) &dueTime, 0, 0, 0, 0);
}

void
os_cancel_wtimer(HANDLE handle)
{
    CancelWaitableTimer(handle);
}

#ifdef LISP_FEATURE_SB_FUTEX
int
futex_wait(int *lock_word, int oldval, long sec, unsigned long usec)
{
  DWORD timeout = sec < 0 ? INFINITE : (sec * 1000) + (usec / 1000);
  struct thread* th = get_sb_vm_thread();
  thread_extra_data(th)->waiting_on_address = lock_word;
  int result = WaitOnAddress(lock_word, &oldval, 4, timeout);
  thread_extra_data(th)->waiting_on_address = 0;
  if (result)
      return 0;
  if (GetLastError() == ERROR_TIMEOUT)
      return 1;
  return 0;
}

int
futex_wake(PVOID lock_word, int n)
{
    if (n == 1)
        WakeByAddressSingle(lock_word);
    else
        WakeByAddressAll(lock_word);
    return 0;
}
#endif

int sb_pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset)
{
  struct extra_thread_data* self = thread_extra_data(get_sb_vm_thread());
  if (oldset)
    *oldset = self->blocked_signal_set;
  if (set) {
    switch (how) {
      case SIG_BLOCK:
        self->blocked_signal_set |= *set;
        break;
      case SIG_UNBLOCK:
        self->blocked_signal_set &= ~(*set);
        break;
      case SIG_SETMASK:
        self->blocked_signal_set = *set;
        break;
    }
  }
  return 0;
}

int sb_pthr_kill(struct thread* thread, int signum)
{
    __sync_fetch_and_or(&thread_extra_data(thread)->pending_signal_set, 1<<signum);
    return 0;
}

/* Signals */
int sched_yield()
{
  /* http://stackoverflow.com/questions/1383943/switchtothread-vs-sleep1
     SwitchToThread(); was here. Unsure what's better for us, just trying.. */

  if(!SwitchToThread())
      Sleep(0);
  return 0;
}

int sigemptyset(sigset_t *set)
{
  *set = 0;
  return 0;
}

int sigfillset(sigset_t *set)
{
  *set = 0xfffffffful;
  return 0;
}

int sigaddset(sigset_t *set, int signum)
{
  *set |= 1 << signum;
  return 0;
}

int sigdelset(sigset_t *set, int signum)
{
  *set &= ~(1 << signum);
  return 0;
}

int sigismember(const sigset_t *set, int signum)
{
  return (*set & (1 << signum)) != 0;
}

/* EOF */
