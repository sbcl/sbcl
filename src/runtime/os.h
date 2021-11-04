/*
 * common interface for OS-dependent functions
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

#if !defined(_OS_H_INCLUDED_)

#define _OS_H_INCLUDED_

#include "sbcl.h"
#include "runtime.h"

#include <inttypes.h>

#if defined(LISP_FEATURE_GENCGC) && !defined(ENABLE_PAGE_PROTECTION)
/* Should we use page protection to help avoid the scavenging of pages
 * that don't have pointers to younger generations?
 * You can change this to 0 if you want SBCL not to install the handlers
 * for SIGSEGV and SIGBUS. That will slow down GC, but might be desirable
 * for debugging or for exploring GC strategies such as remembered sets */
#define ENABLE_PAGE_PROTECTION 1
#endif

#if defined LISP_FEATURE_CHENEYGC || defined LISP_FEATURE_SB_SAFEPOINT
// safepoint traps always require a signal handler
#define INSTALL_SIG_MEMORY_FAULT_HANDLER 1
#elif defined LISP_FEATURE_GENCGC
#define INSTALL_SIG_MEMORY_FAULT_HANDLER ENABLE_PAGE_PROTECTION
#endif

/* Some standard preprocessor definitions and typedefs are needed from
 * the OS-specific #include files. This is an attempt to document
 * them on 20000729, by WHN the impatient reverse engineer.
 *
 * OS_VM_PROT_READ, OS_VM_PROT_WRITE, OS_VM_PROT_EXECUTE
 *   flags for mmap, mprotect, etc. controlling memory protection
 * os_vm_prot_t
 *   type used for flags for mmap, mprotect, etc.
 *
 * os_vm_address_t
 *   the type used to represent addresses? (dunno why not just void*)
 * os_vm_size_t, os_vm_off_t
 *   corresponding to standard (POSIX?) types size_t, off_t
 * os_context_t
 *   the type used to represent context in a POSIX sigaction SA_SIGACTION
 *   handler, i.e. the actual type of the thing pointed to by the
 *   void* third argument of a handler */

#include "target-os.h"


#define OS_VM_PROT_ALL \
  (OS_VM_PROT_READ | OS_VM_PROT_WRITE | OS_VM_PROT_EXECUTE)

#define OS_VM_PROT_NONE 0

extern os_vm_size_t os_vm_page_size;

#if defined LISP_FEATURE_WIN32 || defined LISP_FEATURE_LINUX
boolean os_preinit(char *argv[], char *envp[]);
#else
#define os_preinit(dummy1,dummy2) (0)
#endif
void os_link_runtime();
void os_unlink_runtime();

/* Do anything we need to do when starting up the runtime environment
 * in this OS. */
extern void os_init();

/* Install any OS-dependent low-level signal handlers which are needed
 * by the runtime environment. E.g. the signals raised by a violation
 * of the gencgc write barrier need to be caught at a low level, and
 * they may be SIGSEGV on one OS and SIGBUS on another, so we install
 * them in an OS-dependent way. */
extern void os_install_interrupt_handlers(void);

/* Clear a possibly-huge region of memory using any tricks available to
 * do it efficiently, e.g. possibly unmapping it and then remapping it.
 *
 * FIXME: For the x86 Linux/OpenBSD/FreeBSD ports, I'd be somewhat
 * surprised if bzero() wasn't substantially as efficient as
 * any tricks like this. It might make sense to benchmark it
 * and simplify if the difference isn't too large. */
extern void os_zero(os_vm_address_t addr, os_vm_size_t length);

/* Allocate 'len' bytes at 'addr',
 * or at an OS-chosen address if 'addr' is zero.
 * If 'movable' then 'addr' is a preference, not a requirement.
 * These are discrete bits, not opaque enumerated values.
 * i.e. the consuming code might test via either (x & bit)
 * or (x == bit) depending on the use-case */
#define NOT_MOVABLE      0
#define MOVABLE          1
#define ALLOCATE_LOW     2
#define IS_THREAD_STRUCT 4
#define MOVABLE_LOW      (MOVABLE|ALLOCATE_LOW)
#define IS_GUARD_PAGE    8
extern os_vm_address_t os_validate(int movable,
                                   os_vm_address_t addr,
                                   os_vm_size_t len, int execute, int jit);

#ifdef LISP_FEATURE_WIN32
void* os_commit_memory(os_vm_address_t addr, os_vm_size_t len);
os_vm_address_t os_validate_nocommit(int attributes, os_vm_address_t addr, os_vm_size_t len);
#endif

/* This function seems to undo the effect of os_validate(..). */
extern void os_invalidate(os_vm_address_t addr, os_vm_size_t len);

/* This maps a file into memory, or calls lose(..) for various
 * failures. */
extern void* load_core_bytes(int fd,
                             os_vm_offset_t offset,
                             os_vm_address_t addr,
                             os_vm_size_t len,
                             int execute);
extern void* load_core_bytes_jit(int fd,
                             os_vm_offset_t offset,
                             os_vm_address_t addr,
                             os_vm_size_t len);

/* This presumably flushes the instruction cache, if that can be done
 * explicitly. (It doesn't seem to be an issue for the i386 port,
 * which is all that exists for SBCL. It might be important for some
 * other architecture which CMU CL has been ported to, though. */
extern void os_flush_icache(os_vm_address_t addr, os_vm_size_t len);

/* This sets access rights for an area of memory, e.g.
 * write-protecting a page so that the garbage collector can find out
 * whether it's modified by handling the signal. */
extern void os_protect(os_vm_address_t addr,
                       os_vm_size_t len,
                       os_vm_prot_t protection);

/* Return true for an address (with or without lowtag bits) within
 * any range of memory understood by the garbage collector. */
extern boolean gc_managed_addr_p(lispobj addr);
/* As for above, but consider only the heap spaces, not stacks */
extern boolean gc_managed_heap_space_p(lispobj addr);

/* Given a signal context, return the address for storage of the
 * register, of the specified offset, for that context. The offset is
 * defined in the storage class (SC) defined in the Lisp virtual
 * machine (i.e. the file "vm.lisp" for the appropriate architecture). */
os_context_register_t *
os_context_register_addr(os_context_t *context, int offset);

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset);

/* Given a signal context, return the address for storage of the
 * program counter for that context. */
os_context_register_t *os_context_pc_addr(os_context_t *context);
#ifdef ARCH_HAS_NPC_REGISTER
os_context_register_t *os_context_npc_addr(os_context_t *context);
#endif
#ifdef ARCH_HAS_LINK_REGISTER
os_context_register_t *os_context_lr_addr(os_context_t *context);
#endif

/* Given a signal context, return the address for storage of the
 * system stack pointer for that context. */
#ifdef ARCH_HAS_STACK_POINTER
os_context_register_t *os_context_sp_addr(os_context_t *context);
// os_context_fp_addr might not be defined
os_context_register_t *os_context_fp_addr(os_context_t *context);
#if defined LISP_FEATURE_X86_64
#  define os_context_frame_pointer(context) *os_context_register_addr(context,reg_RBP)
#elif defined LISP_FEATURE_X86
#  define os_context_frame_pointer(context) *os_context_register_addr(context,reg_EBP)
#else
#  define os_context_frame_pointer(context) 0
#endif
#endif
/* Given a signal context, return the address for storage of the
 * signal mask for that context. */
sigset_t *os_context_sigmask_addr(os_context_t *context);

/* (Note that there may be other accessors for os_context_t which
 * depend not only on the OS, but also on the architecture, e.g.
 * getting at EFL/EFLAGS on the x86. Such things are defined in the
 * architecture-dependence files, not the OS-dependence files.) */

/* These are not architecture-specific functions, but are instead
 * general utilities defined in terms of the architecture-specific
 * function os_validate(..) and os_invalidate(..).
 */
extern os_vm_address_t os_allocate(os_vm_size_t len);
extern void os_deallocate(os_vm_address_t addr, os_vm_size_t len);

#define os_trunc_to_page(addr) PTR_ALIGN_DOWN(addr, os_vm_page_size)
#define os_round_up_to_page(addr) PTR_ALIGN_UP(addr, os_vm_page_size)

#define os_trunc_size_to_page(size) \
    (os_vm_size_t)ALIGN_DOWN((uword_t)size, os_vm_page_size)
#define os_round_up_size_to_page(size) \
    (os_vm_size_t)ALIGN_UP((uword_t)size, os_vm_page_size)

/* KLUDGE: The errno error reporting system is an ugly nonreentrant
 * botch which nonetheless wasn't too painful in the old days.
 * However, it's obviously not good for multithreaded programs, and n
 * order to accommodate multithreading while retaining the C-level
 * syntax of the old UNIX interface, errno has now been changed from a
 * true variable to a preprocessor definition which is too hairy for
 * us to try to unscrew in Lisp code. Instead, Lisp code calls this
 * service routine to do whatever hackery is necessary in C code, and
 * to return the value in a way that Lisp can understand. */
int os_get_errno(void);

/* Return an absolute path to the runtime executable, or NULL if this
 * information is unavailable. If a non-null pathname is
 * returned, it must be 'free'd. */
extern char *os_get_runtime_executable_path();

/* Write platforms specific ones when necessary. This is to get us off
 * the ground. */
#define OS_VM_SIZE_FMT PRIuPTR
#define OS_VM_SIZE_FMTX PRIxPTR

#if defined LISP_FEATURE_SB_THREAD && defined LISP_FEATURE_UNIX
#  ifndef USE_DARWIN_GCD_SEMAPHORES
#    include <semaphore.h>
     typedef sem_t os_sem_t;
#  endif
   void os_sem_init(os_sem_t *sem, unsigned int value);
   void os_sem_wait(os_sem_t *sem, char *what);
   void os_sem_post(os_sem_t *sem, char *what);
   void os_sem_destroy(os_sem_t *sem);
#endif

extern int os_reported_page_size;

#endif
