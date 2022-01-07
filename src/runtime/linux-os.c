/*
 * the Linux incarnation of OS-dependent routines.  See also
 * $(sbcl_arch)-linux-os.c
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

#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include "sbcl.h"
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "runtime.h"
#include "genesis/static-symbols.h"
#include "genesis/fdefn.h"

#include <errno.h>

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <linux/version.h>

#include "validate.h"
#include "thread.h"
#include "gc-internal.h"
#include <fcntl.h>

#ifdef LISP_FEATURE_X86
/* Prototype for personality(2). Done inline here since the header file
 * for this isn't available on old versions of glibc. */
int personality (unsigned long);
#define ADDR_NO_RANDOMIZE 0x0040000
#else
#include <sys/personality.h>
#endif

#ifdef LISP_FEATURE_SB_FUTEX
#include <sys/syscall.h>
#include <unistd.h>
#include <errno.h>

#ifdef MUTEX_EVENTRECORDING
#include "genesis/mutex.h"
#define MAXEVENTS 200
static struct {
    struct thread* th;
    struct timespec ts;
    char *label;
    char *mutex_name;
    sword_t timeout;
} events[MAXEVENTS];
static int record_mutex_events;
static int eventcount;

void lisp_mutex_event(char *string) {
    if (record_mutex_events) {
        int id = __sync_fetch_and_add(&eventcount, 1);
        if (id >= MAXEVENTS) lose("event buffer overflow");
        clock_gettime(CLOCK_REALTIME, &events[id].ts);
        events[id].th = get_sb_vm_thread();
        events[id].label = string;
        events[id].mutex_name = 0;
        events[id].timeout = -1;
    }
}
void lisp_mutex_event1(char *string, char *string2) {
    if (record_mutex_events) {
        int id = __sync_fetch_and_add(&eventcount, 1);
        if (id >= MAXEVENTS) lose("event buffer overflow");
        clock_gettime(CLOCK_REALTIME, &events[id].ts);
        events[id].th = get_sb_vm_thread();
        events[id].label = string;
        events[id].mutex_name = string2;
        events[id].timeout = -1;
    }
}
void lisp_mutex_event2(char *string, char *string2, uword_t usec) {
    if (record_mutex_events) {
        int id = __sync_fetch_and_add(&eventcount, 1);
        if (id >= MAXEVENTS) lose("event buffer overflow");
        clock_gettime(CLOCK_REALTIME, &events[id].ts);
        events[id].th = get_sb_vm_thread();
        events[id].label = string;
        events[id].mutex_name = string2;
        events[id].timeout = usec;
    }
}
void lisp_mutex_start_eventrecording() {
    eventcount = 0;
    record_mutex_events = 1;
}
void lisp_mutex_done_eventrecording() {
    record_mutex_events = 0;
    int i;
    fprintf(stderr, "event log:\n");
    struct timespec basetime = events[0].ts;
    for(i=0; i<eventcount;++i) {
        struct thread *th = events[i].th;
        struct thread_instance *ti = (void*)native_pointer(th->lisp_thread);
        struct timespec rel_time = events[i].ts;
        rel_time.tv_sec -= basetime.tv_sec;
        rel_time.tv_nsec -= basetime.tv_nsec;
        if (rel_time.tv_nsec<0) rel_time.tv_nsec += 1000 * 1000 * 1000, rel_time.tv_sec--;
        lispobj threadname = ti->name;
        if (events[i].timeout >= 0) // must also have mutex_name in this case
            fprintf(stderr, "[%d.%09ld] %s: %s '%s' timeout %ld\n",
                    (int)rel_time.tv_sec, rel_time.tv_nsec,
                    (char*)VECTOR(threadname)->data,
                    events[i].label, events[i].mutex_name, events[i].timeout);
        else if (events[i].mutex_name)
            fprintf(stderr, "[%d.%09ld] %s: %s '%s'\n",
                    (int)rel_time.tv_sec, rel_time.tv_nsec,
                    (char*)VECTOR(threadname)->data,
                    events[i].label, events[i].mutex_name);
        else
            fprintf(stderr, "[%d.%09ld] %s: %s\n",
                    (int)rel_time.tv_sec, rel_time.tv_nsec,
                    (char*)VECTOR(threadname)->data,
                    events[i].label);
    }
    fprintf(stderr, "-----\n");
}
#else
#define lisp_mutex_event(x)
#define lisp_mutex_event1(x,y)
#define lisp_mutex_event2(x,y,z)
#endif

/* values taken from the kernel's linux/futex.h.  This header file
   doesn't exist in userspace, which is our excuse for not grovelling
   them automatically */
#define FUTEX_WAIT 0
#define FUTEX_WAKE 1
/* This is also copied from linux/futex.h so that a binary compiled on
 * a not so recent Linux system can still take advantage of private
 * futexes when available.*/
#define FUTEX_WAIT_PRIVATE (0+128)
#define FUTEX_WAKE_PRIVATE (1+128)

/* Not static so that Lisp may query it. */
boolean futex_private_supported_p;

static inline int
futex_wait_op()
{
    return (futex_private_supported_p ? FUTEX_WAIT_PRIVATE : FUTEX_WAIT);
}

static inline int
futex_wake_op()
{
    return (futex_private_supported_p ? FUTEX_WAKE_PRIVATE : FUTEX_WAKE);
}

static inline int sys_futex(void *futex, int op, int val, struct timespec *rel)
{
    return syscall(SYS_futex, futex, op, val, rel);
}

static void
futex_init()
{
    int x = 0;
    sys_futex(&x, FUTEX_WAIT, 1, 0);
    if (errno == ENOSYS)
        lose("This version of SBCL is compiled with threading support, but your kernel\n"
             "is too old to support this. Please use a more recent kernel or\n"
             "a version of SBCL without threading support.");
    sys_futex(&x, FUTEX_WAIT_PRIVATE, 1, 0);
    if (errno == EWOULDBLOCK) {
        futex_private_supported_p = 1;
    } else {
        futex_private_supported_p = 0;
        SHOW("No futex private suppport\n");
    }
}

/* Try to guess the name of the mutex for this futex, based on knowing
 * the two pertinent Lisp object types (WAITQUEUE and MUTEX) that use a futex.
 * Callable from a C debugger */
#include "genesis/vector.h"
char* futex_name(int *lock_word)
{
    // If there is a Lisp string at lock_word+1, return that, otherwise NULL.
    lispobj name = ((lispobj*)lock_word)[1];
    if (lowtag_of(name) == OTHER_POINTER_LOWTAG &&
        header_widetag(VECTOR(name)->header) == SIMPLE_BASE_STRING_WIDETAG)
        return (char*)VECTOR(name)->data;
    return 0;
}

int
futex_wait(int *lock_word, int oldval, long sec, unsigned long usec)
{
  struct timespec timeout;
  int t;

#ifdef MUTEX_EVENTRECORDING
    struct mutex* m = (void*)((char*)lock_word - offsetof(struct mutex,state));
    char *name = m->name != NIL ? (char*)VECTOR(m->name)->data : "(unnamed)";
#endif
  if (sec<0) {
      lisp_mutex_event1("start futex wait", name);
      t = sys_futex(lock_word, futex_wait_op(), oldval, 0);
  }
  else {
      timeout.tv_sec = sec;
      timeout.tv_nsec = usec * 1000;
      lisp_mutex_event2("start futex timedwait", name, usec);
      t = sys_futex(lock_word, futex_wait_op(), oldval, &timeout);
  }
  lisp_mutex_event1("back from sys_futex", name);
  if (t==0)
      return 0;
  else if (errno==ETIMEDOUT)
      return 1;
  else if (errno==EINTR)
      return 2;
  else
      /* EWOULDBLOCK and others, need to check the lock */
      return -1;
}

int
futex_wake(int *lock_word, int n)
{
#ifdef MUTEX_EVENTRECORDING
    struct mutex* m = (void*)((char*)lock_word - offsetof(struct mutex,state));
    char *name = m->name != NIL ? (char*)VECTOR(m->name)->data : "(unnamed)";
    lisp_mutex_event1("waking futex", name);
#endif
    return sys_futex(lock_word, futex_wake_op(),n,0);
}
#endif


void os_init()
{
#ifdef LISP_FEATURE_SB_FUTEX
    futex_init();
#endif
}

#if (defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)) \
     && (!defined(DISABLE_ASLR) || DISABLE_ASLR)
# define ALLOW_PERSONALITY_CHANGE 1
#else
# define ALLOW_PERSONALITY_CHANGE 0
#endif

int os_preinit(char *argv[], char *envp[])
{

#if ALLOW_PERSONALITY_CHANGE
    if (getenv("SBCL_IS_RESTARTING")) {
        /* We restarted due to previously enabled ASLR.  Now,
         * reenable it for fork()'ed children. */
        int pers = personality(0xffffffffUL);
        personality(pers & ~ADDR_NO_RANDOMIZE);
        unsetenv("SBCL_IS_RESTARTING");
        return 0; // ensure_spaces() will win or not. Not much to do here.
    }
#endif
    /* See if we can allocate read-only, static, and linkage table spaces
     * at their required addresses. If we can, then there's no need to try
     * the re-exec trick since dynamic space is relocatable. */
    if (allocate_hardwired_spaces(0)) // soft failure mode
        return 1; // indicate that we already allocated hardwired spaces

#if ALLOW_PERSONALITY_CHANGE
    /* KLUDGE: Disable memory randomization by setting a personality
     * flag and re-executing. (We need to re-execute, since the memory
     * maps that can conflict with the SBCL spaces have already been
     * done at this point).
     */
    int pers = personality(0xffffffffUL);
    if (!(pers & ADDR_NO_RANDOMIZE)) {
        int retval = personality(pers | ADDR_NO_RANDOMIZE);
        /* Allegedly some Linux kernels (the reported case was
         * "hardened Linux 2.6.7") won't set the new personality,
         * but nor will they return -1 for an error. So as a
         * workaround query the new personality...
         */
        int newpers = personality(0xffffffffUL);
        /* ... and don't re-execute if either the setting resulted
         * in an error or if the value didn't change. Otherwise
         * this might result in an infinite loop.
         */

        if (!getenv("SBCL_IS_RESTARTING") &&
            retval != -1 && newpers != pers) {
            /* Use /proc/self/exe instead of trying to figure out
             * the executable path from PATH and argv[0], since
             * that's unreliable. We follow the symlink instead of
             * executing the file directly in order to prevent top
             * from displaying the name of the process as "exe". */
            char runtime[PATH_MAX+1];
            int i = readlink("/proc/self/exe", runtime, PATH_MAX);
            if (i != -1) {
                environ = envp;
                setenv("SBCL_IS_RESTARTING", "T", 1);
                runtime[i] = '\0';
                execv(runtime, argv);
            }
        }
        /* Either changing the personality or execve() failed.
         * Just get on with life and hope for the best. */
    }
#endif
    return 0;
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    // dynamic space should not have protections manipulated
    if (find_page_index(address) >= 0)
        lose("unexpected call to os_protect with software card marks");
#endif
    if (mprotect(address, length, prot)) {
        if (errno == ENOMEM) {
            lose("An mprotect call failed with ENOMEM. This probably means that the maximum amount\n"
                 "of separate memory mappings was exceeded. To fix the problem, either increase\n"
                 "the maximum with e.g. 'echo 262144 > /proc/sys/vm/max_map_count' or recompile\n"
                 "SBCL with a larger value for GENCGC-PAGE-BYTES in\n"
                 "'src/compiler/"SBCL_TARGET_ARCHITECTURE_STRING"/parms.lisp'.");
        } else {
            perror("mprotect");
        }
    }
}

/*
 * any OS-dependent special low-level handling for signals
 */

/*
 * The GC needs to be hooked into whatever signal is raised for
 * page fault on this OS.
 */
static void
fallback_sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    // This calls corruption_warning_and_maybe_lose.
    lisp_memory_fault_error(context, arch_get_bad_addr(signal, info, context));
}

void (*sbcl_fallback_sigsegv_handler)  // Settable by user.
       (int, siginfo_t*, os_context_t*) = fallback_sigsegv_handler;

static void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    os_vm_address_t addr = arch_get_bad_addr(signal, info, context);

#ifdef LISP_FEATURE_SB_SAFEPOINT
    if (handle_safepoint_violation(context, addr)) return;
#endif

#ifdef LISP_FEATURE_GENCGC
    if (gencgc_handle_wp_violation(addr)) return;
#else
    if (cheneygc_handle_wp_violation(context, addr)) return;
#endif
    if (!handle_guard_page_triggered(context, addr))
            sbcl_fallback_sigsegv_handler(signal, info, context);
}

void
os_install_interrupt_handlers(void)
{
    if (INSTALL_SIG_MEMORY_FAULT_HANDLER) {
    ll_install_handler(SIG_MEMORY_FAULT, sigsegv_handler);
    }
}

char *os_get_runtime_executable_path()
{
    char path[PATH_MAX + 1];
    int size;

    size = readlink("/proc/self/exe", path, sizeof(path)-1);
    if (size < 0)
        return NULL;
    else
        path[size] = '\0';

    return copied_string(path);
}
