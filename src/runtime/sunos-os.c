#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/file.h>
#include <limits.h>

#include <unistd.h>
#include <errno.h>
#include <sys/param.h>

#include "sbcl.h"
#include "os.h"
#include "arch.h"
#include "interr.h"
#include "interrupt.h"
#include "globals.h"
#include "validate.h"
#include "target-arch-os.h"

#ifdef LISP_FEATURE_X86
#include "genesis/static-symbols.h"
#include "genesis/fdefn.h"
#endif

#ifdef LISP_FEATURE_GENCGC
#include "gc-internal.h"
#endif

#ifdef LISP_FEATURE_SB_WTIMER
# include <port.h>
# include <time.h>
# include <errno.h>
#endif

void
os_init(char *argv[], char *envp[])
{
}

os_vm_address_t os_validate(int attributes, os_vm_address_t addr, os_vm_size_t len,
            int __attribute__((unused)) execute, int __attribute__((unused)) jit)
{
    int protection = attributes & IS_GUARD_PAGE ? OS_VM_PROT_NONE : OS_VM_PROT_ALL;
    attributes &= ~IS_GUARD_PAGE;
    int flags = MAP_PRIVATE | MAP_NORESERVE | MAP_ANON;
    if (addr)
        flags |= MAP_FIXED;

    addr = mmap(addr, len, protection, flags, -1, 0);

    if (addr == MAP_FAILED) {
        perror("mmap");
        /* While it is generally hard to recover from out-of-memory
         * situations, we require callers to decide on the right course
         * of action here.  Consider thread creation: Failure to mmap
         * here is common if users have started too many threads, and
         * often we can recover from that and treat it as an ordinary
         * error. */
        return 0;
    }

    return addr;
}

void os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if(munmap((void*) addr, len) == -1)
        perror("munmap");
}


void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if(mprotect((void*)address, length, prot) == -1) {
        perror("mprotect");
    }
}

#if defined LISP_FEATURE_GENCGC

void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    void* fault_addr = (void*)info->si_addr;

#ifdef LISP_FEATURE_SB_SAFEPOINT
    if (handle_safepoint_violation(context, fault_addr))
            return;
#endif

    if (!gencgc_handle_wp_violation(fault_addr))
        if(!handle_guard_page_triggered(context, fault_addr))
            lisp_memory_fault_error(context, fault_addr);
}

#else

static void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    os_vm_address_t addr = arch_get_bad_addr(signal, info, context);

    if (!cheneygc_handle_wp_violation(context, addr)) {
        if (!handle_guard_page_triggered(context,addr))
            lisp_memory_fault_error(context, addr);
    }
}

#endif

void
os_install_interrupt_handlers()
{
    ll_install_handler(SIG_MEMORY_FAULT, sigsegv_handler);

    /* OAOOM c.f. linux-os.c.
     * Should we have a reusable function gc_install_interrupt_handlers? */
#ifdef LISP_FEATURE_SB_THREAD
# ifdef LISP_FEATURE_SB_SAFEPOINT
#  ifdef LISP_FEATURE_SB_THRUPTION
    ll_install_handler(SIGURG, thruption_handler);
#  endif
# else
    ll_install_handler(SIG_STOP_FOR_GC, sig_stop_for_gc_handler);
# endif
#endif
}

char *os_get_runtime_executable_path()
{
    char path[PATH_MAX + 1];
    int size = readlink("/proc/self/path/a.out", path, sizeof(path) - 1);
    if (size < 0)
        return NULL;
    path[size] = '\0';

    if (strcmp(path, "unknown") == 0)
        return NULL;
    return copied_string(path);
}

#ifdef LISP_FEATURE_SB_WTIMER
/*
 * Waitable timer implementation for the safepoint-based (SIGALRM-free)
 * timer facility using SunOS completion ports.
 */

struct os_wtimer {
    int port;
    int timer;
};

struct os_wtimer *
os_create_wtimer()
{
    int port = port_create();
    if (port == -1) {
        perror("port_create");
        lose("os_create_wtimer");
    }

    port_notify_t pn;
    pn.portnfy_port = port;
    pn.portnfy_user = 0;

    struct sigevent ev;
    memset(&ev, 0, sizeof(ev));
    ev.sigev_notify = SIGEV_PORT;
    ev.sigev_value.sival_ptr = &pn;

    timer_t timer;
    if (timer_create(CLOCK_HIGHRES, &ev, &timer) == -1
        && (errno != EPERM || timer_create(CLOCK_REALTIME, &ev, &timer) == -1))
    {
        perror("timer_create");
        lose("os_create_wtimer");
    }

    struct os_wtimer *wt = malloc(sizeof(struct os_wtimer));
    if (!wt)
        lose("os_create_wtimer: malloc");

    wt->port = port;
    wt->timer = timer;
    return wt;
}

int
os_wait_for_wtimer(struct os_wtimer *wt)
{
    port_event_t pe;
    if (port_get(wt->port, &pe, 0) == -1) {
        if (errno == EINTR)
            return 1;
        perror("port_get");
        lose("os_wtimer_listen failed");
    }
    return 0;
}

void
os_close_wtimer(struct os_wtimer *wt)
{
    if (close(wt->port) == -1) {
        perror("close");
        lose("os_close_wtimer");
    }
    if (timer_delete(wt->timer) == -1) {
        perror("timer_delete");
        lose("os_close_wtimer");
    }
    free(wt);
}

void
os_set_wtimer(struct os_wtimer *wt, int sec, int nsec)
{
    struct itimerspec spec;
    spec.it_value.tv_sec = sec;
    spec.it_value.tv_nsec = nsec;
    spec.it_interval.tv_sec = 0;
    spec.it_interval.tv_nsec = 0;
    if (timer_settime(wt->timer, 0, &spec, 0) == -1) {
        int x = errno;
        perror("timer_settime");
        if (x != EINVAL)
            lose("os_set_wtimer");
    }
}

void
os_cancel_wtimer(struct os_wtimer *wt)
{
    os_set_wtimer(wt, 0, 0);
}
#endif
