#ifndef WIN32_PTHREAD_INCLUDED
#define WIN32_PTHREAD_INCLUDED

#include <time.h>
#include <errno.h>
#include <sys/types.h>

#ifndef _SIGSET_T
typedef int sigset_t;
#endif


#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdint.h>

/* 0 - Misc */

#ifndef SIG_IGN
#define SIG_IGN ((void (*)(int, siginfo_t, void*))-1)
#endif
#ifndef SIG_DFL
#define SIG_DFL ((void (*)(int, siginfo_t, void*))-2)
#endif

#define SIGHUP    1
#define SIGINT    2 /* Interactive attention */
#define SIGQUIT   3
#define SIGILL    4 /* Illegal instruction */
#define SIGPIPE   5
#define SIGALRM   6
#define SIGURG    7
#define SIGFPE    8 /* Floating point error */
#define SIGTSTP   9
#define SIGCHLD   10
#define SIGSEGV   11 /* Segmentation violation */
#define SIGIO     12
#define SIGXCPU   13
#define SIGXFSZ   14
#define SIGTERM   15 /* Termination request */
#define SIGVTALRM 16
#define SIGPROF   17
#define SIGWINCH  18
#define SIGBREAK  21 /* Control-break */
#define SIGABRT   22 /* Abnormal termination (abort) */

#define SIGRTMIN  23

#ifndef NSIG
#define NSIG 32     /* maximum signal number + 1 */
#endif

void pthreads_win32_init();

/* 1 - Thread */

typedef struct pthread_thread* pthread_t;

int sb_pthr_kill(pthread_t thread, int signum);

extern DWORD thread_self_tls_index;

#define SIG_BLOCK 1
#define SIG_UNBLOCK 2
#define SIG_SETMASK 3
#ifdef PTHREAD_INTERNALS
int _sbcl_pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset);
#endif

#ifndef _TIMESPEC_DEFINED
typedef struct timespec {
  time_t tv_sec;
  long tv_nsec;
} timespec;
#endif

/* some MinGWs seem to include it, others not: */
#ifndef ETIMEDOUT
# define ETIMEDOUT 123 //Something
#endif

int sched_yield();

typedef struct pthread_thread {
  HANDLE handle;
  struct thread *vm_thread;
  sigset_t blocked_signal_set;
  volatile sigset_t pending_signal_set;

  /* For noticed foreign threads, wait_handle contains a result of
     RegisterWaitForSingleObject. */
  HANDLE wait_handle;

  /* Thread TEB base (mostly informative/debugging) */
  void* teb;
} pthread_thread;

typedef struct {
  int bogus;
} siginfo_t;

#define SA_SIGINFO (1u<<1)
#define SA_NODEFER (1u<<2)
#define SA_RESTART (1u<<3)
#define SA_ONSTACK (1u<<4)

struct sigaction {
  void (*sa_handler)(int);
  void (*sa_sigaction)(int, siginfo_t*, void*);
  sigset_t sa_mask;
  int sa_flags;
};
int sigaction(int signum, const struct sigaction* act, struct sigaction* oldact);

int sigpending(sigset_t *set);

void pthread_np_add_pending_signal(pthread_t thread, int signum);
void pthread_np_remove_pending_signal(pthread_t thread, int signum);
sigset_t pthread_np_other_thread_sigpending(pthread_t thread);

int pthread_np_notice_thread();

int sigemptyset(sigset_t *set);
int sigfillset(sigset_t *set);
int sigaddset(sigset_t *set, int signum);
int sigdelset(sigset_t *set, int signum);
int sigismember(const sigset_t *set, int signum);

typedef int sig_atomic_t;

#ifndef PTHREAD_INTERNALS
#define thread_self() ((pthread_t)TlsGetValue(thread_self_tls_index))
static inline int _sbcl_pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset)
{
  pthread_t self = thread_self();
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

#endif  /* !PTHREAD_INTERNALS */
#endif  /* WIN32_PTHREAD_INCLUDED */
