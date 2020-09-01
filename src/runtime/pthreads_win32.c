#include "sbcl.h"
#ifdef LISP_FEATURE_SB_THREAD /* entire file */

#define PTHREAD_INTERNALS
#include "pthreads_win32.h"
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>

/* TLS management internals */

DWORD thread_self_tls_index;

void set_thread_self(pthread_t thread) {
  TlsSetValue(thread_self_tls_index, thread);
}

/* Thread identity, as much as pthreads are concerned, is determined
   by pthread_t structure that is stored in TLS slot
   (thread_self_tls_index). This slot is reassigned when fibers are
   switched with pthread_np API.

   Two reasons for not using fiber-local storage for this purpose: (1)
   Fls is too young: all other things work with Win2000, it requires
   WinXP; (2) this implementation works also with threads that aren't
   fibers, and it's a good thing.

   There is one more case, besides fiber switching, when pthread_self
   identity migrates between system threads: for non-main system
   thread that is not [pthread_create]d, thread-specific data
   destructors run in a thread from a system thread pool, after the
   original thread dies. In order to provide compatibility with
   classic pthread TSD, the system pool thread acquires dead thread's
   identity for the duration of destructor calls.
*/
pthread_t pthread_self()
{
  return (pthread_t)TlsGetValue(thread_self_tls_index);
}

/* Thread function for [pthread_create]d threads.
*/
/* Signals */
struct sigaction signal_handlers[NSIG];

/* Never called for now */
int sigaction(int signum, const struct sigaction* act, struct sigaction* oldact)
{
  struct sigaction newact = *act;
  if (oldact)
    *oldact = signal_handlers[signum];
  if (!(newact.sa_flags & SA_SIGINFO)) {
      newact.sa_sigaction = (typeof(newact.sa_sigaction))newact.sa_handler;
  }
  signal_handlers[signum] = newact;
  return 0;
}

int pthread_equal(pthread_t thread1, pthread_t thread2)
{
  return thread1 == thread2;
}

/* TODO call signal handlers */
int _sbcl_pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset)
{
  pthread_t self = pthread_self();
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

/* Add pending signal to (other) thread */
void pthread_np_add_pending_signal(pthread_t thread, int signum)
{
    /* See __sync_fetch_and_or() for gcc 4.4, at least.  As some
       people are still using gcc 3.x, I prefer to do this in asm.

       For win64 we'll HAVE to rewrite it. __sync_fetch_and_or() seems
       to be a rational choice -- there are plenty of GCCisms in SBCL
       anyway.
    */
    sigset_t to_add = 1<<signum;
    asm("lock orl %1,%0":"=m"(thread->pending_signal_set):"r"(to_add));
}

/* This pthread_kill doesn't do anything to notify target pthread of a
 * new pending signal.
 *
 * DFL: ... or so the original comment claimed, but that was before
 * futexes.  Now that we wake up futexes, it's not entirely accurate
 * anymore, is it? */
int pthread_kill(pthread_t thread, int signum)
{
  pthread_np_add_pending_signal(thread,signum);
  return 0;
}

void pthread_np_remove_pending_signal(pthread_t thread, int signum)
{
    sigset_t to_and = ~(1<<signum);
    asm("lock andl %1,%0":"=m"(thread->pending_signal_set):"r"(to_and));
}

sigset_t pthread_np_other_thread_sigpending(pthread_t thread)
{
    return
        InterlockedCompareExchange((volatile LONG*)&thread->pending_signal_set,
                                   0, 0);
}

int sched_yield()
{
  /* http://stackoverflow.com/questions/1383943/switchtothread-vs-sleep1
     SwitchToThread(); was here. Unsure what's better for us, just trying.. */

  if(!SwitchToThread())
      Sleep(0);
  return 0;
}

void pthreads_win32_init()
{
    thread_self_tls_index = TlsAlloc();
    pthread_np_notice_thread();
}

static
VOID CALLBACK pthreads_win32_unnotice(void* parameter, BOOLEAN timerOrWait)
{
  pthread_t pth = parameter;

  CloseHandle(pth->handle);

  UnregisterWait(pth->wait_handle);

  free(pth);
}

int pthread_np_notice_thread()
{
  if (!pthread_self()) {
    pthread_t pth = (pthread_t)calloc(sizeof(pthread_thread),1);
    pth->teb = NtCurrentTeb();

    sigemptyset(&pth->blocked_signal_set);

    DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                    GetCurrentProcess(), &pth->handle, 0, TRUE,
                    DUPLICATE_SAME_ACCESS);
    set_thread_self(pth);

    RegisterWaitForSingleObject(&pth->wait_handle,
                                  pth->handle,
                                  pthreads_win32_unnotice,
                                  pth,
                                  INFINITE,
                                  WT_EXECUTEONLYONCE);
    return 1;
  } else {
    return 0;
  }
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
int sigpending(sigset_t *set)
{
  *set = InterlockedCompareExchange((volatile LONG*)&pthread_self()->pending_signal_set,
                                    0, 0);
  return 0;
}

#endif
