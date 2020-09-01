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

/* To avoid overusing system TLS, pthread provides its own */
#define PTHREAD_KEYS_MAX 128

#define PTHREAD_DESTRUCTOR_ITERATIONS 4

#define PTHREAD_STACK_MIN 0

void pthreads_win32_init();

/* 1 - Thread */

typedef struct pthread_thread* pthread_t;

typedef int pthread_attr_t; /* arbitrary */

typedef void (*pthread_cleanup_fn)(void* arg);

int pthread_equal(pthread_t thread1, pthread_t thread2);
int pthread_kill(pthread_t thread, int signum);

#ifndef PTHREAD_INTERNALS
pthread_t pthread_self(void) __attribute__((__const__));
#else
pthread_t pthread_self(void);
#endif

extern DWORD thread_self_tls_index;

#define SIG_BLOCK 1
#define SIG_UNBLOCK 2
#define SIG_SETMASK 3
#ifdef PTHREAD_INTERNALS
int _sbcl_pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset);
#endif

/* 2 - Mutex */

typedef struct _pthread_mutex_info {
  char padding[64];
  CRITICAL_SECTION cs;
  pthread_t owner;
  const char* file;
  int line;
} __attribute__((aligned(128))) *pthread_mutex_t;

typedef int pthread_mutexattr_t;
#define PTHREAD_MUTEX_INITIALIZER ((pthread_mutex_t)-1)
int pthread_mutex_init(pthread_mutex_t * mutex, const pthread_mutexattr_t * attr);
int pthread_mutexattr_init(pthread_mutexattr_t*);
int pthread_mutexattr_destroy(pthread_mutexattr_t*);
int pthread_mutexattr_settype(pthread_mutexattr_t*, int);
#define PTHREAD_MUTEX_ERRORCHECK 0
int pthread_mutex_destroy(pthread_mutex_t *mutex);
int pthread_mutex_lock(pthread_mutex_t *mutex);
int pthread_mutex_trylock(pthread_mutex_t *mutex);
int pthread_mutex_lock_annotate_np(pthread_mutex_t *mutex, const char* file, int line);
int pthread_mutex_trylock_annotate_np(pthread_mutex_t *mutex, const char* file, int line);
int pthread_mutex_unlock(pthread_mutex_t *mutex);

/* 3 - Condition variable */

typedef struct thread_wakeup {
  HANDLE event;
  struct thread_wakeup *next;
  volatile intptr_t *uaddr;
  intptr_t uval;
  int info;
} thread_wakeup;

typedef HANDLE (*cv_event_get_fn)();
typedef void (*cv_event_return_fn)(HANDLE event);

typedef struct pthread_cond_t {
  CRITICAL_SECTION wakeup_lock;
  struct thread_wakeup *first_wakeup;
  struct thread_wakeup *last_wakeup;
  unsigned char alertable;
  cv_event_get_fn get_fn;
  cv_event_return_fn return_fn;
} pthread_cond_t;

typedef struct pthread_condattr_t {
  unsigned char alertable;
  cv_event_get_fn get_fn;
  cv_event_return_fn return_fn;
} pthread_condattr_t;

#ifndef _TIMESPEC_DEFINED
typedef struct timespec {
  time_t tv_sec;
  long tv_nsec;
} timespec;
#endif

// not implemented: PTHREAD_COND_INITIALIZER
int pthread_condattr_init(pthread_condattr_t *attr);
int pthread_condattr_destroy(pthread_condattr_t *attr);
int pthread_condattr_setevent_np(pthread_condattr_t *attr,
                                 cv_event_get_fn get_fn, cv_event_return_fn ret_fn);
int pthread_cond_destroy(pthread_cond_t *cond);
int pthread_cond_init(pthread_cond_t * cond, const pthread_condattr_t * attr);
int pthread_cond_broadcast(pthread_cond_t *cond);
int pthread_cond_signal(pthread_cond_t *cond);
int pthread_cond_timedwait(pthread_cond_t * cond, pthread_mutex_t * mutex, const struct timespec * abstime);
int pthread_cond_wait(pthread_cond_t * cond, pthread_mutex_t * mutex);

/* some MinGWs seem to include it, others not: */
#ifndef ETIMEDOUT
# define ETIMEDOUT 123 //Something
#endif

int sched_yield();

void pthread_lock_structures();
void pthread_unlock_structures();

typedef void *(*pthread_fn)(void*);

typedef struct pthread_thread {
  HANDLE handle;
  struct thread *vm_thread;
  pthread_cond_t *waiting_cond;
  void *futex_wakeup;
  sigset_t blocked_signal_set;
  volatile sigset_t pending_signal_set;

  /* For noticed foreign threads, wait_handle contains a result of
     RegisterWaitForSingleObject. */
  HANDLE wait_handle;

  /* Thread TEB base (mostly informative/debugging) */
  void* teb;

  HANDLE cv_event;
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

/* Debugging */
void pthread_np_lose(int trace_depth, const char* fmt, ...);
extern struct _pthread_mutex_info DEAD_MUTEX;

static inline void pthread_np_assert_live_mutex(pthread_mutex_t* ptr,
                                                const char *action)
{
    if (*ptr == &DEAD_MUTEX) {
        pthread_np_lose(5,"Trying to %s dead mutex %p\n",action,ptr);
    }
}

typedef HANDLE sem_t;

#define SEM_VALUE_MAX (int) (~0U >>1)

int sem_init(sem_t *sem, int pshared_not_implemented, unsigned int value);
int sem_post(sem_t *sem);
int sem_wait(sem_t *sem);
int sem_trywait(sem_t *sem);
int sem_destroy(sem_t *sem);

#ifndef PTHREAD_INTERNALS
static inline int _sbcl_pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset)
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

#ifdef PTHREAD_DEBUG_OUTPUT
#define pthread_mutex_lock(mutex)               \
  pthread_mutex_lock_annotate_np(mutex, __FILE__, __LINE__ )
#define pthread_mutex_trylock(mutex)            \
  pthread_mutex_trylock_annotate_np(mutex, __FILE__ ,__LINE__)
#else

/* I'm not after inlinining _everything_, but those two things below are
   (1) fast, (2) critical (3) short */
static inline int pthread_mutex_lock_np_inline(pthread_mutex_t *mutex)
{
    pthread_np_assert_live_mutex(mutex,"lock");
    if ((*mutex) == PTHREAD_MUTEX_INITIALIZER) {
        return pthread_mutex_lock(mutex);
    } else {
        EnterCriticalSection(&(*mutex)->cs);
        return 0;
    }
}

static inline int pthread_mutex_unlock_np_inline(pthread_mutex_t *mutex)
{
    pthread_np_assert_live_mutex(mutex,"unlock");
    LeaveCriticalSection(&(*mutex)->cs);
    return 0;
}

#define pthread_mutex_lock pthread_mutex_lock_np_inline
#define pthread_mutex_unlock pthread_mutex_unlock_np_inline

#endif  /* !PTHREAD_DEBUG_OUTPUT */
#endif  /* !PTHREAD_INTERNALS */
#endif  /* WIN32_PTHREAD_INCLUDED */
