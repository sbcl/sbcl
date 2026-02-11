#ifndef WIN32_PTHREAD_INCLUDED
#define WIN32_PTHREAD_INCLUDED

#include <time.h>

#ifndef _SIGSET_T
typedef int sigset_t;
#endif


#define WIN32_LEAN_AND_MEAN
#include <windows.h>

/* 0 - Misc */

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

/* 1 - Thread */

#define SIG_BLOCK 1
#define SIG_UNBLOCK 2
#define SIG_SETMASK 3

#ifndef _TIMESPEC_DEFINED
typedef struct timespec {
  time_t tv_sec;
  long tv_nsec;
} timespec;
#endif

int sched_yield();

typedef struct {
  int bogus;
} siginfo_t;

int sigemptyset(sigset_t *set);
int sigfillset(sigset_t *set);
int sigaddset(sigset_t *set, int signum);
int sigdelset(sigset_t *set, int signum);
int sigismember(const sigset_t *set, int signum);

#endif  /* WIN32_PTHREAD_INCLUDED */
