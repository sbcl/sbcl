#ifndef WIN32_PTHREAD_INCLUDED
#define WIN32_PTHREAD_INCLUDED

#ifndef _SIGSET_T
typedef int sigset_t;
#endif


#define WIN32_LEAN_AND_MEAN
#include <windows.h>

/* 0 - Misc */

#define SIGHUP    1
#define SIGQUIT   3
#define SIGPIPE   5
#define SIGALRM   6
#define SIGURG    7
#define SIGTSTP   9
#define SIGCHLD   10
#define SIGIO     12
#define SIGXCPU   13
#define SIGXFSZ   14
#define SIGVTALRM 16
#define SIGPROF   17
#define SIGWINCH  18

/* 1 - Thread */

#define SIG_BLOCK 1
#define SIG_UNBLOCK 2
#define SIG_SETMASK 3

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
