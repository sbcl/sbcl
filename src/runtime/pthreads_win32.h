#ifndef WIN32_PTHREAD_INCLUDED
#define WIN32_PTHREAD_INCLUDED

#ifndef _SIGSET_T
typedef int sigset_t;
#endif


#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define SIG_BLOCK 1
#define SIG_UNBLOCK 2
#define SIG_SETMASK 3

int sched_yield();

typedef struct {
  int bogus;
} siginfo_t;

#endif  /* WIN32_PTHREAD_INCLUDED */
