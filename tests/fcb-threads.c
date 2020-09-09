/* In as much as this file simulates "foreign" code,
 * we don't include "sbcl.h" and we use the native thread API
 * for the platform */

#ifdef _WIN32
# include <handleapi.h>
# include <process.h>
# include <processthreadsapi.h>
# include <synchapi.h> // for WaitForSingleObject
#else
# include <pthread.h>
#endif
#include <stdio.h>
#include <stdint.h>
#include <string.h>

struct thread_arg { void * funkyfun; int index; int n_calls; };

char *salutations[8] = {
    "Hello", "Hi there!", "Hey", "Hi!", "Ahoy there", "What's up!", "Hola",
    "That's a winner"
};
int sharedvar;

#ifdef _WIN32
__stdcall unsigned int perftest_thread(LPVOID void_arg)
#else
void* perftest_thread(void* void_arg)
#endif
{
    struct thread_arg* arg = void_arg;
    int (*lispfun)() = arg->funkyfun;
    int ncalls = arg->n_calls;
    int i;
    for (i=0; i<ncalls; ++i) lispfun();
    return 0;
}

int minimal_perftest(void* ptr, int n_calls)
{
  struct thread_arg arg;
  arg.funkyfun = ptr;
  arg.n_calls = n_calls;
#ifdef _WIN32
  HANDLE thr;
  thr = (HANDLE)_beginthreadex(NULL, 0, perftest_thread, &arg, 0, NULL);
  WaitForSingleObject(thr,0xffffffff);
  CloseHandle(thr);
#else
  pthread_t thr;
  pthread_create(&thr, 0, perftest_thread, &arg);
  pthread_join(thr,0);
#endif
  return 0;
}

#ifdef _WIN32
__stdcall unsigned int doThatThing(void* void_arg)
#else
void* doThatThing(void* void_arg)
#endif
{
    struct thread_arg* arg = void_arg;
    int thread_result = 0xC0FEFE;
    int (*lispfun)(char*,double) = arg->funkyfun;
    int i;
    // fprintf(stderr, "enter doThatThing %p\n", void_arg); fflush(stderr);
    for(i=0; i<arg->n_calls; ++i) {
        int index = __sync_fetch_and_add(&sharedvar,1);
        char *salutation = salutations[index % 8];
        int answer = lispfun(salutation, arg->index + i);
        if (answer != (arg->index + i) * strlen(salutation)) thread_result = 0;
    }
    return (void*)(uintptr_t)thread_result;
}

int call_thing_from_threads(void* ptr, int n_threads, int n_calls)
{
    struct {
#ifdef _WIN32
        HANDLE handle;
        DWORD result;
#else
        pthread_t pthread_id;
        void* result;
#endif
        struct thread_arg arg;
    } threads[50];
    if (n_threads>50) {
        fprintf(stderr, "pick a smaller number\n");
        return -1;
    }
    int i;
    for(i=0; i<n_threads; ++i) {
        threads[i].arg.funkyfun = ptr;
        threads[i].arg.index = i + 1;
        threads[i].arg.n_calls = n_calls;
#ifdef _WIN32
        threads[i].handle = (HANDLE)_beginthreadex(NULL, 0, doThatThing, &threads[i].arg, 0, NULL);
#else
        pthread_create(&threads[i].pthread_id, 0, doThatThing, &threads[i].arg);
#endif
    }
    int all_ok = 1;
    for(i=0; i<n_threads; ++i) {
#ifdef _WIN32
        DWORD result;
        // I don't know which header file defines INFINITE (for the timeout)
        // so any large value should do.
        result = WaitForSingleObject(threads[i].handle, 0xffffffff);
        GetExitCodeThread(threads[i].handle, &threads[i].result);
        CloseHandle(threads[i].handle);
        if (threads[i].result != 0xC0FEFE) all_ok = 0;
#else
        pthread_join(threads[i].pthread_id, &threads[i].result);
#if 0
        fprintf(stderr, "%d: pthread %lx returned %p\n",
                i, (long)threads[i].pthread_id, threads[i].result);
#endif
        if ((uintptr_t)threads[i].result != 0xC0FEFE) all_ok = 0;
#endif
    }
    return all_ok;
}

/// The code following is for the no-lockup-on-exit test, unrelated to the above
/// FIXME: implement a watchdog timer for win32
#ifndef _WIN32
#include <signal.h>
#include <sys/time.h>
#include <unistd.h>

static pthread_mutex_t some_global_lock = PTHREAD_MUTEX_INITIALIZER;

void acquire_a_global_lock() {
    pthread_mutex_lock(&some_global_lock);
}
void release_a_global_lock() {
    pthread_mutex_unlock(&some_global_lock);
}
static void alarmclock_expired(int sig)
{
    char msg[] = "timed out\n";
    write(2, msg, sizeof msg-1);
    _exit(1);
}

/// Exit with failure if we can't exit within a set time.
void prepare_exit_test(int seconds)
{
    struct sigaction sa;
    sa.sa_handler = alarmclock_expired;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGALRM, &sa, 0);
    struct itimerval it;
    it.it_value.tv_sec = seconds;
    it.it_value.tv_usec = 0;
    it.it_interval.tv_sec = it.it_interval.tv_usec = 0;
    setitimer(ITIMER_REAL, &it, 0);
}
#endif
