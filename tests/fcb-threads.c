#include <pthread.h>
#include <stdio.h>

struct thread_arg { void * funkyfun; int index; int n_calls; };

void* doThatThing(void* void_arg)
{
    struct thread_arg* arg = void_arg;
    int (*lispfun)(char*,double) = arg->funkyfun;
    int i;
    for(i=0; i<arg->n_calls; ++i) {
        lispfun("Hello", arg->index + i);
    }
    return 0;
}

int call_thing_from_threads(void* ptr, int n_threads, int n_calls)
{
    struct {
        pthread_t pthread_id;
        struct thread_arg arg;
        void* result;
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
        pthread_create(&threads[i].pthread_id, 0, doThatThing, &threads[i].arg);
    }
    for(i=0; i<n_threads; ++i) {
        pthread_join(threads[i].pthread_id, &threads[i].result);
        fprintf(stderr, "%d: pthread %lx returned %p\n",
                i, (long)threads[i].pthread_id, threads[i].result);
    }
    return 0;
}

/// The code following is for the no-lockup-on-exit test, unrelated to the above

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
