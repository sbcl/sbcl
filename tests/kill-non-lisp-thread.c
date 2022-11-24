#include <unistd.h>  // for sleep()
#include <stdio.h>   // for perror()
#include <stdlib.h>  // for exit()
#include <pthread.h>
#include <signal.h>

#ifdef __APPLE__
#include <dispatch/dispatch.h>
dispatch_semaphore_t sem, sem2;
#else
#include <semaphore.h>
sem_t sem, sem2;
#endif

void
wait_a_bit(void)
{
#ifdef __APPLE__
    dispatch_semaphore_signal(sem);
    dispatch_semaphore_wait(sem2, DISPATCH_TIME_FOREVER);
#else
    sem_post(&sem);
    sem_wait(&sem2);
#endif
}

void
kill_non_lisp_thread(void)
{
    pthread_t kid;

#ifdef __APPLE__
    if(!(sem = dispatch_semaphore_create(0)) ||
       !(sem2 = dispatch_semaphore_create(0))) {
        perror("dispatch_semaphore_create");
        exit(1);
    }
#else
    if (sem_init(&sem, 0, 0) || sem_init(&sem2, 0, 0)) {
        perror("sem_init");
        exit(1);
    }
#endif

    if (pthread_create(&kid, 0, (void *(*)(void *))wait_a_bit, 0) < 0) {
        perror("pthread_create");
        exit(1);
    }

#ifdef __APPLE__
    dispatch_semaphore_wait(sem, DISPATCH_TIME_FOREVER);
#else
    sem_wait(&sem);
#endif

    if (pthread_kill(kid, SIGURG)) {
        perror("pthread_kill");
    }

#ifdef __APPLE__
    dispatch_semaphore_signal(sem2);
#else
    sem_post(&sem2);
#endif

}
