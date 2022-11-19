#include <unistd.h>  // for sleep()
#include <stdio.h>   // for perror()
#include <stdlib.h>  // for exit()
#include <pthread.h>
#include <signal.h>
#include <semaphore.h>

sem_t sem, sem2;

void
wait_a_bit(void)
{
    sem_post(&sem);
    sem_wait(&sem2);
}

void
kill_non_lisp_thread(void)
{
    pthread_t kid;
    sem_init(&sem, 0, 0);
    sem_init(&sem2, 0, 0);
    if (pthread_create(&kid, 0, (void *(*)(void *))wait_a_bit, 0) < 0) {
        perror("pthread_create");
        exit(1);
    }

    sem_wait(&sem);

    if (pthread_kill(kid, SIGURG)) {
        perror("pthread_kill");
        sem_post(&sem2);
        exit(1);
    }
    sem_post(&sem2);
}
