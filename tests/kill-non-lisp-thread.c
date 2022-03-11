#include <unistd.h>  // for sleep()
#include <stdio.h>   // for perror()
#include <stdlib.h>  // for exit()
#include <pthread.h>
#include <signal.h>

void
wait_a_bit(void)
{
    sleep(5);
}

void
kill_non_lisp_thread(void)
{
    pthread_t kid;
    if (pthread_create(&kid, 0, (void *(*)(void *))wait_a_bit, 0) < 0) {
        perror("pthread_create");
        exit(1);
    }
    pthread_kill(kid, SIGURG);
}
