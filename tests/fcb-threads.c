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
        fprintf(stderr, "%d: pthread %lx returned %d\n",
                i, (long)threads[i].pthread_id, threads[i].result);
    }
}
