#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <stdlib.h>
#include <semaphore.h>
#include "interr.h"

static unsigned int gc_threads;
static pthread_t *threads;
static os_sem_t start_semaphore;
static os_sem_t join_semaphore;
static int semaphores_initializedp;
static void (*action)(void);

static void *worker(void *nothing) {
  (void)nothing;
  while (1) {
    os_sem_wait(&start_semaphore);
    action();
    os_sem_post(&join_semaphore);
  }
  return NULL;
}

void thread_pool_init() {
  if (!semaphores_initializedp) {
      os_sem_init(&start_semaphore, 0);
      os_sem_init(&join_semaphore, 0);
      semaphores_initializedp = 1;
  }

  char *str = getenv("GC_THREADS"), *tail;
  if (str == NULL) {
    gc_threads = 3;
  } else {
    unsigned long parse = strtoul(str, &tail, 10);
    if (tail == str || parse >= 256) lose("%s isn't a number of GC threads", str);
    gc_threads = parse;
  }
  threads = successful_malloc(sizeof(pthread_t) * gc_threads);

  for (unsigned int i = 0; i < gc_threads; i++)
    if (pthread_create(threads + i, NULL, worker, NULL))
      lose("Failed to create GC thread #%d", i);
    else {
#ifdef LISP_FEATURE_LINUX
      pthread_setname_np(threads[i], "Parallel GC");
#endif
    }
}

static void wake_gc_threads() {
  for (unsigned int i = 0; i < gc_threads; i++) os_sem_post(&start_semaphore);
}

static void join_gc_threads() {
  for (unsigned int i = 0; i < gc_threads; i++) os_sem_wait(&join_semaphore);
}

void run_on_thread_pool(void (*act)(void)) {
  action = act;
  wake_gc_threads();
  act();
  join_gc_threads();
}
