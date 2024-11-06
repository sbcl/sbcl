#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <stdlib.h>
#include <semaphore.h>
#include "interr.h"

static unsigned int gc_threads;
static pthread_t *threads;
static os_sem_t *start_semaphores;
static os_sem_t join_semaphore;
static void (*action)(void);

static void *worker(void *index) {
  uword_t i = (uword_t)index;
  while (1) {
    os_sem_wait(start_semaphores + i);
    // We don't need pthread_jit_write_protect_np(0) here because workers
    // never alter Lisp objects- they'll only touch side tables.
    // Though how does incremental compaction work? I guess _only_ the main thread
    // transports objects after marking is done?
    action();
    os_sem_post(&join_semaphore);
  }
  return NULL;
}

void thread_pool_init() {
  char *str = getenv("GC_THREADS"), *tail;
  if (str == NULL) {
    gc_threads = 3;
  } else {
    unsigned long parse = strtoul(str, &tail, 10);
    if (tail == str || parse >= 256) lose("%s isn't a number of GC threads", str);
    gc_threads = parse;
  }

#ifdef LISP_FEATURE_DARWIN
  if (1) { // pre-existing semaphores aren't visible in a forked child
#else
  if (!start_semaphores) {
#endif
    start_semaphores = checked_malloc(sizeof(os_sem_t) * gc_threads);
    for (unsigned int i = 0; i < gc_threads; i++)
      os_sem_init(start_semaphores + i, 0);
    os_sem_init(&join_semaphore, 0);
  }

  threads = checked_malloc(sizeof(pthread_t) * gc_threads);
  for (uword_t i = 0; i < gc_threads; i++)
    if (pthread_create(threads + i, NULL, worker, (void*)i))
      lose("Failed to create GC thread #%ld", i);
    else {
#ifdef LISP_FEATURE_LINUX
      pthread_setname_np(threads[i], "Parallel GC");
#endif
    }
}

static void wake_gc_threads() {
  for (unsigned int i = 0; i < gc_threads; i++) os_sem_post(start_semaphores + i);
}

static void join_gc_threads() {
  for (unsigned int i = 0; i < gc_threads; i++) os_sem_wait(&join_semaphore);
}

void run_on_thread_pool(void (*act)(void)) {
  action = act;
  wake_gc_threads();
  act();
  join_gc_threads();
  action = 0; // tidy up
}
