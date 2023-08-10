#ifdef USE_PTHREAD_LOCK
/* Probably not something you actually want to do in a signal handler,
 * but it lets you use things like mutrace. */
#include <pthread.h>
typedef pthread_mutex_t lock_t;

static void acquire_lock(lock_t *l) { gc_assert(!pthread_mutex_lock(l)); }
static void release_lock(lock_t *l) { gc_assert(!pthread_mutex_unlock(l)); }

#define LOCK_INITIALIZER PTHREAD_MUTEX_INITIALIZER
#else
/* A lock that has the sole benefit of not being the pthreads lock.
 * And the sole downside of not being the pthreads lock. */
#include <sched.h>
#include <stdatomic.h>

struct lock { _Atomic(int) grabbed; };
typedef struct lock lock_t;

static void acquire_lock(lock_t *l) {
  int expected = 0, cycles = 0;
  /* atomic_compare_exchange_strong kindly clobbers expected for us,
   * when CAS fails. */
  while (!atomic_compare_exchange_strong(&l->grabbed, &expected, 1)) {
    expected = 0;
    if (cycles++ > 1000) sched_yield();
  }
}
static void release_lock(lock_t *l) {
  atomic_store(&l->grabbed, 0);
}
#define LOCK_INITIALIZER { 0 }
#endif
