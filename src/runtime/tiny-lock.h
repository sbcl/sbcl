#ifdef USE_PTHREAD_LOCK
/* Probably not something you actually want to do in a signal handler,
 * but it lets you use things like mutrace. */
#include <pthread.h>
typedef pthread_mutex_t lock_t;

// gc_assert drops out if NDEBUG is defined
static void acquire_lock(lock_t *l) {
    __attribute__((unused)) int r=pthread_mutex_lock(l);
    gc_assert(!r);
}
static void release_lock(lock_t *l) {
    __attribute__((unused)) int r=pthread_mutex_unlock(l);
    gc_assert(!r);
}

#define LOCK_INITIALIZER PTHREAD_MUTEX_INITIALIZER
#else
/* A lock that has the sole benefit of not being the pthreads lock.
 * And the sole downside of not being the pthreads lock. */
#include <stdatomic.h>

struct lock { _Atomic(int) grabbed; };
typedef struct lock lock_t;

#ifdef LISP_FEATURE_SB_FUTEX
// implement 'class mutex2' from "Futexes Are Tricky"
extern int futex_wait(int*,int,long,unsigned long), futex_wake(int*, int);
static inline int cmpxchg(lock_t *l, int old, int new) {
  atomic_compare_exchange_weak(&l->grabbed, &old, new);
  return old;
}
static void __attribute__((unused)) acquire_lock(lock_t *l) {
  int c;
  if ((c = cmpxchg(l, 0, 1)) != 0) {
    do {
      if (c == 2 || cmpxchg(l, 1, 2) != 0)
        futex_wait((int*)&l->grabbed, 2, -1, 0);
    } while ((c = cmpxchg(l, 0, 2) != 0) != 0);
  }
}
static void __attribute__((unused)) release_lock(lock_t* l) {
  if (atomic_fetch_sub(&l->grabbed, 1) != 1) {
    l->grabbed = 0;
    futex_wake((int*)&l->grabbed, 1);
  }
}
#else
#include <sched.h>
static void __attribute__((unused)) acquire_lock(lock_t *l) {
  int expected = 0, cycles = 0;
  /* atomic_compare_exchange_strong kindly clobbers expected for us,
   * when CAS fails. */
  while (!atomic_compare_exchange_strong(&l->grabbed, &expected, 1)) {
    expected = 0;
    if (cycles++ > 1000) sched_yield();
  }
}
static void __attribute__((unused)) release_lock(lock_t *l) {
  atomic_store(&l->grabbed, 0);
}
#endif
static int __attribute__((unused)) try_acquire_lock(lock_t *l) {
  int expected = 0;
  return atomic_compare_exchange_strong(&l->grabbed, &expected, 1);
}

#define LOCK_INITIALIZER { 0 }
#endif
