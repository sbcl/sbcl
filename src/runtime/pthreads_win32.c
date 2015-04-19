#include "sbcl.h"
#ifdef LISP_FEATURE_SB_THREAD /* entire file */

#define PTHREAD_INTERNALS
#include "pthreads_win32.h"
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>

#ifdef PTHREAD_DEBUG_OUTPUT
#define pthshow(fmt,...)                        \
  do {                                          \
  fprintf(stderr,fmt "\n", __VA_ARGS__);        \
  fflush(stderr);                               \
  } while (0)

#define DEBUG_OWN(cs) do {(cs)->owner=pthread_self(); } while(0)
#define DEBUG_RELEASE(cs) do {(cs)->owner=0;} while(0)

#else
#define pthshow(fmt,...) do {} while (0)
#define DEBUG_OWN(cs) do {} while(0)
#define DEBUG_RELEASE(cs) do {} while(0)
#endif


struct freelist_cell {
    struct freelist_cell * next;
    void* data;
};

struct freelist {
    void* (*create_fn)();
    pthread_mutex_t lock;
    struct freelist_cell * empty;
    struct freelist_cell * full;
    unsigned int count;
};

#define FREELIST_INITIALIZER(create_fn)                 \
    {                                                   \
        event_create, PTHREAD_MUTEX_INITIALIZER,        \
            NULL, NULL, 0                               \
            }                                           \


static void* freelist_get(struct freelist *fl)
{
    void* result = NULL;
    if (fl->full) {
        pthread_mutex_lock(&fl->lock);
        if (fl->full) {
            struct freelist_cell *cell = fl->full;
            fl->full = cell->next;
            result = cell->data;
            cell->next = fl->empty;
            fl->empty = cell;
        }
        pthread_mutex_unlock(&fl->lock);
    }
    if (!result) {
        result = fl->create_fn();
    }
    return result;
}

static void freelist_return(struct freelist *fl, void*data)
{
    struct freelist_cell* cell = NULL;
    if (fl->empty) {
        pthread_mutex_lock(&fl->lock);
        if (fl->empty) {
            cell = fl->empty;
            fl->empty = cell->next;
            goto add_locked;
        }
        pthread_mutex_unlock(&fl->lock);
    }
    if (!cell) {
        int i,n=32;
        cell = malloc(sizeof(*cell)*n);
        for (i=0; i<(n-1); ++i)
            cell[i].next = &cell[i+1];
        cell[i].next = NULL;
    }

    pthread_mutex_lock(&fl->lock);
    ++fl->count;
 add_locked:
    cell->data = data;
    cell->next = fl->full;
    fl->full = cell;
    pthread_mutex_unlock(&fl->lock);
}

int pthread_attr_init(pthread_attr_t *attr)
{
  attr->stack_size = 0;
  return 0;
}

int pthread_attr_destroy(pthread_attr_t *attr)
{
  return 0;
}

int pthread_attr_setstack(pthread_attr_t *attr, void *stackaddr, size_t stacksize)
{
  fprintf(stderr, "pthread_attr_setstack called\n");
  ExitProcess(1);
  return 0;
}

int pthread_attr_setstacksize(pthread_attr_t *attr, size_t stacksize)
{
  attr->stack_size = stacksize;
  return 0;
}


typedef unsigned char boolean;

/* TLS management internals */

static DWORD thread_self_tls_index;

static void (*tls_destructors[PTHREAD_KEYS_MAX])(void*);
static boolean tls_used[PTHREAD_KEYS_MAX];
static pthread_key_t tls_max_used_key;
static pthread_mutex_t thread_key_lock = PTHREAD_MUTEX_INITIALIZER;
static void tls_call_destructors();
static pthread_t tls_impersonate(pthread_t other) {
  pthread_t old = pthread_self();
  TlsSetValue(thread_self_tls_index,other);
  return old;
}

static void do_nothing() {}
/* Fiber context hooks */
void (*pthread_save_context_hook)() = do_nothing;
void (*pthread_restore_context_hook)() = do_nothing;

/* Some parts of pthread_np API provide access to Windows NT Fibers
   (cooperatively scheduled coroutines). Each fiber is wrapped in its
   own pthread.

   Fibers may be entered by different threads during their lifetime,
   i.e. they are orthogonal to threads.

   Contrary to the raw NT Fibers API, we will distinguish two kinds of
   objects: fibers-created-as-fibers and any other thing (thread that
   is not a fiber, thread converted to fiber, system thread
   noticed). Consequently, though there is no "main fiber" in NT,
   there _is_ a main pthread for each (wrapped) system thread, living
   or dying with this system thread. It may be converted to fiber, but
   its "fiberness" is incidental, only to be able to switch into
   another fibers or create them.

   Any fiber that is currently running belongs to some thread
   (fiber-created-as-thread, to be exact). Call it FCAT group.

   [1] Entrance lock: prevent double entry.

   [2] Suspend for fibers -> "try locking entrance lock; if failed, do
   real thread suspend"

   [3] Resume for fibers -> two strategies depending on what [2] done.

   [4] Exit/death for fibers -> switch to its FCAT group.

   [2],[3],[4] doesn't apply to threads-converted-to-fibers: full
   stop/resume is done on them if there is no cooperatively-accessed
   published context (of which see below).
*/
void pthread_np_suspend(pthread_t thread)
{
  pthread_mutex_lock(&thread->fiber_lock);
  if (thread->fiber_group) {
      CONTEXT context;
      SuspendThread(thread->fiber_group->handle);
      context.ContextFlags = CONTEXT_FULL;
      GetThreadContext(thread->fiber_group->handle, &context);
  }
}

/* Momentary suspend/getcontext/resume without locking or preventing
   fiber reentrance.  This call is for asymmetric synchronization,
   ensuring that the thread sees global state before doing any
   globally visible stores.
*/
void pthread_np_serialize(pthread_t thread)
{
    CONTEXT winctx;
    winctx.ContextFlags = CONTEXT_INTEGER;
    if (!thread->created_as_fiber) {
        SuspendThread(thread->handle);
        GetThreadContext(thread->handle,&winctx);
        ResumeThread(thread->handle);
    }
}

int pthread_np_get_thread_context(pthread_t thread, CONTEXT* context)
{
  context->ContextFlags = CONTEXT_FULL;
  return thread->fiber_group &&
      GetThreadContext(thread->fiber_group->handle, context) != 0;
}

void pthread_np_resume(pthread_t thread)
{
  HANDLE host_thread = thread->fiber_group ? thread->fiber_group->handle : NULL;
  /* Unlock first, _then_ resume, or we may end up accessing freed
     pthread structure (e.g. at startup with CREATE_SUSPENDED) */
  pthread_mutex_unlock(&thread->fiber_lock);
  if (host_thread) {
    ResumeThread(host_thread);
  }
}

/* FIXME shouldn't be used. */
void pthread_np_request_interruption(pthread_t thread)
{
  if (thread->waiting_cond) {
    pthread_cond_broadcast(thread->waiting_cond);
  }
}

/* Thread identity, as much as pthreads are concerned, is determined
   by pthread_t structure that is stored in TLS slot
   (thread_self_tls_index). This slot is reassigned when fibers are
   switched with pthread_np API.

   Two reasons for not using fiber-local storage for this purpose: (1)
   Fls is too young: all other things work with Win2000, it requires
   WinXP; (2) this implementation works also with threads that aren't
   fibers, and it's a good thing.

   There is one more case, besides fiber switching, when pthread_self
   identity migrates between system threads: for non-main system
   thread that is not [pthread_create]d, thread-specific data
   destructors run in a thread from a system thread pool, after the
   original thread dies. In order to provide compatibility with
   classic pthread TSD, the system pool thread acquires dead thread's
   identity for the duration of destructor calls.
*/
pthread_t pthread_self()
{
  return (pthread_t)TlsGetValue(thread_self_tls_index);
}

const char * state_to_str(pthread_thread_state state)
{
  switch (state) {
    case pthread_state_running: return "running";
    case pthread_state_finished: return "finished";
    case pthread_state_joined: return "joined";
  default: return "unknown";
  }
}

/* Two kinds of threads (or fibers) are supported: (1) created by
   pthread_create, (2) created independently and noticed by
   pthread_np_notice_thread. The first kind is running a predefined
   thread function or fiber function; thread_or_fiber_function
   incorporates whatever they have in common.
*/
static void thread_or_fiber_function(pthread_t self)
{
  pthread_t prev = tls_impersonate(self);
  void* arg = self->arg;
  pthread_fn fn = self->start_routine;

  if (prev) {
    pthread_mutex_lock(&prev->fiber_lock);
    prev->fiber_group = NULL;
    /* Previous fiber, that started us, had assigned our
       fiber_group. Now we clear its fiber_group. */
    pthread_mutex_unlock(&prev->fiber_lock);
  }
  self->retval = fn(arg);
  pthread_mutex_lock(&self->lock);
  self->state = pthread_state_finished;
  pthread_cond_broadcast(&self->cond);
  while (!self->detached && self->state != pthread_state_joined) {
    if (self->created_as_fiber) {
      pthread_mutex_unlock(&self->lock);
      pthread_np_switch_to_fiber(self->fiber_group);
      pthread_mutex_lock(&self->lock);
    } else {
      pthread_cond_wait(&self->cond, &self->lock);
    }
  }
  pthread_mutex_unlock(&self->lock);
  pthread_mutex_destroy(&self->lock);
  pthread_mutex_destroy(&self->fiber_lock);
  pthread_cond_destroy(&self->cond);
  tls_call_destructors();
}

/* Thread function for [pthread_create]d threads. Thread may become a
   fiber later, but (as stated above) it isn't supposed to be
   reattached to other system thread, even after it happens.
*/
DWORD WINAPI Thread_Function(LPVOID param)
{
  pthread_t self = (pthread_t) param;

  self->teb = NtCurrentTeb();
  thread_or_fiber_function(param);
  CloseHandle(self->handle);
  {
    void* fiber = self->fiber;
    free(self);
    if (fiber) {
      /* If thread was converted to fiber, deleting the fiber from
         itself exits the thread. There are some rumors on possible
         memory leaks if we just ExitThread or return here, hence the
         statement below. However, no memory leaks on bare ExitThread
         were observed yet. */
      DeleteFiber(GetCurrentFiber());
    }
  }
  return 0;
}

/* Fiber can't delete itself without exiting the current thread
   simultaneously. We arrange for some other fiber calling
   fiber_destructor when fiber dies but doesn't want to terminate its
   thread. */
static void fiber_destructor(void* fiber) { DeleteFiber(fiber); }

VOID CALLBACK Fiber_Function(LPVOID param)
{
  pthread_t self = (pthread_t) param;
  thread_or_fiber_function(param);
  {
    /* fiber_group is a main thread into which we are to call */
    pthread_t group = self->fiber_group;
    free(self);
    /* pthread_np_run_in_fiber (see below) normally switches back to
       caller. Nullify our identity, so it knows there is nothing to
       switch to, and continues running instead. */
    tls_impersonate(NULL);
    if (group) {
      /* Every running [pthread_create]d fiber runs in some thread
         that has its own pthread_self identity (that was created as
         thread and later converted to fiber). `group' field of
         running fiber always points to that other pthread.

         Now switch to our group ("current master fiber created as
         thread"), asking it to delete our (OS) fiber data with
         fiber_destructor. */
      pthread_np_run_in_fiber(group, fiber_destructor, GetCurrentFiber());
    }
    /* Within current pthread API we never end up here.

     BTW, if fibers are ever pooled, to avoid stack space reallocation
     etc, jumping to the beginning of Fiber_Function should be the
     thing to do here. */
    DeleteFiber(GetCurrentFiber()); /* Exits. See Thread_Function for
                                       explanation -- why not
                                       ExitThread. */
  }
}

/* Signals */
struct sigaction signal_handlers[NSIG];

/* Never called for now */
int sigaction(int signum, const struct sigaction* act, struct sigaction* oldact)
{
  struct sigaction newact = *act;
  if (oldact)
    *oldact = signal_handlers[signum];
  if (!(newact.sa_flags & SA_SIGINFO)) {
      newact.sa_sigaction = (typeof(newact.sa_sigaction))newact.sa_handler;
  }
  signal_handlers[signum] = newact;
  return 0;
}

/* Create thread or fiber, depending on current thread's "fiber
   factory mode". In the latter case, switch into newly-created fiber
   immediately.
*/
int pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                   void *(*start_routine) (void *), void *arg)
{
  pthread_t pth = (pthread_t)calloc(sizeof(pthread_thread),1);
  pthread_t self = pthread_self();
  int i;
  HANDLE createdThread = NULL;

  if (self && self->fiber_factory) {
    pth->fiber = CreateFiber (attr ? attr->stack_size : 0, Fiber_Function, pth);
    if (!pth->fiber) return 1;
    pth->created_as_fiber = 1;
    /* Has no fiber-group until someone enters it (we will) */
  } else {
    createdThread = CreateThread(NULL, attr ? attr->stack_size : 0,
                                 Thread_Function, pth, CREATE_SUSPENDED, NULL);
    if (!createdThread) return 1;
    /* FCAT is its own fiber-group [initially] */
    pth->fiber_group = pth;
    pth->handle = createdThread;
  }
  pth->start_routine = start_routine;
  pth->arg = arg;
  if (self) {
    pth->blocked_signal_set = self->blocked_signal_set;
  } else {
    sigemptyset(&pth->blocked_signal_set);
  }
  pth->state = pthread_state_running;
  pthread_mutex_init(&pth->lock, NULL);
  pthread_mutex_init(&pth->fiber_lock, NULL);
  pthread_cond_init(&pth->cond, NULL);
  pth->detached = 0;
  if (thread) *thread = pth;
  if (pth->fiber) {
    pthread_np_switch_to_fiber(pth);
  } else {
    /* Resume will unlock, so we lock here */
    pthread_mutex_lock(&pth->fiber_lock);
    pthread_np_resume(pth);
  }
  return 0;
}

int pthread_equal(pthread_t thread1, pthread_t thread2)
{
  return thread1 == thread2;
}

int pthread_detach(pthread_t thread)
{
  int retval = 0;
  pthread_mutex_lock(&thread->lock);
  thread->detached = 1;
  pthread_cond_broadcast(&thread->cond);
  pthread_mutex_unlock(&thread->lock);
  return retval;
}

int pthread_join(pthread_t thread, void **retval)
{
  int fiberp = thread->created_as_fiber;
  pthread_mutex_lock(&thread->lock);
  while (thread->state != pthread_state_finished) {
    if (fiberp) {
      /* just trying */
      pthread_mutex_unlock(&thread->lock);
      pthread_np_switch_to_fiber(thread);
      pthread_mutex_lock(&thread->lock);
    } else {
      pthread_cond_wait(&thread->cond, &thread->lock);
    }
  }
  thread->state = pthread_state_joined;
  pthread_cond_broadcast(&thread->cond);
  if (retval)
    *retval = thread->retval;
  pthread_mutex_unlock(&thread->lock);
  if (fiberp)
    pthread_np_switch_to_fiber(thread);
  return 0;
}

/* We manage our own TSD instead of relying on system TLS for anything
   other than pthread identity itself. Reasons: (1) Windows NT TLS
   slots are expensive, (2) pthread identity migration requires only
   one TLS slot assignment, instead of massive copying. */
int pthread_key_create(pthread_key_t *key, void (*destructor)(void*))
{
  pthread_key_t index;
  boolean success = 0;
  pthread_mutex_lock(&thread_key_lock);
  for (index = 0; index < PTHREAD_KEYS_MAX; ++index) {
    if (!tls_used[index]) {
      if (tls_max_used_key<index)
        tls_max_used_key = index;
      tls_destructors[index] = destructor;
      tls_used[index] = 1;
      success = 1;
      break;
    }
  }
  pthread_mutex_unlock(&thread_key_lock);

  if (success) {
    *key = index;
    return 0;
  } else {
    return 1;
  }
}

int pthread_key_delete(pthread_key_t key)
{
  /* tls_used flag is not a machine word. Let's lock, as there is no
     atomic guarantee even on x86.  */
  pthread_mutex_lock(&thread_key_lock);
  tls_destructors[key] = 0;
  /* No memory barrier here: application is responsible for proper
     call sequence, and having the key around at this point is an
     official UB.  */
  tls_used[key] = 0;
  pthread_mutex_unlock(&thread_key_lock);
  return 0;
}

void  __attribute__((sysv_abi)) *pthread_getspecific(pthread_key_t key)
{
  return pthread_self()->specifics[key];
}

/* Internal function calling destructors for current pthread */
static void tls_call_destructors()
{
  pthread_key_t key;
  int i;
  int called;

  for (i = 0; i<PTHREAD_DESTRUCTOR_ITERATIONS; ++i) {
    called = 0;
    for (key = 0; key<=tls_max_used_key; ++key) {
      void *cell = pthread_getspecific(key);
      pthread_setspecific(key,NULL);
      if (cell && tls_destructors[key]) {
        (tls_destructors[key])(cell);
        called = 1;
      }
    }
    if (!called)
      break;
  }
}

pthread_mutex_t once_mutex = PTHREAD_MUTEX_INITIALIZER;

int pthread_once(pthread_once_t *once_control, void (*init_routine)(void))
{
  if (PTHREAD_ONCE_INIT == *once_control) {
    pthread_mutex_lock(&once_mutex);
    if (PTHREAD_ONCE_INIT == *once_control) {
      init_routine();
      *once_control = 42;
    }
    pthread_mutex_unlock(&once_mutex);
  }
  return 0;
}

/* TODO call signal handlers */
int _sbcl_pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset)
{
  pthread_t self = pthread_self();
  if (oldset)
    *oldset = self->blocked_signal_set;
  if (set) {
    switch (how) {
      case SIG_BLOCK:
        self->blocked_signal_set |= *set;
        break;
      case SIG_UNBLOCK:
        self->blocked_signal_set &= ~(*set);
        break;
      case SIG_SETMASK:
        self->blocked_signal_set = *set;
        break;
    }
  }
  return 0;
}

pthread_mutex_t mutex_init_lock;

int pthread_mutex_init(pthread_mutex_t * mutex, const pthread_mutexattr_t * attr)
{
  *mutex = (struct _pthread_mutex_info*)malloc(sizeof(struct _pthread_mutex_info));
  InitializeCriticalSection(&(*mutex)->cs);
  (*mutex)->file = " (free) ";
  return 0;
}

int pthread_mutexattr_init(pthread_mutexattr_t* attr)
{
  return 0;
}
int pthread_mutexattr_destroy(pthread_mutexattr_t* attr)
{
  return 0;
}

int pthread_mutexattr_settype(pthread_mutexattr_t* attr,int mutex_type)
{
  return 0;
}

int pthread_mutex_destroy(pthread_mutex_t *mutex)
{
    if (*mutex != PTHREAD_MUTEX_INITIALIZER) {
        pthread_np_assert_live_mutex(mutex,"destroy");
        DeleteCriticalSection(&(*mutex)->cs);
        free(*mutex);
        *mutex = &DEAD_MUTEX;
    }
    return 0;
}

/* Add pending signal to (other) thread */
void pthread_np_add_pending_signal(pthread_t thread, int signum)
{
    /* See __sync_fetch_and_or() for gcc 4.4, at least.  As some
       people are still using gcc 3.x, I prefer to do this in asm.

       For win64 we'll HAVE to rewrite it. __sync_fetch_and_or() seems
       to be a rational choice -- there are plenty of GCCisms in SBCL
       anyway.
    */
    sigset_t to_add = 1<<signum;
    asm("lock orl %1,%0":"=m"(thread->pending_signal_set):"r"(to_add));
}

static void futex_interrupt(pthread_t thread);

/* This pthread_kill doesn't do anything to notify target pthread of a
 * new pending signal.
 *
 * DFL: ... or so the original comment claimed, but that was before
 * futexes.  Now that we wake up futexes, it's not entirely accurate
 * anymore, is it? */
int pthread_kill(pthread_t thread, int signum)
{
  pthread_np_add_pending_signal(thread,signum);
  futex_interrupt(thread);
  return 0;
}

void pthread_np_remove_pending_signal(pthread_t thread, int signum)
{
    sigset_t to_and = ~(1<<signum);
    asm("lock andl %1,%0":"=m"(thread->pending_signal_set):"r"(to_and));
}

sigset_t pthread_np_other_thread_sigpending(pthread_t thread)
{
    return
        InterlockedCompareExchange((volatile LONG*)&thread->pending_signal_set,
                                   0, 0);
}

/* Mutex implementation uses CRITICAL_SECTIONs. Somethings to keep in
   mind: (1) uncontested locking is cheap; (2) long wait on a busy
   lock causes exception, so it should never be attempted; (3) those
   mutexes are recursive; (4) one thread locks, the other unlocks ->
   the next one hangs. */
int pthread_mutex_lock(pthread_mutex_t *mutex)
{
  pthread_np_assert_live_mutex(mutex,"lock");
  if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
    pthread_mutex_lock(&mutex_init_lock);
    if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
      pthread_mutex_init(mutex, NULL);
    }
    pthread_mutex_unlock(&mutex_init_lock);
  }
  EnterCriticalSection(&(*mutex)->cs);
  DEBUG_OWN(*mutex);
  return 0;
}

int pthread_mutex_trylock(pthread_mutex_t *mutex)
{
  pthread_np_assert_live_mutex(mutex,"trylock");
  if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
    pthread_mutex_lock(&mutex_init_lock);
    if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
      pthread_mutex_init(mutex, NULL);
    }
    pthread_mutex_unlock(&mutex_init_lock);
  }
  if (TryEnterCriticalSection(&(*mutex)->cs)) {
      DEBUG_OWN(*mutex);
      return 0;
  }
  else
    return EBUSY;
}

/* Versions of lock/trylock useful for debugging. Our header file
   conditionally redefines lock/trylock to call them. */

int pthread_mutex_lock_annotate_np(pthread_mutex_t *mutex, const char* file, int line)
{
  int contention = 0;
  pthread_np_assert_live_mutex(mutex,"lock");
  if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
    pthread_mutex_lock(&mutex_init_lock);
    if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
      pthread_mutex_init(mutex, NULL);
      pthshow("Mutex #x%p: automatic initialization; #x%p %s +%d",
              mutex, *mutex,
              file, line);
    }
    pthread_mutex_unlock(&mutex_init_lock);
  }
  if ((*mutex)->owner) {
    pthshow("Mutex #x%p -> #x%p: contention; owned by #x%p, wanted by #x%p",
            mutex, *mutex,
            (*mutex)->owner,
            pthread_self());
    pthshow("Mutex #x%p -> #x%p: contention notes: old %s +%d, new %s +%d",
            mutex, *mutex,
            (*mutex)->file,(*mutex)->line, file, line);
    contention = 1;
  }
  EnterCriticalSection(&(*mutex)->cs);
  if (contention) {
    pthshow("Mutex #x%p -> #x%p: contention end; left by #x%p, taken by #x%p",
            mutex, *mutex,
            (*mutex)->owner,
            pthread_self());
    pthshow("Mutex #x%p -> #x%p: contention notes: old %s +%d, new %s +%d",
            mutex, *mutex,
            (*mutex)->file,(*mutex)->line, file, line);
  }
  (*mutex)->owner = pthread_self();
  (*mutex)->file = file;
  (*mutex)->line = line;
  return 0;
}

int pthread_mutex_trylock_annotate_np(pthread_mutex_t *mutex, const char* file, int line)
{
  int contention = 0;
  pthread_np_assert_live_mutex(mutex,"trylock");
  if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
    pthread_mutex_lock(&mutex_init_lock);
    if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
      pthread_mutex_init(mutex, NULL);
    }
    pthread_mutex_unlock(&mutex_init_lock);
  }
  if ((*mutex)->owner) {
    pthshow("Mutex #x%p -> #x%p: tried contention; owned by #x%p, wanted by #x%p",
            mutex, *mutex,
            (*mutex)->owner,
            pthread_self());
    pthshow("Mutex #x%p -> #x%p: contention notes: old %s +%d, new %s +%d",
            mutex, *mutex,
            (*mutex)->file,(*mutex)->line, file, line);
    contention = 1;
  }
  if (TryEnterCriticalSection(&(*mutex)->cs)) {
    if (contention) {
      pthshow("Mutex #x%p -> #x%p: contention end; left by #x%p, taken by #x%p",
              mutex, *mutex,
              (*mutex)->owner,
              pthread_self());
      pthshow("Mutex #x%p -> #x%p: contention notes: old %s +%d, new %s +%d",
              mutex, *mutex,
              (*mutex)->file,(*mutex)->line, file, line);
    }
    (*mutex)->owner = pthread_self();
    (*mutex)->file = file;
    (*mutex)->line = line;
    return 0;
  }
  else
    return EBUSY;
}

int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
  /* Owner is for debugging only; NB if mutex is used recursively,
     owner field will lie. */
  pthread_np_assert_live_mutex(mutex,"unlock");
  DEBUG_RELEASE(*mutex);
  LeaveCriticalSection(&(*mutex)->cs);
  return 0;
}

/* Condition variables implemented with events and wakeup queues. */

/* Thread-local wakeup events are kept in TSD to avoid kernel object
   creation on each call to pthread_cond_[timed]wait */
static pthread_key_t cv_event_key;

/* .info field in wakeup record is an "opportunistic" indicator that
   wakeup has happened. On timeout from WaitForSingleObject, thread
   doesn't know (1) whether to reset event, (2) whether to (try) to
   find and unlink wakeup record. Let's let it know (of course,
   it will know for sure only under cv_wakeup_lock). */

#define WAKEUP_WAITING_NOTIMEOUT 0
#define WAKEUP_WAITING_TIMEOUT 4

#define WAKEUP_HAPPENED 1
#define WAKEUP_BY_INTERRUPT 2

static void* event_create()
{
    return (void*)CreateEvent(NULL,FALSE,FALSE,NULL);
}

static struct freelist event_freelist = FREELIST_INITIALIZER(event_create);


unsigned int pthread_free_event_pool_size()
{
    return event_freelist.count;
}

static HANDLE fe_get_event()
{
    return (HANDLE)freelist_get(&event_freelist);
}

static void fe_return_event(HANDLE handle)
{
    freelist_return(&event_freelist, (void*)handle);
}

static void cv_event_destroy(void* event)
{
  CloseHandle((HANDLE)event);
}

static HANDLE cv_default_event_get_fn()
{
  HANDLE event = pthread_getspecific(cv_event_key);
  if (!event) {
    event = CreateEvent(NULL, FALSE, FALSE, NULL);
    pthread_setspecific(cv_event_key, event);
  } else {
    /* ResetEvent(event); used to be here. Let's try without.  It's
       safe in pthread_cond_wait: if WaitForSingleObjectEx ever
       returns, event is reset automatically, and the wakeup queue item
       is removed by the signaller under wakeup_lock.

       pthread_cond_timedwait should reset the event if
       cv_wakeup_remove failed to find its wakeup record, otherwise
       it's safe too. */
  }
  return event;
}

static void cv_default_event_return_fn(HANDLE event)
{
  /* ResetEvent(event); could be here as well (and used to be).
     Avoiding syscalls makes sense, however. */
}

static pthread_condattr_t cv_default_attr = {
  0,                            /* alertable */
  fe_get_event,
  fe_return_event,
  /* cv_default_event_get_fn,      /\* get_fn *\/ */
  /* cv_default_event_return_fn    /\* return_fn *\/ */
};

int pthread_cond_init(pthread_cond_t * cv, const pthread_condattr_t * attr)
{
  if (!attr)
    attr = &cv_default_attr;
  pthread_mutex_init(&cv->wakeup_lock, NULL);
  cv->first_wakeup = NULL;
  cv->last_wakeup = NULL;
  cv->alertable = attr->alertable;
  cv->get_fn = attr->get_fn;
  cv->return_fn = attr->return_fn;
  return 0;
}

int pthread_condattr_init(pthread_condattr_t *attr)
{
  *attr = cv_default_attr;
  return 0;
}

int pthread_condattr_destroy(pthread_condattr_t *attr)
{
  return 0;
}
int pthread_condattr_setevent_np(pthread_condattr_t *attr,
                                 cv_event_get_fn get_fn, cv_event_return_fn ret_fn)
{
    attr->get_fn = get_fn ? get_fn : fe_get_event;// cv_default_event_get_fn;
    attr->return_fn = ret_fn ? ret_fn : fe_return_event; // cv_default_event_return_fn;
    return 0;
}

int pthread_cond_destroy(pthread_cond_t *cv)
{
  pthread_mutex_destroy(&cv->wakeup_lock);
  return 0;
}

int pthread_cond_broadcast(pthread_cond_t *cv)
{
  int count = 0;

  HANDLE postponed[128];
  int npostponed = 0,i;

  /* No strict requirements to memory visibility model, because of
     mutex unlock around waiting. */
  if (!cv->first_wakeup)
      return 0;
  pthread_mutex_lock(&cv->wakeup_lock);
  while (cv->first_wakeup)
  {
    struct thread_wakeup * w = cv->first_wakeup;
    HANDLE waitevent = w->event;
    cv->first_wakeup = w->next;
    w->info = WAKEUP_HAPPENED;
    postponed[npostponed++] = waitevent;
    if (/* w->info == WAKEUP_WAITING_TIMEOUT || */ npostponed ==
        sizeof(postponed)/sizeof(postponed[0])) {
        for (i=0; i<npostponed; ++i)
            SetEvent(postponed[i]);
        npostponed = 0;
    }
    ++count;
  }
  cv->last_wakeup = NULL;
  pthread_mutex_unlock(&cv->wakeup_lock);
  for (i=0; i<npostponed; ++i)
      SetEvent(postponed[i]);
  return 0;
}

int pthread_cond_signal(pthread_cond_t *cv)
{
  struct thread_wakeup * w;
  /* No strict requirements to memory visibility model, because of
     mutex unlock around waiting. */
  if (!cv->first_wakeup)
      return 0;
  pthread_mutex_lock(&cv->wakeup_lock);
  w = cv->first_wakeup;
  if (w) {
    HANDLE waitevent = w->event;
    cv->first_wakeup = w->next;
    if (!cv->first_wakeup)
      cv->last_wakeup = NULL;
    w->info = WAKEUP_HAPPENED;
    SetEvent(waitevent);
  }
  pthread_mutex_unlock(&cv->wakeup_lock);
  return 0;
}

/* Return value is used for futexes: 0=ok, 1 on unexpected word change. */
int cv_wakeup_add(struct pthread_cond_t* cv, struct thread_wakeup* w)
{
  HANDLE event;
  w->next = NULL;
  pthread_mutex_lock(&cv->wakeup_lock);
  if (w->uaddr) {
      if (w->uval != *w->uaddr) {
          pthread_mutex_unlock(&cv->wakeup_lock);
          return 1;
      }
      pthread_self()->futex_wakeup = w;
  }
  event = cv->get_fn();
  w->event = event;
  if (cv->last_wakeup == w) {
    fprintf(stderr, "cv->last_wakeup == w\n");
    fflush(stderr);
    ExitProcess(0);
  }
  if (cv->last_wakeup != NULL)
  {
    cv->last_wakeup->next = w;
    cv->last_wakeup = w;
  }
  else
  {
    cv->first_wakeup = w;
    cv->last_wakeup = w;
  }
  pthread_mutex_unlock(&cv->wakeup_lock);
  return 0;
}

/* Return true if wakeup found, false if missing */
int cv_wakeup_remove(struct pthread_cond_t* cv, struct thread_wakeup* w)
{
  int result = 0;
  if (w->info == WAKEUP_HAPPENED || w->info == WAKEUP_BY_INTERRUPT)
      goto finish;
  pthread_mutex_lock(&cv->wakeup_lock);
  {
    if (w->info == WAKEUP_HAPPENED || w->info == WAKEUP_BY_INTERRUPT)
        goto unlock;
    if (cv->first_wakeup == w) {
      cv->first_wakeup = w->next;
      if (cv->last_wakeup == w)
        cv->last_wakeup = NULL;
      result = 1;
    } else {
      struct thread_wakeup * prev = cv->first_wakeup;
      while (prev && prev->next != w)
        prev = prev->next;
      if (!prev) {
        goto unlock;
      }
      prev->next = w->next;
      if (cv->last_wakeup == w)
        cv->last_wakeup = prev;
      result = 1;
    }
  }
 unlock:
  pthread_mutex_unlock(&cv->wakeup_lock);
 finish:
  return result;
}


int pthread_cond_wait(pthread_cond_t * cv, pthread_mutex_t * cs)
{
  struct thread_wakeup w;
  w.uaddr = 0;
  w.info = WAKEUP_WAITING_NOTIMEOUT;
  cv_wakeup_add(cv, &w);
  if (cv->last_wakeup->next == cv->last_wakeup) {
      pthread_np_lose(5,"cv->last_wakeup->next == cv->last_wakeup\n");
  }
  if (cv->last_wakeup->next != NULL) {
      pthread_np_lose(5,"cv->last_wakeup->next == cv->last_wakeup\n");
  }
  pthread_self()->waiting_cond = cv;
  DEBUG_RELEASE(*cs);
  pthread_mutex_unlock(cs);
  do {
      if (cv->alertable) {
          while (WaitForSingleObjectEx(w.event, INFINITE, TRUE) == WAIT_IO_COMPLETION);
      } else {
          WaitForSingleObject(w.event, INFINITE);
      }
  } while (w.info == WAKEUP_WAITING_NOTIMEOUT);
  pthread_self()->waiting_cond = NULL;
  /* Event is signalled once, wakeup is dequeued by signaller. */
  cv->return_fn(w.event);
  pthread_mutex_lock(cs);
  DEBUG_OWN(*cs);
  return 0;
}

int pthread_cond_timedwait(pthread_cond_t * cv, pthread_mutex_t * cs,
                           const struct timespec * abstime)
{
  DWORD rv;
  struct thread_wakeup w;
  pthread_t self = pthread_self();

  w.info = WAKEUP_WAITING_TIMEOUT;
  w.uaddr = 0;
  cv_wakeup_add(cv, &w);
  if (cv->last_wakeup->next == cv->last_wakeup) {
    fprintf(stderr, "cv->last_wakeup->next == cv->last_wakeup\n");
    ExitProcess(0);
  }
  self->waiting_cond = cv;
  DEBUG_RELEASE(*cs);
  /* barrier (release); waiting_cond globally visible */
  pthread_mutex_unlock(cs);
  {
    struct timeval cur_tm;
    long sec, msec;
    gettimeofday(&cur_tm, NULL);
    sec = abstime->tv_sec - cur_tm.tv_sec;
    msec = sec * 1000 + abstime->tv_nsec / 1000000 - cur_tm.tv_usec / 1000;
    if (msec < 0)
      msec = 0;
    do {
        if (cv->alertable) {
            while ((rv = WaitForSingleObjectEx(w.event, msec, TRUE))
                   == WAIT_IO_COMPLETION);
        } else {
            rv = WaitForSingleObject(w.event, msec);
        }
    } while (rv == WAIT_OBJECT_0 && w.info == WAKEUP_WAITING_TIMEOUT);
  }
  self->waiting_cond = NULL;

  if (rv == WAIT_TIMEOUT) {
    if (!cv_wakeup_remove(cv, &w)) {
      /* Someone removed our wakeup record: though we got a timeout,
         event was (will be) signalled before we are here.
         Consume this wakeup. */
      WaitForSingleObject(w.event, INFINITE);
    }
  }
  cv->return_fn(w.event);
  pthread_mutex_lock(cs);
  DEBUG_OWN(*cs);
  if (rv == WAIT_TIMEOUT)
    return ETIMEDOUT;
  else
    return 0;
}

int sched_yield()
{
  /* http://stackoverflow.com/questions/1383943/switchtothread-vs-sleep1
     SwitchToThread(); was here. Unsure what's better for us, just trying.. */

  if(!SwitchToThread())
      Sleep(0);
  return 0;
}

void pthread_lock_structures()
{
  pthread_mutex_lock(&mutex_init_lock);
}

void pthread_unlock_structures()
{
  pthread_mutex_unlock(&mutex_init_lock);
}

static int pthread_initialized = 0;

static pthread_cond_t futex_pseudo_cond;

void pthreads_win32_init()
{
  if (!pthread_initialized) {
    thread_self_tls_index = TlsAlloc();
    pthread_mutex_init(&mutex_init_lock, NULL);
    pthread_np_notice_thread();
    pthread_key_create(&cv_event_key,cv_event_destroy);
    pthread_cond_init(&futex_pseudo_cond, NULL);
    pthread_initialized = 1;
  }
}

static
VOID CALLBACK pthreads_win32_unnotice(void* parameter, BOOLEAN timerOrWait)
{
  pthread_t pth = parameter;
  pthread_t self = tls_impersonate(pth);

  tls_call_destructors();
  CloseHandle(pth->handle);
  /*
  if (pth->fiber && pth->own_fiber) {
    DeleteFiber(pth->fiber);
    } */
  UnregisterWait(pth->wait_handle);

  tls_impersonate(self);
  pthread_mutex_destroy(&pth->fiber_lock);
  pthread_mutex_destroy(&pth->lock);
  free(pth);
}

int pthread_np_notice_thread()
{
  if (!pthread_self()) {
    pthread_t pth = (pthread_t)calloc(sizeof(pthread_thread),1);
    pth->teb = NtCurrentTeb();
    pthread_mutex_init(&pth->fiber_lock,NULL);
    pthread_mutex_init(&pth->lock,NULL);
    pth->state = pthread_state_running;
    pth->fiber_group = pth;

    sigemptyset(&pth->blocked_signal_set);

    DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                    GetCurrentProcess(), &pth->handle, 0, TRUE,
                    DUPLICATE_SAME_ACCESS);
    tls_impersonate(pth);

    if (pthread_initialized) {
      RegisterWaitForSingleObject(&pth->wait_handle,
                                  pth->handle,
                                  pthreads_win32_unnotice,
                                  pth,
                                  INFINITE,
                                  WT_EXECUTEONLYONCE);
    }
    return 1;
  } else {
    return 0;
  }
}

int pthread_np_convert_self_to_fiber()
{
  pthread_t pth = pthread_self();
  if (!pth)
    return 1;
  if (!pth->fiber) {
    void* fiber = GetCurrentFiber();
    /* Beware: undocumented (but widely used) method below to check if
       the thread is already converted. */
    if (fiber != NULL && fiber != (void*)0x1E00) {
      pth->fiber = fiber;
      pth->own_fiber = 0;
    } else {
      pth->fiber = ConvertThreadToFiber(pth);
      pth->own_fiber = 1;
    }
    if (!pth->fiber)
      return 1;
  }
  return 0;
}

int pthread_np_set_fiber_factory_mode(int on)
{
  pthread_t pth = pthread_self();
  if (on && pthread_np_convert_self_to_fiber()) {
    return 1;
  }
  pth->fiber_factory = on;
  return 0;
}

int pthread_np_switch_to_fiber(pthread_t pth)
{
  pthread_t self = pthread_self();

 again:
  if (pth == self) {
    /* Switch to itself is a successful no-op.
       NB. SwitchToFiber(GetCurrentFiber()) is not(!). */
    return 0;
  }

  if (!pth->fiber) {
    /* Switch to not-a-fiber-at-all */
    return -1;
  }

  if (!pth->created_as_fiber) {
    /* Switch to main thread (group): fails if... */
    if (self && (self->fiber_group != pth)) {
      /* ...trying to switch from [under] one main thread into another */
      return -1;
    }
  }
  if (!self && pth->created_as_fiber) {
    /* Switch to free fiber from non-noticed thread */
    return -1;
  }

  if (self && pthread_np_convert_self_to_fiber()) {
    /* Current thread can't become a fiber (and run fibers) */
    return -1;
  }

  /* If target fiber is suspened, we wait here. */
  pthread_mutex_lock(&pth->fiber_lock);
  if (pth->fiber_group) {
    /* Reentering a running fiber */
    pthread_mutex_unlock(&pth->fiber_lock);
    /* Don't wait for a running fiber here, just fail. If an
       application wants to wait, it should use some separate
       synchronization. */
    return -1;
  }
  if (self) {
    /* Target fiber group is like mine */
    pth->fiber_group = self->fiber_group;
  } else {
    /* Switch-from-null-self (always into thread, usually from
       terminating fiber) */
    pth->fiber_group = pth;
  }
  /* Target fiber now marked as busy */
  pthread_mutex_unlock(&pth->fiber_lock);

  if (self) {
    pthread_save_context_hook();
  }
  /* NB we don't set pthread TLS, let target fiber do it by itself. */
  SwitchToFiber(pth->fiber);

  /* When we return here... */
  pth = tls_impersonate(self);

  /* Now pth contains fiber that entered this one */
  pthread_restore_context_hook();

  if (pth) {
    pthread_mutex_lock(&pth->fiber_lock);
    if (pth->fiber_group == self->fiber_group) {
      pth->fiber_group = NULL;
    }
    pthread_mutex_unlock(&pth->fiber_lock);
  }
  /* Self surely is not NULL, or we'd never be here */

  /* Implement call-in-fiber */
  if (self->fiber_callback) {
    void (*cb)(void*) = self->fiber_callback;
    void *ctx = self->fiber_callback_context;

    /* Nested callbacks and fiber switches are possible, so clean
       up a cb pointer here */
    self->fiber_callback = NULL;
    self->fiber_callback_context = NULL;
    cb(ctx);
    if (pth) {
      /* Return to caller without recursive
       pthread_np_switch_to_fiber.  This way, an "utility fiber"
       serving multiple callbacks won't grow its stack to infinity */
      goto again;
    }
    /* There is no `callback client' pretending to be returned
       into: it means callback shouldn't yield to caller. */
  }
  return 0; /* success */
}

int pthread_np_run_in_fiber(pthread_t pth, void (*callback)(void*),
                            void* context)
{
  pth->fiber_callback = callback;
  pth->fiber_callback_context = context;
  return pthread_np_switch_to_fiber(pth);
}

HANDLE pthread_np_get_handle(pthread_t pth)
{
  return pth->handle;
}

void* pthread_np_get_lowlevel_fiber(pthread_t pth)
{
  return pth->fiber;
}

int pthread_np_delete_lowlevel_fiber(void* fiber)
{
  DeleteFiber(fiber);
  return 0;
}

int sigemptyset(sigset_t *set)
{
  *set = 0;
  return 0;
}

int sigfillset(sigset_t *set)
{
  *set = 0xfffffffful;
  return 0;
}

int sigaddset(sigset_t *set, int signum)
{
  *set |= 1 << signum;
  return 0;
}

int sigdelset(sigset_t *set, int signum)
{
  *set &= ~(1 << signum);
  return 0;
}

int sigismember(const sigset_t *set, int signum)
{
  return (*set & (1 << signum)) != 0;
}
int sigpending(sigset_t *set)
{
  int i;
  *set = InterlockedCompareExchange((volatile LONG*)&pthread_self()->pending_signal_set,
                                    0, 0);
  return 0;
}


#define FUTEX_EWOULDBLOCK 3
#define FUTEX_EINTR 2
#define FUTEX_ETIMEDOUT 1

int
futex_wait(volatile intptr_t *lock_word, intptr_t oldval, long sec, unsigned long usec)
{
  struct thread_wakeup w;
  pthread_t self = pthread_self();
  DWORD msec = sec<0 ? INFINITE : (sec*1000 + usec/1000);
  DWORD wfso;
  int result;
  sigset_t pendset, blocked;
  int maybeINTR;
  int info = sec<0 ? WAKEUP_WAITING_NOTIMEOUT: WAKEUP_WAITING_TIMEOUT;

  sigpending(&pendset);
  if (pendset & ~self->blocked_signal_set)
      return FUTEX_EINTR;
  w.uaddr = lock_word;
  w.uval = oldval;
  w.info = info;

  if (cv_wakeup_add(&futex_pseudo_cond,&w)) {
      return FUTEX_EWOULDBLOCK;
  }
  self->futex_wakeup = &w;
  do {
      wfso = WaitForSingleObject(w.event, msec);
  } while (wfso == WAIT_OBJECT_0 && w.info == info);
  self->futex_wakeup = NULL;
  sigpending(&pendset);
  maybeINTR = (pendset & ~self->blocked_signal_set)? FUTEX_EINTR : 0;

  switch(wfso) {
  case WAIT_TIMEOUT:
      if (!cv_wakeup_remove(&futex_pseudo_cond,&w)) {
          /* timeout, but someone other removed wakeup. */
          result = maybeINTR;
          WaitForSingleObject(w.event,INFINITE);
      } else {
          result = FUTEX_ETIMEDOUT;
      }
      break;
  case WAIT_OBJECT_0:
      result = maybeINTR;
      break;
  default:
      result = -1;
      break;
  }
  futex_pseudo_cond.return_fn(w.event);
  return result;
}

int
futex_wake(volatile intptr_t *lock_word, int n)
{
    pthread_cond_t *cv = &futex_pseudo_cond;
    int result = 0;
    struct thread_wakeup *w, *prev;
    HANDLE postponed[128];
    int npostponed = 0,i;

    if (n==0) return 0;

    pthread_mutex_lock(&cv->wakeup_lock);
    for (w = cv->first_wakeup, prev = NULL; w && n;) {
        if (w->uaddr == lock_word) {
            HANDLE event = w->event;
            int oldinfo = w->info;
            w->info = WAKEUP_HAPPENED;
            if (cv->last_wakeup == w)
                cv->last_wakeup = prev;
            w = w->next;
            if (!prev) {
                cv->first_wakeup = w;
            } else {
                prev->next = w;
            }
            n--;
            postponed[npostponed++] = event;
            if (npostponed == sizeof(postponed)/sizeof(postponed[0])) {
                for (i=0; i<npostponed; ++i)
                    SetEvent(postponed[i]);
                npostponed = 0;
            }
        } else {
            prev=w, w=w->next;
        }
    }
    pthread_mutex_unlock(&cv->wakeup_lock);
    for (i=0; i<npostponed; ++i)
        SetEvent(postponed[i]);
    return 0;
}


static void futex_interrupt(pthread_t thread)
{
    if (thread->futex_wakeup) {
        pthread_cond_t *cv = &futex_pseudo_cond;
        struct thread_wakeup *w;
        HANDLE event;
        pthread_mutex_lock(&cv->wakeup_lock);
        if ((w = thread->futex_wakeup)) {
            /* we are taking wakeup_lock recursively - ok with
               CRITICAL_SECTIONs */
            if (cv_wakeup_remove(&futex_pseudo_cond,w)) {
                event = w->event;
                w->info = WAKEUP_BY_INTERRUPT;
                thread->futex_wakeup = NULL;
            } else {
                w = NULL;
            }
        }
        if (w) {
            SetEvent(event);
        }
        pthread_mutex_unlock(&cv->wakeup_lock);
    }
}

void pthread_np_lose(int trace_depth, const char* fmt, ...)
{
    va_list header;
    void* frame;
    int n = 0;
    void** lastseh;

    va_start(header,fmt);
    vfprintf(stderr,fmt,header);
    for (lastseh = *(void**)NtCurrentTeb();
         lastseh && (lastseh!=(void*)0xFFFFFFFF);
         lastseh = *lastseh);

    fprintf(stderr, "Backtrace: %s (pthread %p)\n", header, pthread_self());
    for (frame = __builtin_frame_address(0); frame; frame=*(void**)frame)
        {
            if ((n++)>trace_depth)
                return;
            fprintf(stderr, "[#%02d]: ebp = %p, ret = %p\n",n,
                    frame, ((void**)frame)[1]);
        }
    ExitProcess(0);
}

int
sem_init(sem_t *sem, int pshared_not_implemented, unsigned int value)
{
    sem_t semh = CreateSemaphore(NULL, value, SEM_VALUE_MAX, NULL);
    if (!semh)
        return -1;
    *sem = semh;
    return 0;
}

int
sem_post(sem_t *sem)
{
    return !ReleaseSemaphore(*sem, 1, NULL);
}

static int
sem_wait_timeout(sem_t *sem, DWORD ms)
{
    switch (WaitForSingleObject(*sem, ms)) {
    case WAIT_OBJECT_0:
        return 0;
    case WAIT_TIMEOUT:
        /* errno = EAGAIN; */
        return -1;
    default:
        /* errno = EINVAL; */
        return -1;
    }
}

int
sem_wait(sem_t *sem)
{
    return sem_wait_timeout(sem, INFINITE);
}

int
sem_trywait(sem_t *sem)
{
    return sem_wait_timeout(sem, 0);
}

int
sem_destroy(sem_t *sem)
{
    return !CloseHandle(*sem);
}

#endif
