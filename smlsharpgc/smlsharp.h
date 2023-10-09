/**
 * smlsharp.h - SML# runtime implemenatation
 * @copyright (c) 2007-2009, Tohoku University.
 * @author UENO Katsuhiro
 * @version $Id: $
 */
#ifndef SMLSHARP__SMLSHARP_H__
#define SMLSHARP__SMLSHARP_H__

extern int use_smlgc;
extern int enable_async_gc;
#define DEADBEEF 0xFFFFFFFFDEADBEEF
#define VERBOSE_LOGGING 0
extern int n_smlgcs;
extern void tprintf_(char *fmt, ...);
#define TPRINTF(msgclass, fmt, ...) if(0 /* msgclass & 2 */) tprintf_(fmt, ##__VA_ARGS__)
extern void suspend_mutator(char* reason);
extern void unsuspend_mutator();
#include <stdio.h>
extern FILE* get_gc_thread_log();
void show_map(char*,void*);
void show_fractional_usage();
extern void get_segment_pool_bounds(char* bounds[2]);
extern char *phase_names[8];
extern char *phase_names_inactive[8];
//extern FILE* get_large_object_logfile();
extern int get_gc_cycle_number();

static inline char* phase_name(int phase) {
  if (phase & 0x10U) return phase_names_inactive[phase-0x10];
  return phase_names[phase];
}

/*
 * One of the following macros may be defined by the command line:
 * - WITHOUT_MULTITHREAD: Remove multithread support at all.
 * - WITHOUT_CONCURRENCY: pthread support + stop-the-world collector.
 * - WITHOUT_MASSIVETHREADS: turn off massivethreads support.
 * The default setting is pthread support + concurrent gc + massivethreads
 */
#ifdef WITHOUT_MULTITHREAD
#undef  WITHOUT_CONCURRENCY
#define WITHOUT_CONCURRENCY
#undef  WITHOUT_MASSIVETHREADS
#define WITHOUT_MASSIVETHREADS
#endif /* WITHOUT_MULTITHREAD */

#ifdef WITHOUT_CONCURRENCY
#undef  WITHOUT_MASSIVETHREADS
#define WITHOUT_MASSIVETHREADS
#endif /* WITHOUT_CONCURRENCY */

#define DPRINTF(x) {}

#if !defined __STDC_VERSION__ || __STDC_VERSION__ < 199901L
# error C99 is required
#endif
#if defined __GNUC__ && __GNUC__ < 4
#error GCC version 4.0 or later is required
#endif

/*#ifdef FOO_LISP_FEATURE_SBCL
 #include "smlsharp-config.h"
 #infdef WITHOUT_MASSIVETHREADS
 #define WITHOUT_MASSIVETHREADS
 #endif */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif
#include <stddef.h>
#include <assert.h>
#include <inttypes.h>
#ifndef WITHOUT_MULTITHREAD
#include <pthread.h>
#include <sched.h>
#endif /* !WITHOUT_MULTITHREAD */
#ifdef HAVE_STDATOMIC_H
# include <stdatomic.h>
#endif

#ifndef WITHOUT_MASSIVETHREADS
#include <myth/myth.h>
#endif /* !WITHOUT_MASSIVETHREADS */

#ifndef HAVE_STDATOMIC_H
# ifdef HAVE_GCC_ATOMIC
#  define _Atomic(ty) __typeof__(ty)
#  define memory_order_relaxed __ATOMIC_RELAXED
#  define memory_order_acquire __ATOMIC_ACQUIRE
#  define memory_order_release __ATOMIC_RELEASE
#  define memory_order_acq_rel __ATOMIC_ACQ_REL
#  define memory_order_seq_cst __ATOMIC_SEQ_CST
#  define ATOMIC_VAR_INIT(v) (v)
#  define atomic_init(p, val) (void)(*(p) = (val))
#  define atomic_load_explicit(p, order) \
        __atomic_load_n(p, order)
#  define atomic_store_explicit(p, val, order) \
        __atomic_store_n(p, val, order)
#  define atomic_compare_exchange_weak_explicit(p, expect, val, succ, fail) \
        __atomic_compare_exchange_n(p, expect, val, 1, succ, fail)
#  define atomic_compare_exchange_strong_explicit(p, expect, val, succ, fail) \
        __atomic_compare_exchange_n(p, expect, val, 0, succ, fail)
#  define atomic_exchange_explicit(p, val, order) \
        __atomic_exchange_n(p, val, order)
#  define atomic_fetch_add_explicit(p, arg, order) \
        __atomic_fetch_add(p, arg, order)
#  define atomic_fetch_sub_explicit(p, arg, order) \
        __atomic_fetch_sub(p, arg, order)
#  define atomic_fetch_or_explicit(p, arg, order) \
        __atomic_fetch_or(p, arg, order)
#  define atomic_fetch_and_explicit(p, arg, order) \
        __atomic_fetch_and(p, arg, order)
#  define atomic_fetch_xor_explicit(p, arg, order) \
        __atomic_fetch_xor(p, arg, order)
# else
#  error atomic builtins are required
# endif /* HAVE_GCC_ATOMIC */
#endif /* !HAVE_STDATOMIC_H */

/* short hands for frequently used synchronization primitives */
#define load_relaxed(p) \
        atomic_load_explicit(p, memory_order_relaxed)
#define load_acquire(p) \
        atomic_load_explicit(p, memory_order_acquire)
#define store_relaxed(p,v) \
        atomic_store_explicit(p, v, memory_order_relaxed)
#define store_release(p,v) \
        atomic_store_explicit(p, v, memory_order_release)
#define cmpswap_relaxed(p,e,v) \
        atomic_compare_exchange_strong_explicit \
          (p, e, v, memory_order_relaxed, memory_order_relaxed)
#define cmpswap_acquire(p,e,v) \
        atomic_compare_exchange_strong_explicit \
          (p, e, v, memory_order_acquire, memory_order_acquire)
#define cmpswap_release(p,e,v) \
        atomic_compare_exchange_strong_explicit \
          (p, e, v, memory_order_release, memory_order_relaxed)
#define cmpswap_acq_rel(p,e,v) \
        atomic_compare_exchange_strong_explicit \
          (p, e, v, memory_order_acq_rel, memory_order_acquire)
#define cmpswap_weak_relaxed(p,e,v) \
        atomic_compare_exchange_weak_explicit \
          (p, e, v, memory_order_relaxed, memory_order_relaxed)
#define cmpswap_weak_acquire(p,e,v) \
        atomic_compare_exchange_weak_explicit \
          (p, e, v, memory_order_acquire, memory_order_acquire)
#define cmpswap_weak_release(p,e,v) \
        atomic_compare_exchange_weak_explicit \
          (p, e, v, memory_order_release, memory_order_relaxed)
#define cmpswap_weak_acq_rel(p,e,v) \
        atomic_compare_exchange_weak_explicit \
          (p, e, v, memory_order_acq_rel, memory_order_acquire)
#define swap(order,p,v) \
        atomic_exchange_explicit(p, v, memory_order_##order)
#define fetch_add(order,p,v) \
        atomic_fetch_add_explicit(p, v, memory_order_##order)
#define fetch_sub(order,p,v) \
        atomic_fetch_sub_explicit(p, v, memory_order_##order)
#define fetch_or(order,p,v) \
        atomic_fetch_or_explicit(p, v, memory_order_##order)
#define fetch_and(order,p,v) \
        atomic_fetch_and_explicit(p, v, memory_order_##order)
#define fetch_xor(order,p,v) \
        atomic_fetch_xor_explicit(p, v, memory_order_##order)
#define ASSERT_ERROR_(e) \
        do { int no_error ATTR_UNUSED = e; assert(no_error == 0); } while (0)
#ifndef WITHOUT_MULTITHREAD
#define mutex_init(m) ASSERT_ERROR_(pthread_mutex_init(m, NULL))
#define mutex_destroy(m) ASSERT_ERROR_(pthread_mutex_destroy(m))
#define mutex_lock(m) ASSERT_ERROR_(pthread_mutex_lock(m))
#define mutex_unlock(m) ASSERT_ERROR_(pthread_mutex_unlock(m))
#define cond_init(c) ASSERT_ERROR_(pthread_cond_init(c, NULL))
#define cond_destroy(c) ASSERT_ERROR_(pthread_cond_destroy(c))
#define cond_wait(c, m) ASSERT_ERROR_(pthread_cond_wait(c, m))
#define cond_broadcast(c) ASSERT_ERROR_(pthread_cond_broadcast(c))
#define cond_signal(c) ASSERT_ERROR_(pthread_cond_signal(c))
#else /* !WITHOUT_MULTITHREAD */
#define mutex_init(m) ((void)0)
#define mutex_destroy(m) ((void)0)
#define mutex_lock(m) ((void)0)
#define mutex_unlock(m) ((void)0)
#define cond_init(c) ((void)0)
#define cond_destroy(c) ((void)0)
#define cond_wait(c, m) ((void)0)
#define cond_broadcast(c) ((void)0)
#define cond_signal(c) ((void)0)
#endif /* !WITHOUT_MULTITHREAD */

#undef ATOMIC_VAR_INIT
#define ATOMIC_VAR_INIT(x) x

/* spin lock */
#ifndef WITHOUT_MULTITHREAD
typedef struct { _Atomic(int) lock; } sml_spinlock_t;
#define SPIN_LOCK_INIT {ATOMIC_VAR_INIT(0)}
extern void bump_spinlock_busyct();
static inline void spin_lock(sml_spinlock_t *l) {
        int old, i = 8192;
        for (;;) {
                old = 0;
                if (cmpswap_weak_acquire(&l->lock, &old, 1)) break;
                if (--i == 0) { sched_yield(); i = 8192; bump_spinlock_busyct(); }
        }
}
static inline void spin_unlock(sml_spinlock_t *l) {
        store_release(&l->lock, 0);
}
#endif /* WITHOUT_MULTITHREAD */

/*
 * support for thread local variables (tlv)
 */
#define single_tlv_alloc(ty, k, destructor)  static ty single_tlv__##k##__
#define single_tlv_init(k)  ((void)0)
#define single_tlv_get(k)  (single_tlv__##k##__)
#define single_tlv_set(k,v)  ((void)(single_tlv__##k##__ = (v)))

#define pth_tlv_alloc__(ty, k, destructor) \
        static pthread_key_t pth_tlv_key__##k##__; \
        static pthread_once_t pth_tlv_key__##k##__once__ = PTHREAD_ONCE_INIT; \
        static void pth_tlv_destruct__##k##__(void *p__) { destructor(p__); } \
        static void pth_tlv_init__##k##__once__() { \
                pthread_key_create(&pth_tlv_key__##k##__, \
                                   pth_tlv_destruct__##k##__); \
        } \
        static inline void pth_tlv_init__##k##__() { \
                pthread_once(&pth_tlv_key__##k##__once__, \
                             pth_tlv_init__##k##__once__); \
        } \
        static inline void pth_tlv_set__##k##__(ty const arg__) { \
                pth_tlv_init__##k##__(); \
                pthread_setspecific(pth_tlv_key__##k##__, arg__); \
        }
#define pth_tlv_alloc(ty, k, destructor) \
        pth_tlv_alloc__(ty, k, destructor) \
        static inline ty pth_tlv_get__##k##__() { \
                return pthread_getspecific(pth_tlv_key__##k##__); \
        }
#define pth_tlv_init(k) (pth_tlv_init__##k##__())
#define pth_tlv_get(k) (pth_tlv_get__##k##__())
#define pth_tlv_set(k,v) (pth_tlv_set__##k##__(v))

/* Even if operating system provides thread local storage (TLS), we use
 * pthread_key in order to ensure that thread local variables are correctly
 * destructed even if the thread terminates abnormally.  To ensure this,
 * tlv_set operation updates both TLS and pthread_key.  This makes tlv_set
 * slower.  This overhead should be negligible since tlv_set is typically
 * used only at thread initialization.  In contrast, tlv_get only reads TLS;
 * so, it is pretty fast.  In Linux, tlv_get is often compiled to just one
 * CPU instruction. */
#define tls_tlv_alloc(ty, k, destructor) \
        pth_tlv_alloc__(ty, k, destructor) \
        static _Thread_local ty tls_tlv__##k##__; \
        static inline void tls_tlv_set__##k##__(ty const arg__) { \
                pth_tlv_set__##k##__(arg__); \
                tls_tlv__##k##__ = arg__; \
        }
#define tls_tlv_init(k) (pth_tlv_init__##k##__())
#define tls_tlv_get(k) (tls_tlv__##k##__)
#define tls_tlv_set(k,v) (tls_tlv_set__##k##__(v))

/* thread local variables for massivethreads.
 * The massivethreads library uses pthread for worker threads.
 * Therefore, we can use pthread_key_t for worker-local variables.
 * In addition, since user threads are non-preemptive, pthread
 * synchronization primitives may work safely (but less efficient) even in
 * a user thread. */
#define mth_tlv_alloc(ty, k, destructor) \
        static myth_key_t mth_tlv_key__##k##__; \
        static myth_once_t mth_tlv_key__##k##__once__; \
        static void mth_tlv_destruct__##k##__(void *p__) { destructor(p__); } \
        static void mth_tlv_init__##k##__once__() { \
                myth_key_create(&mth_tlv_key__##k##__, \
                                mth_tlv_destruct__##k##__); \
        } \
        static inline void mth_tlv_init__##k##__() { \
                myth_once(&mth_tlv_key__##k##__once__, \
                          mth_tlv_init__##k##__once__); \
        } \
        static inline void mth_tlv_set__##k##__(ty const arg__) { \
                mth_tlv_init__##k##__(); \
                myth_setspecific(mth_tlv_key__##k##__, arg__); \
        } \
        static inline ty mth_tlv_get__##k##__() { \
                return myth_getspecific(mth_tlv_key__##k##__); \
        }
#define mth_tlv_init(k) (mth_tlv_init__##k##__())
#define mth_tlv_get(k) mth_tlv_get__##k##__()
#define mth_tlv_set(k,v) (mth_tlv_set__##k##__(v))

#if defined WITHOUT_MULTITHREAD
#define worker_tlv_alloc single_tlv_alloc
#define worker_tlv_init single_tlv_init
#define worker_tlv_get single_tlv_get
#define worker_tlv_set single_tlv_set
#elif defined HAVE_TLS
#define worker_tlv_alloc tls_tlv_alloc
#define worker_tlv_init tls_tlv_init
#define worker_tlv_get tls_tlv_get
#define worker_tlv_set tls_tlv_set
#else /* !WITHOUT_MULTITHREAD && !HAVE_TLS */
#define worker_tlv_alloc pth_tlv_alloc
#define worker_tlv_init pth_tlv_init
#define worker_tlv_get pth_tlv_get
#define worker_tlv_set pth_tlv_set
#endif /* !WITHOUT_MULTITHREAD && !HAVE_TLS */

#ifndef WITHOUT_MASSIVETHREADS
#define user_tlv_alloc mth_tlv_alloc
#define user_tlv_init mth_tlv_init
#define user_tlv_get mth_tlv_get
#define user_tlv_set mth_tlv_set
#else /* WITHOUT_MASSIVETHREADS */
#define user_tlv_alloc worker_tlv_alloc
#define user_tlv_init worker_tlv_init
#define user_tlv_get worker_tlv_get
#define user_tlv_set worker_tlv_set
#endif /* WITHOUT_MASSIVETHREADS */

#define worker_tlv_get_or_init(k) (worker_tlv_init(k), worker_tlv_get(k))
#define user_tlv_get_or_init(k) (user_tlv_init(k), user_tlv_get(k))


/* helpful attributes */

#ifdef __GNUC__
# define ALWAYS_INLINE __attribute__((always_inline))
# define NOINLINE __attribute__((noinline))
# define ATTR_MALLOC __attribute__((malloc))
# define ATTR_PURE __attribute__((pure))
# define ATTR_NONNULL(n) __attribute__((nonnull(n)))
# define ATTR_PRINTF(m,n) __attribute__((format(printf,m,n))) ATTR_NONNULL(m)
# define ATTR_NORETURN __attribute__((noreturn))
# define ATTR_UNUSED __attribute__((unused))
#else
# define ALWAYS_INLINE inline
# define NOINLINE
# define ATTR_MALLOC
# define ATTR_PURE
# define ATTR_NONNULL(n)
# define ATTR_PRINTF(m,n)
# define ATTR_NORETURN
# define ATTR_UNUSED
#endif /* __GNUC__ */

/*
 * The calling convention for SML# runtime primitives.
 * The SML# compiler emits call sequences compliant with this convention
 * for runtime primitive calls.
 */
#ifdef __GNUC__
/* first three arguments are passed by machine registers */
# define SML_PRIMITIVE __attribute__((regparm(3))) NOINLINE
#else
# error regparm(3) calling convention is not supported
#endif

/*
 * macros for calculating size
 */
/* the number of elements of an array. */
#define arraysize(a)    (sizeof(a) / sizeof(a[0]))
/* CEIL(x,y) : round x upwards to the nearest multiple of y. */
#define CEILING(x,y)  (((x) + (y) - 1) - ((x) + (y) - 1) % (y))

/*
 * the most conservative memory alignment.
 * It should be differed for each architecture.
 */
#ifndef MAXALIGN
# if defined HAVE_ALIGNOF && defined HAVE_MAX_ALIGN_T
#  define MAXALIGN alignof(max_align_t)
# elif defined HAVE_ALIGNOF
#  define MAXALIGN \
        alignof(union { long long n; double d; long double x; void *p; })
# else
#  define MAXALIGN \
        sizeof(union { long long n; double d; long double x; void *p; })
# endif
#endif

/*
 * print fatal error message and abort the program.
 * err : error status describing why this error happened.
 *       (0: no error status, positive: system errno, negative: runtime error)
 * format, ... : standard output format (same as printf)
 */
void sml_fatal(int err, const char *format, ...)
        ATTR_PRINTF(2, 3) ATTR_NORETURN;
/* print error message. */
void sml_error(int err, const char *format, ...) ATTR_PRINTF(2, 3);
/* print warning message. */
void sml_warn(int err, const char *format, ...) ATTR_PRINTF(2, 3);
/* print fatal error message with system error status and abort the program. */
void sml_sysfatal(const char *format, ...) ATTR_PRINTF(1, 2) ATTR_NORETURN;
/* print error message with system error status. */
void sml_syserror(const char *format, ...) ATTR_PRINTF(1, 2);
/* print warning message with system error status. */
void sml_syswarn(const char *format, ...) ATTR_PRINTF(1, 2);
/* print notice message if verbosity >= MSG_NOTICE */
void sml_notice(const char *format, ...) ATTR_PRINTF(1, 2);
/* print debug message if verbosity >= MSG_DEBUG */
void sml_debug(const char *format, ...) ATTR_PRINTF(1, 2);

void sml_msg_init(void);

/* macros for debug print */
#ifndef NDEBUG
#define DBGMSG_(fmt) "%s:%d: "fmt"%s\n", __FILE__, __LINE__
#define DBG_(fmt, ...) sml_debug(DBGMSG_(fmt), __VA_ARGS__)
#define DBG(...) DBG_(__VA_ARGS__, "")
#else
#define DBG(...) ((void)0)
#endif

/* pretty alternative to #ifndef NDEBUG ... #endif */
#ifndef NDEBUG
#define DEBUG(e) do { e; } while (0)
#else
#define DEBUG(e) do { } while (0)
#endif

/* for performance debug */
#define asm_rdtsc() ({ \
        uint32_t a__, d__; \
        __asm__ volatile ("rdtsc" : "=a" (a__), "=d" (d__)); \
        ((uint64_t)d__ << 32) | a__; \
})

/*
 * malloc with error checking
 * If allocation failed, program exits immediately.
 */
void *sml_xmalloc(size_t size) ATTR_MALLOC;
//void *sml_xrealloc(void *p, size_t size) ATTR_MALLOC;
#define xmalloc(addr,reason) sml_xmalloc(addr)
//#define xrealloc sml_xrealloc
#define xfree(addr,reason) free(addr)

/*
 * GC root set management including stack frame layouts
 */
struct sml_frame_layout {
        uint16_t num_safe_points;
        uint16_t frame_size;      /* in words */
        uint16_t num_roots;
        uint16_t root_offsets[];  /* in words */
};

void sml_gcroot(void *, void (*)(void), void *, void *);
struct sml_gcroot *sml_gcroot_load(void (* const *)(void *), unsigned int);
void sml_gcroot_unload(struct sml_gcroot *);
const struct sml_frame_layout *sml_lookup_frametable(void *retaddr);
void sml_global_enum_ptr(void (*trace)(void *, void *), void *data);
void lisp_global_enum_ptr(void (*trace)(uintptr_t, void *), void *data);

/* remove all thread-local data for SML# */
void sml_deatch(void);

/*
 * thread management
 */
void sml_control_init(void);

/* create an SML# execution context for current thread.
 * This is called when program or a callback starts.
 * Its argument is 3-pointer-size work area for SML# runtime. */
SML_PRIMITIVE void sml_start(void *);
/* destroy current SML# execution context.
 * This is called when program or a callback exits. */
SML_PRIMITIVE void sml_end(void);
/* leave current SML# excecution context temporarily.
 * This is called before calling a foreign function. */
SML_PRIMITIVE void sml_leave(void);
/* reenter current SML# excecution context.
 * This is called after returning from a foreign function. */
SML_PRIMITIVE void sml_enter(void);
/* save current frame pointer to SML# execution context for further root
 * set enumeration that would be carried out by a runtime primitive.
 * This is called before calling a primitive function that would allocate an
 * SML# object. */
SML_PRIMITIVE void sml_save(void);
/* clear the saved frame pointer by sml_save.
 * This is called after returning from an object-allocating runtime primitive
 * function. */
SML_PRIMITIVE void sml_unsave(void);
/* check collector's state and perform synchronization if needed. */
SML_PRIMITIVE void sml_check(unsigned int);
/* a flag indicating that mutators are requested to be synchronized.
 * If this is non-zero, mutators must call sml_check at their GC safe point
 * as soon as possible. */
extern _Atomic(unsigned int) sml_check_flag;
/* the main routine of garbage collection */
void sml_gc(void);

struct sml_user;
void sml_stack_enum_ptr(struct sml_user *, void (*)(void **, void *), void *);
void lisp_stack_enum_ptr(struct sml_user *, void (*)(uintptr_t, void *), void *);

int sml_set_signal_handler(void(*)(void));
int sml_send_signal(void);

enum sml_sync_phase {
        /* An even-number phases is the pre-phase of its successor phase.
         * See control_leave in control.c. */
        ASYNC = 1,
        PRESYNC1 = 2,
        SYNC1 = 3,
        PRESYNC2 = 4,
        SYNC2 = 5,
        MARK = 7
};

#if !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY
int sml_stop_the_world(void);
void sml_run_the_world(void);
#endif /* !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY */

void *sml_leave_internal(void *frame_pointer);
void sml_enter_internal(void *old_top);
void sml_check_internal(void *frame_pointer);
enum sml_sync_phase sml_current_phase(void);
int sml_saved(void); /* for debug */

SML_PRIMITIVE void sml_save_exn(void *);
SML_PRIMITIVE void *sml_unsave_exn(void *);

/*
 * stack frame address
 * FIXME: platform dependent
 */
#define CALLER_FRAME_END_ADDRESS() \
        ((void**)__builtin_frame_address(0) + 2)
#define FRAME_CODE_ADDRESS(frame_end) \
        (*((void**)(frame_end) - 1))
#define NEXT_FRAME(frame_begin) \
        ((void**)frame_begin + 1)

/*
 * SML# heap object management
 */
struct list_item {
        struct list_item *next;
};
SML_PRIMITIVE void *sml_alloc(unsigned int objsize);
SML_PRIMITIVE void *sml_load_intinf(const char *hexsrc);
SML_PRIMITIVE void **sml_find_callback(void *codeaddr, void *env);
SML_PRIMITIVE void *sml_alloc_code(void);

SML_PRIMITIVE int sml_obj_equal(void *obj1, void *obj2);
SML_PRIMITIVE void sml_write(void *objaddr, void **writeaddr, void *new_value);
void sml_copyary(void **src, unsigned int si, void **dst, unsigned int di,
                 unsigned int len);

struct sml_intinf;
typedef struct sml_intinf sml_intinf_t;

void sml_obj_enum_ptr(void *obj, void (*callback)(void **, void *), void *);
int lispobj_enum_ptr(void *obj, void (*callback)(uintptr_t, void *), void *);
void *sml_obj_alloc(unsigned int objtype, size_t payload_size);
NOINLINE char *sml_str_new(const char *str);
char *sml_str_new2(const char *str, unsigned int len);
sml_intinf_t *sml_intinf_new(void);
void *sml_intinf_hex(void *obj);

/*
 * exception support
 */
void sml_matchcomp_bug(void) ATTR_NORETURN;

SML_PRIMITIVE void sml_raise(void *exn) ATTR_NORETURN;
/*
_Unwind_Reason_Code
sml_personality(int version, _Unwind_Action actions, uint64_t exnclass,
                struct _Unwind_Exception *exception,
                struct _Unwind_Context *context);
*/

/*
 * callback support
 */
void sml_callback_init(void);
void sml_callback_destroy(void);
void sml_callback_enum_ptr(void (*trace)(void **, void *), void *data);
SML_PRIMITIVE void **sml_find_callback(void *codeaddr, void *env);
SML_PRIMITIVE void *sml_alloc_code(void);

/*
 * finalizer support
 */
void sml_finalize_init(void);
void sml_finalize_destroy(void);
void sml_set_finalizer(void *obj, void (*finalizer)(void *));
void sml_run_finalizer(void);

/*
 * Initialize and finalize SML# runtime
 */
void sml_init(int argc, char **argv);
void sml_finish(void);
ATTR_NORETURN void sml_exit(int status);

/*
 * bit pointer
 */
#include "alloc_ptr.h"
#define BITPTR_WORDBITS  32U
#define BITPTR(p,n) \
        ((sml_bitptr_t){.ptr = (p) + (n) / 32U, .mask = 1 << ((n) % 32U)})
#define BITPTRW(p,n) \
        ((sml_bitptrw_t){.wptr = (p) + (n) / 32U, .mask = 1 << ((n) % 32U)})

#define BITPTRW_TEST(b)  (*(b).wptr & (b).mask)
#define BITPTRW_SET(b)  (*(b).wptr |= (b).mask)
#define BITPTRW_UNSET(b)  (*(b).wptr &= ~(b).mask)
#define BITPTRW_WORD(b)  (*(b).wptr)
#define BITPTRW_EQUAL(b1,b2)  ((b1).wptr == (b2).wptr && (b1).mask == (b2).mask)
#define BITPTR_TEST(b)  (*(b).ptr & (b).mask)
#define BITPTR_WORD(b)  (*(b).ptr)
//#define BITPTR_EQUAL(b1,b2)  ((b1).ptr == (b2).ptr && (b1).mask == (b2).mask)
#define BITPTR_WORDINDEX(b,begin)  ((b).ptr - (begin))
//#define BITPTR_NEXTWORD(b)  ((b).ptr++, (b).mask = 1U)

/* BITPTR_NEXT0: move to next 0 bit in the current word.
 * mask becomes zero if failed. */
#define BITPTR_NEXT0(b) do { \
        uint32_t tmp__ = *(b).ptr | ((b).mask - 1U); \
        (b).mask = (tmp__ + 1U) & ~tmp__; \
} while (0)
#define BITPTR_NEXT_FAILED(b)  ((b).mask == 0)

/* BITPTR_NEXT1: move to next 1 bit in the current word.
 * mask becomes zero if failed. */
#define BITPTR_NEXT1(b) do { \
        uint32_t tmp__ = *(b).ptr & -((b).mask << 1); \
        (b).mask = tmp__ & -tmp__; \
} while (0)

/* BITPTR_INC: move to the next bit */
#define BITPTR_INC(b) do { \
        (b).ptr += ((b).mask >> 31); \
        (b).mask = ((b).mask << 1) | ((b).mask >> 31); \
} while (0)

/* BITPTR_MASKINDEX: returns the bit index of the mask */
#define BITPTR_MASKINDEX(b) __builtin_ctz((b).mask)

/* BITPTR_INDEX: returns the bit offset of bitptr b from p */
#define BITPTR_INDEX(b,p) \
        (BITPTR_WORDINDEX(b,p) * BITPTR_WORDBITS + BITPTR_MASKINDEX(b))

/* CEIL_LOG2: ceiling the given integer x to the smallest 2^i larger than x.
 * x must not be 1. */
#define CEIL_LOG2(x)  (32 - __builtin_clz((uint32_t)(x) - 1))

/*
 * memory page allocation
 */
#ifdef MINGW32
/* include <windows.h> */
#define GetPageSize() ({ SYSTEM_INFO si; GetSystemInfo(&si); si.dwPageSize; })
#define AllocPageError NULL
#define AllocPage(addr, size) \
        VirtualAlloc(addr, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE)
#define ReservePage(addr, size)        \
        VirtualAlloc(addr, size, MEM_RESERVE, PAGE_NOACCESS)
#define ReleasePage(addr, size) \
        VirtualFree(addr, size, MEM_RELEASE)
#define CommitPage(addr, size) \
        VirtualAlloc(addr, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE)
#define UncommitPage(addr, size) \
        VirtualFree(addr, size, MEM_DECOMMIT)
#else
#include <sys/mman.h>
/* inclue <unistd.h> */
void *wrapped_mmap(void *addr, size_t length, int prot, int flags, int fd, long offset);
int wrapped_munmap(void *addr, size_t length);
int wrapped_mprotect(void *addr, size_t len, int prot);
#define GetPageSize() getpagesize()
#define AllocPageError MAP_FAILED
#define AllocPage(addr, size) \
        mmap(addr, size, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0)
#define ReservePage(addr, size) \
        mmap(addr, size, PROT_NONE, MAP_ANON | MAP_PRIVATE, -1, 0)
#define ReleasePage(addr, size) \
        munmap(addr, size)
#define CommitPage(addr, size) \
        mprotect(addr, size, PROT_EXEC | PROT_READ | PROT_WRITE)
#define UncommitPage(addr, size) \
        mmap(addr, size, PROT_NONE, MAP_ANON | MAP_PRIVATE | MAP_FIXED, -1, 0)
#endif /* MINGW32 */

void trace_enter(char*);
void trace_leave(char*);
void trace_leaf(char*);
extern int gc_verbose;
extern void hexdump_sml_heap_to_file(char*);

extern int ignorable_space_p(uintptr_t);
extern int large_code_subspace_p(char*);
//extern void* untagged_baseptr(uintptr_t);
extern void* otherptr_mseg(uintptr_t);

extern int smlgc_verbose;
extern void ldb_monitor();

#endif /* SMLSHARP__SMLSHARP_H__ */
