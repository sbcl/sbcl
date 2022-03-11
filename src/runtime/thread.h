#if !defined(_INCLUDE_THREAD_H_)
#define _INCLUDE_THREAD_H_

#include <sys/types.h>
#include <unistd.h>
#include <stddef.h>
#include "sbcl.h"
#include "globals.h"
#include "runtime.h"
#include "os.h"
#ifdef LISP_FEATURE_GENCGC
#include "gencgc-alloc-region.h"
#endif
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"

struct thread_state_word {
  // - control_stack_guard_page_protected is referenced from
  //   hand-written assembly code. (grep "THREAD_STATE_WORD_OFFSET")
  // - sprof_enable is referenced with SAPs.
  //   (grep "sb-vm:thread-state-word-slot")
  char control_stack_guard_page_protected;
  char sprof_enable; // statistical CPU profiler switch
  char state;
  char user_thread_p; // opposite of lisp's ephemeral-p
#ifdef LISP_FEATURE_64_BIT
  char padding[4];
#endif
};

// (DEFCONSTANT +N-SMALL-BUCKETS+ 32)
typedef lispobj size_histogram[32+N_WORD_BITS];

#include "genesis/thread.h"
#include "genesis/thread-instance.h"
#include "genesis/fdefn.h"
#include "genesis/vector.h"
#include "interrupt.h"
#include "validate.h"           /* for BINDING_STACK_SIZE etc */

enum threadstate {STATE_RUNNING=1, STATE_STOPPED, STATE_DEAD};

#ifdef LISP_FEATURE_SB_THREAD
void set_thread_state(struct thread *thread, char state, boolean);
int thread_wait_until_not(int state, struct thread *thread);
#endif

#if defined(LISP_FEATURE_SB_SAFEPOINT)
struct gcing_safety {
    lispobj csp_around_foreign_call;
};

int handle_safepoint_violation(os_context_t *context, os_vm_address_t addr);
void* os_get_csp(struct thread* th);
void assert_on_stack(struct thread *th, void *esp);
#endif /* defined(LISP_FEATURE_SB_SAFEPOINT) */

/* The thread struct is generated from lisp during genesis and it
 * needs to know the sizes of all its members, but some types may have
 * arbitrary lengths, thus the pointers are stored instead. This
 * structure is used to help allocation of those types.
 * This structure is located at an address computable based on a 'struct thread'
 * so there is no need for a pointer from one to the other */
struct extra_thread_data
{
    // Lisp needs to be able to access this array. KEEP IT AS THE FIRST FIELD!
    os_context_t* sigcontexts[MAX_INTERRUPTS];
#ifdef LISP_FEATURE_GC_METRICS
    long on_cpu_time;
    long avg_gc_wait;
    long worst_gc_wait;
    long n_gc_wait;
    long sum_gc_wait;
#endif

    // Data from here down are never looked at from Lisp.
    struct interrupt_data interrupt_data;
#if defined LISP_FEATURE_SB_THREAD && !defined LISP_FEATURE_SB_SAFEPOINT
    // 'state_sem' is a binary semaphore used just like a mutex.
    // I guess we figure that semaphores are OK to use in signal handlers (which is
    // technically false), whereas a mutex would be more certainly wrong?
    os_sem_t state_sem;
    // These are basically "gates" a la SB-CONCURRENCY:GATE. They might be better
    // as condition variables, but condvars are not allowed in signal handlers.
    // Strictly speaking, sem_wait isn't either, but it seems to work.
    os_sem_t state_not_running_sem;
    os_sem_t state_not_stopped_sem;
    // We count waiters on each gate to know how many times to sem_post() to open them.
    // The counts themselves are protected against concurrent access by 'state_sem'.
    // Since we're not constrained by compiler/generic/objdef any more, we can
    // make these "only" 4 bytes each, instead of lispwords.
    uint32_t state_not_running_waitcount;
    uint32_t state_not_stopped_waitcount;
#endif
#if defined LISP_FEATURE_SB_THREAD && defined LISP_FEATURE_UNIX
    // According to https://github.com/adrienverge/openfortivpn/issues/105
    //   "using GCD semaphore in signal handlers is documented to be unsafe"
    // which seems almost impossible to believe, considering that sem_t is
    // documented to be safe, yet the sem_ functions produce a warning:
    //  warning: 'sem_init' is deprecated [-Wdeprecated-declarations]
    // So how could there be no signal-safe replacement?
    os_sem_t sprof_sem;
#endif
    int sprof_lock;
#ifdef LISP_FEATURE_WIN32
    // these are different from the masks that interrupt_data holds
    sigset_t pending_signal_set;
    sigset_t blocked_signal_set;
#define NUM_PRIVATE_EVENTS 2
#define thread_private_events(th,i) thread_extra_data(th)->private_events[i]
    HANDLE private_events[NUM_PRIVATE_EVENTS];
    // Context base pointer for running on top of system libraries built using
    // -fomit-frame-pointer.  Currently truly required and implemented only
    // for (and win32 x86-64),
    os_context_register_t carried_base_pointer;
    HANDLE synchronous_io_handle_and_flag;
    void* waiting_on_address; // used only if #+sb-futex
#endif
};
#define thread_extra_data(thread) \
  ((struct extra_thread_data*)((char*)(thread) + dynamic_values_bytes))
#define nth_interrupt_context(n,thread) thread_extra_data(thread)->sigcontexts[n]
#define thread_interrupt_data(thread) thread_extra_data(thread)->interrupt_data

extern struct thread *all_threads;
extern int dynamic_values_bytes;

#define THREAD_ALIGNMENT_BYTES BACKEND_PAGE_BYTES
#define CONTROL_STACK_ALIGNMENT_BYTES BACKEND_PAGE_BYTES

#ifdef LISP_FEATURE_SB_THREAD
#define for_each_thread(th) for(th=all_threads;th;th=th->next)
#else
/* there's some possibility a SSC could notice this never actually
 * loops  */
#define for_each_thread(th) for(th=all_threads;th;th=0)
#endif

#ifndef LISP_FEATURE_SB_THREAD
/* no threads: every symbol's tls_index is statically zero */
#  define tls_index_of(x) 0
#  define per_thread_value(sym, thread) sym->value
#else
#ifdef LISP_FEATURE_64_BIT
static inline unsigned int
tls_index_of(struct symbol *symbol) // untagged pointer
{
#ifdef LISP_FEATURE_X86_64
  return ((unsigned int*)symbol)[1];
#else
  return symbol->header >> 32;
#endif
}
#else
#  define tls_index_of(x) (x)->tls_index
#endif
#  define per_thread_value(sym,th) *(lispobj*)(tls_index_of(sym) + (char*)th)
#endif

static inline lispobj
SymbolValue(lispobj tagged_symbol_pointer, void *thread)
{
    struct symbol *sym = SYMBOL(tagged_symbol_pointer);
    if(thread && tls_index_of(sym)) {
        lispobj r = per_thread_value(sym, thread);
        if(r!=NO_TLS_VALUE_MARKER_WIDETAG) return r;
    }
    return sym->value;
}

static inline void
SetSymbolValue(lispobj tagged_symbol_pointer,lispobj val, void *thread)
{
    struct symbol *sym = SYMBOL(tagged_symbol_pointer);
    if(thread && tls_index_of(sym)) {
        if (per_thread_value(sym, thread) != NO_TLS_VALUE_MARKER_WIDETAG) {
            per_thread_value(sym, thread) = val;
            return;
        }
    }
    sym->value = val;
}

#ifdef LISP_FEATURE_SB_THREAD
/* write_TLS assigns directly into TLS causing the symbol to
 * be thread-local without saving a prior value on the binding stack. */
# define write_TLS(sym, val, thread) write_TLS_index(sym##_tlsindex, val, thread, _ignored_)
# define write_TLS_index(index, val, thread, sym) \
   *(lispobj*)(index + (char*)thread) = val
# define read_TLS(sym, thread) *(lispobj*)(sym##_tlsindex + (char*)thread)
#else
# define write_TLS(sym, val, thread) SYMBOL(sym)->value = val
# define write_TLS_index(index, val, thread, sym) sym->value = val
# define read_TLS(sym, thread) SYMBOL(sym)->value
#endif

// FIXME: very random that this is defined in 'thread.h'
#define StaticSymbolFunction(x) FdefnFun(x##_FDEFN)
/* Return 'fun' given a tagged pointer to an fdefn. */
static inline lispobj FdefnFun(lispobj fdefn)
{
    return FDEFN(fdefn)->fun;
}

/* These are for use during GC, on the current thread, or on prenatal
 * threads only. */
#if defined(LISP_FEATURE_SB_THREAD)
#define get_binding_stack_pointer(thread)       \
    ((thread)->binding_stack_pointer)
#define set_binding_stack_pointer(thread,value) \
    ((thread)->binding_stack_pointer = (lispobj *)(value))
#define access_control_stack_pointer(thread) \
    ((thread)->control_stack_pointer)
#  if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
#define access_control_frame_pointer(thread) \
    ((thread)->control_frame_pointer)
#  endif
#else
#  if defined(BINDING_STACK_POINTER)
#define get_binding_stack_pointer(thread)       \
    SymbolValue(BINDING_STACK_POINTER, thread)
#define set_binding_stack_pointer(thread,value) \
    SetSymbolValue(BINDING_STACK_POINTER, (lispobj)(value), thread)
#  else
#define get_binding_stack_pointer(thread)       \
    (current_binding_stack_pointer)
#define set_binding_stack_pointer(thread,value) \
    (current_binding_stack_pointer = (lispobj *)(value))
#  endif
#define access_control_stack_pointer(thread)    \
    (current_control_stack_pointer)
#  if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
#define access_control_frame_pointer(thread) \
    (current_control_frame_pointer)
#  endif
#endif

#ifdef LISP_FEATURE_SB_THREAD
// FIXME: these names should be consistent with one another
# ifdef LISP_FEATURE_GCC_TLS
extern __thread struct thread *current_thread;
# elif !defined LISP_FEATURE_WIN32
extern pthread_key_t specials;
#endif
#endif

#ifndef LISP_FEATURE_SB_SAFEPOINT
# define THREAD_CSP_PAGE_SIZE 0
#else
# define THREAD_CSP_PAGE_SIZE os_reported_page_size
#endif

#if defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
#define ALT_STACK_SIZE 0
#else
#define ALT_STACK_SIZE 32 * SIGSTKSZ
#endif

/* As a helpful reminder of how this calculation arises, the summands should
 * correspond, in the correct order, to the picture in thread.c */
#define THREAD_STRUCT_SIZE \
  (THREAD_ALIGNMENT_BYTES + \
   thread_control_stack_size + BINDING_STACK_SIZE + ALIEN_STACK_SIZE + \
   THREAD_CSP_PAGE_SIZE + \
   (THREAD_HEADER_SLOTS*N_WORD_BYTES) + dynamic_values_bytes + \
   sizeof (struct extra_thread_data) + ALT_STACK_SIZE)

/* sigaltstack() - "Signal stacks are automatically adjusted
 * for the direction of stack growth and alignment requirements." */
static inline void* calc_altstack_base(struct thread* thread) {
    // Refer to the picture in the comment above create_thread_struct().
    // Always return the lower limit as the base even if stack grows down.
    return ((char*) thread) + dynamic_values_bytes
        + ALIGN_UP(sizeof (struct extra_thread_data), N_WORD_BYTES);
}
static inline void* calc_altstack_end(struct thread* thread) {
    return (char*)thread->os_address + THREAD_STRUCT_SIZE;
}
static inline int calc_altstack_size(struct thread* thread) {
    // 'end' is calculated as exactly the end address we got from the OS.
    // The usually ends up making the stack slightly larger than ALT_STACK_SIZE
    // bytes due to the addition of THREAD_ALIGNMENT_BYTES of padding.
    // If the memory was as aligned as we'd like, the padding is ours to keep.
    return (char*)calc_altstack_end(thread) - (char*)calc_altstack_base(thread);
}
#if defined(LISP_FEATURE_WIN32)
static inline struct thread* get_sb_vm_thread()
    __attribute__((__const__));
int sb_pthr_kill(struct thread* thread, int signum);
#endif

/* This is clearly per-arch and possibly even per-OS code, but we can't
 * put it somewhere sensible like x86-linux-os.c because it needs too
 * much stuff like struct thread and all_threads to be defined, which
 * usually aren't by that time.  So, it's here instead.  Sorry */

static inline struct thread *get_sb_vm_thread(void)
{
#if !defined(LISP_FEATURE_SB_THREAD)
     return all_threads;

#elif defined(LISP_FEATURE_X86) && defined(LISP_FEATURE_WIN32)
    // FIXME: why not use TlsGetvalue(OUR_TLS_INDEX) here?
    register struct thread *me=0;
    __asm__ volatile ("movl %%fs:0xE10+(4*63), %0" : "=r"(me) :);
    return me;

#elif defined LISP_FEATURE_WIN32
    return (struct thread*)TlsGetValue(OUR_TLS_INDEX);

#else

# if defined(LISP_FEATURE_X86)
    if (!all_threads) return 0;
#endif

    /* Otherwise, use pthreads to find the right value.  We do not load
     * directly from %fs:this even on x86 platforms (like Linux and
     * Solaris) with dependable %fs, because we want to return NULL if
     * called by a non-Lisp thread, and %fs would not be initialized
     * suitably in that case. */
    struct thread *th;
# ifdef LISP_FEATURE_GCC_TLS
    th = current_thread;
# else
    th = pthread_getspecific(specials);
# endif

# if defined LISP_FEATURE_X86 && (defined LISP_FEATURE_DARWIN || defined LISP_FEATURE_FREEBSD)
    /* Restore the %FS register. This is potentially an "expensive" call.
     * It rightfully belongs with RESTORE_FP_CONTROL_WORD, but I don't care to try it. */
    if (th) {
        void arch_os_load_ldt(struct thread*);
        arch_os_load_ldt(th);
    }
# endif

    return th;
#endif
}

inline static int lisp_thread_p(os_context_t __attribute__((unused)) *context) {
#ifdef LISP_FEATURE_SB_THREAD
# ifdef LISP_FEATURE_GCC_TLS
    return current_thread != 0;
# elif defined LISP_FEATURE_WIN32
    return TlsGetValue(OUR_TLS_INDEX) != 0;
# else
    return pthread_getspecific(specials) != NULL;
# endif
#elif defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
    char *csp = (char *)*os_context_sp_addr(context);
    return (char *)all_threads->control_stack_start < csp &&
        (char *)all_threads->control_stack_end > (char *) csp;
#else
    /* Can't really tell since pthreads are required to get the
       dimensions of the C stack. */
    return 1;
#endif
}

extern void record_backtrace_from_context(void*,struct thread*);

#if defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
extern kern_return_t mach_lisp_thread_init(struct thread *thread);
extern void mach_lisp_thread_destroy(struct thread *thread);
#endif

typedef struct init_thread_data {
    sigset_t oldset;
#ifdef LISP_FEATURE_SB_SAFEPOINT
    struct gcing_safety safety;
#endif
} init_thread_data;

#ifdef LISP_FEATURE_SB_SAFEPOINT
void thread_in_safety_transition(os_context_t *ctx);
void thread_in_lisp_raised(os_context_t *ctx);
void thread_interrupted(os_context_t *ctx);
extern void thread_register_gc_trigger();

# ifdef LISP_FEATURE_SB_SAFEPOINT
void wake_thread(struct thread_instance*),
     wake_thread_impl(struct thread_instance*);
# endif

#define csp_around_foreign_call(thread) *(((lispobj*)thread)-(1+THREAD_HEADER_SLOTS))

static inline
void push_gcing_safety(struct gcing_safety *into)
{
    struct thread* th = get_sb_vm_thread();
    asm volatile ("");
    into->csp_around_foreign_call = csp_around_foreign_call(th);
    csp_around_foreign_call(th) = 0;
    asm volatile ("");
}

static inline
void pop_gcing_safety(struct gcing_safety *from)
{
    struct thread* th = get_sb_vm_thread();
    asm volatile ("");
    csp_around_foreign_call(th) = from->csp_around_foreign_call;
    asm volatile ("");
}

#define WITH_GC_AT_SAFEPOINTS_ONLY_hygenic(var)        \
    struct gcing_safety var;                    \
    push_gcing_safety(&var);                    \
    RUN_BODY_ONCE(var, pop_gcing_safety(&var))

#define WITH_GC_AT_SAFEPOINTS_ONLY()                           \
    WITH_GC_AT_SAFEPOINTS_ONLY_hygenic(sbcl__gc_safety)

int check_pending_thruptions(os_context_t *ctx);

#else
#define WITH_GC_AT_SAFEPOINTS_ONLY()
#endif

extern void create_main_lisp_thread(lispobj);

#ifdef LISP_FEATURE_WIN32
extern CRITICAL_SECTION all_threads_lock;
#elif defined LISP_FEATURE_SB_THREAD
extern pthread_mutex_t all_threads_lock;
#endif

#ifndef LISP_FEATURE_SB_THREAD
// Put in an empty conversion to avoid warning at the point of use:
// "warning: too many arguments for format [-Wformat-extra-args]"
# define THREAD_ID_LABEL "%s"
# define THREAD_ID_VALUE ""
#elif defined LISP_FEATURE_WIN32
# define THREAD_ID_LABEL "%ld"
# define THREAD_ID_VALUE (GetCurrentThreadId())
#elif defined __linux__
extern int sb_GetTID();
# define THREAD_ID_LABEL " tid %d"
# define THREAD_ID_VALUE (sb_GetTID())
#else
# define THREAD_ID_LABEL " pthread %p"
# define THREAD_ID_VALUE ((void*)thread_self())
#endif

#ifdef LISP_FEATURE_DARWIN_JIT
#define THREAD_JIT(x) pthread_jit_write_protect_np((x))
#else
#define THREAD_JIT(x)
#endif
#endif /* _INCLUDE_THREAD_H_ */
