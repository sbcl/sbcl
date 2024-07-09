#if !defined(_INCLUDE_THREAD_H_)
#define _INCLUDE_THREAD_H_

#include <sys/types.h>
#include <stdbool.h>
#include <unistd.h>
#include <stddef.h>
#include "genesis/sbcl.h"
#include "globals.h"
#include "runtime.h"
#include "os.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include "genesis/thread.h"
#include "genesis/vector.h"
#include "interrupt.h"
#include "validate.h"           /* for BINDING_STACK_SIZE etc */
#include "gc-typedefs.h" // for page_index_t

enum threadstate {STATE_RUNNING=1, STATE_STOPPED, STATE_DEAD};

#ifdef LISP_FEATURE_SB_THREAD
void set_thread_state(struct thread *thread, char state, bool);
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
    int arena_count; // number of structures in arena_saveareas
    arena_state* arena_savearea;
    // These values influence get_alloc_start_page() when arenas are in use
    // and allocation switches back and forth between arena and heap.
    page_index_t mixed_page_hint;
    page_index_t cons_page_hint;
};
#define thread_extra_data(thread) \
  ((struct extra_thread_data*)((char*)(thread) + dynamic_values_bytes))
#define nth_interrupt_context(n,thread) thread_extra_data(thread)->sigcontexts[n]
#define thread_interrupt_data(thread) thread_extra_data(thread)->interrupt_data

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
# define ASSIGN_CURRENT_THREAD(dummy)
#elif defined LISP_FEATURE_GCC_TLS
# define ASSIGN_CURRENT_THREAD(x) current_thread = x
#elif !defined LISP_FEATURE_WIN32
# define ASSIGN_CURRENT_THREAD(x) pthread_setspecific(current_thread, x)
#else
# define ASSIGN_CURRENT_THREAD(x) TlsSetValue(OUR_TLS_INDEX, x)
#endif

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
# ifdef LISP_FEATURE_GCC_TLS
extern __thread struct thread *current_thread;
# elif !defined LISP_FEATURE_WIN32
extern pthread_key_t current_thread;
#endif
#endif

#ifndef LISP_FEATURE_SB_SAFEPOINT
# define THREAD_CSP_PAGE_SIZE 0
#else
# define THREAD_CSP_PAGE_SIZE os_reported_page_size
#endif

#ifdef LISP_FEATURE_WIN32
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
    // Refer to the picture in the comment above alloc_thread_struct().
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
    th = pthread_getspecific(current_thread);
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
    return pthread_getspecific(current_thread) != NULL;
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
extern char* vm_thread_name(struct thread*);

extern void record_backtrace_from_context(void*,struct thread*);

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
#define THREAD_JIT_WP(x) pthread_jit_write_protect_np((x))
#else
#define THREAD_JIT_WP(x)
#endif

extern bool is_in_stack_space(lispobj);
extern void scrub_control_stack(void);
extern void scrub_thread_control_stack(struct thread *);
extern void scavenge_control_stack(struct thread *th);
extern void scavenge_interrupt_contexts(struct thread *thread);
extern void gc_close_thread_regions(struct thread*, int);

#endif /* _INCLUDE_THREAD_H_ */
