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
#ifdef LISP_FEATURE_WIN32
#include "win32-thread-private-events.h"
#endif
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"

#include "genesis/thread.h"
#include "genesis/fdefn.h"
#include "interrupt.h"
#include "validate.h"           /* for BINDING_STACK_SIZE etc */

#define STATE_RUNNING MAKE_FIXNUM(1)
#define STATE_STOPPED MAKE_FIXNUM(2)
#define STATE_DEAD MAKE_FIXNUM(3)
#if defined(LISP_FEATURE_SB_SAFEPOINT)
# define STATE_SUSPENDED_BRIEFLY MAKE_FIXNUM(4)
# define STATE_GC_BLOCKER MAKE_FIXNUM(5)
# define STATE_PHASE1_BLOCKER MAKE_FIXNUM(5)
# define STATE_PHASE2_BLOCKER MAKE_FIXNUM(6)
# define STATE_INTERRUPT_BLOCKER MAKE_FIXNUM(7)
#endif

#ifdef LISP_FEATURE_SB_THREAD
lispobj thread_state(struct thread *thread);
void set_thread_state(struct thread *thread, lispobj state);
void wait_for_thread_state_change(struct thread *thread, lispobj state);

#if defined(LISP_FEATURE_SB_SAFEPOINT)
enum threads_suspend_reason {
    SUSPEND_REASON_NONE,
    SUSPEND_REASON_GC,
    SUSPEND_REASON_INTERRUPT,
    SUSPEND_REASON_GCING
};

struct threads_suspend_info {
    int suspend;
    pthread_mutex_t world_lock;
    pthread_mutex_t lock;
    enum threads_suspend_reason reason;
    int phase;
    struct thread * gc_thread;
    struct thread * interrupted_thread;
    int blockers;
    int used_gc_page;
};

struct suspend_phase {
    int suspend;
    enum threads_suspend_reason reason;
    int phase;
    struct suspend_phase *next;
};

extern struct threads_suspend_info suspend_info;

struct gcing_safety {
    lispobj csp_around_foreign_call;
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    lispobj* pc_around_foreign_call;
#endif
};

int handle_safepoint_violation(os_context_t *context, os_vm_address_t addr);
void** os_get_csp(struct thread* th);
void alloc_gc_page();
void assert_on_stack(struct thread *th, void *esp);
#endif /* defined(LISP_FEATURE_SB_SAFEPOINT) */

extern pthread_key_t lisp_thread;
#endif

extern int kill_safely(os_thread_t os_thread, int signal);

#define THREAD_SLOT_OFFSET_WORDS(c) \
 (offsetof(struct thread,c)/(sizeof (struct thread *)))

union per_thread_data {
    struct thread thread;
    lispobj dynamic_values[1];  /* actually more like 4000 or so */
};

/* The thread struct is generated from lisp during genesis and it
 * needs to know the sizes of all its members, but some types may have
 * arbitrary lengths, thus the pointers are stored instead. This
 * structure is used to help allocation of those types, so that the
 * pointers can be later shoved into the thread struct. */
struct nonpointer_thread_data
{
#if defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_SB_SAFEPOINT)
    os_sem_t state_sem;
    os_sem_t state_not_running_sem;
    os_sem_t state_not_stopped_sem;
#endif
};

extern struct thread *all_threads;
extern int dynamic_values_bytes;

#if defined(LISP_FEATURE_DARWIN)
#define CONTROL_STACK_ALIGNMENT_BYTES 8192 /* darwin wants page-aligned stacks */
#define THREAD_ALIGNMENT_BYTES CONTROL_STACK_ALIGNMENT_BYTES
#else
#define THREAD_ALIGNMENT_BYTES BACKEND_PAGE_BYTES
#define CONTROL_STACK_ALIGNMENT_BYTES 16
#endif


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
static inline int
tls_index_of(struct symbol *symbol) // untagged pointer
{
  return symbol->header >> 32;
}
#else
#  define tls_index_of(x) (x->tls_index)
#endif
#define per_thread_value(sym,th) \
  ((union per_thread_data *)th)->dynamic_values[tls_index_of(sym)>>WORD_SHIFT]
#endif

static inline lispobj *
SymbolValueAddress(u64 tagged_symbol_pointer, void *thread)
{
    struct symbol *sym= SYMBOL(tagged_symbol_pointer);
    if(thread && tls_index_of(sym)) {
        lispobj *r = &per_thread_value(sym, thread);
        if((*r)!=NO_TLS_VALUE_MARKER_WIDETAG) return r;
    }
    return &sym->value;
}

static inline lispobj
SymbolValue(u64 tagged_symbol_pointer, void *thread)
{
    struct symbol *sym= SYMBOL(tagged_symbol_pointer);
    if(thread && tls_index_of(sym)) {
        lispobj r = per_thread_value(sym, thread);
        if(r!=NO_TLS_VALUE_MARKER_WIDETAG) return r;
    }
    return sym->value;
}

static inline void
SetSymbolValue(u64 tagged_symbol_pointer,lispobj val, void *thread)
{
    struct symbol *sym= SYMBOL(tagged_symbol_pointer);
    if(thread && tls_index_of(sym)) {
        if (per_thread_value(sym, thread) != NO_TLS_VALUE_MARKER_WIDETAG) {
            per_thread_value(sym, thread) = val;
            return;
        }
    }
    sym->value = val;
}

static inline lispobj
SymbolTlValue(u64 tagged_symbol_pointer, void *thread)
{
    struct symbol *sym= SYMBOL(tagged_symbol_pointer);
    return per_thread_value(sym, thread);
}

static inline void
SetTlSymbolValue(u64 tagged_symbol_pointer,lispobj val, void *thread)
{
    struct symbol *sym= SYMBOL(tagged_symbol_pointer);
    // dynbind asserts that there is a tls_index in multithread runtime
    per_thread_value(sym, thread) = val;
}

/* This only works for static symbols. */
static inline lispobj
StaticSymbolFunction(lispobj sym)
{
    return ((struct fdefn *)native_pointer(SymbolValue(sym, 0)))->fun;
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

#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_GCC_TLS)
extern __thread struct thread *current_thread;
#endif

#ifndef LISP_FEATURE_SB_SAFEPOINT
# define THREAD_CSP_PAGE_SIZE 0
#elif defined(LISP_FEATURE_PPC)
  /* BACKEND_PAGE_BYTES is nice and large on this platform, but therefore
   * does not fit into an immediate, making it awkward to access the page
   * relative to the thread-tn... */
# define THREAD_CSP_PAGE_SIZE 4096
#else
# define THREAD_CSP_PAGE_SIZE BACKEND_PAGE_BYTES
#endif

#ifdef LISP_FEATURE_WIN32
/*
 * Win32 doesn't have SIGSTKSZ, and we're not switching stacks anyway,
 * so define it arbitrarily
 */
#define SIGSTKSZ 1024
#endif

#define THREAD_STRUCT_SIZE (thread_control_stack_size + BINDING_STACK_SIZE + \
                            ALIEN_STACK_SIZE +                          \
                            sizeof(struct nonpointer_thread_data) +     \
                            dynamic_values_bytes +                      \
                            32 * SIGSTKSZ +                             \
                            THREAD_ALIGNMENT_BYTES +                    \
                            THREAD_CSP_PAGE_SIZE)

#if defined(LISP_FEATURE_WIN32)
static inline struct thread* arch_os_get_current_thread()
    __attribute__((__const__));
#endif

/* This is clearly per-arch and possibly even per-OS code, but we can't
 * put it somewhere sensible like x86-linux-os.c because it needs too
 * much stuff like struct thread and all_threads to be defined, which
 * usually aren't by that time.  So, it's here instead.  Sorry */

static inline struct thread *arch_os_get_current_thread(void)
{
#if !defined(LISP_FEATURE_SB_THREAD)
     return all_threads;

#elif defined(LISP_FEATURE_X86) && defined(LISP_FEATURE_WIN32)
    register struct thread *me=0;
    __asm__ volatile ("movl %%fs:0xE10+(4*63), %0" : "=r"(me) :);
    return me;

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

# if defined(LISP_FEATURE_RESTORE_FS_SEGMENT_REGISTER_FROM_TLS)
    /* If enabled by make-config (currently Darwin and FreeBSD only),
     * re-setup %fs.  This is an out-of-line call, and potentially
     * expensive.*/
    if (th) {
        void arch_os_load_ldt(struct thread*);
        arch_os_load_ldt(th);
    }
# endif

    return th;
#endif
}

#if defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
extern kern_return_t mach_lisp_thread_init(struct thread *thread);
extern kern_return_t mach_lisp_thread_destroy(struct thread *thread);
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
void thread_pitstop(os_context_t *ctxptr);
extern void thread_register_gc_trigger();

# ifdef LISP_FEATURE_SB_THRUPTION
int wake_thread(os_thread_t os_thread);
#  ifdef LISP_FEATURE_WIN32
void wake_thread_win32(struct thread *thread);
#  else
int wake_thread_posix(os_thread_t os_thread);
#  endif
# endif

static inline
void push_gcing_safety(struct gcing_safety *into)
{
    struct thread* th = arch_os_get_current_thread();
    asm volatile ("");
    if ((into->csp_around_foreign_call =
         *th->csp_around_foreign_call)) {
        *th->csp_around_foreign_call = 0;
        asm volatile ("");
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        into->pc_around_foreign_call = th->pc_around_foreign_call;
        th->pc_around_foreign_call = 0;
        asm volatile ("");
#endif
    } else {
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        into->pc_around_foreign_call = 0;
#endif
    }
}

static inline
void pop_gcing_safety(struct gcing_safety *from)
{
    struct thread* th = arch_os_get_current_thread();
    if (from->csp_around_foreign_call) {
        asm volatile ("");
        *th->csp_around_foreign_call = from->csp_around_foreign_call;
        asm volatile ("");
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        th->pc_around_foreign_call = from->pc_around_foreign_call;
        asm volatile ("");
#endif
    }
}

/* Even with just -O1, gcc optimizes the jumps in this "loop" away
 * entirely, giving the ability to define WITH-FOO-style macros. */
#define RUN_BODY_ONCE(prefix, finally_do)               \
    int prefix##done = 0;                               \
    for (; !prefix##done; finally_do, prefix##done = 1)

#define WITH_GC_AT_SAFEPOINTS_ONLY_hygenic(var)        \
    struct gcing_safety var;                    \
    push_gcing_safety(&var);                    \
    RUN_BODY_ONCE(var, pop_gcing_safety(&var))

#define WITH_GC_AT_SAFEPOINTS_ONLY()                           \
    WITH_GC_AT_SAFEPOINTS_ONLY_hygenic(sbcl__gc_safety)

#define WITH_STATE_SEM_hygenic(var, thread)                             \
    os_sem_wait((thread)->state_sem, "thread_state");                   \
    RUN_BODY_ONCE(var, os_sem_post((thread)->state_sem, "thread_state"))

#define WITH_STATE_SEM(thread)                                     \
    WITH_STATE_SEM_hygenic(sbcl__state_sem, thread)

int check_pending_thruptions(os_context_t *ctx);

void attach_os_thread(init_thread_data *);
void detach_os_thread(init_thread_data *);

# if defined(LISP_FEATURE_SB_SAFEPOINT_STRICTLY) && !defined(LISP_FEATURE_WIN32)

void signal_handler_callback(lispobj, int, void *, void *);
# endif

#endif

extern void create_initial_thread(lispobj);

#ifdef LISP_FEATURE_SB_THREAD
extern pthread_mutex_t all_threads_lock;
#endif

#endif /* _INCLUDE_THREAD_H_ */
