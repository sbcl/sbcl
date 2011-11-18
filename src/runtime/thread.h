#if !defined(_INCLUDE_THREAD_H_)
#define _INCLUDE_THREAD_H_

#include <sys/types.h>
#include <unistd.h>
#include <stddef.h>
#include <errno.h>
#include <stdio.h>
#include "sbcl.h"
#include "globals.h"
#include "runtime.h"
#include "os.h"
#ifdef LISP_FEATURE_GENCGC
#include "gencgc-alloc-region.h"
#else
struct alloc_region { };
#endif
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"

#ifdef LISP_FEATURE_SB_THREAD
# ifndef LISP_FEATURE_DARWIN
#   include <semaphore.h>
    typedef sem_t os_sem_t;
# else
#   include <mach/semaphore.h>
    typedef semaphore_t os_sem_t;
# endif
#endif

#include "genesis/thread.h"
#include "genesis/fdefn.h"
#include "interrupt.h"

#define STATE_RUNNING MAKE_FIXNUM(1)
#define STATE_STOPPED MAKE_FIXNUM(2)
#define STATE_DEAD MAKE_FIXNUM(3)

#ifdef LISP_FEATURE_SB_THREAD

# ifndef LISP_FEATURE_DARWIN

static inline void
os_sem_init(os_sem_t *sem, unsigned int value)
{
    if (-1==sem_init(sem, 0, value))
        lose("os_sem_init(%p, %u): %s", sem, value, strerror(errno));
    FSHOW((stderr, "os_sem_init(%p, %u)\n", sem, value));
}

static inline void
os_sem_wait(os_sem_t *sem, char *what)
{
    FSHOW((stderr, "%s: os_sem_wait(%p) ...\n", what, sem));
    while (-1 == sem_wait(sem))
        if (EINTR!=errno)
            lose("%s: os_sem_wait(%p): %s", what, sem, strerror(errno));
    FSHOW((stderr, "%s: os_sem_wait(%p) => ok\n", what, sem));
}

static inline void
os_sem_post(sem_t *sem, char *what)
{
    if (-1 == sem_post(sem))
        lose("%s: os_sem_post(%p): %s", what, sem, strerror(errno));
    FSHOW((stderr, "%s: os_sem_post(%p)\n", what, sem));
}

static inline void
os_sem_destroy(os_sem_t *sem)
{
    if (-1==sem_destroy(sem))
        lose("os_sem_destroy(%p): %s", sem, strerror(errno));
}

# else

static inline void
os_sem_init(os_sem_t *sem, unsigned int value)
{
    if (KERN_SUCCESS!=semaphore_create(current_mach_task, sem, SYNC_POLICY_FIFO, (int)value))
        lose("os_sem_init(%p): %s", sem, strerror(errno));
}

static inline void
os_sem_wait(os_sem_t *sem, char *what)
{
    kern_return_t ret;
  restart:
    FSHOW((stderr, "%s: os_sem_wait(%p)\n", what, sem));
    ret = semaphore_wait(*sem);
    FSHOW((stderr, "%s: os_sem_wait(%p) => %s\n", what, sem,
           KERN_SUCCESS==ret ? "ok" : strerror(errno)));
    switch (ret) {
    case KERN_SUCCESS:
        return;
    case KERN_OPERATION_TIMED_OUT:
        fprintf(stderr, "%s: os_sem_wait(%p): %s", what, sem, strerror(errno));
        goto restart;
    default:
        lose("%s: os_sem_wait(%p): %s", what, sem, strerror(errno));
    }
}

static inline void
os_sem_post(os_sem_t *sem, char *what)
{
    if (KERN_SUCCESS!=semaphore_signal(*sem))
        lose("%s: os_sem_post(%p): %s", what, sem, strerror(errno));
    FSHOW((stderr, "%s: os_sem_post(%p) ok\n", what, sem));
}

static inline void
os_sem_destroy(os_sem_t *sem)
{
    if (-1==semaphore_destroy(current_mach_task, *sem))
        lose("os_sem_destroy(%p): %s", sem, strerror(errno));
}

# endif

/* Only access thread state with blockables blocked. */
static inline lispobj
thread_state(struct thread *thread)
{
    lispobj state;
    sigset_t old;
    block_blockable_signals(NULL, &old);
    os_sem_wait(thread->state_sem, "thread_state");
    state = thread->state;
    os_sem_post(thread->state_sem, "thread_state");
    thread_sigmask(SIG_SETMASK, &old, NULL);
    return state;
}

static inline void
set_thread_state(struct thread *thread, lispobj state)
{
    int i, waitcount = 0;
    sigset_t old;
    block_blockable_signals(NULL, &old);
    os_sem_wait(thread->state_sem, "set_thread_state");
    if (thread->state != state) {
        if ((STATE_STOPPED==state) ||
            (STATE_DEAD==state)) {
            waitcount = thread->state_not_running_waitcount;
            thread->state_not_running_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(thread->state_not_running_sem, "set_thread_state (not running)");
        }
        if ((STATE_RUNNING==state) ||
            (STATE_DEAD==state)) {
            waitcount = thread->state_not_stopped_waitcount;
            thread->state_not_stopped_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(thread->state_not_stopped_sem, "set_thread_state (not stopped)");
        }
        thread->state = state;
    }
    os_sem_post(thread->state_sem, "set_thread_state");
    thread_sigmask(SIG_SETMASK, &old, NULL);
}

static inline void
wait_for_thread_state_change(struct thread *thread, lispobj state)
{
    sigset_t old;
    os_sem_t *wait_sem;
    block_blockable_signals(NULL, &old);
  start:
    os_sem_wait(thread->state_sem, "wait_for_thread_state_change");
    if (thread->state == state) {
        switch (state) {
        case STATE_RUNNING:
            wait_sem = thread->state_not_running_sem;
            thread->state_not_running_waitcount++;
            break;
        case STATE_STOPPED:
            wait_sem = thread->state_not_stopped_sem;
            thread->state_not_stopped_waitcount++;
            break;
        default:
            lose("Invalid state in wait_for_thread_state_change: "OBJ_FMTX"\n", state);
        }
    } else {
        wait_sem = NULL;
    }
    os_sem_post(thread->state_sem, "wait_for_thread_state_change");
    if (wait_sem) {
        os_sem_wait(wait_sem, "wait_for_thread_state_change");
        goto start;
    }
    thread_sigmask(SIG_SETMASK, &old, NULL);
}

extern pthread_key_t lisp_thread;
#endif

extern int kill_safely(os_thread_t os_thread, int signal);

#define THREAD_SLOT_OFFSET_WORDS(c) \
 (offsetof(struct thread,c)/(sizeof (struct thread *)))

union per_thread_data {
    struct thread thread;
    lispobj dynamic_values[1];  /* actually more like 4000 or so */
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

static inline lispobj *
SymbolValueAddress(u64 tagged_symbol_pointer, void *thread)
{
    struct symbol *sym= (struct symbol *)
        (pointer_sized_uint_t)(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    if(thread && sym->tls_index) {
        lispobj *r = &(((union per_thread_data *)thread)
                       ->dynamic_values[(sym->tls_index) >> WORD_SHIFT]);
        if((*r)!=NO_TLS_VALUE_MARKER_WIDETAG) return r;
    }
#endif
    return &sym->value;
}

static inline lispobj
SymbolValue(u64 tagged_symbol_pointer, void *thread)
{
    struct symbol *sym= (struct symbol *)
        (pointer_sized_uint_t)(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    if(thread && sym->tls_index) {
        lispobj r=
            ((union per_thread_data *)thread)
            ->dynamic_values[(sym->tls_index) >> WORD_SHIFT];
        if(r!=NO_TLS_VALUE_MARKER_WIDETAG) return r;
    }
#endif
    return sym->value;
}

static inline lispobj
SymbolTlValue(u64 tagged_symbol_pointer, void *thread)
{
    struct symbol *sym= (struct symbol *)
        (pointer_sized_uint_t)(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    return ((union per_thread_data *)thread)
        ->dynamic_values[(sym->tls_index) >> WORD_SHIFT];
#else
    return sym->value;
#endif
}

static inline void
SetSymbolValue(u64 tagged_symbol_pointer,lispobj val, void *thread)
{
    struct symbol *sym= (struct symbol *)
        (pointer_sized_uint_t)(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    if(thread && sym->tls_index) {
        lispobj *pr= &(((union per_thread_data *)thread)
                       ->dynamic_values[(sym->tls_index) >> WORD_SHIFT]);
        if(*pr!=NO_TLS_VALUE_MARKER_WIDETAG) {
            *pr=val;
            return;
        }
    }
#endif
    sym->value = val;
}

static inline void
SetTlSymbolValue(u64 tagged_symbol_pointer,lispobj val, void *thread)
{
#ifdef LISP_FEATURE_SB_THREAD
    struct symbol *sym= (struct symbol *)
        (pointer_sized_uint_t)(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
    ((union per_thread_data *)thread)
        ->dynamic_values[(sym->tls_index) >> WORD_SHIFT]
        =val;
#else
    SetSymbolValue(tagged_symbol_pointer,val,thread) ;
#endif
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
#elif defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
#define get_binding_stack_pointer(thread)       \
    SymbolValue(BINDING_STACK_POINTER, thread)
#define set_binding_stack_pointer(thread,value) \
    SetSymbolValue(BINDING_STACK_POINTER, (lispobj)(value), thread)
#define access_control_stack_pointer(thread)    \
    (current_control_stack_pointer)
#else
#define get_binding_stack_pointer(thread)       \
    (current_binding_stack_pointer)
#define set_binding_stack_pointer(thread,value) \
    (current_binding_stack_pointer = (lispobj *)(value))
#define access_control_stack_pointer(thread) \
    (current_control_stack_pointer)
#define access_control_frame_pointer(thread) \
    (current_control_frame_pointer)
#endif

#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_GCC_TLS)
extern __thread struct thread *current_thread;
#endif

/* This is clearly per-arch and possibly even per-OS code, but we can't
 * put it somewhere sensible like x86-linux-os.c because it needs too
 * much stuff like struct thread and all_threads to be defined, which
 * usually aren't by that time.  So, it's here instead.  Sorry */

static inline struct thread *arch_os_get_current_thread(void)
{
#if defined(LISP_FEATURE_SB_THREAD)
#if defined(LISP_FEATURE_X86)
    register struct thread *me=0;
    if(all_threads) {
#if defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_RESTORE_FS_SEGMENT_REGISTER_FROM_TLS)
        sel_t sel;
        struct thread *th = pthread_getspecific(specials);
        sel.index = th->tls_cookie;
        sel.rpl = USER_PRIV;
        sel.ti = SEL_LDT;
        __asm__ __volatile__ ("movw %w0, %%fs" : : "r"(sel));
#elif defined(LISP_FEATURE_FREEBSD)
#ifdef LISP_FEATURE_GCC_TLS
        struct thread *th = current_thread;
#else
        struct thread *th = pthread_getspecific(specials);
#endif
#ifdef LISP_FEATURE_RESTORE_TLS_SEGMENT_REGISTER_FROM_TLS
        unsigned int sel = LSEL(th->tls_cookie, SEL_UPL);
        unsigned int fs = rfs();

        /* Load FS only if it's necessary.  Modifying a selector
         * causes privilege checking and it takes long time. */
        if (fs != sel)
            load_fs(sel);
#endif
        return th;
#endif
        __asm__ __volatile__ ("movl %%fs:%c1,%0" : "=r" (me)
                 : "i" (offsetof (struct thread,this)));
    }
    return me;
#else
#ifdef LISP_FEATURE_GCC_TLS
    return current_thread;
#else
    return pthread_getspecific(specials);
#endif
#endif /* x86 */
#else
     return all_threads;
#endif
}

#if defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
#define THREAD_STRUCT_TO_EXCEPTION_PORT(th) ((mach_port_t) th)
#define EXCEPTION_PORT_TO_THREAD_STRUCT(th) ((struct thread *) th)
#endif

extern void create_initial_thread(lispobj);

#endif /* _INCLUDE_THREAD_H_ */
