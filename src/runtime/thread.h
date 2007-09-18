#if !defined(_INCLUDE_THREAD_H_)
#define _INCLUDE_THREAD_H_

#include <sys/types.h>
#include <unistd.h>
#include <stddef.h>
#include "sbcl.h"
#include "globals.h"
#include "runtime.h"
#include "os.h"
#include "interrupt.h"
#ifdef LISP_FEATURE_GENCGC
#include "gencgc-alloc-region.h"
#else
struct alloc_region { };
#endif
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include "genesis/thread.h"

#define STATE_RUNNING (make_fixnum(1))
#define STATE_SUSPENDED (make_fixnum(2))
#define STATE_DEAD (make_fixnum(3))

#define THREAD_SLOT_OFFSET_WORDS(c) \
 (offsetof(struct thread,c)/(sizeof (struct thread *)))

union per_thread_data {
    struct thread thread;
    lispobj dynamic_values[1];  /* actually more like 4000 or so */
};

extern struct thread * volatile all_threads;
extern int dynamic_values_bytes;

#ifdef LISP_FEATURE_SB_THREAD
#define for_each_thread(th) for(th=all_threads;th;th=th->next)
#else
/* there's some possibility a SSC could notice this never actually
 * loops  */
#define for_each_thread(th) for(th=all_threads;th;th=0)
#endif

static inline lispobj
SymbolValue(u64 tagged_symbol_pointer, void *thread)
{
    struct symbol *sym= (struct symbol *)
        (pointer_sized_uint_t)(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    if(thread && sym->tls_index) {
        lispobj r=
            ((union per_thread_data *)thread)
            ->dynamic_values[fixnum_value(sym->tls_index)];
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
        ->dynamic_values[fixnum_value(sym->tls_index)];
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
                       ->dynamic_values[fixnum_value(sym->tls_index)]);
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
        ->dynamic_values[fixnum_value(sym->tls_index)]
        =val;
#else
    SetSymbolValue(tagged_symbol_pointer,val,thread) ;
#endif
}

static inline
os_context_t *get_interrupt_context_for_thread(struct thread *th)
{
    return th->interrupt_contexts
        [fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX,th)-1)];
}

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

#if defined(LISP_FEATURE_SB_THREAD)
#define thread_self pthread_self
#define thread_kill pthread_kill
#define thread_sigmask pthread_sigmask
#define thread_mutex_lock(l) pthread_mutex_lock(l)
#define thread_mutex_unlock(l) pthread_mutex_unlock(l)
#else
#define thread_self getpid
#define thread_kill kill
#define thread_sigmask sigprocmask
#define thread_mutex_lock(l) 0
#define thread_mutex_unlock(l) 0
#endif

extern void create_initial_thread(lispobj);
extern int kill_thread_safely(os_thread_t os_thread, int signo);

#endif /* _INCLUDE_THREAD_H_ */
