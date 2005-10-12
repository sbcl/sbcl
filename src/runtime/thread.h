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

#define STATE_STARTING (make_fixnum(0))
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
SymbolValue(u64 tagged_symbol_pointer, void *thread) {
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
SymbolTlValue(u64 tagged_symbol_pointer, void *thread) {
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
SetSymbolValue(u64 tagged_symbol_pointer,lispobj val, void *thread) {
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
SetTlSymbolValue(u64 tagged_symbol_pointer,lispobj val, void *thread) {
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

/* This is clearly per-arch and possibly even per-OS code, but we can't
 * put it somewhere sensible like x86-linux-os.c because it needs too
 * much stuff like struct thread and all_threads to be defined, which
 * usually aren't by that time.  So, it's here instead.  Sorry */

static inline struct thread *arch_os_get_current_thread() {
#if defined(LISP_FEATURE_SB_THREAD)
#if defined(LISP_FEATURE_X86)
    register struct thread *me=0;
    if(all_threads)
        __asm__ __volatile__ ("movl %%fs:%c1,%0" : "=r" (me)
                 : "i" (offsetof (struct thread,this)));
    return me;
#else
    return pthread_getspecific(specials);
#endif /* x86 */
#else
     return all_threads;
#endif
}

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
#define thread_mutex_lock(l)
#define thread_mutex_unlock(l)
#endif

extern void create_initial_thread(lispobj);

#endif /* _INCLUDE_THREAD_H_ */
