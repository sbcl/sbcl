
#if !defined(_INCLUDE_THREAD_H_)
#define _INCLUDE_THREAD_H_

#include <sys/types.h>
#include <unistd.h>
#include "runtime.h"
#include "sbcl.h"
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

#define THREAD_SLOT_OFFSET_WORDS(c) \
 (offsetof(struct thread,c)/(sizeof (struct thread *)))

union per_thread_data {
    struct thread thread;
    lispobj dynamic_values[1];	/* actually more like 4000 or so */
};

extern struct thread *all_threads;
extern int dynamic_values_bytes;
extern struct thread *find_thread_by_pid(pid_t pid);

#ifdef LISP_FEATURE_SB_THREAD
#define for_each_thread(th) for(th=all_threads;th;th=th->next)
#else
/* there's some possibility a SSC could notice this never actually
 * loops  */
#define for_each_thread(th) for(th=all_threads;th;th=0)
#endif

static inline lispobj SymbolValue(u32 tagged_symbol_pointer, void *thread) {
    struct symbol *sym= (struct symbol *)
	(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    if(thread && sym->tls_index) {
	lispobj r=
	    ((union per_thread_data *)thread)
	    ->dynamic_values[fixnum_value(sym->tls_index)];
	if(r!=UNBOUND_MARKER_WIDETAG) return r;
    }
#endif
    return sym->value;
}
static inline lispobj SymbolTlValue(u32 tagged_symbol_pointer, void *thread) {
    struct symbol *sym= (struct symbol *)
	(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    return ((union per_thread_data *)thread)
	->dynamic_values[fixnum_value(sym->tls_index)];
#else
    return sym->value;
#endif
}

static inline void SetSymbolValue(u32 tagged_symbol_pointer,lispobj val, void *thread) {
    struct symbol *sym=	(struct symbol *)
	(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    if(thread && sym->tls_index) {
	lispobj *pr= &(((union per_thread_data *)thread)
		       ->dynamic_values[fixnum_value(sym->tls_index)]);
	if(*pr!= UNBOUND_MARKER_WIDETAG) {
	    *pr=val;
	    return;
	}
    }
#endif
    sym->value = val;
}
static inline void SetTlSymbolValue(u32 tagged_symbol_pointer,lispobj val, void *thread) {
#ifdef LISP_FEATURE_SB_THREAD
    struct symbol *sym=	(struct symbol *)
	(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
    ((union per_thread_data *)thread)
	->dynamic_values[fixnum_value(sym->tls_index)]
	=val;
#else
    SetSymbolValue(tagged_symbol_pointer,val,thread) ;
#endif
}

int arch_os_thread_init(struct thread *thread);
extern struct thread *arch_os_get_current_thread();

#endif /* _INCLUDE_THREAD_H_ */
