
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
#error "threading doesn't work with cheney gc yet"
#endif

#define THREAD_SLOT_OFFSET_WORDS(c) \
 (offsetof(struct thread,c)/(sizeof (struct thread *)))

struct thread {
    lispobj unbound_marker;	/* tls[0] = UNBOUND_MARKER_WIDETAG */
    /* unbound_marker is borrowed very briefly at thread startup to 
     * pass the address of initial_function into new_thread_trampoline 
     */
    lispobj *binding_stack_start;
    lispobj *binding_stack_pointer;
    lispobj *control_stack_start;
    lispobj *alien_stack_start;
    lispobj *alien_stack_pointer; 
    struct alloc_region alloc_region; /* 5 words: first 2 are pointer, end */
    os_context_t *interrupt_contexts[MAX_INTERRUPTS];
    pid_t pid;
    u32 tls_cookie;		/* on x86, the LDT index */
    struct thread *this,*next;
    lispobj pseudo_atomic_atomic,pseudo_atomic_interrupted; /* slots 23,24 */
};
union per_thread_data {
    struct thread thread;
    lispobj dynamic_values[1];	/* actually more like 4000 or so */
};

extern struct thread *all_threads;
extern int dynamic_values_bytes;
extern struct thread *find_thread_by_pid(pid_t pid);

#define for_each_thread(th) for(th=all_threads;th;th=th->next)

static inline lispobj SymbolValue(u32 tagged_symbol_pointer, void *thread) {
    struct symbol *sym= (struct symbol *)
	(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
    if(thread && sym->tls_index) {
	lispobj r=
	    ((union per_thread_data *)thread)
	    ->dynamic_values[fixnum_value(sym->tls_index)];
	if(r!=UNBOUND_MARKER_WIDETAG) return r;
    }
    return sym->value;
}
static inline lispobj SymbolTlValue(u32 tagged_symbol_pointer, void *thread) {
    struct symbol *sym= (struct symbol *)
	(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
    return ((union per_thread_data *)thread)
	->dynamic_values[fixnum_value(sym->tls_index)];
}

static inline void SetSymbolValue(u32 tagged_symbol_pointer,lispobj val, void *thread) {
    struct symbol *sym=	(struct symbol *)
	(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
    if(thread && sym->tls_index) {
	lispobj *pr= &(((union per_thread_data *)thread)
		       ->dynamic_values[fixnum_value(sym->tls_index)]);
	if(*pr!= UNBOUND_MARKER_WIDETAG) {
	    *pr=val;
	    return;
	}
    }
    sym->value = val;
}
static inline void SetTlSymbolValue(u32 tagged_symbol_pointer,lispobj val, void *thread) {
    struct symbol *sym=	(struct symbol *)
	(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
    ((union per_thread_data *)thread)
	->dynamic_values[fixnum_value(sym->tls_index)]
	=val;
}

    

#endif /* _INCLUDE_THREAD_H_ */
