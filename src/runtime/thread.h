#include <sys/types.h>
#include <unistd.h>
#include "runtime.h"
#include "sbcl.h"

struct thread {
    lispobj *control_stack_start;
    lispobj *binding_stack_start;
    lispobj *alien_stack_start;
    lispobj *dynamic_values_start;
    pid_t pid;
    int tls_cookie;		/* on x86, the LDT index */
    struct thread *next;
};
extern struct thread *all_threads;
extern int dynamic_values_bytes;
extern struct thread *find_thread_by_pid(pid_t pid);

#define for_each_thread(th) for(th=all_threads;th;th=th->next)

static inline lispobj SymbolValue(u32 tagged_symbol_pointer, void *thread) {
    struct symbol *sym= (struct symbol *)
	(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
    if(thread && sym->tls_index) {
	return ((struct thread *)thread)->dynamic_values_start[sym->tls_index];
    } else {
	return sym->value;
    }
}
static inline void SetSymbolValue(u32 tagged_symbol_pointer,lispobj val, void *thread) {
    struct symbol *sym=	(struct symbol *)
	(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
    if(thread && sym->tls_index) {
	((struct thread *)thread)->dynamic_values_start[sym->tls_index]=val;
    } else {
	sym->value = val;
    }
}

