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

#define for_each_thread(th) for(th=all_threads;th;th=th->next)
