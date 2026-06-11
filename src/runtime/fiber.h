#ifndef SBCL_FIBER_H
#define SBCL_FIBER_H

#include "lispobj.h"

#ifdef LISP_FEATURE_SB_FIBER

#include "genesis/thread.h"
#include <stddef.h>
#include <stdint.h>

#if defined(LISP_FEATURE_X86_64)
#  include "x86-64-fiber.h"
#elif defined(LISP_FEATURE_ARM64)
#  include "arm64-fiber.h"
#else
#  error "fiber support not implemented on this architecture"
#endif

/* Mirrored by the fiber-state constants on the Lisp side. */
enum fiber_state {
    FIBER_NEW      = 0,
    FIBER_RUNNABLE = 1,
    FIBER_RUNNING  = 2,
    FIBER_DEAD     = 3
};

#define FIBER_DEFAULT_STACK_SIZE         65536
#define FIBER_DEFAULT_BINDING_STACK_SIZE 8192

struct sb_fiber_ctx {
    struct sb_fiber_regs regs;
    enum fiber_state     state;

    /* Control stack */
    void    *stack_base;
    void    *stack_start;
    void    *stack_end;
    size_t   stack_alloc_size;

    /* Binding stack */
    lispobj *binding_stack_base;
    lispobj *binding_stack_end;
    lispobj *binding_stack_pointer;
    size_t   binding_stack_alloc_size;
    /* What to install into thread->binding_stack_start while we run. */
    lispobj *binding_stack_start_for_thread;
    /* Per-fiber Lisp-stack bounds installed in thread->control_stack_* */
    lispobj *control_stack_base;
    lispobj *control_stack_end;
#ifdef LISP_FEATURE_ARM64
    lispobj *control_stack_pointer;
    lispobj *control_frame_pointer;
    size_t   control_stack_alloc_size;
    lispobj *dirty_high;
#endif

    /* Saved thread-struct fields. */
    lispobj  current_catch_block;
    lispobj  current_unwind_protect_block;

    struct sb_fiber_ctx *next;
    struct thread       *owner;

    void (*entry_fn)(void *arg);
    void  *entry_arg;

    struct sb_fiber_ctx *return_fiber;  /* auto-return target */

    unsigned char cs_guard_protected;
    unsigned char bs_guard_protected;
};

/* Lifecycle */
struct sb_fiber_ctx *sb_fiber_create(size_t stack_size, size_t binding_stack_size);
struct sb_fiber_ctx *sb_fiber_create_main(struct thread *th);
void                 sb_fiber_release(struct sb_fiber_ctx *fiber);

/* GC registration */
void sb_fiber_register  (struct thread *th, struct sb_fiber_ctx *fiber);
void sb_fiber_unregister(struct thread *th, struct sb_fiber_ctx *fiber);

/* Cross-thread migration */
int  sb_fiber_migrate(struct sb_fiber_ctx *fiber, struct thread *dest);

/* Thread-exit cleanup.  Called from free_thread_struct. */
void sb_fiber_release_registered(struct thread *th);

/* Binding-stack guard manipulation. */
void sb_fiber_lower_bs_guard(struct sb_fiber_ctx *f);
void sb_fiber_reset_bs_guard(struct sb_fiber_ctx *f);

/* Handle a SIGSEGV / SIGBUS address that may have landed on a
 * fiber's binding-stack guard. */
int sb_fiber_handle_bs_fault(void *context, void *addr, struct thread *th);

void sb_fiber_switch_prep(struct sb_fiber_ctx *from, struct sb_fiber_ctx *to);
void sb_fiber_exit_pa    (struct thread *th);

/* Arch-specific helpers */
void  sb_fiber_prepare(struct sb_fiber_ctx *f, void (*fn)(void *), void *arg);
void *sb_fiber_sp (const struct sb_fiber_ctx *f);
void  sb_fiber_rebind_thread(struct sb_fiber_ctx *f, struct thread *new_owner);
/* Write the fiber's callee-saved GC-relevant register words into OUT
 * (up to MAX). Return the count actually written. */
int   sb_fiber_gc_regs(const struct sb_fiber_ctx *f, lispobj *out, int max);

/* GC-side entry points (shared body in fiber.c).  Each GC TU provides
 * gc_preserve_fiber_word and gc_scav_fiber_binding_stack as leaf
 * functions; the shared scanners call them by name. */
void gc_scan_fiber_stacks         (struct thread *th);
void gc_scav_fiber_binding_stacks (struct thread *th);

/* Per-fiber Lisp control stack hooks */
int  sb_fiber_lisp_stack_alloc       (struct sb_fiber_ctx *f, size_t size);
void sb_fiber_lisp_stack_free        (struct sb_fiber_ctx *f);
void sb_fiber_lisp_stack_capture_main(struct sb_fiber_ctx *f, struct thread *th);
void sb_fiber_lisp_stack_suspend     (struct sb_fiber_ctx *f, struct thread *th);
void sb_fiber_lisp_stack_resume      (struct sb_fiber_ctx *f, struct thread *th);

void fiber_tramp_c(struct sb_fiber_ctx *self);

#endif /* LISP_FEATURE_SB_FIBER */
#endif /* SBCL_FIBER_H */
