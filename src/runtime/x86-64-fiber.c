#include "fiber.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include <stdint.h>

void sb_fiber_prepare(struct sb_fiber *f,
                      void (*fn)(void *), void *arg)
{
    f->entry_fn = fn;
    f->entry_arg = arg;

    void **sp = (void **)f->stack_end;
    sp = (void **)((uintptr_t)sp & ~0xFULL);   /* 16-byte align */
    *(--sp) = (void *)0;                       /* padding */
    *(--sp) = (void *)SYMBOL(FIBER_TRAMPOLINE_ASM)->value;     /* RET target */

    f->ctx.rsp = sp;
    f->ctx.rbp = 0;
    f->ctx.rbx = 0;
    f->ctx.r12 = (void *)f;
    f->ctx.r13 = 0;
    f->ctx.r14 = 0;
    f->ctx.r15 = 0;

    uint32_t mxcsr;
    __asm__ volatile ("stmxcsr %0" : "=m"(mxcsr));
    f->ctx.mxcsr = mxcsr;
    f->ctx.pad_  = 0;
}

void *sb_fiber_ctx_sp(const struct sb_fiber *f)
{
    return f->ctx.rsp;
}

void sb_fiber_ctx_foreach_gc_reg(const struct sb_fiber *f,
                                 void (*cb)(lispobj))
{
    cb((lispobj)f->ctx.rbx);
    cb((lispobj)f->ctx.rbp);
    cb((lispobj)f->ctx.r12);
    cb((lispobj)f->ctx.r13);
    cb((lispobj)f->ctx.r14);
    cb((lispobj)f->ctx.r15);
}

/* Lisp-stack hooks: alias control_stack_* to the fiber's native C
 * stack range.  base points at the HARD guard so the validate.h
 * guard-derivation macros land on the three real guard pages.
 * SBCL queries th->control_stack_* via *CONTROL-STACK-START/END*
 * (STACK-ALLOCATED-P, COPY-CTYPE AVER, ...), so it must track the
 * running fiber. */

int sb_fiber_lisp_stack_alloc(struct sb_fiber *f, size_t size)
{
    (void)size;
    f->control_stack_base = (lispobj *)f->stack_base;
    f->control_stack_end  = (lispobj *)f->stack_end;
    return 0;
}

void sb_fiber_lisp_stack_free(struct sb_fiber *f)
{
    f->control_stack_base = NULL;
    f->control_stack_end  = NULL;
}

void sb_fiber_lisp_stack_capture_main(struct sb_fiber *f, struct thread *th)
{
    f->control_stack_base = th->control_stack_start;
    f->control_stack_end  = th->control_stack_end;
    f->cs_guard_protected =
        th->state_word.control_stack_guard_page_protected;
}

void sb_fiber_lisp_stack_suspend(struct sb_fiber *f, struct thread *th)
{
    f->control_stack_base = th->control_stack_start;
    f->control_stack_end  = th->control_stack_end;
    f->cs_guard_protected = th->state_word.control_stack_guard_page_protected;
}

void sb_fiber_lisp_stack_resume(struct sb_fiber *f, struct thread *th)
{
    th->control_stack_start = f->control_stack_base;
    th->control_stack_end   = f->control_stack_end;
    th->state_word.control_stack_guard_page_protected = f->cs_guard_protected;
}
