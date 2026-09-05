#include "fiber.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include <stdint.h>

void sb_fiber_prepare(struct sb_fiber_ctx *f,
                      void (*fn)(void *), void *arg)
{
    f->entry_fn = fn;
    f->entry_arg = arg;

    void **sp = (void **)f->stack_end;
    sp = (void **)((uintptr_t)sp & ~0xFULL);      /* 16-byte align */
    *(--sp) = (void *)0;
    *(--sp) = (void *)SYMBOL(FIBER_TRAMP)->value; /* RET target */

    f->regs.rsp = sp;
    f->regs.rbp = 0;
    f->regs.rbx = 0;
    f->regs.r12 = (void *)f;
    f->regs.r13 = 0;
    f->regs.r14 = 0;
    f->regs.r15 = 0;

    uint32_t mxcsr;
    __asm__ volatile ("stmxcsr %0" : "=m"(mxcsr));
    f->regs.mxcsr = mxcsr;
    f->regs.pad_  = 0;
}

void *sb_fiber_sp(const struct sb_fiber_ctx *f)
{
    return f->regs.rsp;
}

int sb_fiber_gc_regs(const struct sb_fiber_ctx *f, lispobj *out, int max)
{
    static const int n = 6;
    if (max < n) return 0;
    int i = 0;
    out[i++] = (lispobj)f->regs.rbx;
    out[i++] = (lispobj)f->regs.rbp;
    out[i++] = (lispobj)f->regs.r12;
    out[i++] = (lispobj)f->regs.r13;
    out[i++] = (lispobj)f->regs.r14;
    out[i++] = (lispobj)f->regs.r15;
    return i;
}

void sb_fiber_rebind_thread(struct sb_fiber_ctx *f, struct thread *new_owner)
{
    f->regs.r13 = (void *)new_owner;
}

/* Lisp-stack hooks: alias control_stack_* to the fiber's native C
   stack range. */

int sb_fiber_lisp_stack_alloc(struct sb_fiber_ctx *f, size_t size)
{
    (void)size;
    f->control_stack_base = (lispobj *)f->stack_base;
    f->control_stack_end  = (lispobj *)f->stack_end;
    return 0;
}

void sb_fiber_lisp_stack_free(struct sb_fiber_ctx *f)
{
    f->control_stack_base = NULL;
    f->control_stack_end  = NULL;
}

void sb_fiber_lisp_stack_capture_main(struct sb_fiber_ctx *f, struct thread *th)
{
    f->control_stack_base = th->control_stack_start;
    f->control_stack_end  = th->control_stack_end;
    f->cs_guard_protected =
        th->state_word.control_stack_guard_page_protected;
}

void sb_fiber_lisp_stack_suspend(struct sb_fiber_ctx *f, struct thread *th)
{
    f->control_stack_base = th->control_stack_start;
    f->control_stack_end  = th->control_stack_end;
    f->cs_guard_protected = th->state_word.control_stack_guard_page_protected;
}

void sb_fiber_lisp_stack_resume(struct sb_fiber_ctx *f, struct thread *th)
{
    th->control_stack_start = f->control_stack_base;
    th->control_stack_end   = f->control_stack_end;
    th->state_word.control_stack_guard_page_protected = f->cs_guard_protected;
}

size_t sb_fiber_control_stack_size_bytes(const struct sb_fiber_ctx *f)
{
    if (f->stack_start && f->stack_end)
        return (char *)f->stack_end - (char *)f->stack_start;
    if (f->control_stack_base && f->control_stack_end)
        return (char *)f->control_stack_end - (char *)f->control_stack_base;
    return 0;
}

size_t sb_fiber_control_stack_used_bytes(const struct sb_fiber_ctx *f)
{
    void *sp = f->regs.rsp;
    if (sp && f->stack_end && (char *)sp < (char *)f->stack_end)
        return (char *)f->stack_end - (char *)sp;
    return 0;
}
