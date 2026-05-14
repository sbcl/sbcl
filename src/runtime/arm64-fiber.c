#include "fiber.h"
#include "align.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include "os.h"
#include "thread.h"
#include "validate.h"
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>

#ifndef MAP_STACK
#define MAP_STACK 0
#endif

void sb_fiber_prepare(struct sb_fiber *f,
                      void (*fn)(void *), void *arg)
{
    f->entry_fn = fn;
    f->entry_arg = arg;
    /* AAPCS64 requires SP to be 16-byte aligned at public interfaces. */
    f->ctx.sp  = (void *)((uintptr_t)f->stack_end & ~(uintptr_t)0xF);
    f->ctx.fp  = 0;
    f->ctx.lr  = (void *)SYMBOL(FIBER_TRAMPOLINE_ASM)->value;
    f->ctx.x19 = (void *)f;
    f->ctx.x20 = 0;
    f->ctx.x21 = 0;
    f->ctx.x22 = 0;
    f->ctx.x23 = 0;
    f->ctx.x24 = 0;
    f->ctx.x25 = 0;
    f->ctx.x26 = 0;
    f->ctx.x27 = 0;
    f->ctx.x28 = 0;
    f->ctx.d8 = f->ctx.d9 = f->ctx.d10 = f->ctx.d11 = 0.0;
    f->ctx.d12 = f->ctx.d13 = f->ctx.d14 = f->ctx.d15 = 0.0;
}

void *sb_fiber_ctx_sp(const struct sb_fiber *f)
{
    return f->ctx.sp;
}

void sb_fiber_ctx_foreach_gc_reg(const struct sb_fiber *f,
                                 void (*cb)(lispobj))
{
    cb((lispobj)f->ctx.fp);
    cb((lispobj)f->ctx.x19);
    cb((lispobj)f->ctx.x20);
    cb((lispobj)f->ctx.x21);
    cb((lispobj)f->ctx.x22);
    cb((lispobj)f->ctx.x23);
    cb((lispobj)f->ctx.x24);
    cb((lispobj)f->ctx.x25);
    cb((lispobj)f->ctx.x26);
    cb((lispobj)f->ctx.x27);
    cb((lispobj)f->ctx.x28);
}

int sb_fiber_lisp_stack_alloc(struct sb_fiber *f, size_t size)
{
    size_t ps = os_reported_page_size;
    size_t guard = STACK_GUARD_SIZE;
    size = ALIGN_UP(size ? size : 65536, ps);
    size_t total = size + 3*guard;
    void *p = mmap(NULL, total, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK, -1, 0);
    if (p == MAP_FAILED) return -1;
    mprotect((char *)p + size + guard,   guard, PROT_NONE);
    mprotect((char *)p + size + 2*guard, guard, PROT_NONE);
    f->control_stack_base       = (lispobj *)p;
    f->control_stack_end        = (lispobj *)((char *)p + total);
    f->control_stack_pointer    = (lispobj *)p;
    f->control_frame_pointer    = NULL;
    f->control_stack_alloc_size = total;
    f->cs_guard_protected       = 1;
    f->dirty_high               = (lispobj *)p;
    return 0;
}

void sb_fiber_lisp_stack_free(struct sb_fiber *f)
{
    if (f->control_stack_alloc_size && f->control_stack_base)
        munmap(f->control_stack_base, f->control_stack_alloc_size);
    f->control_stack_base       = NULL;
    f->control_stack_end        = NULL;
    f->control_stack_pointer    = NULL;
    f->control_frame_pointer    = NULL;
    f->control_stack_alloc_size = 0;
}

void sb_fiber_lisp_stack_capture_main(struct sb_fiber *f, struct thread *th)
{
    f->control_stack_base       = th->control_stack_start;
    f->control_stack_end        = th->control_stack_end;
    f->control_stack_pointer    = th->control_stack_pointer;
    f->control_frame_pointer    = th->control_frame_pointer;
    f->control_stack_alloc_size = 0;   /* not owned */
    f->cs_guard_protected =
        th->state_word.control_stack_guard_page_protected;
    f->dirty_high = th->control_stack_pointer;
}

void sb_fiber_lisp_stack_suspend(struct sb_fiber *f, struct thread *th)
{
    f->control_stack_base    = th->control_stack_start;
    f->control_stack_end     = th->control_stack_end;
    f->control_stack_pointer = th->control_stack_pointer;
    f->control_frame_pointer = th->control_frame_pointer;
    f->cs_guard_protected    =
        th->state_word.control_stack_guard_page_protected;
    if (f->control_stack_pointer && f->control_stack_end) {
        char *csp = (char *)f->control_stack_pointer;
        char *usable_end = (char *)f->control_stack_end
                           - 3 * STACK_GUARD_SIZE;
        char *dirty_high = (char *)f->dirty_high;
        if (csp > dirty_high) {
            if (csp < usable_end)
                memset(csp, 0, usable_end - csp);
            f->dirty_high = (lispobj *)csp;
        } else if (csp < dirty_high) {
            memset(csp, 0, dirty_high - csp);
            f->dirty_high = (lispobj *)csp;
        }
    }
}

void sb_fiber_lisp_stack_resume(struct sb_fiber *f, struct thread *th)
{
    th->control_stack_start   = f->control_stack_base;
    th->control_stack_end     = f->control_stack_end;
    th->control_stack_pointer = f->control_stack_pointer;
    th->control_frame_pointer = f->control_frame_pointer;
    th->state_word.control_stack_guard_page_protected = f->cs_guard_protected;
}
