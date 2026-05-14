#include "fiber.h"
#include "os.h"
#include "globals.h"
#include "thread.h"
#include "validate.h"
#include "interr.h"
#include "interrupt.h"
#include "align.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include "lispobj.h"
#include <sys/mman.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>

#ifndef MAP_STACK
#define MAP_STACK 0
#endif

static void fiber_release(struct sb_fiber *f)
{
    if (f->stack_base && f->stack_base != MAP_FAILED)
        munmap(f->stack_base, f->stack_alloc_size);
    if (f->binding_stack_base)
        munmap(f->binding_stack_base, f->binding_stack_alloc_size);
    sb_fiber_lisp_stack_free(f);
    free(f);
}

void sb_fiber_release_registered(struct thread *th)
{
    struct extra_thread_data *ed = thread_extra_data(th);
    struct sb_fiber *f = ed->fiber_list;
    ed->fiber_list = NULL;
    while (f) {
        struct sb_fiber *next = f->next;
        f->owner = NULL;
        f->next = NULL;
        fiber_release(f);
        f = next;
    }
}

struct sb_fiber *sb_fiber_create(size_t stack_size,
                                 size_t binding_stack_size)
{
    size_t ps = os_reported_page_size;
    struct sb_fiber *f = calloc(1, sizeof(struct sb_fiber));
    if (!f) return NULL;

    size_t guard = STACK_GUARD_SIZE;
    stack_size = ALIGN_UP(stack_size ? stack_size : 65536, ps);
    f->stack_alloc_size = 3*guard + stack_size;
    f->stack_base = mmap(NULL, f->stack_alloc_size,
                         PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK,
                         -1, 0);
    if (f->stack_base == MAP_FAILED) { free(f); return NULL; }

    mprotect(f->stack_base,                    guard, PROT_NONE);
    mprotect((char *)f->stack_base + guard,    guard, PROT_NONE);
    f->stack_start = (char *)f->stack_base + 3*guard;
    f->stack_end   = (char *)f->stack_base + f->stack_alloc_size;
    f->cs_guard_protected = 1;

    binding_stack_size = ALIGN_UP(
        binding_stack_size ? binding_stack_size : 8192, ps);
    f->binding_stack_alloc_size = binding_stack_size + 3 * ps;
    f->binding_stack_base = mmap(NULL, f->binding_stack_alloc_size,
                                 PROT_READ | PROT_WRITE,
                                 MAP_PRIVATE | MAP_ANONYMOUS,
                                 -1, 0);
    if (f->binding_stack_base == MAP_FAILED) {
        munmap(f->stack_base, f->stack_alloc_size);
        free(f);
        return NULL;
    }
    mprotect((char *)f->binding_stack_base + binding_stack_size + ps,
             2 * ps, PROT_NONE);
    f->binding_stack_end = (lispobj *)((char *)f->binding_stack_base
                                       + binding_stack_size);
    f->binding_stack_pointer = f->binding_stack_base;
    f->binding_stack_start_for_thread = f->binding_stack_base;
    f->bs_guard_protected = 1;

    if (sb_fiber_lisp_stack_alloc(f, stack_size) != 0) {
        munmap(f->stack_base, f->stack_alloc_size);
        munmap(f->binding_stack_base, f->binding_stack_alloc_size);
        free(f);
        return NULL;
    }
    return f;
}

/* main fiber wraps the thread's own stack. */
struct sb_fiber *sb_fiber_create_main(struct thread *th)
{
    struct sb_fiber *f = calloc(1, sizeof(struct sb_fiber));
    if (!f) return NULL;
    f->state = FIBER_RUNNING;
    f->binding_stack_pointer          = get_binding_stack_pointer(th);
    f->binding_stack_start_for_thread = th->binding_stack_start;
    f->current_catch_block            = th->current_catch_block;
    f->current_unwind_protect_block   = th->current_unwind_protect_block;
    f->cs_guard_protected = th->state_word.control_stack_guard_page_protected;
    sb_fiber_lisp_stack_capture_main(f, th);
    return f;
}

void sb_fiber_destroy(struct sb_fiber *f)
{
    if (!f) return;
    assert(f->state != FIBER_RUNNING);
    if (f->owner) sb_fiber_unregister(f->owner, f);
    fiber_release(f);
}

void sb_fiber_register(struct thread *th, struct sb_fiber *fiber)
{
    assert(fiber->owner == NULL);
    fiber->owner = th;
    fiber->next = thread_extra_data(th)->fiber_list;
    __atomic_store_n(&thread_extra_data(th)->fiber_list,
                     fiber, __ATOMIC_RELEASE);
}

void sb_fiber_unregister(struct thread *th, struct sb_fiber *fiber)
{
    struct sb_fiber **pp = &thread_extra_data(th)->fiber_list;
    while (*pp) {
        if (*pp == fiber) {
            *pp = fiber->next;
            fiber->next = NULL;
            fiber->owner = NULL;
            return;
        }
        pp = &(*pp)->next;
    }
}

static inline void swap_bindings_forward (struct thread *th,
                                          lispobj *base, lispobj *limit);
static inline void swap_bindings_backward(struct thread *th,
                                          lispobj *base, lispobj *limit);

void sb_fiber_lower_bs_guard(struct sb_fiber *f)
{
    size_t ps = os_reported_page_size;
    char *base = (char *)f->binding_stack_base;
    size_t usable = f->binding_stack_alloc_size - 3 * ps;
    mprotect(base + usable + ps, ps, PROT_READ | PROT_WRITE);
    mprotect(base + usable,      ps, PROT_NONE);
    f->bs_guard_protected = 0;
}

void sb_fiber_reset_bs_guard(struct sb_fiber *f)
{
    size_t ps = os_reported_page_size;
    char *base = (char *)f->binding_stack_base;
    size_t usable = f->binding_stack_alloc_size - 3 * ps;
    mprotect(base + usable + ps, ps, PROT_NONE);
    mprotect(base + usable,      ps, PROT_READ | PROT_WRITE);
    f->bs_guard_protected = 1;
}

static int sb_fiber_classify_bs_fault(struct thread *th, void *addr,
                                      struct sb_fiber **out_fiber)
{
    size_t ps = os_reported_page_size;
    char *a = (char *)addr;
    for (struct sb_fiber *f = thread_extra_data(th)->fiber_list;
         f; f = f->next) {
        if (!f->binding_stack_base) continue;
        char *base = (char *)f->binding_stack_base;
        size_t usable = f->binding_stack_alloc_size - 3 * ps;
        char *r = base + usable, *s = r + ps, *h = s + ps;
        if (a >= h && a < h + ps) { if (out_fiber) *out_fiber = f; return 1; }
        if (a >= s && a < s + ps) { if (out_fiber) *out_fiber = f; return 2; }
        if (a >= r && a < r + ps) { if (out_fiber) *out_fiber = f; return 3; }
    }
    return 0;
}

int sb_fiber_handle_bs_fault(void *context_, void *addr, struct thread *th)
{
    os_context_t *context = (os_context_t *)context_;
    struct sb_fiber *f = NULL;
    int kind = sb_fiber_classify_bs_fault(th, addr, &f);
    if (kind == 1) {                    /* HARD guard hit */
        fake_foreign_function_call(context);
        lose("Fiber binding stack exhausted, fault: %p, PC: %p",
             addr, (void*)os_context_pc(context));
    } else if (kind == 2) {             /* SOFT guard hit */
        sb_fiber_lower_bs_guard(f);
        if (lose_on_corruption_p) {
            fake_foreign_function_call(context);
            lose("Fiber binding stack exhausted (lose-on-corruption)");
        }
        unblock_signals_in_context_and_maybe_warn(context);
        arrange_return_to_lisp_function
            (context, StaticSymbolFunction(BINDING_STACK_EXHAUSTED_ERROR));
        return 1;
    } else if (kind == 3) {             /* RETURN guard -- re-arm */
        sb_fiber_reset_bs_guard(f);
        return 1;
    }
    return 0;
}

typedef void (*fiber_swap_context_fn)(void *save, void *restore);

void fiber_trampoline_c(struct sb_fiber *self)
{
    struct thread *th = get_sb_vm_thread();
    sb_fiber_exit_pa(th);
#ifdef LISP_FEATURE_ARM64
    th->control_stack_pointer = self->control_stack_pointer;
    th->control_frame_pointer = self->control_frame_pointer;
#endif
    self->entry_fn(self->entry_arg);
    self->state = FIBER_DEAD;

    if (self->return_fiber) {
        struct sb_fiber *ret = self->return_fiber;
        sb_fiber_switch_prep(self, ret);
        th->current_catch_block          = ret->current_catch_block;
        th->current_unwind_protect_block = ret->current_unwind_protect_block;
        set_binding_stack_pointer(th, ret->binding_stack_pointer);
        th->binding_stack_start          = ret->binding_stack_start_for_thread;
        ret->state = FIBER_RUNNING;
        ((fiber_swap_context_fn)SYMBOL(FIBER_SWAP_CONTEXT)->value)
            (&self->ctx, &ret->ctx);
    }
    lose("fiber_trampoline_c reached past auto-return");
}

/* --- Binding-stack swap --- */

#ifdef LISP_FEATURE_TLS_LOAD_INDIRECT
extern lispobj *tlsindex_to_symbol_map;
extern int tls_map_starting_offset;
#  ifndef NO_TLS_VALUE_MARKER
#    define NO_TLS_VALUE_MARKER (~(uword_t)0)
#  endif
#endif

static inline void exchange_binding_with_tls(struct thread *th, struct binding *b)
{
    if (b->symbol && b->symbol != UNBOUND_MARKER_WIDETAG) {
#ifdef LISP_FEATURE_SB_THREAD
        lispobj *tls_slot = (lispobj *)(b->symbol + (char *)th);
#else
        lispobj *tls_slot = &SYMBOL(b->symbol)->value;
#endif
        lispobj tmp = *tls_slot;
        *tls_slot = b->value;
        b->value = tmp;

#ifdef LISP_FEATURE_TLS_LOAD_INDIRECT
        /* Maintain the indirect cell for symbols at or above the
         * map-starting offset (others are :always-thread-local; see
         * README.md "Binding-stack swap"). */
        if (b->symbol >= (lispobj)tls_map_starting_offset) {
            lispobj *indirect_cell =
                (lispobj *)((char *)th + b->symbol - N_WORD_BYTES);
            if (*tls_slot == (lispobj)NO_TLS_VALUE_MARKER) {
                lispobj symbol =
                    tlsindex_to_symbol_map[b->symbol >> (1 + WORD_SHIFT)];
                *indirect_cell =
                    (symbol == (lispobj)NO_TLS_VALUE_MARKER)
                    ? (lispobj)((char *)th + b->symbol - 1)
                    : symbol;
            } else {
                *indirect_cell = (lispobj)((char *)th + b->symbol - 1);
            }
        }
#endif
    }
}

/* Swap-in: oldest -> newest. */
static inline void swap_bindings_forward(struct thread *th,
                                         lispobj *base, lispobj *limit)
{
    for (struct binding *b = (struct binding *)base,
           *end = (struct binding *)limit;
         b < end; b++)
        exchange_binding_with_tls(th, b);
}

/* Swap-out: newest -> oldest. */
static inline void swap_bindings_backward(struct thread *th,
                                          lispobj *base, lispobj *limit)
{
    struct binding *start = (struct binding *)base;
    struct binding *b     = (struct binding *)limit;
    while (b > start) { b--; exchange_binding_with_tls(th, b); }
}

static inline void fiber_enter_pa(struct thread *th)
{
#if defined LISP_FEATURE_ARM64
    /* Low 32 bits = flag_PseudoAtomic; high 32 bits = PA_INTERRUPTED. */
    ((volatile uint32_t *)&th->pseudo_atomic_bits)[0] = flag_PseudoAtomic;
#else
    /* Whole word nonzero; bit 0 reserved for PA_INTERRUPTED. */
    th->pseudo_atomic_bits = (uword_t)th;
#endif
}

void sb_fiber_switch_prep(struct sb_fiber *from, struct sb_fiber *to)
{
    struct thread *th = get_sb_vm_thread();
    fiber_enter_pa(th);

    from->binding_stack_pointer = get_binding_stack_pointer(th);

    sb_fiber_lisp_stack_suspend(from, th);
    sb_fiber_lisp_stack_resume(to, th);

    if (from->binding_stack_base)
        swap_bindings_backward(th, from->binding_stack_base,
                               from->binding_stack_pointer);
    if (to->binding_stack_base)
        swap_bindings_forward(th, to->binding_stack_base,
                              to->binding_stack_pointer);
}

void sb_fiber_exit_pa(struct thread *th)
{
#if defined LISP_FEATURE_ARM64
    volatile uint32_t *halves = (volatile uint32_t *)&th->pseudo_atomic_bits;
    halves[0] = 0;
    if (halves[1]) {
        halves[1] = 0;
        asm volatile("brk %0" : : "i"(trap_PendingInterrupt));
    }
#else
    uword_t pa = __sync_xor_and_fetch(&th->pseudo_atomic_bits, (uword_t)th);
    if (pa) {
#if defined LISP_FEATURE_UD2_BREAKPOINTS
        asm volatile("ud2\n\t.byte %c0" : : "i"(trap_PendingInterrupt));
#else
        asm volatile("ud2");
#endif
    }
#endif
}
