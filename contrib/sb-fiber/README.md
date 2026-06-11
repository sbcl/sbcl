# sb-fiber notes

These are some technical notes on implementation of fibers.
User-facing API documentation can be found in `sb-fiber.texinfo`.

## Code organization

| Role       | Files                               |
|------------|-------------------------------------|
| Lisp API   | `contrib/sb-fiber/fiber.lisp`       |
| Lisp <-> C | `contrib/sb-fiber/fiber-ffi.lisp`   |
| VOPs       | `contrib/sb-fiber/<arch>-vops.lisp` |
| Assembly   | `src/assembly/<arch>/tramps.lisp`   |
| Runtime    | `src/runtime/<arch>-fiber.{c, h}`   |
|            | `src/runtime/fiber.{c, h}`          |

C manages allocation of stacks, GC integration, binding stack swap,
pseudo-atomic entry and exit, and defines per-architecture context
structures to hold callee-saved registers that are preserved when a
fiber is switched.

Lisp manages argument validation, catch and unwind chain save and
install, `*current-fiber*`, re-signaling conditions captured by the
trampoline, and defines the VOP that implements register swap inline
at the `switch-fiber` call site.

## Stack layout

Fibers' control and binding stacks are mapped regions with guard pages
that follow the per-thread overflow layout.  On
`#-c-stack-is-control-stack` targets (e.g. arm64) the Lisp control
stack and the C stack are separate regions that both belong to a
fiber.

## Binding stack swap

A binding stack entry is `(value, tls-index)`, where value is the
TLS value held just before the binding was pushed.  `N` nested
bindings of the same symbol form a chain: entry `k` stores `V_{k-1}`,
and the live TLS slot holds `V_N`.

`swap_bindings_{forward,backward}` in `fiber.c`:

To suspend a fiber, walk the entries from newest to oldest, exchanging
each entry's saved value with the live TLS.  After the pass, entry `k`
holds `V_k` and TLS holds `V_0`. To resume, walk oldest to newest.

The `:tls-load-indirect` feature maintains an indirect cell per TLS
slot pointing at the live value; `exchange_binding_with_tls` maintains
it alongside the TLS exchange.

## Pseudo-atomic switch window

A thread is inconsistent during `switch-fiber`, so the swap is bracketed
by pseudo-atomic. The Lisp shim enters PA by calling the C function
`sb_fiber_switch_prep`, which stages BSP swap, control stack bounds
swap, state flip, and binding stack swap.

The `%swap-regs` VOP then does the register and SP swap, and
transfers control to the resuming fiber's stack.  The resuming side,
at the VOP's `RESUME` label, exits PA and checks to see if a signal
arrived during the window.

`sb_fiber_exit_pa` is the same exit path for the trampoline's
auto-return flow, which runs in C and can't use the VOP's exit.

## GC

`extra_thread_data->fiber_list` enumerates every registered fiber on
a thread: GC walks it.

Suspended fibers marked runnable or new have their saved SP range
`[ctx.sp .. stack_end)` and their callee-saved registers
conservatively pinned.  On arm64 the separate Lisp control stack
`[base, csp_save)` is pinned the same way; the region above
`csp_save` is dead and unscanned.

On arm64, a conservative scanner walks `[base, CSP_save)` on a
suspended fiber and pins anything pointer-shaped.

`sb_fiber_lisp_stack_suspend` maintains a `dirty_high` per fiber: the
address such that `[dirty_high, usable_end)` is known clean.  On each
suspend:

- `CSP == dirty_high` (tight yield loop): no scrub.
- `CSP < dirty_high` (fiber returned to a shallower depth): scrub
  `[CSP, dirty_high)`.
- `CSP > dirty_high` (fiber grew above prior clean boundary):
  scrub all the way to `usable_end`.

Tight-yield fibers pay zero scrub cost after the first suspend.

## Trampoline

When a fiber's entry function returns normally, control re-enters
`fiber_tramp_c`, which marks the fiber dead and switches to
`self->return_fiber` via `sb_fiber_switch_prep` + the assembly
`fiber_swap_context`.

The resuming fiber exits PA via its own VOP tail or
`sb_fiber_exit_pa`.

The Lisp wrapper for the resumed fiber is found via
`*current-fiber*`. The Lisp shim sets it to `to` before the swap, and
TLS persists across the stack swap unchanged.

## Image survival

A saved core's restart restores Lisp wrappers but not the C
`sb_fiber_ctx` structs they point at.  An `*init-hooks*` callback
clears `*current-fiber*` on startup; user code holding wrappers across
`save-lisp-and-die` is on its own, like `sb-thread`.
