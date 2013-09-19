/*
 * support for dynamic binding from C
 * See the "Chapter 9: Specials" of the SBCL Internals Manual.
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#include <stdio.h>
#include <stdlib.h>

#include "sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "dynbind.h"
#include "thread.h"
#include "pseudo-atomic.h"
#include "genesis/symbol.h"
#include "genesis/binding.h"
#include "genesis/static-symbols.h"

void bind_variable(lispobj symbol, lispobj value, void *th)
{
    struct binding *binding;
    struct thread *thread=(struct thread *)th;
    binding = (struct binding *)get_binding_stack_pointer(thread);
    set_binding_stack_pointer(thread,binding+1);
#ifdef LISP_FEATURE_SB_THREAD
    {
        struct symbol *sym=(struct symbol *)native_pointer(symbol);
        if(!sym->tls_index) {
            lispobj *tls_index_lock=
                &((struct symbol *)native_pointer(TLS_INDEX_LOCK))->value;
            FSHOW_SIGNAL((stderr, "entering dynbind tls alloc\n"));
            set_pseudo_atomic_atomic(thread);
            get_spinlock(tls_index_lock,(uword_t)th);
            if(!sym->tls_index) {
                sym->tls_index=SymbolValue(FREE_TLS_INDEX,0);
                SetSymbolValue(FREE_TLS_INDEX, sym->tls_index+N_WORD_BYTES, 0);
                if((sym->tls_index)>=(TLS_SIZE << WORD_SHIFT)) {
                    lose("Thread local storage exhausted.");
                }
            }
            release_spinlock(tls_index_lock);
            FSHOW_SIGNAL((stderr, "exiting dynbind tls alloc\n"));
            clear_pseudo_atomic_atomic(thread);
            if (get_pseudo_atomic_interrupted(thread))
                do_pending_interrupt();
        }
        binding->symbol = sym->tls_index;
        binding->value = SymbolTlValue(symbol, thread);
    }
#else
    binding->symbol = symbol;
    binding->value = SymbolTlValue(symbol, thread);
#endif
    SetTlSymbolValue(symbol, value, thread);
}

void
unbind(void *th)
{
    struct thread *thread=(struct thread *)th;
    struct binding *binding;
    lispobj symbol;

    binding = ((struct binding *)get_binding_stack_pointer(thread)) - 1;

    /* On sb-thread, it's actually a tls-index */
    symbol = binding->symbol;

#ifdef LISP_FEATURE_SB_THREAD

    ((union per_thread_data *)thread)->dynamic_values[(symbol) >> WORD_SHIFT]
        = binding->value;
#else
    SetSymbolValue(symbol, binding->value, thread);
#endif

    binding->symbol = 0;
    binding->value = 0;

    set_binding_stack_pointer(thread,binding);
}

void
unbind_to_here(lispobj *bsp,void *th)
{
    struct thread *thread=(struct thread *)th;
    struct binding *target = (struct binding *)bsp;
    struct binding *binding = (struct binding *)get_binding_stack_pointer(thread);
    lispobj symbol;

    while (target < binding) {
        binding--;

        symbol = binding->symbol;
        if (symbol) {
            if (symbol != UNBOUND_MARKER_WIDETAG) {
#ifdef LISP_FEATURE_SB_THREAD
                ((union per_thread_data *)thread)->dynamic_values[(symbol) >> WORD_SHIFT]
                    = binding->value;
#else
                SetSymbolValue(symbol, binding->value, thread);
#endif
            }
            binding->symbol = 0;
            binding->value = 0;
        }
    }
    set_binding_stack_pointer(thread,binding);
}
