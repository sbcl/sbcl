/*
 * support for dynamic binding from C
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
#include "genesis/thread.h"
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
            get_spinlock(tls_index_lock,(long)th);
            if(!sym->tls_index) {
                sym->tls_index=SymbolValue(FREE_TLS_INDEX,0);
                SetSymbolValue(FREE_TLS_INDEX,
                               make_fixnum(fixnum_value(sym->tls_index)+1),0);
                if(fixnum_value(sym->tls_index)>=TLS_SIZE) {
                    lose("Thread local storage exhausted.");
                }
            }
            release_spinlock(tls_index_lock);
            FSHOW_SIGNAL((stderr, "exiting dynbind tls alloc\n"));
            clear_pseudo_atomic_atomic(thread);
            if (get_pseudo_atomic_interrupted(thread))
                do_pending_interrupt();
        }
    }
#endif
    binding->value = SymbolTlValue(symbol, thread);
    binding->symbol = symbol;
    SetTlSymbolValue(symbol, value, thread);
}

void
unbind(void *th)
{
    struct thread *thread=(struct thread *)th;
    struct binding *binding;
    lispobj symbol;

    binding = ((struct binding *)get_binding_stack_pointer(thread)) - 1;

    symbol = binding->symbol;

    SetTlSymbolValue(symbol, binding->value,thread);

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
                SetTlSymbolValue(symbol, binding->value,thread);
            }
            binding->symbol = 0;
            binding->value = 0;
        }
    }
    set_binding_stack_pointer(thread,binding);
}
