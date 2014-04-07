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
#include "genesis/symbol.h"
#include "genesis/binding.h"

/* Specially bind SYMBOL to VALUE. In a multithreaded build, SYMBOL must already
   have been assigned a thread-local storage index. See *KNOWN-TLS-SYMBOLS* in
   compiler/generic/genesis for the symbols whose indices are pre-assigned. */
void bind_variable(lispobj symbol, lispobj value, void *th)
{
    struct binding *binding;
    struct thread *thread=(struct thread *)th;
    binding = (struct binding *)get_binding_stack_pointer(thread);
    set_binding_stack_pointer(thread,binding+1);
#ifdef LISP_FEATURE_SB_THREAD
    {
        struct symbol *sym=(struct symbol *)native_pointer(symbol);
        // We could provide a c-callable static Lisp function to assign TLS
        // indices if anyone really needs dynamic binding of dynamic symbols.
        binding->symbol = tls_index_of(sym);
        if(!binding->symbol)
            lose("Oops! Static symbol missing from *KNOWN-TLS-SYMBOLS*");
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
