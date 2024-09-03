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

#include "genesis/sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "thread.h"
#include "interr.h"
#include "genesis/symbol.h"

/* Specially bind SYMBOL to VALUE. In a multithreaded build, SYMBOL must already
   have been assigned a thread-local storage index. See *KNOWN-TLS-SYMBOLS* in
   compiler/generic/genesis for the symbols whose indices are pre-assigned. */
#ifdef LISP_FEATURE_SB_THREAD
#define value_address(thing, thread) (lispobj*)(thing + (char*)thread)
void bind_tls_cell(unsigned symbol, lispobj value, void *th)
#else
#define value_address(thing, thread) &SYMBOL(thing)->value
void bind_variable(lispobj symbol, lispobj value, void *th)
#endif
{
    struct binding *binding;
    __attribute__((unused)) struct thread *thread = (struct thread *)th;
    binding = (struct binding *)get_binding_stack_pointer(thread);
    set_binding_stack_pointer(thread,binding+1);
    binding->symbol = symbol;
    lispobj* where = value_address(symbol, thread);
    binding->value = *where;
    *where = value;
}

void
unbind(void *th)
{
     __attribute__((unused)) struct thread *thread = (struct thread *)th;
    struct binding *binding;

    binding = ((struct binding *)get_binding_stack_pointer(thread)) - 1;

    /* On sb-thread, 'binding->symbol' is actually a tls-index */
    *value_address(binding->symbol, thread) = binding->value;

    binding->symbol = 0;
    binding->value = 0;

    set_binding_stack_pointer(thread,binding);
}

void
unbind_to_here(lispobj *bsp,void *th)
{
     __attribute__((unused)) struct thread *thread = (struct thread *)th;
    struct binding *target = (struct binding *)bsp;
    struct binding *binding = (struct binding *)get_binding_stack_pointer(thread);
    lispobj symbol;

    while (target < binding) {
        binding--;

        symbol = binding->symbol;
        if (symbol) {
            if (symbol != UNBOUND_MARKER_WIDETAG) {
                *value_address(symbol, thread) = binding->value;
            }
            binding->symbol = 0;
            binding->value = 0;
        }
    }
    set_binding_stack_pointer(thread,binding);
}
