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

#include "sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "dynbind.h"
#include "thread.h"
#include "genesis/symbol.h"
#include "genesis/binding.h"
#include "genesis/thread.h"
#include "genesis/static-symbols.h"

#if defined(BINDING_STACK_POINTER)
#define GetBSP() ((struct binding *)SymbolValue(BINDING_STACK_POINTER,thread))
#define SetBSP(value) SetSymbolValue(BINDING_STACK_POINTER, (lispobj)(value),thread)
#else
#define GetBSP() ((struct binding *)current_binding_stack_pointer)
#define SetBSP(value) (current_binding_stack_pointer=(lispobj *)(value))
#endif

void bind_variable(lispobj symbol, lispobj value, void *th)
{
    lispobj old_tl_value;
    struct binding *binding;
    struct thread *thread=(struct thread *)th;
#ifdef LISP_FEATURE_SB_THREAD
    struct symbol *sym=(struct symbol *)native_pointer(symbol);
#endif
    binding = GetBSP();
    SetBSP(binding+1);
#ifdef LISP_FEATURE_SB_THREAD
    if(!sym->tls_index) {
	sym->tls_index=SymbolValue(FREE_TLS_INDEX,0);
	SetSymbolValue(FREE_TLS_INDEX,
		       make_fixnum(fixnum_value(sym->tls_index)+1),0);
    }
#endif
    old_tl_value=SymbolTlValue(symbol,thread);
    binding->value = old_tl_value;
    binding->symbol = symbol;
    SetTlSymbolValue(symbol, value,thread);
}

void
unbind(void *th)
{
    struct thread *thread=(struct thread *)th;
    struct binding *binding;
    lispobj symbol;
	
    binding = GetBSP() - 1;
		
    symbol = binding->symbol;

    SetTlSymbolValue(symbol, binding->value,thread);

    binding->symbol = 0;

    SetBSP(binding);
}

void
unbind_to_here(lispobj *bsp,void *th)
{
    struct thread *thread=(struct thread *)th;
    struct binding *target = (struct binding *)bsp;
    struct binding *binding = GetBSP();
    lispobj symbol;

    while (target < binding) {
	binding--;

	symbol = binding->symbol;
	if (symbol) {
	    SetTlSymbolValue(symbol, binding->value,thread);
	    binding->symbol = 0;
	}
    }
    SetBSP(binding);
}
