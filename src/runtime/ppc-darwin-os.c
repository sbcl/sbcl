#include "globals.h"
#include <signal.h>
#include <ucontext.h>
#include "bsd-os.h"

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    ppc_saved_state_t *state = &context->uc_mcontext->ss;
    switch(offset) {
    case 0:
	return &state->r0;
    case 1:
	return &state->r1;
    case 2:
	return &state->r2;
    case 3:
	return &state->r3;
    case 4:
	return &state->r4;
    case 5:
	return &state->r5;
    case 6:
	return &state->r6;
    case 7:
	return &state->r7;
    case 8:
	return &state->r8;
    case 9:
	return &state->r9;
    case 10:
	return &state->r10;
    case 11:
	return &state->r11;
    case 12:
	return &state->r12;
    case 13:
	return &state->r13;
    case 14:
	return &state->r14;
    case 15:
	return &state->r15;
    case 16:
	return &state->r16;
    case 17:
	return &state->r17;
    case 18:
	return &state->r18;
    case 19:
	return &state->r19;
    case 20:
	return &state->r20;
    case 21:
	return &state->r21;
    case 22:
	return &state->r22;
    case 23:
	return &state->r23;
    case 24:
	return &state->r24;
    case 25:
	return &state->r25;
    case 26:
	return &state->r26;
    case 27:
	return &state->r27;
    case 28:
	return &state->r28;
    case 29:
	return &state->r29;
    case 30:
	return &state->r30;
    case 31:
	return &state->r31;
    case 41:
	/* PT_DAR */
	return &context->uc_mcontext->es.dar;
    case 42:
	/* PT_DSISR */
	return &context->uc_mcontext->es.dsisr;
    }
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return &context->uc_mcontext->ss.lr;
}

void 
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    /* see ppc-arch.c */
    ppc_flush_icache(address,length);
}
