/*
 * simple backtrace facility
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
#include <signal.h>
#include "runtime.h"
#include "sbcl.h"
#include "globals.h"
#include "os.h"
#include "interrupt.h"
#include "lispregs.h"

#ifndef __i386__

/* KLUDGE: Sigh ... I know what the call frame looks like and it had
 * better not change. */

struct call_frame {
#ifndef alpha
	struct call_frame *old_cont;
#else
        u32 old_cont;
#endif
	lispobj saved_lra;
        lispobj code;
	lispobj other_state[5];
};

struct call_info {
#ifndef alpha
    struct call_frame *frame;
#else
    u32 frame;
#endif
    int interrupted;
#ifndef alpha
    struct code *code;
#else
    u32 code;
#endif
    lispobj lra;
    int pc; /* Note: this is the trace file offset, not the actual pc. */
};

#define HEADER_LENGTH(header) ((header)>>8)

static int previous_info(struct call_info *info);

static struct code *
code_pointer(lispobj object)
{
    lispobj *headerp, header;
    int type, len;

    headerp = (lispobj *) native_pointer(object);
    header = *headerp;
    type = widetag_of(header);

    switch (type) {
        case CODE_HEADER_WIDETAG:
            break;
        case RETURN_PC_HEADER_WIDETAG:
        case SIMPLE_FUN_HEADER_WIDETAG:
        case CLOSURE_FUN_HEADER_WIDETAG:
            len = HEADER_LENGTH(header);
            if (len == 0)
                headerp = NULL;
            else
                headerp -= len;
            break;
        default:
            headerp = NULL;
    }

    return (struct code *) headerp;
}

static boolean
cs_valid_pointer_p(struct call_frame *pointer)
{
  /* lose("stub: hasn't been updated for X86"); */
    return (((char *) CONTROL_STACK_START <= (char *) pointer) &&
	    ((char *) pointer < (char *) current_control_stack_pointer));
}

static void
call_info_from_lisp_state(struct call_info *info)
{
    info->frame = (struct call_frame *)current_control_frame_pointer;
    info->interrupted = 0;
    info->code = NULL;
    info->lra = 0;
    info->pc = 0;

    previous_info(info);
}

static void
call_info_from_context(struct call_info *info, os_context_t *context)
{
    unsigned long pc;

    info->interrupted = 1;
    if (lowtag_of(*os_context_register_addr(context, reg_CODE))
	== FUN_POINTER_LOWTAG) {
        /* We tried to call a function, but crapped out before $CODE could
         * be fixed up. Probably an undefined function. */
        info->frame =
	    (struct call_frame *)(*os_context_register_addr(context,
							    reg_OCFP));
        info->lra = (lispobj)(*os_context_register_addr(context, reg_LRA));
        info->code = code_pointer(info->lra);
        pc = (unsigned long)native_pointer(info->lra);
    }
    else {
        info->frame =
	    (struct call_frame *)(*os_context_register_addr(context, reg_CFP));
        info->code =
	    code_pointer(*os_context_register_addr(context, reg_CODE));
        info->lra = NIL;
        pc = *os_context_pc_addr(context);
    }
    if (info->code != NULL)
        info->pc = pc - (unsigned long) info->code -
#ifndef alpha
            (HEADER_LENGTH(info->code->header) * sizeof(lispobj));
#else
            (HEADER_LENGTH(((struct code *)info->code)->header) * sizeof(lispobj));
#endif
    else
        info->pc = 0;
}

static int
previous_info(struct call_info *info)
{
    struct call_frame *this_frame;
    int free;

    if (!cs_valid_pointer_p(info->frame)) {
        printf("Bogus callee value (0x%08x).\n", (unsigned long)info->frame);
        return 0;
    }

    this_frame = info->frame;
    info->lra = this_frame->saved_lra;
    info->frame = this_frame->old_cont;
    info->interrupted = 0;

    if (info->frame == NULL || info->frame == this_frame)
        return 0;

    if (info->lra == NIL) {
        /* We were interrupted. Find the correct signal context. */
        free = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
        while (free-- > 0) {
	    os_context_t *context = 
		lisp_interrupt_contexts[free];
            if ((struct call_frame *)(*os_context_register_addr(context,
								reg_CFP))
		== info->frame) {
                call_info_from_context(info, context);
                break;
            }
        }
    }
    else {
        info->code = code_pointer(info->lra);
        if (info->code != NULL)
            info->pc = (unsigned long)native_pointer(info->lra) -
                (unsigned long)info->code -
#ifndef alpha
                (HEADER_LENGTH(info->code->header) * sizeof(lispobj));
#else
                (HEADER_LENGTH(((struct code *)info->code)->header) * sizeof(lispobj));
#endif
        else
            info->pc = 0;
    }

    return 1;
}

void
backtrace(int nframes)
{
    struct call_info info;
	
    call_info_from_lisp_state(&info);

    do {
        printf("<Frame 0x%08x%s, ", (unsigned long) info.frame,
                info.interrupted ? " [interrupted]" : "");

        if (info.code != (struct code *) 0) {
            lispobj function;

            printf("CODE: 0x%08X, ", (unsigned long) info.code | OTHER_POINTER_LOWTAG);

#ifndef alpha
            function = info.code->entry_points;
#else
            function = ((struct code *)info.code)->entry_points;
#endif
            while (function != NIL) {
                struct simple_fun *header;
                lispobj name;

                header = (struct simple_fun *) native_pointer(function);
                name = header->name;

                if (lowtag_of(name) == OTHER_POINTER_LOWTAG) {
                    lispobj *object;

                    object = (lispobj *) native_pointer(name);

                    if (widetag_of(*object) == SYMBOL_HEADER_WIDETAG) {
                        struct symbol *symbol;

                        symbol = (struct symbol *) object;
                        object = (lispobj *) native_pointer(symbol->name);
                    }
                    if (widetag_of(*object) == SIMPLE_STRING_WIDETAG) {
                        struct vector *string;

                        string = (struct vector *) object;
                        printf("%s, ", (char *) string->data);
                    } else
                        printf("(Not simple string??\?), ");
                } else
                    printf("(Not other pointer??\?), ");


                function = header->next;
            }
        }
        else
            printf("CODE: ???, ");

        if (info.lra != NIL)
            printf("LRA: 0x%08x, ", (unsigned long)info.lra);
        else
            printf("<no LRA>, ");

        if (info.pc)
            printf("PC: 0x%x>\n", info.pc);
        else
            printf("PC: ??\?>\n");

    } while (--nframes > 0 && previous_info(&info));
}

#else

void
backtrace(int nframes)
{
    printf("Can't backtrace on this hardware platform.\n");
}

#endif
