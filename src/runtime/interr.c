/*
 * stuff to handle internal errors
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
#include <stdarg.h>
#include <stdlib.h>

#include "sbcl.h"
#include "arch.h"
#include "signal.h"

#include "runtime.h"
#include "interr.h"
#include "print.h"
#include "lispregs.h"
#include "genesis/static-symbols.h"
#include "genesis/vector.h"
#include "thread.h"

/* the way that we shut down the system on a fatal error */

static void
default_lossage_handler(void)
{
    exit(1);
}
static void (*lossage_handler)(void) = default_lossage_handler;
void
set_lossage_handler(void handler(void))
{
    lossage_handler = handler;
}

never_returns
lose(char *fmt, ...)
{
    va_list ap;
    fprintf(stderr, "fatal error encountered in SBCL pid %d",getpid());
    /* freeze all the other threads, so we have a chance of debugging them 
     */
    if(all_threads) {
	struct thread *th1,*th=arch_os_get_current_thread();
	for_each_thread(th1) {
	    if(th1!=th) kill_thread(th1->os_thread,SIGSTOP);
	}
    }

    if (fmt) {
	fprintf(stderr, ":\n");
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
    }
    fprintf(stderr, "\n");
    fflush(stderr);
    lossage_handler();
    fprintf(stderr, "Argh! lossage_handler() returned, total confusion..\n");
    exit(1);
}

/* internal error handler for when the Lisp error system doesn't exist
 *
 * FIXME: Shouldn't error output go to stderr instead of stdout? (Alas,
 * this'd require changes in a number of things like brief_print(..),
 * or I'd have changed it immediately.) */
void
describe_internal_error(os_context_t *context)
{
    unsigned char *ptr = arch_internal_error_arguments(context);
    int len, scoffset, sc, offset, ch;

    len = *ptr++;
    printf("internal error #%d\n", *ptr++);
    len--;
    while (len > 0) {
	scoffset = *ptr++;
	len--;
	if (scoffset == 253) {
	    scoffset = *ptr++;
	    len--;
	}
	else if (scoffset == 254) {
	    scoffset = ptr[0] + ptr[1]*256;
	    ptr += 2;
	    len -= 2;
	}
	else if (scoffset == 255) {
	    scoffset = ptr[0] + (ptr[1]<<8) + (ptr[2]<<16) + (ptr[3]<<24);
	    ptr += 4;
	    len -= 4;
	}
	sc = scoffset & 0x1f;
	offset = scoffset >> 5;
		
	printf("    SC: %d, Offset: %d", sc, offset);
	switch (sc) {
	case sc_AnyReg:
	case sc_DescriptorReg:
	    putchar('\t');
	    brief_print(*os_context_register_addr(context, offset));
	    break;

	case sc_CharacterReg:
	    ch = *os_context_register_addr(context, offset);
#ifdef LISP_FEATURE_X86
	    if (offset&1)
		ch = ch>>8;
	    ch = ch & 0xff;
#endif
	    switch (ch) {
	    case '\n': printf("\t'\\n'\n"); break;
	    case '\b': printf("\t'\\b'\n"); break;
	    case '\t': printf("\t'\\t'\n"); break;
	    case '\r': printf("\t'\\r'\n"); break;
	    default:
		if (ch < 32 || ch > 127)
		    printf("\\%03o", ch);
		else
		    printf("\t'%c'\n", ch);
		break;
	    }
	    break;
	case sc_SapReg:
#ifdef sc_WordPointerReg
	case sc_WordPointerReg:
#endif
	    printf("\t0x%08lx\n", *os_context_register_addr(context, offset));
	    break;
	case sc_SignedReg:
	    printf("\t%ld\n", *os_context_register_addr(context, offset));
	    break;
	case sc_UnsignedReg:
	    printf("\t%lu\n", *os_context_register_addr(context, offset));
	    break;
#ifdef sc_SingleFloatReg
	case sc_SingleFloatReg:
	    printf("\t%g\n", *(float *)&context->sc_fpregs[offset]);
	    break;
#endif
#ifdef sc_DoubleFloatReg
	case sc_DoubleFloatReg:
	    printf("\t%g\n", *(double *)&context->sc_fpregs[offset]);
	    break;
#endif
	default:
	    printf("\t???\n");
	    break;
	}
    }
}

/* utility routines used by miscellaneous pieces of code */

lispobj debug_print(lispobj string)
{
    /* This is a kludge.  It's not actually safe - in general - to use 
       %primitive print on the alpha, because it skips half of the
       number stack setup that should usually be done on a function call,
       so the called routine (i.e. this one) ends up being able to overwrite
       local variables in the caller.  Rather than fix this everywhere
       that %primitive print is used (it's only a debugging aid anyway)
       we just put guarantee our safety by putting an unused buffer on
       the stack before doing anything else here */
    char untouched[32]; /* GCC warns about not using this, but that's the point.. */
    fprintf(stderr, "%s\n", 
	    (char *)(((struct vector *)native_pointer(string))->data),untouched);
    return NIL;
}
