/* code for low-level debugging/diagnostic output */

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

/*
 * FIXME:
 *   Some of the code in here (the various
 *   foo_slots[], at least) is deeply broken, depending on guessing
 *   already out-of-date values instead of getting them from sbcl.h.
 */

#include <stdio.h>

#include "print.h"
#include "runtime.h"
#include "sbcl.h"

/* This file can be skipped if we're not supporting LDB. */
#if defined(LISP_FEATURE_SB_LDB)

#include "monitor.h"
#include "vars.h"
#include "os.h"
#include "gencgc-alloc-region.h" /* genesis/thread.h needs this */
#include "genesis/static-symbols.h"
#include "genesis/primitive-objects.h"

#include "genesis/static-symbols.h"



static int max_lines = 20, cur_lines = 0;
static int max_depth = 5, brief_depth = 2, cur_depth = 0;
static int max_length = 5;
static boolean dont_descend = 0, skip_newline = 0;
static int cur_clock = 0;

static void print_obj(char *prefix, lispobj obj);

#define NEWLINE_OR_RETURN if (continue_p(1)) newline(NULL); else return;

char *lowtag_Names[] = {
    "even fixnum",
    "instance pointer",
    "other immediate [0]",
    "list pointer",
    "odd fixnum",
    "function pointer",
    "other immediate [1]",
    "other pointer"
};

/* FIXME: Yikes! This table implicitly depends on the values in sbcl.h,
 * but doesn't actually depend on them, so if they change, it gets
 * all broken. We should either get rid of it or
 * rewrite the code so that it's cleanly initialized by gc_init_tables[]
 * in a way which varies correctly with the values in sbcl.h. */
char *subtype_Names[] = {
    "unused 0",
    "unused 1",
    "bignum",
    "ratio",
    "single float",
    "double float",
#ifdef LONG_FLOAT_WIDETAG
    "long float",
#endif
    "complex",
#ifdef COMPLEX_SINGLE_FLOAT_WIDETAG
    "complex single float",
#endif
#ifdef COMPLEX_DOUBLE_FLOAT_WIDETAG
    "complex double float",
#endif
#ifdef COMPLEX_LONG_FLOAT_WIDETAG
    "complex long float",
#endif
    "simple-array",
    "simple-string",
    "simple-bit-vector",
    "simple-vector",
    "(simple-array (unsigned-byte 2) (*))",
    "(simple-array (unsigned-byte 4) (*))",
    "(simple-array (unsigned-byte 8) (*))",
    "(simple-array (unsigned-byte 16) (*))",
    "(simple-array (unsigned-byte 32) (*))",
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
    "(simple-array (signed-byte 8) (*))",
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
    "(simple-array (signed-byte 16) (*))",
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG
    "(simple-array fixnum (*))",
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
    "(simple-array (signed-byte 32) (*))",
#endif
    "(simple-array single-float (*))",
    "(simple-array double-float (*))",
#ifdef SIMPLE_ARRAY_LONG_FLOAT_WIDETAG
    "(simple-array long-float (*))",
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG
    "(simple-array (complex single-float) (*))",
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG
    "(simple-array (complex double-float) (*))",
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
    "(simple-array (complex long-float) (*))",
#endif
    "complex-string",
    "complex-bit-vector",
    "(array * (*))",
    "array",
    "code header",
    "function header",
    "closure header",
    "funcallable-instance header",
    "unused function header 1",
    "unused function header 2",
    "unused function header 3",
    "closure function header",
    "return PC header",
    "value cell header",
    "symbol header",
    "character",
    "SAP",
    "unbound marker",
    "weak pointer",
    "instance header",
    "fdefn"
};

static void indent(int in)
{
    static char *spaces = "                                                                ";

    while (in > 64) {
        fputs(spaces, stdout);
        in -= 64;
    }
    if (in != 0)
        fputs(spaces + 64 - in, stdout);
}

static boolean continue_p(boolean newline)
{
    char buffer[256];

    if (cur_depth >= max_depth || dont_descend)
        return 0;

    if (newline) {
        if (skip_newline)
            skip_newline = 0;
        else
            putchar('\n');

        if (cur_lines >= max_lines) {
            printf("More? [y] ");
            fflush(stdout);

            fgets(buffer, sizeof(buffer), stdin);

            if (buffer[0] == 'n' || buffer[0] == 'N')
                throw_to_monitor();
            else
                cur_lines = 0;
        }
    }

    return 1;
}

static void newline(char *label)
{
    cur_lines++;
    if (label != NULL)
        fputs(label, stdout);
    putchar('\t');
    indent(cur_depth * 2);
}


static void brief_fixnum(lispobj obj)
{
#ifndef alpha
    printf("%ld", ((long)obj)>>2);
#else
    printf("%d", ((s32)obj)>>2);
#endif
}

static void print_fixnum(lispobj obj)
{
#ifndef alpha
    printf(": %ld", ((long)obj)>>2);
#else
    printf(": %d", ((s32)obj)>>2);
#endif
}

static void brief_otherimm(lispobj obj)
{
    int type, c, idx;
    char buffer[10];

    type = widetag_of(obj);
    switch (type) {
        case BASE_CHAR_WIDETAG:
            c = (obj>>8)&0xff;
            switch (c) {
                case '\0':
                    printf("#\\Null");
                    break;
                case '\n':
                    printf("#\\Newline");
                    break;
                case '\b':
                    printf("#\\Backspace");
                    break;
                case '\177':
                    printf("#\\Delete");
                    break;
                default:
                    strcpy(buffer, "#\\");
                    if (c >= 128) {
                        strcat(buffer, "m-");
                        c -= 128;
                    }
                    if (c < 32) {
                        strcat(buffer, "c-");
                        c += '@';
                    }
                    printf("%s%c", buffer, c);
                    break;
            }
            break;

        case UNBOUND_MARKER_WIDETAG:
            printf("<unbound marker>");
            break;

        default:
	    idx = type >> 2;
	    if (idx < (sizeof(lowtag_Names) / sizeof(char *)))
		    printf("%s", lowtag_Names[idx]);
	    else
		    printf("unknown type (0x%0x)", type);
            break;
    }
}

static void print_otherimm(lispobj obj)
{
    int type, idx;

    type = widetag_of(obj);
    idx = type >> 2;

    if (idx < (sizeof(lowtag_Names) / sizeof(char *)))
	    printf(", %s", lowtag_Names[idx]);
    else
	    printf(", unknown type (0x%0x)", type);

    switch (widetag_of(obj)) {
        case BASE_CHAR_WIDETAG:
            printf(": ");
            brief_otherimm(obj);
            break;

        case SAP_WIDETAG:
        case UNBOUND_MARKER_WIDETAG:
            break;

        default:
            printf(": data=%ld", (long) (obj>>8)&0xffffff);
            break;
    }
}

static void brief_list(lispobj obj)
{
    int space = 0;
    int length = 0;

    if (!is_valid_lisp_addr((os_vm_address_t)native_pointer(obj)))
	printf("(invalid Lisp-level address)");
    else if (obj == NIL)
        printf("NIL");
    else {
        putchar('(');
        while (lowtag_of(obj) == LIST_POINTER_LOWTAG) {
            struct cons *cons = (struct cons *)native_pointer(obj);

            if (space)
                putchar(' ');
            if (++length >= max_length) {
                printf("...");
                obj = NIL;
                break;
            }
            print_obj(NULL, cons->car);
            obj = cons->cdr;
            space = 1;
            if (obj == NIL)
                break;
        }
        if (obj != NIL) {
            printf(" . ");
            print_obj(NULL, obj);
        }
        putchar(')');
    }
}

static void print_list(lispobj obj)
{
    if (!is_valid_lisp_addr((os_vm_address_t)native_pointer(obj))) {
	printf("(invalid address)");
    } else if (obj == NIL) {
        printf(" (NIL)");
    } else {
        struct cons *cons = (struct cons *)native_pointer(obj);

        print_obj("car: ", cons->car);
        print_obj("cdr: ", cons->cdr);
    }
}

static void brief_struct(lispobj obj)
{
    printf("#<ptr to 0x%08lx instance>",
           (unsigned long) ((struct instance *)native_pointer(obj))->slots[0]);
}

static void print_struct(lispobj obj)
{
    struct instance *instance = (struct instance *)native_pointer(obj);
    int i;
    char buffer[16];
    print_obj("type: ", ((struct instance *)native_pointer(obj))->slots[0]);
    for (i = 1; i < HeaderValue(instance->header); i++) {
	sprintf(buffer, "slot %d: ", i);
	print_obj(buffer, instance->slots[i]);
    }
}

static void brief_otherptr(lispobj obj)
{
    lispobj *ptr, header;
    int type;
    struct symbol *symbol;
    struct vector *vector;
    char *charptr;

    ptr = (lispobj *) native_pointer(obj);

    if (!is_valid_lisp_addr((os_vm_address_t)obj)) {
	    printf("(invalid address)");
	    return;
    }

    header = *ptr;
    type = widetag_of(header);
    switch (type) {
        case SYMBOL_HEADER_WIDETAG:
            symbol = (struct symbol *)ptr;
            vector = (struct vector *)native_pointer(symbol->name);
            for (charptr = (char *)vector->data; *charptr != '\0'; charptr++) {
                if (*charptr == '"')
                    putchar('\\');
                putchar(*charptr);
            }
            break;

        case SIMPLE_BASE_STRING_WIDETAG:
            vector = (struct vector *)ptr;
            putchar('"');
            for (charptr = (char *)vector->data; *charptr != '\0'; charptr++) {
                if (*charptr == '"')
                    putchar('\\');
                putchar(*charptr);
            }
            putchar('"');
            break;

        default:
            printf("#<ptr to ");
            brief_otherimm(header);
            putchar('>');
    }
}

static void print_slots(char **slots, int count, lispobj *ptr)
{
    while (count-- > 0) {
        if (*slots) {
            print_obj(*slots++, *ptr++);
        } else {
            print_obj("???: ", *ptr++);
	}
    }
}

/* FIXME: Yikes again! This, like subtype_Names[], needs to depend
 * on the values in sbcl.h (or perhaps be generated automatically
 * by GENESIS as part of sbcl.h). */
static char *symbol_slots[] = {"value: ", "unused: ",
    "plist: ", "name: ", "package: ",
#ifdef LISP_FEATURE_SB_THREAD
    "tls-index: " ,
#endif			       
    NULL};
static char *ratio_slots[] = {"numer: ", "denom: ", NULL};
static char *complex_slots[] = {"real: ", "imag: ", NULL};
static char *code_slots[] = {"words: ", "entry: ", "debug: ", NULL};
static char *fn_slots[] = {
    "self: ", "next: ", "name: ", "arglist: ", "type: ", NULL};
static char *closure_slots[] = {"fn: ", NULL};
static char *funcallable_instance_slots[] = {"fn: ", "lexenv: ", "layout: ", NULL};
static char *weak_pointer_slots[] = {"value: ", NULL};
static char *fdefn_slots[] = {"name: ", "function: ", "raw_addr: ", NULL};
static char *value_cell_slots[] = {"value: ", NULL};

static void print_otherptr(lispobj obj)
{
    if (!is_valid_lisp_addr((os_vm_address_t)obj)) {
	printf("(invalid address)");
    } else {
#ifndef alpha
        lispobj *ptr;
        unsigned long header;
        unsigned long length;
#else
        u32 *ptr;
        u32 header;
        u32 length;
#endif
        int count, type, index;
        char *cptr, buffer[16];

	ptr = (lispobj*) native_pointer(obj);
	if (ptr == NULL) {
		printf(" (NULL Pointer)");
		return;
	}

	header = *ptr++;
	length = (*ptr) >> 2;
	count = header>>8;
	type = widetag_of(header);

        print_obj("header: ", header);
        if (lowtag_of(header) != OTHER_IMMEDIATE_0_LOWTAG &&
	    lowtag_of(header) != OTHER_IMMEDIATE_1_LOWTAG) {
            NEWLINE_OR_RETURN;
            printf("(invalid header object)");
            return;
        }

        switch (type) {
            case BIGNUM_WIDETAG:
                ptr += count;
                NEWLINE_OR_RETURN;
                printf("0x");
                while (count-- > 0)
                    printf("%08lx", (unsigned long) *--ptr);
                break;

            case RATIO_WIDETAG:
                print_slots(ratio_slots, count, ptr);
                break;

            case COMPLEX_WIDETAG:
                print_slots(complex_slots, count, ptr);
                break;

            case SYMBOL_HEADER_WIDETAG:
                print_slots(symbol_slots, count, ptr);
                break;

            case SINGLE_FLOAT_WIDETAG:
                NEWLINE_OR_RETURN;
                printf("%g", ((struct single_float *)native_pointer(obj))->value);
                break;

            case DOUBLE_FLOAT_WIDETAG:
                NEWLINE_OR_RETURN;
                printf("%g", ((struct double_float *)native_pointer(obj))->value);
                break;

#ifdef LONG_FLOAT_WIDETAG
            case LONG_FLOAT_WIDETAG:
                NEWLINE_OR_RETURN;
                printf("%Lg", ((struct long_float *)native_pointer(obj))->value);
                break;
#endif

#ifdef COMPLEX_SINGLE_FLOAT_WIDETAG
            case COMPLEX_SINGLE_FLOAT_WIDETAG:
                NEWLINE_OR_RETURN;
                printf("%g", ((struct complex_single_float *)native_pointer(obj))->real);
                NEWLINE_OR_RETURN;
                printf("%g", ((struct complex_single_float *)native_pointer(obj))->imag);
                break;
#endif

#ifdef COMPLEX_DOUBLE_FLOAT_WIDETAG
            case COMPLEX_DOUBLE_FLOAT_WIDETAG:
                NEWLINE_OR_RETURN;
                printf("%g", ((struct complex_double_float *)native_pointer(obj))->real);
                NEWLINE_OR_RETURN;
                printf("%g", ((struct complex_double_float *)native_pointer(obj))->imag);
                break;
#endif

#ifdef COMPLEX_LONG_FLOAT_WIDETAG
            case COMPLEX_LONG_FLOAT_WIDETAG:
                NEWLINE_OR_RETURN;
                printf("%Lg", ((struct complex_long_float *)native_pointer(obj))->real);
                NEWLINE_OR_RETURN;
                printf("%Lg", ((struct complex_long_float *)native_pointer(obj))->imag);
                break;
#endif

            case SIMPLE_BASE_STRING_WIDETAG:
                NEWLINE_OR_RETURN;
                cptr = (char *)(ptr+1);
                putchar('"');
                while (length-- > 0)
                    putchar(*cptr++);
                putchar('"');
                break;

            case SIMPLE_VECTOR_WIDETAG:
                NEWLINE_OR_RETURN;
                printf("length = %ld", length);
                ptr++;
                index = 0;
                while (length-- > 0) {
                    sprintf(buffer, "%d: ", index++);
                    print_obj(buffer, *ptr++);
                }
                break;

            case INSTANCE_HEADER_WIDETAG:
                NEWLINE_OR_RETURN;
                printf("length = %ld", (long) count);
                index = 0;
                while (count-- > 0) {
                    sprintf(buffer, "%d: ", index++);
                    print_obj(buffer, *ptr++);
                }
                break;

            case SIMPLE_ARRAY_WIDETAG:
            case SIMPLE_BIT_VECTOR_WIDETAG:
            case SIMPLE_ARRAY_UNSIGNED_BYTE_2_WIDETAG:
            case SIMPLE_ARRAY_UNSIGNED_BYTE_4_WIDETAG:
            case SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG:
            case SIMPLE_ARRAY_UNSIGNED_BYTE_16_WIDETAG:
            case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
	    case SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
	    case SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG
	    case SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
	    case SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG:
#endif
            case SIMPLE_ARRAY_SINGLE_FLOAT_WIDETAG:
            case SIMPLE_ARRAY_DOUBLE_FLOAT_WIDETAG:
#ifdef SIMPLE_ARRAY_LONG_FLOAT_WIDETAG
            case SIMPLE_ARRAY_LONG_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG
	    case SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG
	    case SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
	    case SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG:
#endif
            case COMPLEX_BASE_STRING_WIDETAG:
            case COMPLEX_VECTOR_NIL_WIDETAG:
            case COMPLEX_BIT_VECTOR_WIDETAG:
            case COMPLEX_VECTOR_WIDETAG:
            case COMPLEX_ARRAY_WIDETAG:
                break;

            case CODE_HEADER_WIDETAG:
                print_slots(code_slots, count-1, ptr);
                break;

            case SIMPLE_FUN_HEADER_WIDETAG:
                print_slots(fn_slots, 5, ptr);
                break;

            case RETURN_PC_HEADER_WIDETAG:
                print_obj("code: ", obj - (count * 4));
                break;

            case CLOSURE_HEADER_WIDETAG:
                print_slots(closure_slots, count, ptr);
                break;

            case FUNCALLABLE_INSTANCE_HEADER_WIDETAG:
                print_slots(funcallable_instance_slots, count, ptr);
                break;

            case VALUE_CELL_HEADER_WIDETAG:
		print_slots(value_cell_slots, 1, ptr);
                break;

            case SAP_WIDETAG:
                NEWLINE_OR_RETURN;
#ifndef alpha
                printf("0x%08lx", (unsigned long) *ptr);
#else
                printf("0x%016lx", *(lispobj*)(ptr+1));
#endif
                break;

            case WEAK_POINTER_WIDETAG:
		print_slots(weak_pointer_slots, 1, ptr);
                break;

            case BASE_CHAR_WIDETAG:
            case UNBOUND_MARKER_WIDETAG:
                NEWLINE_OR_RETURN;
                printf("pointer to an immediate?");
                break;

	    case FDEFN_WIDETAG:
		print_slots(fdefn_slots, count, ptr);
		break;
		
            default:
                NEWLINE_OR_RETURN;
                printf("Unknown header object?");
                break;
        }
    }
}

static void print_obj(char *prefix, lispobj obj)
{
    static void (*verbose_fns[])(lispobj obj)
	= {print_fixnum, print_struct, print_otherimm, print_list,
	   print_fixnum, print_otherptr, print_otherimm, print_otherptr};
    static void (*brief_fns[])(lispobj obj)
	= {brief_fixnum, brief_struct, brief_otherimm, brief_list,
	   brief_fixnum, brief_otherptr, brief_otherimm, brief_otherptr};
    int type = lowtag_of(obj);
    struct var *var = lookup_by_obj(obj);
    char buffer[256];
    boolean verbose = cur_depth < brief_depth;

    if (!continue_p(verbose))
        return;

    if (var != NULL && var_clock(var) == cur_clock)
        dont_descend = 1;

    if (var == NULL &&
	/* FIXME: What does this "x & y & z & .." expression mean? */
	(obj & FUN_POINTER_LOWTAG & LIST_POINTER_LOWTAG & INSTANCE_POINTER_LOWTAG & OTHER_POINTER_LOWTAG) != 0)
        var = define_var(NULL, obj, 0);

    if (var != NULL)
        var_setclock(var, cur_clock);

    cur_depth++;
    if (verbose) {
        if (var != NULL) {
            sprintf(buffer, "$%s=", var_name(var));
            newline(buffer);
        }
        else
            newline(NULL);
        printf("%s0x%08lx: ", prefix, (unsigned long) obj);
        if (cur_depth < brief_depth) {
            fputs(lowtag_Names[type], stdout);
            (*verbose_fns[type])(obj);
        }
        else
            (*brief_fns[type])(obj);
    }
    else {
        if (dont_descend)
            printf("$%s", var_name(var));
        else {
            if (var != NULL)
                printf("$%s=", var_name(var));
            (*brief_fns[type])(obj);
        }
    }
    cur_depth--;
    dont_descend = 0;
}

void reset_printer()
{
    cur_clock++;
    cur_lines = 0;
    dont_descend = 0;
}

void print(lispobj obj)
{
    skip_newline = 1;
    cur_depth = 0;
    max_depth = 5;
    max_lines = 20;

    print_obj("", obj);

    putchar('\n');
}

void brief_print(lispobj obj)
{
    skip_newline = 1;
    cur_depth = 0;
    max_depth = 1;
    max_lines = 5000;

    print_obj("", obj);
    putchar('\n');
}

#else

void
brief_print(lispobj obj)
{
    printf("lispobj 0x%lx\n", (unsigned long)obj);
}
     
#endif /* defined(LISP_FEATURE_SB_LDB) */
