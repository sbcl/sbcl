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
#include <string.h>

#include "sbcl.h"
#include "print.h"
#include "runtime.h"
#include <stdarg.h>
#include "thread.h"              /* genesis/primitive-objects.h needs this */
#include <errno.h>
#include <stdlib.h>

/* FSHOW and odxprint provide debugging output for low-level information
 * (signal handling, exceptions, safepoints) which is hard to debug by
 * other means.
 *
 * If enabled at all, environment variables control whether calls of the
 * form odxprint(name, ...) are enabled at run-time, e.g. using
 * SBCL_DYNDEBUG="fshow fshow_signal safepoints".
 *
 * In the case of FSHOW and FSHOW_SIGNAL, old-style code from runtime.h
 * can also be used to enable or disable these more aggressively.
 */

struct dyndebug_config dyndebug_config = {
    QSHOW == 2, QSHOW_SIGNALS == 2
};

void
dyndebug_init()
{
#define DYNDEBUG_NFLAGS (sizeof(struct dyndebug_config) / sizeof(int))
#define dyndebug_init1(lowercase, uppercase)                    \
    do {                                                        \
        int *ptr = &dyndebug_config.dyndebug_##lowercase;       \
        ptrs[n] = ptr;                                          \
        names[n] = #lowercase;                                  \
        char *val = getenv("SBCL_DYNDEBUG__" uppercase);        \
        *ptr = val && strlen(val);                              \
        n++;                                                    \
    } while (0)
    int n = 0;
    char *names[DYNDEBUG_NFLAGS];
    int *ptrs[DYNDEBUG_NFLAGS];

    dyndebug_init1(fshow,          "FSHOW");
    dyndebug_init1(fshow_signal,   "FSHOW_SIGNAL");
    dyndebug_init1(gencgc_verbose, "GENCGC_VERBOSE");
    dyndebug_init1(safepoints,     "SAFEPOINTS");
    dyndebug_init1(seh,            "SEH");
    dyndebug_init1(misc,           "MISC");
    dyndebug_init1(pagefaults,     "PAGEFAULTS");
    dyndebug_init1(io,             "IO");
    dyndebug_init1(runtime_link,   "RUNTIME_LINK");

    int n_output_flags = n;
    dyndebug_init1(backtrace_when_lost, "BACKTRACE_WHEN_LOST");
    dyndebug_init1(sleep_when_lost,     "SLEEP_WHEN_LOST");

    if (n != DYNDEBUG_NFLAGS)
        fprintf(stderr, "Bug in dyndebug_init\n");

#if defined(LISP_FEATURE_GENCGC)
    gencgc_verbose = dyndebug_config.dyndebug_gencgc_verbose;
#endif

    char *featurelist = getenv("SBCL_DYNDEBUG");
    if (featurelist) {
        int err = 0;
        featurelist = strdup(featurelist);
        char *ptr = featurelist;
        for (;;) {
            char *token = strtok(ptr, " ");
            if (!token) break;
            int i;
            if (!strcmp(token, "all"))
                for (i = 0; i < n_output_flags; i++)
                    *ptrs[i] = 1;
            else {
                for (i = 0; i < (int)DYNDEBUG_NFLAGS; i++)
                    if (!strcmp(token, names[i])) {
                        *ptrs[i] = 1;
                        break;
                    }
                if (i == DYNDEBUG_NFLAGS) {
                    fprintf(stderr, "No such dyndebug flag: `%s'\n", token);
                    err = 1;
                }
            }
            ptr = 0;
        }
        free(featurelist);
        if (err) {
            fprintf(stderr, "Valid flags are:\n");
            fprintf(stderr, "  all  ;enables all of the following:\n");
            int i;
            for (i = 0; i < (int)DYNDEBUG_NFLAGS; i++) {
                if (i == n_output_flags)
                    fprintf(stderr, "Additional options:\n");
                fprintf(stderr, "  %s\n", names[i]);
            }
        }
    }

#undef dyndebug_init1
#undef DYNDEBUG_NFLAGS
}

/* Temporarily, odxprint merely performs the equivalent of a traditional
 * FSHOW call, i.e. it merely formats to stderr.  Ultimately, it should
 * be restored to its full win32 branch functionality, where output to a
 * file or to the debugger can be selected at runtime. */

void vodxprint_fun(const char *, va_list);

void
odxprint_fun(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vodxprint_fun(fmt, args);
    va_end(args);
}

void
vodxprint_fun(const char *fmt, va_list args)
{
#ifdef LISP_FEATURE_WIN32
    DWORD lastError = GetLastError();
#endif
    int original_errno = errno;

    QSHOW_BLOCK;

    char buf[1024];
    int n = 0;

#ifdef LISP_FEATURE_SB_THREAD
    struct thread *arch_os_get_current_thread(void);
    struct thread *self = arch_os_get_current_thread();
    void *pth = self ? (void *) self->os_thread : 0;
    snprintf(buf, sizeof(buf), "[%p/%p] ", self, pth);
    n = strlen(buf);
#endif

    vsnprintf(buf + n, sizeof(buf) - n - 1, fmt, args);
    /* buf is now zero-terminated (even in case of overflow).
     * Our caller took care of the newline (if any) through `fmt'. */

    /* A sufficiently POSIXy implementation of stdio will provide
     * per-FILE locking, as defined in the spec for flockfile.  At least
     * glibc complies with this.  Hence we do not need to perform
     * locking ourselves here.  (Should it turn out, of course, that
     * other libraries opt for speed rather than safety, we need to
     * revisit this decision.) */
    fputs(buf, stderr);

#ifdef LISP_FEATURE_WIN32
    /* stdio's stderr is line-bufferred, i.e. \n ought to flush it.
     * Unfortunately, MinGW does not behave the way I would expect it
     * to.  Let's be safe: */
    fflush(stderr);
#endif

    QSHOW_UNBLOCK;

#ifdef LISP_FEATURE_WIN32
    SetLastError(lastError);
#endif
    errno = original_errno;
}

/* Translate the rather awkward syntax
 *   FSHOW((stderr, "xyz"))
 * into the new and cleaner
 *   odxprint("xyz").
 * If we were willing to clean up all existing call sites, we could remove
 * this wrapper function.  (This is a function, because I don't know how to
 * strip the extra parens in a macro.) */
void
fshow_fun(void __attribute__((__unused__)) *ignored,
          const char *fmt,
          ...)
{
    va_list args;
    va_start(args, fmt);
    vodxprint_fun(fmt, args);
    va_end(args);
}

/* This file can be skipped if we're not supporting LDB. */
#if defined(LISP_FEATURE_SB_LDB)

#include "monitor.h"
#include "vars.h"
#include "os.h"
#ifdef LISP_FEATURE_GENCGC
#include "gencgc-alloc-region.h" /* genesis/thread.h needs this */
#endif
#if defined(LISP_FEATURE_WIN32)
# include "win32-thread-private-events.h" /* genesis/thread.h needs this */
#endif
#include "genesis/static-symbols.h"
#include "genesis/primitive-objects.h"
#include "genesis/static-symbols.h"
#include "genesis/tagnames.h"

static int max_lines = 20, cur_lines = 0;
static int max_depth = 5, brief_depth = 2, cur_depth = 0;
static int max_length = 5;
static boolean dont_descend = 0, skip_newline = 0;
static int cur_clock = 0;

static void print_obj(char *prefix, lispobj obj);

#define NEWLINE_OR_RETURN if (continue_p(1)) newline(NULL); else return;

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

            if (fgets(buffer, sizeof(buffer), stdin)) {
                if (buffer[0] == 'n' || buffer[0] == 'N')
                    throw_to_monitor();
                else
                    cur_lines = 0;
            } else {
                printf("\nUnable to read response, assuming y.\n");
                cur_lines = 0;
            }
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


static void print_unknown(lispobj obj)
{
  printf("unknown object: %p", (void *)obj);
}

static void brief_fixnum(lispobj obj)
{
    /* KLUDGE: Rather than update the tables in print_obj(), we
       declare all fixnum-or-unknown tags to be fixnums and sort it
       out here with a guard clause. */
    if (!fixnump(obj)) return print_unknown(obj);

#ifndef LISP_FEATURE_ALPHA
    printf("%ld", ((long)obj)>>N_FIXNUM_TAG_BITS);
#else
    printf("%d", ((s32)obj)>>N_FIXNUM_TAG_BITS);
#endif
}

static void print_fixnum(lispobj obj)
{
    /* KLUDGE: Rather than update the tables in print_obj(), we
       declare all fixnum-or-unknown tags to be fixnums and sort it
       out here with a guard clause. */
    if (!fixnump(obj)) return print_unknown(obj);

#ifndef LISP_FEATURE_ALPHA
    printf(": %ld", ((long)obj)>>N_FIXNUM_TAG_BITS);
#else
    printf(": %d", ((s32)obj)>>N_FIXNUM_TAG_BITS);
#endif
}

static void brief_otherimm(lispobj obj)
{
    int type, c;
    char buffer[10];

    type = widetag_of(obj);
    switch (type) {
        case CHARACTER_WIDETAG:
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
            printf("%s", widetag_names[type >> 2]);
            break;
    }
}

static void print_otherimm(lispobj obj)
{
    printf(", %s", widetag_names[widetag_of(obj) >> 2]);

    switch (widetag_of(obj)) {
        case CHARACTER_WIDETAG:
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
            print_obj("", cons->car);
            obj = cons->cdr;
            space = 1;
            if (obj == NIL)
                break;
        }
        if (obj != NIL) {
            printf(" . ");
            print_obj("", obj);
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
    struct instance *instance = (struct instance *)native_pointer(obj);
    if (!is_valid_lisp_addr((os_vm_address_t)instance)) {
        printf("(invalid address)");
    } else {
        printf("#<ptr to 0x%08lx instance>",
               (unsigned long) instance->slots[0]);
    }
}

static void print_struct(lispobj obj)
{
    struct instance *instance = (struct instance *)native_pointer(obj);
    unsigned int i;
    char buffer[16];
    if (!is_valid_lisp_addr((os_vm_address_t)instance)) {
        printf("(invalid address)");
    } else {
        print_obj("type: ", ((struct instance *)native_pointer(obj))->slots[0]);
        for (i = 1; i < HeaderValue(instance->header); i++) {
            sprintf(buffer, "slot %d: ", i);
            print_obj(buffer, instance->slots[i]);
        }
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

#ifdef LISP_FEATURE_SB_UNICODE
        case SIMPLE_CHARACTER_STRING_WIDETAG:
            vector = (struct vector *)ptr;
            fputs("u\"", stdout);
            {
                int i, ch, len = fixnum_value(vector->length);
                uint32_t *chars = (uint32_t*)vector->data;
                for (i=0 ; i<len ; i++) {
                    ch = chars[i];
                    if (ch >= 32 && ch < 127) {
                        if (ch == '"' || ch == '\\')
                            putchar('\\');
                        putchar(ch);
                    } else {
                      // ambiguous, e.g. #\xaaa is either #\GUJARATI_LETTER_PA
                      // or #\FEMININE_ORDINAL_INDICATOR + #\a. oh well.
                      printf("\\x%x", ch);
                    }
                }
            }
            putchar('"');
            break;
#endif

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

/* FIXME: Yikes! This needs to depend on the values in sbcl.h (or
 * perhaps be generated automatically by GENESIS as part of
 * sbcl.h). */
static char *symbol_slots[] = {"value: ", "hash: ",
    "info: ", "name: ", "package: ",
#if defined (LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_X86_64)
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
#ifndef LISP_FEATURE_ALPHA
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
        length = fixnum_value(*ptr);
        count = HeaderValue(header);
        type = widetag_of(header);

        print_obj("header: ", header);
        if (!other_immediate_lowtag_p(header)) {
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
                // Only 1 byte of a symbol header conveys its size.
                // The other bytes may be freely used by the backend.
                print_slots(symbol_slots, count & 0xFF, ptr);
                break;

#if N_WORD_BITS == 32
            case SINGLE_FLOAT_WIDETAG:
                NEWLINE_OR_RETURN;
                printf("%g", ((struct single_float *)native_pointer(obj))->value);
                break;
#endif
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
#ifdef LISP_FEATURE_X86_64
                printf("%g", ((struct complex_single_float *)native_pointer(obj))->data.data[0]);
#else
                printf("%g", ((struct complex_single_float *)native_pointer(obj))->real);
#endif
                NEWLINE_OR_RETURN;
#ifdef LISP_FEATURE_X86_64
                printf("%g", ((struct complex_single_float *)native_pointer(obj))->data.data[1]);
#else
                printf("%g", ((struct complex_single_float *)native_pointer(obj))->imag);
#endif
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
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
        case SIMPLE_CHARACTER_STRING_WIDETAG: /* FIXME */
#endif
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
            case SIMPLE_ARRAY_UNSIGNED_BYTE_7_WIDETAG:
            case SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG:
            case SIMPLE_ARRAY_UNSIGNED_BYTE_15_WIDETAG:
            case SIMPLE_ARRAY_UNSIGNED_BYTE_16_WIDETAG:

            case SIMPLE_ARRAY_UNSIGNED_FIXNUM_WIDETAG:

            case SIMPLE_ARRAY_UNSIGNED_BYTE_31_WIDETAG:
            case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG
            case SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG
            case SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
            case SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
            case SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG:
#endif

            case SIMPLE_ARRAY_FIXNUM_WIDETAG:

#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
            case SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG
            case SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG:
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
#ifdef COMPLEX_CHARACTER_STRING_WIDETAG
        case COMPLEX_CHARACTER_STRING_WIDETAG:
#endif
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
#ifndef LISP_FEATURE_ALPHA
                printf("0x%08lx", (unsigned long) *ptr);
#else
                printf("0x%016lx", *(lispobj*)(ptr+1));
#endif
                break;

            case WEAK_POINTER_WIDETAG:
                print_slots(weak_pointer_slots, 1, ptr);
                break;

            case CHARACTER_WIDETAG:
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
#ifdef LISP_FEATURE_X86_64
    static void (*verbose_fns[])(lispobj obj)
        = {print_fixnum, print_otherimm, print_fixnum, print_struct,
           print_fixnum, print_otherimm, print_fixnum, print_list,
           print_fixnum, print_otherimm, print_fixnum, print_otherptr,
           print_fixnum, print_otherimm, print_fixnum, print_otherptr};
    static void (*brief_fns[])(lispobj obj)
        = {brief_fixnum, brief_otherimm, brief_fixnum, brief_struct,
           brief_fixnum, brief_otherimm, brief_fixnum, brief_list,
           brief_fixnum, brief_otherimm, brief_fixnum, brief_otherptr,
           brief_fixnum, brief_otherimm, brief_fixnum, brief_otherptr};
#else
    static void (*verbose_fns[])(lispobj obj)
        = {print_fixnum, print_struct, print_otherimm, print_list,
           print_fixnum, print_otherptr, print_otherimm, print_otherptr};
    static void (*brief_fns[])(lispobj obj)
        = {brief_fixnum, brief_struct, brief_otherimm, brief_list,
           brief_fixnum, brief_otherptr, brief_otherimm, brief_otherptr};
#endif
    int type = lowtag_of(obj);
    struct var *var = lookup_by_obj(obj);
    char buffer[256];
    boolean verbose = cur_depth < brief_depth;

    if (!continue_p(verbose))
        return;

    if (var != NULL && var_clock(var) == cur_clock)
        dont_descend = 1;

    if (var == NULL && is_lisp_pointer(obj))
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
            fputs(lowtag_names[type], stdout);
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
    cur_lines = 0;

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
