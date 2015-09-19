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
    char * charname = 0;

    type = widetag_of(obj);
    switch (type) {
        case CHARACTER_WIDETAG:
            c = obj>>8; // no mask. show whatever's there
            printf("#\\");
            switch (c) {
                case '\0': charname = "Nul"; break;
                case '\n': charname = "Newline"; break;
                case '\b': charname = "Backspace"; break;
                case '\177': charname = "Delete"; break;
                default:
                  if (c < 32) printf("^%c", c+64);
                  else printf(c < 128 ? "%c" : "U+%X", c);
            }
            if (charname)
                fputs(charname, stdout);
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
            printf(": data=%"OBJ_FMTX, (obj>>8));
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

// takes native pointer as input
char * simple_base_stringize(struct vector * string)
{
  if (widetag_of(string->header) == SIMPLE_BASE_STRING_WIDETAG)
      return (char*)string->data;
  int length = string->length;
  char * newstring = malloc(length+1);
  uint32_t * data = (uint32_t*)string->data;
  int i;
  for(i=0;i<length;++i)
      newstring[i] = data[i] < 128 ? data[i] : '?';
  newstring[length] = 0;
  return newstring;
}

static void brief_struct(lispobj obj)
{
    struct instance *instance = (struct instance *)native_pointer(obj);
    if (!is_valid_lisp_addr((os_vm_address_t)instance)) {
        printf("(invalid address)");
    } else {
        extern struct vector * instance_classoid_name(lispobj*);
        struct vector * classoid_name;
        classoid_name = instance_classoid_name((lispobj*)instance);
        if ( classoid_name ) {
          char * namestring = simple_base_stringize(classoid_name);
          printf("#<ptr to %p %s instance>",
                 (void*)instance_layout((lispobj*)instance), namestring);
          if ( namestring != (char*)classoid_name->data )
              free(namestring);
        } else {
          printf("#<ptr to %p instance>",
                 (void*)instance_layout((lispobj*)instance));
        }
    }
}

#include "genesis/layout.h"
static boolean untagged_slot_p(struct layout * layout,
                               int slot_index)
{
#ifdef LISP_FEATURE_INTERLEAVED_RAW_SLOTS
  extern boolean positive_bignum_logbitp(int,struct bignum*);
  lispobj bitmap = layout->untagged_bitmap;
  return fixnump(bitmap)
         ? (fixnum_value(bitmap) >> slot_index) & 1
         : positive_bignum_logbitp(slot_index,
                                   (struct bignum*)native_pointer(bitmap));
#else
  // STANDARD-OBJECT has layout-length = number of defined slots, but
  // the primitive object always occupies 4 physical words. The guard on
  // n_untagged_slots ensures that for classes with no slots, we don't
  // wrongly show all words of the primitive object as untagged,
  // because the second half of the expression reduces to slot_index>=1.
  return layout->n_untagged_slots > 0
         && slot_index >= (fixnum_value(layout->length)|1)
                          - fixnum_value(layout->n_untagged_slots);
#endif
}

static void print_struct(lispobj obj)
{
    struct instance *instance = (struct instance *)native_pointer(obj);
    unsigned int i;
    char buffer[16];
    if (!is_valid_lisp_addr((os_vm_address_t)instance)) {
        printf("(invalid address)");
    } else {
        lispobj layout_obj =  instance_layout(native_pointer(obj));
        print_obj("type: ", layout_obj);
        struct layout * layout = (struct layout*)native_pointer(layout_obj);
        for (i=INSTANCE_DATA_START; i<instance_length(instance->header); i++) {
            sprintf(buffer, "slot %d: ", i);
            if (layout==NULL || untagged_slot_p(layout, i)) {
                newline(NULL);
                printf("\n\t    %s0x%"OBJ_FMTX" [raw]", buffer, instance->slots[i]);
            } else
                print_obj(buffer, instance->slots[i]);
        }
    }
}

void show_lstring(struct vector * string, int quotes, FILE *s)
{
  int ucs4_p = 0;
  int i, len = fixnum_value(string->length);

#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
  if (widetag_of(string->header) == SIMPLE_CHARACTER_STRING_WIDETAG) {
      ucs4_p = 1;
      if (quotes)
          putc('u', s); /* an arbitrary notational convention */
  }
#endif
  if (quotes) putc('"', s);
  for (i=0 ; i<len ; i++) {
      // hopefully the compiler will optimize out the ucs4_p test
      // when the runtime is built without Unicode support
      int ch;
      if (ucs4_p)
          ch = i[(uint32_t*)string->data];
      else
          ch = i[(char*)string->data];
      if (ch >= 32 && ch < 127) {
          if (quotes && (ch == '"' || ch == '\\'))
              putc('\\', s);
          putc(ch, s);
      } else {
          fprintf(s, ch > 0xffff ? "\\U%08X" :
                     ch > 0xff ? "\\u%04X" : "\\x%02X", ch);
      }
  }
  if (quotes) putc('"', s);
}

static void brief_otherptr(lispobj obj)
{
    lispobj *ptr, header;
    int type;
    struct symbol *symbol;

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
            if (symbol->package == NIL)
                printf("#:");
            show_lstring((struct vector *)native_pointer(symbol->name),
                         0, stdout);
            break;

        case SIMPLE_BASE_STRING_WIDETAG:
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
        case SIMPLE_CHARACTER_STRING_WIDETAG:
#endif
            show_lstring((struct vector*)ptr, 1, stdout);
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
    "self: ", "next: ", "name: ", "arglist: ", "type: ", "info: ", NULL};
static char *closure_slots[] = {"fn: ", NULL};
static char *funcallable_instance_slots[] = {"raw_fn: ", "fn: ", "layout: ", NULL};
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
        char buffer[16];

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

        if (unprintable_array_types[type/8] & (1<<(type % 8)))
            return;
        switch (type) {
            case BIGNUM_WIDETAG:
                ptr += count;
                NEWLINE_OR_RETURN;
                printf("0x");
                while (count-- > 0)
                    printf(
#if N_WORD_BITS == 32
                           "%08lx%s",
#else
                           "%016lx%s",
#endif
                           (unsigned long) *--ptr, (count?"_":""));
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
#ifdef LISP_FEATURE_64_BIT
                printf("%g", ((struct complex_single_float *)native_pointer(obj))->data.data[0]);
#else
                printf("%g", ((struct complex_single_float *)native_pointer(obj))->real);
#endif
                NEWLINE_OR_RETURN;
#ifdef LISP_FEATURE_64_BIT
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
            case SIMPLE_CHARACTER_STRING_WIDETAG:
#endif
                NEWLINE_OR_RETURN;
                show_lstring((struct vector*)native_pointer(obj), 1, stdout);
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

            // FIXME: This case looks unreachable. print_struct() does it
            case INSTANCE_HEADER_WIDETAG:
                NEWLINE_OR_RETURN;
                printf("length = %ld", (long) count);
                index = 0;
                while (count-- > 0) {
                    sprintf(buffer, "%d: ", index++);
                    print_obj(buffer, *ptr++);
                }
                break;

            case CODE_HEADER_WIDETAG:
                print_slots(code_slots, count-1, ptr);
                break;

            case SIMPLE_FUN_HEADER_WIDETAG:
                print_slots(fn_slots, 6, ptr);
                break;

#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
            case RETURN_PC_HEADER_WIDETAG:
                print_obj("code: ", obj - (count * 4));
                break;
#endif

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
#ifdef LISP_FEATURE_64_BIT
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
