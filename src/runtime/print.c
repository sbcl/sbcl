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

#include <stdio.h>
#include <string.h>

#include "genesis/sbcl.h"
#include "print.h"
#include "runtime.h"
#include "code.h"
#include "gc.h"
#include "genesis/gc-tables.h"
#include "thread.h"
#include <errno.h>
#include <stdlib.h>
#include <inttypes.h>
#include <setjmp.h>

struct dyndebug_config dyndebug_config;

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
#if defined(LISP_FEATURE_GENERATIONAL)
    if (dyndebug_config.dyndebug_gencgc_verbose) {
        gencgc_verbose = 1;
    }
#endif

#undef dyndebug_init1
#undef DYNDEBUG_NFLAGS
}

#include "vars.h"
#include "os.h"
#include "genesis/static-symbols.h"
#include "genesis/primitive-objects.h"
#include "genesis/static-symbols.h"
#include "genesis/tagnames.h"

static int max_lines = 20, cur_lines = 0;
static int max_depth = 5, brief_depth = 2, cur_depth = 0;
static int max_length = 5;
static bool dont_descend = 0, skip_newline = 0;
static int cur_clock = 0;

static void print_obj(char *prefix, lispobj obj, iochannel_t);

#define IO (*io)
#define NEWLINE_OR_RETURN if (continue_p(1,io)) newline(NULL,io->out); else return;

static void indent(int n, FILE* f)
{
    static char *spaces = "                                                                ";

    while (n > 64) {
        fputs(spaces, f);
        n -= 64;
    }
    if (n != 0)
        fputs(spaces + 64 - n, f);
}

static jmp_buf ldb_print_nlx;
static bool continue_p(bool newline, iochannel_t io)
{
    char buffer[256];

    if (cur_depth >= max_depth || dont_descend)
        return 0;

    if (newline) {
        if (skip_newline)
            skip_newline = 0;
        else
            putc('\n', IO.out);

        if (cur_lines >= max_lines) {
            fprintf(IO.out, "More? [y] ");
            fflush(IO.out);

            if (fgets(buffer, sizeof(buffer), IO.in)) {
                if (buffer[0] == 'n' || buffer[0] == 'N')
                    longjmp(ldb_print_nlx, 1);
                else
                    cur_lines = 0;
            } else {
                fprintf(IO.out, "\nUnable to read response, assuming y.\n");
                cur_lines = 0;
            }
        }
    }

    return 1;
}

static void newline(char *label, FILE* f)
{
    cur_lines++;
    if (label != NULL)
        fputs(label, f);
    putc('\t', f);
    indent(cur_depth * 2, f);
}


static void print_unknown(lispobj obj, iochannel_t io)
{
    fprintf(IO.out, "unknown object: %p", (void *)obj);
}

#ifdef PRIdPTR
# define OBJ_FMTd PRIdPTR
#else
# error "Your inttypes.h is lame"
#endif

static void brief_fixnum(lispobj obj, iochannel_t io)
{
    /* KLUDGE: Rather than update the tables in print_obj(), we
       declare all fixnum-or-unknown tags to be fixnums and sort it
       out here with a guard clause. */
    if (!fixnump(obj)) return print_unknown(obj, io);
    fprintf(IO.out, "%"OBJ_FMTd, fixnum_value(obj));
}

static void print_fixnum(lispobj obj, iochannel_t io)
{
    /* KLUDGE: Rather than update the tables in print_obj(), we
       declare all fixnum-or-unknown tags to be fixnums and sort it
       out here with a guard clause. */
    if (!fixnump(obj)) return print_unknown(obj, io);
    fprintf(IO.out, ": %"OBJ_FMTd, fixnum_value(obj));
}

static void brief_otherimm(lispobj obj, iochannel_t io)
{
    int type, c;
    char * charname = 0;

    type = header_widetag(obj);
    switch (type) {
        case CHARACTER_WIDETAG:
            c = obj>>8; // no mask. show whatever's there
            fprintf(IO.out, "#\\");
            switch (c) {
                case '\0': charname = "Nul"; break;
                case '\n': charname = "Newline"; break;
                case '\b': charname = "Backspace"; break;
                case '\177': charname = "Delete"; break;
                default:
                  if (c < 32) fprintf(IO.out, "^%c", c+64);
                  else fprintf(IO.out, c < 128 ? "%c" : "U+%X", c);
            }
            if (charname)
                fputs(charname, IO.out);
            break;

        case UNBOUND_MARKER_WIDETAG:
            fprintf(IO.out, "<unbound marker>");
            break;

        default:
            fprintf(IO.out, "%s", widetag_names[type >> 2]);
            break;
    }
}

static void print_otherimm(lispobj obj, iochannel_t io)
{
    fprintf(IO.out, ", %s", widetag_names[header_widetag(obj) >> 2]);

    switch (header_widetag(obj)) {
    case CHARACTER_WIDETAG:
        fprintf(IO.out, ": ");
        brief_otherimm(obj, io);
        break;

    case SAP_WIDETAG:
    case UNBOUND_MARKER_WIDETAG:
        break;

    default:
        fprintf(IO.out, ": data=%"OBJ_FMTX, (obj>>8));
        break;
    }
}

static void brief_list(lispobj obj, iochannel_t io)
{
    int space = 0;
    int length = 0;

    if (obj == NIL)
        fprintf(IO.out, "NIL");
    else {
        putc('(', IO.out);
        while (listp(obj)) {
            if (space)
                putc(' ', IO.out);
            if (++length >= max_length) {
                fprintf(IO.out, "...");
                obj = NIL;
                break;
            }
            print_obj("", CONS(obj)->car, io);
            obj = CONS(obj)->cdr;
            space = 1;
            if (obj == NIL)
                break;
        }
        if (obj != NIL) {
            fprintf(IO.out, " . ");
            print_obj("", obj, io);
        }
        putc(')', IO.out);
    }
}

void print_list_car_ptrs(lispobj obj, FILE* f)
{
    char sep = '(';
    int len = 0;
    if (obj == NIL) { fprintf(f, "NIL"); return; }
    do {
        if (++len > 20) { fprintf(f, "...)"); return; }
        fprintf(f, "%c%p", sep, (void*)CONS(obj)->car);
        obj = CONS(obj)->cdr;
        sep = ' ';
    } while (listp(obj) && obj != NIL);
    if (obj != NIL) fprintf(f, " . %p", (void*)obj);
    putc(')', f);
}


static void print_list(lispobj obj, iochannel_t io)
{
    if (obj == NIL) {
        fprintf(IO.out, " (NIL)");
    } else {
        print_obj("car: ", CONS(obj)->car, io);
        print_obj("cdr: ", CONS(obj)->cdr, io);
    }
}

// takes native pointer as input
char * simple_base_stringize(struct vector * string)
{
  if (widetag_of(&string->header) == SIMPLE_BASE_STRING_WIDETAG)
      return (char*)string->data;
  int length = vector_len(string);
  char * newstring = malloc(length+1);
  uint32_t * data = (uint32_t*)string->data;
  int i;
  for(i=0;i<length;++i)
      newstring[i] = data[i] < 128 ? data[i] : '?';
  newstring[length] = 0;
  return newstring;
}

static void brief_struct(lispobj obj, iochannel_t io)
{
    struct instance *instance = INSTANCE(obj);
    extern struct vector * instance_classoid_name(lispobj*);
    struct vector * classoid_name;
    classoid_name = instance_classoid_name((lispobj*)instance);
    lispobj layout = instance_layout((lispobj*)instance);
    if ( classoid_name ) {
        char * namestring = simple_base_stringize(classoid_name);
        fprintf(IO.out, "#<ptr to %"OBJ_FMTX" %s instance>", layout, namestring);
        if ( namestring != (char*)classoid_name->data )
            free(namestring);
    } else {
        fprintf(IO.out, "#<ptr to %"OBJ_FMTX" instance>", layout);
    }
}

#include "genesis/defstruct-description.h"
static bool tagged_slot_p(struct layout *layout, int slot_index)
{
    // Since we're doing this scan, we could return the name
    // and exact raw type.
    if (instancep(layout->_info)) {
        struct defstruct_description* dd = (void*)(layout->_info-INSTANCE_POINTER_LOWTAG);
        lispobj slots = dd->slots;
        for ( ; slots != NIL ; slots = CONS(slots)->cdr ) {
            struct defstruct_slot_description* dsd =
                (void*)(CONS(slots)->car-INSTANCE_POINTER_LOWTAG);
            if ((fixnum_value(dsd->bits) >> DSD_INDEX_SHIFT) == slot_index)
                return (fixnum_value(dsd->bits) & DSD_RAW_TYPE_MASK) == 0;
        }
    }
    /* Revision 2b783b49 said to prefer LAYOUT-INFO vs BITMAP because the bitmap
     * can indicate a 0 bit ("raw") for any slot that _may_ be ignored by GC, such as
     * slots constrained to FIXNUM. Unfortunately that misses that CONDITION instances
     * have trailing variable-length tagged data. In practice an instance may have raw
     * words only if it has a DD, which most CONDITION subtypes do not. Therefore this
     * could almost always return 1. But layout-of-layout is an important use of trailing
     * raw slots. Attempting to print random words as tagged could be disastrous.
     * Therefore, test the bitmap if the above loop failed to find slot_index. */
    return bitmap_logbitp(slot_index, get_layout_bitmap(layout));
}

static void print_struct(lispobj obj, iochannel_t io)
{
    struct instance *instance = INSTANCE(obj);
    short int i;
    char buffer[16];
    lispobj layout = instance_layout(native_pointer(obj));
    print_obj("type: ", layout, io);
    for (i=INSTANCE_DATA_START; i<instance_length(instance->header); i++) {
        sprintf(buffer, "slot %d: ", i);
        if (layout && tagged_slot_p(LAYOUT(layout), i)) {
            print_obj(buffer, instance->slots[i], io);
        } else {
            newline(NULL, IO.out);
            fprintf(IO.out, "\n\t    %s0x%"OBJ_FMTX" [raw]", buffer, instance->slots[i]);
        }
    }
}

void show_lstring(struct vector * string, int quotes, FILE *s)
{
  int ucs4_p = 0;
  int i, len = vector_len(string);

#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
  if (widetag_of(&string->header) == SIMPLE_CHARACTER_STRING_WIDETAG) {
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

static void brief_fun_or_otherptr(lispobj obj, iochannel_t io)
{
    lispobj *ptr, header;
    int type;
    struct symbol *symbol;

    ptr = native_pointer(obj);
    header = *ptr;
    type = header_widetag(header);
    switch (type) {
        case SYMBOL_WIDETAG:
            symbol = (struct symbol *)ptr;
            lispobj package = symbol_package(symbol);
            if (package == NIL)
                fprintf(IO.out, "#:");
            show_lstring(symbol_name(symbol), 0, IO.out);
            break;

        case SIMPLE_BASE_STRING_WIDETAG:
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
        case SIMPLE_CHARACTER_STRING_WIDETAG:
#endif
            show_lstring((struct vector*)ptr, 1, IO.out);
            break;

        default:
            fprintf(IO.out, "#<ptr to ");
            brief_otherimm(header, io);
            if (type == FDEFN_WIDETAG) {  // Try to print name, if a symbol
                // FIXME: more address validity checks perhaps?
                lispobj name = ((struct fdefn*)ptr)->name;
                if (lowtag_of(name) == OTHER_POINTER_LOWTAG
                    && widetag_of(native_pointer(name)) == SYMBOL_WIDETAG) {
                  fprintf(IO.out, " for ");
                  struct vector* str = symbol_name(SYMBOL(name));
                  safely_show_lstring(str, 0, IO.out);
                }
            }
            putc('>', IO.out);
    }
}

static void print_slots(char **slots, int count, lispobj *ptr, iochannel_t io)
{
    while (count-- > 0) {
        if (*slots) {
            // kludge for encoded slots
            lispobj word = *ptr;
            char* slot_name = *slots;
            if (N_WORD_BYTES == 8 && !strcmp(slot_name, "boxed_size: ")) word = word & 0xFFFFFFFF;
            print_obj(slot_name, word, io);
            slots++;
        } else {
            print_obj("???: ", *ptr, io);
        }
        ptr++;
    }
}

static void print_fun_or_otherptr(lispobj obj, iochannel_t io)
{
    lispobj *ptr;
    int index;
    char buffer[16];

    ptr = native_pointer(obj);
    if (ptr == NULL) {
        fprintf(IO.out, " (NULL Pointer)");
        return;
    }

    lispobj header = *ptr;
    unsigned char type = header_widetag(header);
    // recall that object_size deliberately croaks on simple-funs
    int count = (type != SIMPLE_FUN_WIDETAG) ? (object_size(ptr) - 1) : 0;
    ++ptr;

    print_obj("header: ", header, io);
    if (!other_immediate_lowtag_p(header)) {
        NEWLINE_OR_RETURN;
        fprintf(IO.out, "(invalid header object)");
        return;
    }

    switch (type) {
    case BIGNUM_WIDETAG:
        ptr += count;
        NEWLINE_OR_RETURN;
        fprintf(IO.out, "0x");
        while (count-- > 0)
            fprintf(IO.out,
#if N_WORD_BITS == 32
                   "%08lx%s",
#else
                   "%016lx%s",
#endif
                   (unsigned long) *--ptr, (count?"_":""));
        break;

    case RATIO_WIDETAG:
        print_slots(ratio_slots, count, ptr, io);
        break;

    case COMPLEX_RATIONAL_WIDETAG:
        print_slots(complex_slots, count, ptr, io);
        break;

    case SYMBOL_WIDETAG:
        print_slots(symbol_slots, count, ptr, io);
        struct symbol* sym = (void*)(ptr - 1);
        if (symbol_function(sym) != NIL) print_obj("fun: ", symbol_function(sym), io);
#ifdef LISP_FEATURE_SB_THREAD
        int tlsindex = tls_index_of(sym);
        struct thread*th = get_sb_vm_thread();
        if (th != 0 && tlsindex != 0) {
            lispobj v = *(lispobj*)(tlsindex + (char*)th);
            print_obj("tlsval: ", v, io);
        }
#endif
#ifdef LISP_FEATURE_COMPACT_SYMBOL
        // print_obj doesn't understand raw words, so make it a fixnum
        int pkgid = symbol_package_id(sym) << N_FIXNUM_TAG_BITS;
        print_obj("package_id: ", pkgid, io);
#endif
#ifdef LISP_FEATURE_LINKAGE_SPACE
        int fname_index = symbol_linkage_index(sym);
        fprintf(IO.out, "\nindex: %x linkage_table[index]: %p",
               fname_index, (void*)linkage_space[fname_index]);
#endif
        break;

#if N_WORD_BITS == 32
    case SINGLE_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
        fprintf(IO.out, "%g", ((struct single_float *)native_pointer(obj))->value);
        break;
#endif
    case DOUBLE_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
        fprintf(IO.out, "%g", ((struct double_float *)native_pointer(obj))->value);
        break;

#ifdef LONG_FLOAT_WIDETAG
    case LONG_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
        fprintf(IO.out, "%Lg", ((struct long_float *)native_pointer(obj))->value);
        break;
#endif

#ifdef COMPLEX_SINGLE_FLOAT_WIDETAG
    case COMPLEX_SINGLE_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
#ifdef LISP_FEATURE_64_BIT
        fprintf(IO.out, "%g", ((struct complex_single_float *)native_pointer(obj))->data.data[0]);
#else
        fprintf(IO.out, "%g", ((struct complex_single_float *)native_pointer(obj))->real);
#endif
        NEWLINE_OR_RETURN;
#ifdef LISP_FEATURE_64_BIT
        fprintf(IO.out, "%g", ((struct complex_single_float *)native_pointer(obj))->data.data[1]);
#else
        fprintf(IO.out, "%g", ((struct complex_single_float *)native_pointer(obj))->imag);
#endif
        break;
#endif

#ifdef COMPLEX_DOUBLE_FLOAT_WIDETAG
    case COMPLEX_DOUBLE_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
        fprintf(IO.out, "%g", ((struct complex_double_float *)native_pointer(obj))->real);
        NEWLINE_OR_RETURN;
        fprintf(IO.out, "%g", ((struct complex_double_float *)native_pointer(obj))->imag);
        break;
#endif

#ifdef COMPLEX_LONG_FLOAT_WIDETAG
    case COMPLEX_LONG_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
        fprintf(IO.out, "%Lg", ((struct complex_long_float *)native_pointer(obj))->real);
        NEWLINE_OR_RETURN;
        fprintf(IO.out, "%Lg", ((struct complex_long_float *)native_pointer(obj))->imag);
        break;
#endif

    case SIMPLE_BASE_STRING_WIDETAG:
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
    case SIMPLE_CHARACTER_STRING_WIDETAG:
#endif
        NEWLINE_OR_RETURN;
        show_lstring((struct vector*)native_pointer(obj), 1, IO.out);
        break;

    case SIMPLE_VECTOR_WIDETAG:
        NEWLINE_OR_RETURN;
        {
        long length = vector_len(VECTOR(obj));
        fprintf(IO.out, "length = %ld", length);
        ptr++;
        index = 0;
        while (length-- > 0) {
            sprintf(buffer, "%d: ", index++);
            print_obj(buffer, *ptr++, io);
        }
        }
        break;

    case SIMPLE_BIT_VECTOR_WIDETAG:
        NEWLINE_OR_RETURN;
        {
        long length = vector_len(VECTOR(obj));
        fprintf(IO.out, "length = %ld : ", length);
        int bits_to_print = (length < N_WORD_BITS) ? length : N_WORD_BITS;
        uword_t word = ptr[1];
        int i;
        for(i=0; i<bits_to_print; ++i) {
            putc((word & 1) ? '1' : '0', IO.out);
            if ((i%8)==7) putc('_', IO.out);
            word >>= 1;
        }
        if(bits_to_print < length) fprintf(IO.out, "...");
        fprintf(IO.out, "\n");
        }
        break;

    case CODE_HEADER_WIDETAG:
        // ptr was already bumped up
        count = code_header_words((struct code*)(ptr-1));
        for_each_simple_fun(fun_index, fun, (struct code*)(ptr-1), 0, {
            sprintf(buffer, "f[%d]: ", fun_index);
            print_obj(buffer, make_lispobj(fun,FUN_POINTER_LOWTAG), io);
        });
        print_slots(code_slots, count-1, ptr, io);
        break;

    case SIMPLE_FUN_WIDETAG: {
        struct simple_fun* fun = (void*)native_pointer(obj);
        struct code* code = fun_code_header(fun);
        print_obj("code: ", make_lispobj(code,OTHER_POINTER_LOWTAG), io);
        print_slots(simple_fun_slots,
                    sizeof simple_fun_slots/sizeof(char*)-1, ptr, io);
        lispobj name = debug_function_name_from_pc(code, (char*)(2 + (lispobj*)fun));
        print_obj("name: ", name, io);
        break;
    }

#ifdef RETURN_PC_WIDETAG
    case RETURN_PC_WIDETAG:
        print_obj("code: ", obj - (count * N_WORD_BYTES), io);
        break;
#endif

    case CLOSURE_WIDETAG:
        print_slots(closure_slots, count, ptr, io);
        break;

    case FUNCALLABLE_INSTANCE_WIDETAG:
        print_slots(funcallable_instance_slots, count, ptr, io);
        break;

    case VALUE_CELL_WIDETAG:
        print_slots(value_cell_slots, 1, ptr, io);
        break;

    case SAP_WIDETAG:
        NEWLINE_OR_RETURN;
        fprintf(IO.out, "%p", (void*)*ptr);
        break;

    case WEAK_POINTER_WIDETAG:
        print_slots(weak_pointer_slots, 1, ptr, io);
        break;

    case CHARACTER_WIDETAG:
    case UNBOUND_MARKER_WIDETAG:
        NEWLINE_OR_RETURN;
        fprintf(IO.out, "pointer to an immediate?");
        break;

    case FDEFN_WIDETAG:
        print_slots(fdefn_slots, count, ptr, io);
        break;

    // Make some vectors printable from C, for when all hell breaks lose
    case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
        NEWLINE_OR_RETURN;
        {
        long length = vector_len(VECTOR(obj));
        uint32_t * data = (uint32_t*)(ptr + 1);
        long i;
        fprintf(IO.out, "#(");
        for (i=0; i<length; ++i) {
            fprintf(IO.out, "%s%d", i>0?" ":"", data[i]);
            if(i==255 && length>256) { fprintf(IO.out, " ..."); break; }
        }
        fprintf(IO.out, ")");
        }
        break;
    default:
        NEWLINE_OR_RETURN;
        if (specialized_vector_widetag_p(type))
            fprintf(IO.out, "length = %"OBJ_FMTd, vector_len(VECTOR(obj)));
        else
            fprintf(IO.out, "Unknown header object?");
        break;
    }
}

static void print_obj(char *prefix, lispobj obj, iochannel_t io)
{
#include "genesis/print.inc"
    int type = lowtag_of(obj);
    struct var *var = lookup_by_obj(obj);
    char buffer[256];
    bool verbose = cur_depth < brief_depth;

    if (!continue_p(verbose, io))
        return;

    if (var != NULL && var_clock(var) == cur_clock)
        dont_descend = 1;

    if (var == NULL && is_lisp_pointer(obj))
        var = define_var(NULL, obj, 0);

    if (var != NULL)
        var_setclock(var, cur_clock);

    void (**fns)(lispobj,iochannel_t) = NULL;
    cur_depth++;
    if (verbose) {
        if (var != NULL) {
            sprintf(buffer, "$%s=", var_name(var));
            newline(buffer, IO.out);
        }
        else
            newline(NULL, IO.out);
        fprintf(IO.out, "%s0x%08lx: ", prefix, (unsigned long) obj);
        if (cur_depth < brief_depth) {
            fputs(lowtag_names[type], IO.out);
            fns = print_fns;
        }
        else
            fns = brief_fns;
    }
    else {
        if (dont_descend)
            fprintf(IO.out, "$%s", var_name(var));
        else {
            if (var != NULL)
                fprintf(IO.out, "$%s=", var_name(var));
            fns = brief_fns;
        }
    }
    if (!fns)
        ;
    else if (is_lisp_pointer(obj) && !gc_managed_addr_p(obj))
        fprintf(IO.out, "(bad-address)");
    else
        (*fns[type])(obj, io);
    cur_depth--;
    dont_descend = 0;
}

void reset_printer()
{
    cur_clock++;
    cur_lines = 0;
    dont_descend = 0;
}

void print_to_iochan(lispobj obj, iochannel_t io)
{
    skip_newline = 1;
    cur_depth = 0;
    max_depth = 5;
    max_lines = 20;

    if (!setjmp(ldb_print_nlx))
        print_obj("", obj, io);

    putc('\n', IO.out);
}
void print(lispobj obj) // This can be called from Lisp
{
    struct iochannel io = {stdout, stdin};
    print_to_iochan(obj, &io);
}

void brief_print(lispobj obj, iochannel_t io)
{
    skip_newline = 1;
    cur_depth = 0;
    max_depth = 1;
    max_lines = 5000;
    cur_lines = 0;

    print_obj("", obj, io);
    putc('\n', IO.out);
}

// The following accessors, which take a valid native pointer as input
// and return a Lisp string, are designed to be foolproof during GC,
// hence all the forwarding checks.

struct vector * symbol_name(struct symbol* sym)
{
  if (forwarding_pointer_p((lispobj*)sym))
    sym = (void*)native_pointer(forwarding_pointer_value((lispobj*)sym));
  lispobj name = sym->name;
  if (lowtag_of(name) != OTHER_POINTER_LOWTAG) return NULL;
  lispobj string = decode_symbol_name(name);
  return VECTOR(follow_fp(string)); // can't have a nameless symbol
}
struct vector * classoid_name(lispobj * classoid)
{
  if (forwarding_pointer_p(classoid))
      classoid = native_pointer(forwarding_pointer_value(classoid));
  // Classoids are named by symbols even though a CLASS name is arbitrary (theoretically)
  lispobj sym = ((struct classoid*)classoid)->name;
  return lowtag_of(sym) != OTHER_POINTER_LOWTAG ? NULL : symbol_name(SYMBOL(sym));
}
struct vector * layout_classoid_name(lispobj * layout)
{
  if (forwarding_pointer_p(layout))
      layout = native_pointer(forwarding_pointer_value(layout));
  lispobj classoid = ((struct layout*)layout)->classoid;
  return instancep(classoid) ? classoid_name(native_pointer(classoid)) : NULL;
}
struct vector * instance_classoid_name(lispobj * instance)
{
  if (forwarding_pointer_p(instance))
      instance = native_pointer(forwarding_pointer_value(instance));
  lispobj layout = instance_layout(instance);
  return instancep(layout) ? layout_classoid_name(native_pointer(layout)) : NULL;
}
void safely_show_lstring(struct vector * string, int quotes, FILE *s)
{
  extern void show_lstring(struct vector*, int, FILE*);
  if (forwarding_pointer_p((lispobj*)string))
      string = (struct vector*)forwarding_pointer_value((lispobj*)string);
  if (
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
      header_widetag(string->header) == SIMPLE_CHARACTER_STRING_WIDETAG ||
#endif
      header_widetag(string->header) == SIMPLE_BASE_STRING_WIDETAG)
    show_lstring(string, quotes, s);
  else {
    fprintf(s, "#<[widetag=%02X]>", header_widetag(string->header));
  }
}
