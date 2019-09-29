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

/* needed if we want dladdr() and Dl_Info from glibc's dlfcn.h */
#define _GNU_SOURCE

#include <stdio.h>
#include <signal.h>
#include "sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "os.h"
#include "interrupt.h"
#include "lispregs.h"
#include <wchar.h>
#include "arch.h"
#include "genesis/compiled-debug-fun.h"
#include "genesis/compiled-debug-info.h"
#include "genesis/package.h"
#include "genesis/static-symbols.h"
#include "genesis/primitive-objects.h"
#include "thread.h"
#include "gc.h"
#include "code.h"
#include "var-io.h"

#ifdef LISP_FEATURE_OS_PROVIDES_DLADDR
# include <dlfcn.h>
#endif

static void
sbcl_putwc(wchar_t c, FILE *file)
{
#ifdef LISP_FEATURE_OS_PROVIDES_PUTWC
    putwc(c, file);
#else
    if (c < 256) {
        fputc(c, file);
    } else {
        fputc('?', file);
    }
#endif
}

static int decode_locs(lispobj packed_integer, int *offset, int *elsewhere)
{
    struct varint_unpacker unpacker;
    varint_unpacker_init(&unpacker, packed_integer);
    return varint_unpack(&unpacker, offset) && varint_unpack(&unpacker, elsewhere);
}

struct compiled_debug_fun *
debug_function_from_pc (struct code* code, void *pc)
{
    sword_t offset = (char*)pc - code_text_start(code);
    struct compiled_debug_info *di;

    if (!instancep(code->debug_info))
        return NULL;

    di = (struct compiled_debug_info *) native_pointer(code->debug_info);

    if (!instancep(di->fun_map))
        return NULL;

    struct compiled_debug_fun *df = (struct compiled_debug_fun*)native_pointer(di->fun_map);
    int begin, end, elsewhere_begin, elsewhere_end;
    if (!decode_locs(df->encoded_locs, &begin, &elsewhere_begin))
        return NULL;
    while (df) {
        struct compiled_debug_fun *next;
        if (df->next != NIL) {
            next = (struct compiled_debug_fun*) native_pointer(df->next);
            if (!decode_locs(next->encoded_locs, &end, &elsewhere_end))
                return NULL;
        } else {
            next = 0;
            end = elsewhere_end = code_text_size(code);
        }
        if ((begin <= offset && offset < end) ||
            (elsewhere_begin <= offset && offset < elsewhere_end))
            return df;
        begin = end;
        elsewhere_begin = elsewhere_end;
        df = next;
    }

    return NULL;
}

static void
print_string (struct vector *vector, FILE *f)
{
  int tag = widetag_of(&vector->header);

#define doit(TYPE)                              \
  do {                                          \
    int i;                                      \
    int n = fixnum_value(vector->length);       \
    TYPE *data = (TYPE *) vector->data;         \
    for (i = 0; i < n; i++) {                   \
      wchar_t c = (wchar_t) data[i];            \
      if (c == '\\' || c == '"')                \
        putc('\\', f);                          \
      sbcl_putwc(c, f);                         \
    }                                           \
  } while (0)

  switch (tag) {
  case SIMPLE_BASE_STRING_WIDETAG:
    doit(unsigned char);
    break;
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
  case SIMPLE_CHARACTER_STRING_WIDETAG:
    doit(unsigned int);
    break;
#endif
  default:
    fprintf(f, "<??? type %d>", tag);
  }
#undef doit
}

static int string_equal (struct vector *vector, char *string)
{
    if (widetag_of(&vector->header) != SIMPLE_BASE_STRING_WIDETAG)
        return 0;
    return !strcmp((char *) vector->data, string);
}

static void
print_entry_name (lispobj name, FILE *f)
{
    if (listp(name)) {
        putc('(', f);
        while (name != NIL) {
            if (!listp(name)) {
                fprintf(f, "%p: unexpected lowtag while printing a cons\n",
                       (void*)name);
                return;
            }
            print_entry_name(CONS(name)->car, f);
            name = CONS(name)->cdr;
            if (name != NIL)
                putc(' ', f);
        }
        putc(')', f);
    } else if (lowtag_of(name) == OTHER_POINTER_LOWTAG) {
        lispobj *object = native_pointer(name);
        if (widetag_of(object) == SYMBOL_WIDETAG) {
            struct symbol *symbol = (struct symbol *) object;
            if (symbol->package != NIL) {
                struct package *pkg
                    = (struct package *) native_pointer(symbol->package);
                struct vector *pkg_name = VECTOR(pkg->_name);
                if (string_equal(pkg_name, "COMMON-LISP"))
                    ;
                else if (string_equal(pkg_name, "COMMON-LISP-USER")) {
                    fputs("CL-USER::", f);
                }
                else if (string_equal(pkg_name, "KEYWORD")) {
                    putc(':', f);
                } else {
                    print_string(pkg_name, f);
                    fputs("::", f);
                }
            }
            print_string(VECTOR(symbol->name), f);
        } else if (widetag_of(object) == SIMPLE_BASE_STRING_WIDETAG
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
                   || widetag_of(object) == SIMPLE_CHARACTER_STRING_WIDETAG
#endif
            ) {
            putc('"', f);
            print_string((struct vector*)object, f);
            putc('"', f);
        } else {
            fprintf(f, "<??? type %d>", widetag_of(object));
        }
    } else if (fixnump(name)) {
        fprintf(f, "%d", (int)fixnum_value(name));
    } else {
        fprintf(f, "<??? lowtag %d>", (int) lowtag_of(name));
    }
}

static void
print_entry_points (struct code *code, FILE *f)
{
    int n_funs = code_n_funs(code);
    for_each_simple_fun(index, fun, code, 0, {
        if (widetag_of(&fun->header) != SIMPLE_FUN_WIDETAG) {
            fprintf(f, "%p: bogus function entry", fun);
            return;
        }
        print_entry_name(code->constants[CODE_SLOTS_PER_SIMPLE_FUN*index], f);
        if ((index + 1) < n_funs) fprintf(f, ", ");
    });
}


#if !(defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))

/* KLUDGE: Sigh ... I know what the call frame looks like and it had
 * better not change. */

struct call_frame {
#ifndef LISP_FEATURE_ALPHA
        struct call_frame *old_cont;
#else
        u32 old_cont;
#endif
        lispobj saved_lra;
        lispobj code;
        lispobj other_state[5];
};

struct call_info {
#ifndef LISP_FEATURE_ALPHA
    struct call_frame *frame;
#else
    u32 frame;
#endif
    int interrupted;
#ifndef LISP_FEATURE_ALPHA
    struct code *code;
#else
    u32 code;
#endif
    lispobj lra;
    int pc; /* Note: this is the trace file offset, not the actual pc. */
};

// simple-fun headers have a pointer to layout-of-function in the
// upper bytes if words are 8 bytes, so mask off those bytes.
#define HEADER_LENGTH(header) (((header)>>8) & FUN_HEADER_NWORDS_MASK)

static int previous_info(struct call_info *info);

static struct code *
code_pointer(lispobj object)
{
    lispobj *headerp = native_pointer(object);
    int len;
    switch (widetag_of(headerp)) {
        case CODE_HEADER_WIDETAG:
            break;
        case RETURN_PC_WIDETAG:
        case SIMPLE_FUN_WIDETAG:
            len = HEADER_LENGTH(*headerp);
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
    struct thread *thread=arch_os_get_current_thread();
    return (((char *) thread->control_stack_start <= (char *) pointer) &&
            ((char *) pointer < (char *) access_control_stack_pointer(thread)));
}

static void
call_info_from_lisp_state(struct call_info *info)
{
    info->frame = (struct call_frame *)access_control_frame_pointer(arch_os_get_current_thread());
    info->interrupted = 0;
    info->code = NULL;
    info->lra = 0;
    info->pc = 0;

    previous_info(info);
}

static void
call_info_from_context(struct call_info *info, os_context_t *context)
{
    uword_t pc;

    info->interrupted = 1;
#if !defined(LISP_FEATURE_ARM) && !defined(LISP_FEATURE_ARM64)
    if (functionp(*os_context_register_addr(context, reg_CODE))) {
        /* We tried to call a function, but crapped out before $CODE could
         * be fixed up. Probably an undefined function. */
        info->frame =
            (struct call_frame *)(uword_t)
                (*os_context_register_addr(context, reg_OCFP));
        info->lra = (lispobj)(*os_context_register_addr(context, reg_LRA));
        info->code = code_pointer(info->lra);
        pc = (uword_t)native_pointer(info->lra);
    } else
#endif
    {
        info->frame =
            (struct call_frame *)(uword_t)
                (*os_context_register_addr(context, reg_CFP));
        info->code =
            code_pointer(*os_context_register_addr(context, reg_CODE));
        info->lra = NIL;
        pc = *os_context_pc_addr(context);
    }
    if (info->code != NULL)
        info->pc = pc - (uword_t) info->code -
#ifndef LISP_FEATURE_ALPHA
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
    struct thread *thread=arch_os_get_current_thread();
    int free_ici;
    lispobj lra;

    if (!cs_valid_pointer_p(info->frame)) {
        printf("Bogus callee value (0x%lx).\n", (long)info->frame);
        return 0;
    }

    this_frame = info->frame;
    info->lra = this_frame->saved_lra;
    info->frame = this_frame->old_cont;
    info->interrupted = 0;

    if (info->frame == NULL || info->frame == this_frame)
        return 0;
    lra = info->lra;
    if (lra == NIL) {
        /* We were interrupted. Find the correct signal context. */
        free_ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,thread));
        while (free_ici-- > 0) {
            os_context_t *context = nth_interrupt_context(free_ici, thread);
            if ((struct call_frame *)(uword_t)
                (*os_context_register_addr(context, reg_CFP))
                == info->frame) {
                call_info_from_context(info, context);
                break;
            }
        }
    } else if (fixnump(lra)) {
        info->code = (struct code*)native_pointer(this_frame->code);
        // FIXME: is this right? fixnumish LRAs are based off the object base address
        // and not the code text start?
        info->pc = (uword_t)(info->code + lra);
        info->lra = NIL;
    } else {
        info->code = code_pointer(lra);
        if (info->code != NULL)
            info->pc = (char*)native_pointer(info->lra) - code_text_start(info->code);
        else
            info->pc = 0;
    }

    return 1;
}

void
lisp_backtrace(int nframes)
{
    struct call_info info;
    int i = 0;
    call_info_from_lisp_state(&info);

    do {
        printf("%4d: ", i);

        if (info.code != (struct code *) 0) {
            struct compiled_debug_fun *df ;
            if (info.lra != NIL &&
                (df = debug_function_from_pc((struct code *)info.code, (void *)info.lra)))
                print_entry_name(df->name, stdout);
            else
                print_entry_points((struct code *)info.code, stdout);

            printf(" %p", (void*)((uword_t) info.code | OTHER_POINTER_LOWTAG));
        }
        else
            printf("CODE = ???");
        printf("%s fp = %p", info.interrupted ? " [interrupted]" : "",
               info.frame);

        if (info.lra != NIL)
            printf(" LRA = %p", (void*)info.lra);
        else
            printf(" <no LRA>");

        if (info.pc)
            printf(" pc_ofs = %p", (void*)(long)info.pc);
        putchar('\n');

    } while (i++ < nframes && previous_info(&info));
}

#else

static int
altstack_pointer_p (void __attribute__((unused)) *p) {
#ifndef LISP_FEATURE_WIN32
    struct thread* thread = arch_os_get_current_thread();
    // FIXME: shouldn't this be testing '>=' start and '<' end ?
    //        i.e. Was it only right because the calculations themselves were wrong ?
    return (p > calc_altstack_base(thread) && p <= calc_altstack_end(thread));
#else
    /* Win32 doesn't do altstack */
    return 0;
#endif
}

static int
stack_pointer_p (void *p)
{
    /* we are using sizeof(long) here, because that is the right value on both
     * x86 and x86-64.  (But note that false positives would not cause much harm
     * given the heuristical nature of x86_call_context.) */
    uword_t stack_alignment = sizeof(void*);
    void *stack_start;
    struct thread *thread = arch_os_get_current_thread();

    if (altstack_pointer_p(p))
        return 1;

    if (altstack_pointer_p(&p)) {
        stack_start = (void *) thread->control_stack_start;
    } else {
        /* Use the current frame address, since there should be no
         * relevant frames below. */
        stack_start = &p;
    }
    return p >= stack_start
        && p < (void *) thread->control_stack_end
        && (((uword_t) p) & (stack_alignment-1)) == 0;
}

static int
ra_pointer_p (void *ra)
{
  /* the check against 4096 is still a mystery to everyone interviewed about
   * it, but recent changes to sb-sprof seem to suggest that such values
   * do occur sometimes. */
  return ((uword_t) ra) > 4096 && !stack_pointer_p (ra);
}

static int NO_SANITIZE_MEMORY
x86_call_context (void *fp, void **ra, void **ocfp)
{
  void *c_ocfp;
  void *c_ra;
  int c_valid_p;

  if (!stack_pointer_p(fp))
    return 0;

  c_ocfp    = *((void **) fp);
  c_ra      = *((void **) fp + 1);

  c_valid_p = (c_ocfp > fp
               && stack_pointer_p(c_ocfp)
               && ra_pointer_p(c_ra));

  if (c_valid_p)
    *ra = c_ra, *ocfp = c_ocfp;
  else
    return 0;

  return 1;
}

void
describe_thread_state(void)
{
    struct thread *thread = arch_os_get_current_thread();
    struct interrupt_data *data = thread->interrupt_data;
#ifndef LISP_FEATURE_WIN32
    sigset_t mask;
    get_current_sigmask(&mask);
    printf("Signal mask:\n");
    printf(" SIGALRM = %d\n", sigismember(&mask, SIGALRM));
    printf(" SIGINT = %d\n", sigismember(&mask, SIGINT));
    printf(" SIGPROF = %d\n", sigismember(&mask, SIGPROF));
#ifdef SIG_STOP_FOR_GC
    printf(" SIG_STOP_FOR_GC = %d\n", sigismember(&mask, SIG_STOP_FOR_GC));
#endif
#endif
    printf("Specials:\n");
    printf(" *GC-INHIBIT* = %s\n", (read_TLS(GC_INHIBIT, thread) == T) ? "T" : "NIL");
    printf(" *GC-PENDING* = %s\n", (read_TLS(GC_PENDING, thread) == T) ? "T" : "NIL");
    printf(" *INTERRUPTS-ENABLED* = %s\n", (read_TLS(INTERRUPTS_ENABLED, thread) == T) ? "T" : "NIL");
#ifdef STOP_FOR_GC_PENDING
    printf(" *STOP-FOR-GC-PENDING* = %s\n", (read_TLS(STOP_FOR_GC_PENDING, thread) == T) ? "T" : "NIL");
#endif
    printf("Pending handler = %p\n", data->pending_handler);
}

static void print_backtrace_frame(char *pc, void *fp, int i, FILE *f) {
    lispobj *p;
    fprintf(f, "%4d: ", i);

    p = component_ptr_from_pc(pc);

    if (p) {
        struct code *cp = (struct code *) p;
        struct compiled_debug_fun *df = debug_function_from_pc(cp, pc);
        if (df)
            print_entry_name(df->name, f);
        else
            print_entry_points(cp, f);
        fprintf(f, ", pc = %p, fp = %p", pc, fp);
    } else {
#ifdef LISP_FEATURE_OS_PROVIDES_DLADDR
        Dl_info info;
        if (dladdr(pc, &info)) {
            fprintf(f, "Foreign function %s, pc = %p, fp = %p", info.dli_sname, pc, fp);
        } else
#endif
            fprintf(f, "Foreign function, pc = %p, fp = %p", pc, fp);
    }

    putc('\n', f);
}

/* This function has been split from lisp_backtrace() to enable Lisp
 * backtraces from gdb with call backtrace_from_fp(...). Useful for
 * example when debugging threading deadlocks.
 */
void NO_SANITIZE_MEMORY
log_backtrace_from_fp(void *fp, int nframes, int start, FILE *f)
{
  int i = start;

  for (; i < nframes; ++i) {
    void *ra;
    void *next_fp;

    if (!x86_call_context(fp, &ra, &next_fp))
      break;
    print_backtrace_frame(ra, next_fp, i, f);
    fp = next_fp;
  }
}
void backtrace_from_fp(void *fp, int nframes, int start) {
    log_backtrace_from_fp(fp, nframes, start, stdout);
}

void backtrace_from_context(os_context_t *context, int nframes) {
#ifdef LISP_FEATURE_X86
    void *fp = (void *)*os_context_register_addr(context,reg_EBP);
#elif defined (LISP_FEATURE_X86_64)
    void *fp = (void *)*os_context_register_addr(context,reg_RBP);
#endif
    print_backtrace_frame((void *)*os_context_pc_addr(context), fp, 0, stdout);
    backtrace_from_fp(fp, nframes - 1, 1);
}

void
lisp_backtrace(int nframes)
{
    struct thread *thread=arch_os_get_current_thread();
    int free_ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,thread));

    if (free_ici) {
        os_context_t *context = nth_interrupt_context(free_ici - 1, thread);
        backtrace_from_context(context, nframes);
    } else {
        void *fp;

#ifdef LISP_FEATURE_X86
        asm("movl %%ebp,%0" : "=g" (fp));
#elif defined (LISP_FEATURE_X86_64)
        asm("movq %%rbp,%0" : "=g" (fp));
#endif
        backtrace_from_fp(fp, nframes, 0);
    }
}
#endif
