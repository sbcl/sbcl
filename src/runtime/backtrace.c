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
#include "gc-internal.h"
#include "forwarding-ptr.h"
#include "lispstring.h"

#ifdef LISP_FEATURE_OS_PROVIDES_DLADDR
# include <dlfcn.h>
#endif

static int decode_locs(lispobj packed_integer, int *offset, int *elsewhere)
{
    struct varint_unpacker unpacker;
    varint_unpacker_init(&unpacker, packed_integer);
    return varint_unpack(&unpacker, offset) && varint_unpack(&unpacker, elsewhere);
}

struct compiled_debug_fun *
debug_function_from_pc (struct code* code, void *pc)
{
    struct compiled_debug_info *di;

    if (instancep(code->debug_info))
        di = (void*)native_pointer(code->debug_info);
    else if (listp(code->debug_info) && instancep(CONS(code->debug_info)->car))
        di = (void*)native_pointer(CONS(code->debug_info)->car);
    else
        return NULL;

    if (!instancep(di->fun_map))
        return NULL;

    struct compiled_debug_fun *df = (struct compiled_debug_fun*)native_pointer(di->fun_map);
    int begin, end, elsewhere_begin, elsewhere_end;
    if (!decode_locs(df->encoded_locs, &begin, &elsewhere_begin))
        return NULL;
    sword_t offset = (char*)pc - code_text_start(code);
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
    if (!string_widetag_p(widetag_of(&vector->header))) {
        fprintf(f, "<??? type %d>", widetag_of(&vector->header));
        return;
    }
    int i;
    int n = vector_len(vector);
    for (i = 0; i < n; i++) {
        unsigned int c = schar(vector, i);
        if (c > 0xFFFF) fprintf(f,"\\U%08x", c);
        // without knowing whether the terminal can accept
        // character codes 128 through 255, it's conservative
        // to just output unicode escapes.
        else if (c > 0x7F) fprintf(f,"\\u%04x", c);
        else {
            if (c == '\\' || c == '"') putc('\\', f);
            putc(c, f);
        }
    }
}

lispobj debug_print(lispobj string)
{
    print_string(VECTOR(string), stderr);
    putc('\n', stderr);
    return 0;
}

lispobj symbol_package(struct symbol* s)
{
    static int warned;
    // If using ldb when debugging cold-init, this can be confusing to see all symbols
    // as if they were uninterned, but package-IDs are always available in the symbol.
    // End-users should never see this failure.
    if (!lisp_package_vector) {
        if (!warned) {
          fprintf(stderr, "Warning: package vector has not been initialized yet\n");
          warned = 1;
        }
        return NIL;
    }
    struct vector* v = VECTOR(lisp_package_vector);
    int id = symbol_package_id(s);
    if (id < vector_len(v)) return v->data[id];
    lose("can't decode package ID %d", id);
}

static void
print_entry_name (lispobj name, FILE *f)
{
    name = follow_maybe_fp(name);
    if (listp(name)) {
        putc('(', f);
        while (name != NIL) {
            if (!listp(name)) {
                fprintf(f, "%p: unexpected lowtag while printing a cons\n",
                       (void*)name);
                return;
            }
            print_entry_name(CONS(name)->car, f);
            name = follow_maybe_fp(CONS(name)->cdr);
            if (name != NIL)
                putc(' ', f);
        }
        putc(')', f);
    } else if (lowtag_of(name) == OTHER_POINTER_LOWTAG) {
        struct symbol *symbol = SYMBOL(name);
        char* prefix = 0;
        int widetag = header_widetag(symbol->header);
        switch (widetag) {
        case SYMBOL_WIDETAG:
            switch (symbol_package_id(symbol)) {
            case PACKAGE_ID_NONE: prefix = "#:"; break;
            case PACKAGE_ID_LISP: prefix = ""; break;
            case PACKAGE_ID_USER: prefix = "CL-USER::"; break;
            case PACKAGE_ID_KEYWORD: prefix = ":"; break;
            }
            if (prefix) fputs(prefix, f); else {
                struct package *pkg
                    = (struct package *)native_pointer(symbol_package(symbol));
                struct vector *pkg_name = VECTOR(follow_maybe_fp(pkg->_name));
                print_string(pkg_name, f);
                fputs("::", f);
            }
            print_string(symbol_name(symbol), f);
            break;
        case SIMPLE_BASE_STRING_WIDETAG:
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
        case SIMPLE_CHARACTER_STRING_WIDETAG:
#endif
            putc('"', f);
            print_string((struct vector*)symbol, f);
            putc('"', f);
            break;
        default:
            fprintf(f, "<??? type %d>", widetag);
        }
    } else if (fixnump(name)) {
        fprintf(f, "%d", (int)fixnum_value(name));
    } else {
        fprintf(f, "<??? lowtag %d>", (int) lowtag_of(name));
    }
}

static void __attribute__((unused))
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

#include "callframe.inc"

static struct code *
code_pointer(lispobj object)
{
    lispobj *headerp = native_pointer(object);
    int len;
    switch (widetag_of(headerp)) {
        case CODE_HEADER_WIDETAG:
            break;
#ifdef RETURN_PC_WIDETAG
        case RETURN_PC_WIDETAG:
#endif
        case SIMPLE_FUN_WIDETAG:
            len = (HeaderValue(*headerp) & FUN_HEADER_NWORDS_MASK);
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
cs_valid_pointer_p(struct thread *thread, struct call_frame *pointer)
{
    return (((char *) thread->control_stack_start <= (char *) pointer) &&
            ((char *) pointer < (char *) access_control_stack_pointer(thread)));
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
        pc = *os_context_pc_addr(context);
        info->frame =
            (struct call_frame *)(uword_t)
                (*os_context_register_addr(context, reg_CFP));
        info->code =
#ifdef reg_CODE
            code_pointer(*os_context_register_addr(context, reg_CODE));
#else
        (struct code *)dynamic_space_code_from_pc((char *)pc);
#endif
        info->lra = NIL;

    }

    if (info->code != NULL)
        info->pc = (char*)pc - (char*)info->code;
    else
        info->pc = 0;
}

// Return 1 if we have a valid frame, 0 if not.
int lisp_frame_previous(struct thread *thread, struct call_info *info)
{
    struct call_frame *this_frame;
    int free_ici;
    lispobj lra;

    if (!cs_valid_pointer_p(thread, info->frame)) return 0;

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
        info->code =
#ifdef reg_CODE
        (struct code*)native_pointer(this_frame->code);
        info->pc = lra;
#else
        (struct code *)dynamic_space_code_from_pc((char *)lra);
        info->pc = (char*)native_pointer(lra) - (char*)info->code;
#endif

        info->lra = NIL;
    } else {
        info->code = code_pointer(lra);
        if (info->code != NULL)
            info->pc = (char*)native_pointer(info->lra) - (char*)info->code;
        else
            info->pc = 0;
    }

    return 1;
}

void
lisp_backtrace(int nframes)
{
    struct thread *thread = get_sb_vm_thread();
    struct call_info info;

    info.frame = (struct call_frame *)access_control_frame_pointer(thread);
    info.interrupted = 0;
    info.code = NULL;
    info.lra = 0;
    info.pc = 0;

    int i = 0;
    int footnotes = 0;
    do {
        if (!lisp_frame_previous(thread, &info)) {
            if (info.frame) // 0 is normal termination of the call chain
                printf("Bad frame pointer %p [valid range=%p..%p]\n", info.frame,
                       thread->control_stack_start, thread->control_stack_end);
            break;
        }
        printf("%4d: ", i);
        // Print spaces to keep the alignment nice
        if (info.interrupted
#ifdef reg_CODE
            || info.lra == NIL
#endif
            ) {
            putchar('[');
            if (info.interrupted) { footnotes |= 1; putchar('I'); }
#ifdef reg_CODE
            if (info.lra == NIL) { footnotes |= 2; putchar('*'); }
#endif
            putchar(']');
            if (!(info.lra == NIL && info.interrupted)) putchar(' ');
        } else {
            printf("    ");
        }
        printf("%p ", info.frame);
        void* absolute_pc = 0;
        if (info.code) {
            absolute_pc = (char*)info.code + info.pc;
            printf("pc=%p {%p+%04x} ", absolute_pc, info.code, (int)info.pc);
        } else {
            absolute_pc = (char*)info.pc;
            printf("pc=%p ", absolute_pc);
        }

        // If LRA does not match the PC, print it. This should not happen.
        if (info.lra != make_lispobj(absolute_pc, OTHER_POINTER_LOWTAG)
            && info.lra != NIL)
            printf("LRA=%p ", (void*)info.lra);

        if (info.code) {
            struct compiled_debug_fun *df;
            if (absolute_pc &&
                (df = debug_function_from_pc((struct code *)info.code, absolute_pc)))
                print_entry_name(df->name, stdout);
            else
                // I can't imagine a scenario where we have info.code
                // but do not have an absolute_pc, or debug-fun can't be found.
                // Anyway, we can uniquely identify code by serial# now.
                printf("{code_serialno=%x}", code_serialno(info.code));
        }

        putchar('\n');

    } while (++i <= nframes);
    if (footnotes) printf("Note: [I] = interrupted"
#ifdef reg_CODE
                          ", [*] = no LRA"
#endif
                          "\n");
}

#else

static int
altstack_pointer_p(__attribute__((unused)) struct thread* thread,
                   __attribute__((unused)) void *p) {
#ifndef LISP_FEATURE_WIN32
    // FIXME: shouldn't this be testing '>=' start and '<' end ?
    //        i.e. Was it only right because the calculations themselves were wrong ?
    return (p > calc_altstack_base(thread) && p <= calc_altstack_end(thread));
#else
    /* Win32 doesn't do altstack */
    return 0;
#endif
}

static int
stack_pointer_p(struct thread* thread, void *p)
{
    /* we are using sizeof(long) here, because that is the right value on both
     * x86 and x86-64.  (But note that false positives would not cause much harm
     * given the heuristical nature of x86_call_context.) */
    uword_t stack_alignment = sizeof(void*);
    void *stack_start;

    if (altstack_pointer_p(thread, p))
        return 1;

    if (altstack_pointer_p(thread, &p)) {
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
ra_pointer_p (struct thread* th, void *ra)
{
  /* the check against 4096 is still a mystery to everyone interviewed about
   * it, but recent changes to sb-sprof seem to suggest that such values
   * do occur sometimes. */
  return ((uword_t) ra) > 4096 && !stack_pointer_p (th, ra);
}

static int NO_SANITIZE_MEMORY
x86_call_context (struct thread* th, void *fp, void **ra, void **ocfp)
{
  void *c_ocfp;
  void *c_ra;
  int c_valid_p;

  if (!stack_pointer_p(th, fp))
    return 0;

  c_ocfp    = *((void **) fp);
  c_ra      = *((void **) fp + 1);

  c_valid_p = (c_ocfp > fp
               && stack_pointer_p(th, c_ocfp)
               && ra_pointer_p(th, c_ra));

  if (c_valid_p)
    *ra = c_ra, *ocfp = c_ocfp;
  else
    return 0;

  return 1;
}

void
describe_thread_state(void)
{
    struct thread *thread = get_sb_vm_thread();
    struct interrupt_data *data = &thread_interrupt_data(thread);
#ifndef LISP_FEATURE_WIN32
    sigset_t mask;
    char string[180];
    thread_sigmask(SIG_BLOCK, 0, &mask);
    sigset_tostring(&mask, string, sizeof string);
    if (string[0]) printf("Signal mask: %s\n", string);
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
    fprintf(f, "%4d: fp=%p pc=%p ", i, fp, pc);
    struct code *code = (void*)component_ptr_from_pc(pc);
    if (code) {
        struct compiled_debug_fun *df = debug_function_from_pc(code, pc);
        if (df)
            print_entry_name(df->name, f);
        else if (pc >= (char*)asm_routines_start && pc < (char*)asm_routines_end)
            fprintf(f, "(assembly routine)");
        else
            fprintf(f, "{code_serialno=%x}", code_serialno(code));
    } else if (gc_managed_heap_space_p((uword_t)pc)) {
#ifdef LISP_FEATURE_X86
        // can't actually have a PC inside a random object, it's got to be a frame
        // that didn't set up the pointer chain, quite possibly a signal frame such as:
        //   7: fp=0xd78c8460 pc=0xf7fb51b0 Foreign function __kernel_rt_sigreturn
        //   8: fp=0xd78c8478 pc=0xd9c43159 (bad PC)
        //   9: fp=0xd78c84ec pc=0xd849a17e (FLET SB-C::DO-1-USE :IN SB-C::TENSION-IF-IF-1)
        // where, if you print the PC actually from the context, line 8 would be 0xd823ea78.
        fprintf(f, "(bad PC)");
#else
        // It could be a generic-function with self-contained tramponline code,
        // or the executable JMP instruction in an fdefn.
        fprintf(f, "(unknown lisp object)");
#endif
    } else {
#ifdef LISP_FEATURE_OS_PROVIDES_DLADDR
        Dl_info info;
        if (dladdr(pc, &info)) {
            fprintf(f, "Foreign function %s", info.dli_sname);
        } else
#endif
            fprintf(f, "Foreign function");
    }

    putc('\n', f);
}

/* This function has been split from lisp_backtrace() to enable Lisp
 * backtraces from gdb with call backtrace_from_fp(...). Useful for
 * example when debugging threading deadlocks.
 */
void NO_SANITIZE_MEMORY
log_backtrace_from_fp(struct thread* th, void *fp, int nframes, int start, FILE *f)
{
  int i = start;

  for (; i < nframes; ++i) {
    void *ra;
    void *next_fp;

    if (!x86_call_context(th, fp, &ra, &next_fp))
      break;
    print_backtrace_frame(ra, next_fp, i, f);
    fp = next_fp;
  }
}
void backtrace_from_fp(void *fp, int nframes, int start) {
    log_backtrace_from_fp(get_sb_vm_thread(), fp, nframes, start, stdout);
}

void backtrace_from_context(os_context_t *context, int nframes) {
    void *fp = (void *)os_context_frame_pointer(context);
    print_backtrace_frame((void *)*os_context_pc_addr(context), fp, 0, stdout);
    backtrace_from_fp(fp, nframes - 1, 1);
}

void
lisp_backtrace(int nframes)
{
    struct thread *thread = get_sb_vm_thread();
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

// Find the simple_fun that contains 'pc' in 'code'
int simple_fun_index_from_pc(struct code* code, char *pc)
{
    char *instruction_area = code_text_start(code);
    unsigned int* offsets = code_fun_table(code) - 1;
    int index;
    for (index = code_n_funs(code) - 1; index >= 0; --index) {
        char *base = instruction_area + offsets[-index];
        if (pc >= base) return index;
    }
    return -1;
}

static boolean __attribute__((unused)) print_lisp_fun_name(char* pc)
{
  struct code* code;
  if (gc_managed_heap_space_p((uword_t)pc) &&
      (code = (void*)component_ptr_from_pc(pc)) != 0) {
      struct compiled_debug_fun* df = debug_function_from_pc(code, pc);
      if (df) {
          fprintf(stderr, " %p [", pc);
          print_entry_name(df->name, stderr);
          fprintf(stderr, "]\n");
          return 1;
      }
  }
  return 0;
}

#ifdef LISP_FEATURE_BACKTRACE_ON_SIGNAL
#define UNW_LOCAL_ONLY
#ifdef HAVE_LIBUNWIND
#include <libunwind.h>
#endif
#include "genesis/thread-instance.h"
#include "genesis/mutex.h"
static __attribute__((unused))int backtrace_completion_pipe[2] = {-1,-1};
void libunwind_backtrace(struct thread *th, os_context_t *context)
{
    fprintf(stderr, "Lisp thread @ %p, tid %d", th, (int)th->os_kernel_tid);
#ifdef LISP_FEATURE_SB_THREAD
    // the TLS area is not used if #-sb-thread. And if so, it must be "main thred"
    struct thread_instance* lispthread = (void*)native_pointer(th->lisp_thread);
    if (lispthread->name != NIL) {
        fprintf(stderr, " (\"");
        print_string(VECTOR(lispthread->name), stderr);
        fprintf(stderr, "\")");
    }
    putc('\n', stderr);
    if (lispthread->waiting_for != NIL) {
        fprintf(stderr, "waiting for %p", (void*)lispthread->waiting_for);
        if (instancep(lispthread->waiting_for)) {
            // THREAD-WAITING-FOR can be a mutex or a waitqueue (if not a cons).
            // Accessing it as if it's a mutex works because both a waitqueue
            // and a mutex have a name at the same slot offset (if #+sb-futex).
            // So to reiterate the comment from linux-os.c -
            // "Use this only if you know what you're doing"
            struct mutex* lispmutex = (void*)native_pointer(lispthread->waiting_for);
            if (lispmutex->name != NIL) {
                fprintf(stderr, " (MUTEX:\"");
                print_string(VECTOR(lispmutex->name), stderr);
                fprintf(stderr, "\")");
            }
        }
        putc('\n', stderr);
    }
#endif
#ifdef HAVE_LIBUNWIND
    char procname[100];
    unw_cursor_t cursor;
    // "unw_init_local() is thread-safe as well as safe to use from a signal handler."
    // "unw_get_proc_name() is thread-safe. If cursor cp is in the local address-space,
    //  this routine is also safe to use from a signal handler."
    if (context) {
        unw_init_local(&cursor, context);
    } else {
        unw_context_t here;
        unw_getcontext(&here);
        unw_init_local(&cursor, &here);
    }
    do {
        uword_t offset;
        char *pc;
        unw_get_reg(&cursor, UNW_TDEP_IP, (uword_t*)&pc);
        if (print_lisp_fun_name(pc)) {
            // printed
        } else if (!unw_get_proc_name(&cursor, procname, sizeof procname, &offset)) {
            fprintf(stderr, " %p [%s]\n", pc, procname);
        } else {
            fprintf(stderr, " %p ?\n", pc);
        }
    } while (unw_step(&cursor));
#else
    // If you don't have libunwind, this will almost surely not work,
    // because we can't figure out how to get backwards past a signal frame.
    log_backtrace_from_fp(th, (void*)*os_context_fp_addr(context), 100, 0, stderr);
#endif
}
void backtrace_lisp_threads(int __attribute__((unused)) signal,
                                   siginfo_t __attribute__((unused)) *info,
                                   os_context_t *context)
{
    struct thread* this_thread = get_sb_vm_thread();
#ifdef LISP_FEATURE_SB_THREAD
    if (backtrace_completion_pipe[1] >= 0) {
        libunwind_backtrace(current_thread, context);
        write(backtrace_completion_pipe[1], context /* any random byte */, 1);
        return;
    }
    struct thread *th;
    int nthreads = 0;
    for_each_thread(th) { ++nthreads; }
    if (signal)
        fprintf(stderr, "Caught backtrace-all signal in tid %d, %d threads\n",
                (int)this_thread->os_kernel_tid, nthreads);
    // Would be nice if we could forcibly stop all the other threads,
    // but pthread_mutex_trylock is not safe to use in a signal handler.
    if (nthreads > 1) {
        pipe(backtrace_completion_pipe);
    }
    for_each_thread(th) {
        if (th == this_thread)
            libunwind_backtrace(th, context);
        else {
            char junk;
            pthread_kill(th->os_thread, SIGXCPU);
            read(backtrace_completion_pipe[0], &junk, 1);
        }
    }
    if (nthreads > 1) {
        close(backtrace_completion_pipe[1]);
        close(backtrace_completion_pipe[0]);
        backtrace_completion_pipe[0] = backtrace_completion_pipe[1] = -1;
    }
#else
    libunwind_backtrace(this_thread, context);
#endif
}
static int watchdog_pipe[2] = {-1,-1};
static pthread_t watchdog_tid;
static void* watchdog_thread(void* arg) {
    struct timeval timeout;
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(watchdog_pipe[0], &fds);
    timeout.tv_sec = (long)arg;
    timeout.tv_usec = 0;
    int nfds = select(watchdog_pipe[0]+1, &fds, 0, 0, &timeout);
    if (nfds == 0) {
        // Ensure this message comes out in one piece even if nothing following it does.
        char msg[] = "Watchdog timer expired\n"; write(2, msg, sizeof msg-1);
        backtrace_lisp_threads(0, 0, 0);
        _exit(1); // cause the test suite to exit with failure
    }
    return 0;
}
void start_watchdog(int sec) {
    if (pipe(watchdog_pipe)) lose("Can't make watchdog pipe");
    pthread_create(&watchdog_tid, 0, watchdog_thread, (void*)(long)sec);
    char msg[] = "Started watchdog thread\n"; write(2, msg, sizeof msg-1);
}
void stop_watchdog() {
    char c[1] = {0};
    write(watchdog_pipe[1], c, 1);
    close(watchdog_pipe[1]);
    void* result;
    pthread_join(watchdog_tid, &result);
    close(watchdog_pipe[0]);
    watchdog_pipe[0] = watchdog_pipe[1] = -1;
}
#endif
