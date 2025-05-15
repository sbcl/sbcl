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

#include "genesis/sbcl.h"
#include "lispobj.h"

#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <sys/time.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/resource.h>
#endif
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>

#include "runtime.h"
#include "parse.h"
#include "vars.h"

#include "print.h"
#include "arch.h"
#include "interr.h"
#include "search.h"
#include "globals.h"
#include "lispregs.h"
#include "interrupt.h"
#include "thread.h"
#include "genesis/instance.h"
#include "genesis/static-symbols.h"
#include "genesis/primitive-objects.h"
#include "genesis/gc-tables.h"
#include "genesis/unwind-block.h"
#include "genesis/hash-table.h"
#include "gc.h"
#include "../../tlsf-bsd/tlsf/tlsf.h"
extern void* tlsf_control;

typedef int cmd(char **ptr,iochannel_t);

struct crash_preamble {
    uword_t signature;
    uword_t linkage_start;
    uword_t linkage_nbytes;
    uword_t static_start;
    uword_t static_nbytes;
    uword_t static_freeptr;
    uword_t readonly_start;
    uword_t readonly_nbytes;
    uword_t permgen_start;
    uword_t permgen_nbytes;
    uword_t dynspace_start;
    long dynspace_npages_total;
    long dynspace_npages_used;
    int card_size;
    int card_table_nbits;
    // fixedobj data dumped: pages, page table
    uword_t fixedobj_start, fixedobj_size, fixedobj_free_pointer;
    // text data dumped: pages, touched_bits, page table
    uword_t text_start, text_size;
    lispobj *tlsf_mem_start, *text_space_highwatermark;
    lispobj sentinel_block[3];
    void* tlsf_control_address;
    int nthreads;
    int tls_size;
    lispobj lisp_package_vector;
    int sizeof_context;
    int tlsf_control_size;
    char sprof_enabled;
    char pin_dynspace_code;
};
struct crash_thread_preamble {
    uword_t address;
    uword_t has_context;
    uword_t control_stack_nbytes;
    uword_t binding_stack_nbytes;
};

// Prevent some mixups in case you add fields to the crash dump
const uword_t CRASH_PREAMBLE_SIGNATURE =
    (sizeof (struct crash_preamble) << 16) | sizeof (struct crash_thread_preamble);

__attribute__((unused))
static void maybe_show_contents(__attribute__((unused)) char *legend,
                                __attribute__((unused)) void* buf,
                                __attribute__((unused)) long nbytes)
{
#if 0 // def LISP_FEATURE_64_BIT
    int want = legend && (!strcmp(legend, "preamble") || legend[0] == ' ');
    if (!want) return;
    int nwords = nbytes>>WORD_SHIFT;
    int i;
    for (i=0;i<nwords;++i)
        fprintf(stderr, "%s %16lx", (i%8)?"":(i?"\n   ":"   "), ((uword_t*)buf)[i]);
    putc('\n', stderr);
#endif
}

#if defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64 // un-tested elsewhere
#include <errno.h>
struct filewriter { long total; int fd; bool verbose; };

static void checked_write(char *section, struct filewriter* writer, void* buf, long nbytes)
{
    char label[8];
    // I don't know why strncpy() gets a warning here. Isn't truncation exactly the point?
    // monitor.c:118:5: warning: ‘strncpy’ output truncated before terminating nul copying 8 bytes
    //   from a string of the same length [-Wstringop-truncation]
    memset(label, 0, sizeof label);
    memcpy(label, section, strlen(section)<sizeof label?strlen(section):sizeof label);
    ssize_t wrote;
    if (write(writer->fd, label, sizeof label) != sizeof label
        || (wrote = write(writer->fd, buf, nbytes)) != nbytes)
        lose("short write, errno=%d", errno);
    if (writer->verbose) fprintf(stderr, "%s: %lx bytes\n", section, (long)(nbytes + sizeof label));
    maybe_show_contents(section, buf, nbytes);
    writer->total += nbytes + sizeof label;
}

#include "immobile-space.h"
void save_gc_crashdump(char *pathname,
                       lispobj* cur_thread_approx_stackptr,
                       bool verbose)
{
    extern int pin_all_dynamic_space_code;
    int fd = open(pathname, O_WRONLY|O_CREAT|O_TRUNC, 0666);
    struct thread* th;
    int nthreads = 0;
    for_each_thread(th) ++nthreads;
    if (verbose) fprintf(stderr, "save: %d threads\n", nthreads);
    struct crash_preamble preamble;
    unsigned long nbytes_heap = next_free_page * GENCGC_PAGE_BYTES;
    int nbytes_tls = SymbolValue(FREE_TLS_INDEX,0);
    preamble.signature = CRASH_PREAMBLE_SIGNATURE;
#ifdef LISP_FEATURE_LINKAGE_SPACE
    preamble.linkage_start = (uword_t)linkage_space;
    preamble.linkage_nbytes = LISP_LINKAGE_SPACE_SIZE;
#endif
    preamble.static_start = STATIC_SPACE_START;
    // saving all of static space is easier the separating out the constants at the end
    preamble.static_nbytes = STATIC_SPACE_SIZE; // (uword_t)static_space_free_pointer - STATIC_SPACE_START;
    preamble.static_freeptr = (uword_t)static_space_free_pointer;
    preamble.readonly_start = READ_ONLY_SPACE_START;
    preamble.readonly_nbytes = (uword_t)read_only_space_free_pointer - READ_ONLY_SPACE_START;
    preamble.permgen_start = PERMGEN_SPACE_START;
    preamble.permgen_nbytes = (uword_t)permgen_space_free_pointer - PERMGEN_SPACE_START;
    preamble.dynspace_start = DYNAMIC_SPACE_START;
    preamble.dynspace_npages_total = dynamic_space_size / GENCGC_PAGE_BYTES;
    preamble.dynspace_npages_used = next_free_page;
    preamble.card_size = GENCGC_CARD_BYTES;
    preamble.card_table_nbits = gc_card_table_nbits;
    preamble.nthreads = nthreads;
    preamble.tls_size = nbytes_tls;
    preamble.lisp_package_vector = lisp_package_vector;
    preamble.sprof_enabled = sb_sprof_enabled;
    preamble.pin_dynspace_code = pin_all_dynamic_space_code;
    preamble.sizeof_context = sizeof (os_context_t);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    char *tlsf_memory_end = (char*)TEXT_SPACE_START + text_space_size;
    preamble.fixedobj_start = FIXEDOBJ_SPACE_START;
    preamble.fixedobj_size = FIXEDOBJ_SPACE_SIZE;
    preamble.fixedobj_free_pointer = (uword_t)fixedobj_free_pointer;
    preamble.text_start = TEXT_SPACE_START;
    preamble.text_size = text_space_size;
    preamble.text_space_highwatermark = text_space_highwatermark;
    preamble.tlsf_mem_start = tlsf_mem_start;
    preamble.tlsf_control_address = tlsf_control;
    preamble.tlsf_control_size = tlsf_size();
    memcpy(preamble.sentinel_block, tlsf_memory_end-3*N_WORD_BYTES, 3*N_WORD_BYTES);
#endif
    struct filewriter writer = { .fd = fd, .total = 0, .verbose = verbose };
    // write the preamble and static + readonly spaces
    checked_write("preamble", &writer, &preamble, sizeof preamble);
#ifdef LISP_FEATURE_LINKAGE_SPACE
    checked_write("linkage", &writer, (char*)linkage_space, preamble.linkage_nbytes);
#endif
    checked_write("static", &writer, (char*)STATIC_SPACE_START, preamble.static_nbytes);
    checked_write("R/O", &writer, (char*)READ_ONLY_SPACE_START, preamble.readonly_nbytes);
    checked_write("perm", &writer, (char*)PERMGEN_SPACE_START, preamble.permgen_nbytes);

    // write the dynamic-space, PTEs, card table
    checked_write("dynamic", &writer, (char*)DYNAMIC_SPACE_START, nbytes_heap);
    checked_write("PTE", &writer, page_table, sizeof (struct page) * next_free_page);
    checked_write("cardmark", &writer, gc_card_mark, 1+gc_card_table_mask);
#ifdef LISP_FEATURE_MARK_REGION_GC
    checked_write("allocated", &writer, allocation_bitmap, bitmap_size(next_free_page));
    extern line_index_t line_count;
    checked_write("linemap", &writer, line_bytemap, line_count);
#endif
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    int usage = (uword_t)fixedobj_free_pointer - FIXEDOBJ_SPACE_START;
    checked_write("fixedobj", &writer, (char*)FIXEDOBJ_SPACE_START, usage);
    int total_npages = FIXEDOBJ_SPACE_SIZE / IMMOBILE_CARD_BYTES;
    checked_write("fixedobj_PTE", &writer, fixedobj_pages, total_npages * sizeof sizeof(struct fixedobj_page));
    usage = (uword_t)text_space_highwatermark - TEXT_SPACE_START;
    // write the block_header_t that is just beyond the high water mark
    checked_write("text", &writer, (char*)TEXT_SPACE_START, usage+3*N_WORD_BYTES);
    total_npages = text_space_size / IMMOBILE_CARD_BYTES;
    int n_bitmap_elts = ALIGN_UP(total_npages, 32) / 32;
    checked_write("text_gen", &writer, text_page_genmask, total_npages); // 1 byte per page
    checked_write("text_WP", &writer, text_page_touched_bits, n_bitmap_elts * sizeof (int));
    checked_write("TLSF_control", &writer, tlsf_control, preamble.tlsf_control_size);
    int tlsf_memory_size = tlsf_memory_end - (char*)tlsf_mem_start;
    int n_tlsf_pages = tlsf_memory_size / IMMOBILE_CARD_BYTES;
    checked_write("TLSF_sso", &writer, tlsf_page_sso, n_tlsf_pages * sizeof (short));
#endif
    struct crash_thread_preamble thread_preamble;
    for_each_thread(th) {
        int ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th));
        os_context_t* threadcontext = nth_interrupt_context(0, th);
        uword_t sp;
        if (ici) {
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
            sp = *os_context_register_addr(threadcontext, reg_SP);
#elif defined reg_CSP
            sp = *os_context_register_addr(threadcontext, reg_CSP);
#endif
        } else if (th->state_word.state == STATE_DEAD) {
            gc_assert(th->binding_stack_pointer == th->binding_stack_start);
            // FIXME: assumes stack grows down
            sp = (uword_t)th->control_stack_end;
        } else {
            if (th != get_sb_vm_thread()) {
              char msg[80];
              int n = snprintf(msg, sizeof msg,
                               "thread %p state %d - No stackptr for crash dump\n",
                               th, th->state_word.state);
              ignore_value(write(2, msg, n));
              _exit(1);
            }
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
            sp = (uword_t)cur_thread_approx_stackptr;
#else
            sp = access_control_stack_pointer(th);
#endif
        }
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        int nbytes_control_stack =
            (char*)th->control_stack_end - (char*)sp; // grows downward
        int nbytes_binding_stack =
            (char*)th->binding_stack_pointer - (char*)th->binding_stack_start; // grows upward
#else
        int nbytes_control_stack = (char*)sp - (char*)th->control_stack_start; // grows upward
        int nbytes_binding_stack =
          (char*)get_binding_stack_pointer(th) - (char*)th->binding_stack_start;
#endif
        thread_preamble.address = (uword_t)th;
        thread_preamble.has_context = ici != 0; // boolean for have context or not
        thread_preamble.control_stack_nbytes = nbytes_control_stack;
        thread_preamble.binding_stack_nbytes = nbytes_binding_stack;
        // write the preamble
        checked_write("thread", &writer, &thread_preamble, sizeof thread_preamble);
        // write 0 or 1 contexts, control-stack, binding-stack, TLS
        if (ici) checked_write(" ctxt", &writer, threadcontext, preamble.sizeof_context);
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        checked_write(" stack", &writer, (char*)sp, nbytes_control_stack);
#else
        checked_write(" stack", &writer, th->control_stack_start, nbytes_control_stack);
#endif
        checked_write(" bindings", &writer, th->binding_stack_start, nbytes_binding_stack);
        checked_write(" TLS", &writer, th, nbytes_tls);
    }
    checked_write("sig", &writer, "SB.Crash", 8); // trailing signature
    close(fd);
    if (verbose) fprintf(stderr, "Total: %ld bytes\n", writer.total);
}
#endif

static cmd call_cmd, dump_cmd, print_cmd, quit_cmd, help_cmd;
static cmd flush_cmd, regs_cmd, exit_cmd;
static cmd print_context_cmd, pte_cmd, search_cmd, hashtable_cmd;
static cmd backtrace_cmd, threadbt_cmd, catchers_cmd;
static cmd threads_cmd, findpath_cmd, layouts_cmd;

extern void gc_stop_the_world(), gc_start_the_world();
static void suspend_other_threads() {
    gc_stop_the_world();
    // It might make sense for each thread's stop-for-gc handler to close its region
    // versus doing this loop
    struct thread *th;
    for_each_thread(th) { gc_close_thread_regions(th, 0); }
    gc_close_collector_regions(0);
}
static void unsuspend_other_threads() {
    gc_start_the_world();
}

static int save_cmd(char **ptr, iochannel_t io) {
    char *name  = parse_token(ptr);
    if (!name) {
        fprintf(io->out, "Need filename\n");
        return 0;
    }
#if (defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64) && defined LISP_FEATURE_SB_THREAD
    suspend_other_threads();
    save_gc_crashdump(name, (lispobj*)__builtin_frame_address(0), 1);
    unsuspend_other_threads();
#else
    fprintf(io->out, "Unimplemented\n");
#endif
    return 0;
}
static int gc_and_save_cmd(char **ptr, iochannel_t io) {
    /* The use-case for this is as follows: suppose you're testing a shiny new GC
     * on a large Lisp application, but gc_and_save crashes 1 time in 10.
     * How do you effectively debug that if merely getting to the point where you
     * would run S-L-A-D takes significant time?  Just inject save_gc_crashdump()
     * into every save at the top of prepare_to_save() or thereabouts, and run
     * until you collect at least one crash dump. Then iterate on editing the
     * runtime including the GC, and restarting 'ldb' on the saved state from a
     * crashed run to try to isolate what went wrong without having to build up
     * the Lisp heap again from scratch */
    char *name  = parse_token(ptr);
    if (!name) {
        fprintf(io->out, "Need filename\n");
        return 0;
    }
#if defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_WIN32)
    ASSIGN_CURRENT_THREAD(all_threads);
#endif
    extern void gc_and_save(char*,bool,bool,bool,bool,int,int);
    gc_and_save(name, 0, 0, 0, 0, 0, 0); // never returns
    return 0;
}
void list_lisp_threads(int regions, FILE* f) {
    struct thread* th;
    fprintf(f, "(thread*, pthread, sb-thread:thread, name)\n");
    void* pthread;
    for_each_thread(th) {
        memcpy(&pthread, &th->os_thread, N_WORD_BYTES);
        struct thread_instance* i = (void*)(th->lisp_thread - INSTANCE_POINTER_LOWTAG);
        lispobj name = instancep(th->lisp_thread) ? i->_name : 0;
        char* cname = NULL;
        // it's OK to call widetag_of on NIL but not NULL
        if (name && simple_base_string_p(name)) cname = vector_sap(name);
        fprintf(f, "%p %p %p \"%s\"\n", th, pthread, (void*)i, cname);
        if (regions) {
#define show_tlab(label, r) fprintf(f, "  %s @ %p: %p %p %p\n", label, \
         &th->r, th->r.start_addr, th->r.end_addr, th->r.free_pointer)
            show_tlab("usr cons ", cons_tlab);
            show_tlab("usr mix  ", mixed_tlab);
            show_tlab("sys cons ", sys_cons_tlab);
            show_tlab("sys mix  ", sys_mixed_tlab);
#undef show_tlab
        }
    }
    if (regions) {
#define show_tlab(label, r) fprintf(f, "  %s @ %p: %p %p %p\n", label, \
         r, r->start_addr, r->end_addr, r->free_pointer)
        fprintf(stderr, "global regions:\n");
        show_tlab("mixed    ", mixed_region);
        show_tlab("smallmix ", small_mixed_region);
        show_tlab("unboxed  ", unboxed_region);
        show_tlab("code     ", code_region);
        show_tlab("boxed    ", boxed_region);
        show_tlab("cons     ", cons_region);
#undef show_tlab
    }
}
static int threads_cmd(char **ptr, iochannel_t io) {
    list_lisp_threads(more_p(ptr) && !strncmp(*ptr, "-r", 2), io->out);
    return 0;
}
extern int heap_trace_verbose;
extern int gc_pathfind_aux(lispobj*, lispobj, lispobj, lispobj, int);

static int findpath_cmd(char **ptr, iochannel_t io) {
    // prevent the path finder from seeing the object that results from parsing the command
    lispobj* stackptr = (lispobj*)&ptr;
    // overaligned for 32-bit but doesn't matter
    struct vector __attribute__ ((aligned (16))) result;
    struct cons __attribute__ ((aligned (16))) list;
    /* Despite capturing the stack pointer coming in to this function,
     * it's very difficult to ensure that a locally allocated weak pointer
     * is not above that (and hence its value slot seen).
     * So it has to be malloc()ed. */
    char* wp_mem = malloc(sizeof (struct weak_pointer) + N_WORD_BYTES);
    struct weak_pointer* wp =
      (void*)(wp_mem + (((uword_t)wp_mem & LOWTAG_MASK) ? N_WORD_BYTES : 0));
    wp->header = ((WEAK_POINTER_SIZE-1)<<N_WIDETAG_BITS)|WEAK_POINTER_WIDETAG;
    if (parse_lispobj(ptr, &wp->value)) {
        list.car = make_lispobj(wp, OTHER_POINTER_LOWTAG);
        list.cdr = NIL;
        result.header = SIMPLE_VECTOR_WIDETAG;
        result.length_ = make_fixnum(1);
        result.data[0] = 0;
        suspend_other_threads();
        int save = heap_trace_verbose;
        heap_trace_verbose = 4;
        gc_pathfind_aux(stackptr,
                        make_lispobj(&list, LIST_POINTER_LOWTAG),
                        make_lispobj(&result, OTHER_POINTER_LOWTAG),
                        0, 2);
        heap_trace_verbose = save;
        unsuspend_other_threads();
        lispobj path = result.data[0];
        if (listp(path)) {
            fprintf(io->out, "Answer:\n");
            while (path != NIL) {
                struct cons* pair = CONS(CONS(path)->car);
                if (listp(pair->cdr)) {
                    // thread root - complicated to print
                } else {
                    // otherwise, object and word index
                    fprintf(io->out, " %"OBJ_FMTX" word %d\n",
                            pair->car, (int)pair->cdr);
                }
                path = CONS(path)->cdr;
            }
        }
    }
    free(wp_mem);
    return 0;
}
static int verify_cmd(char __attribute__((unused)) **ptr,
                      __attribute__((unused)) iochannel_t io) {
    gencgc_verbose = 1;
    suspend_other_threads();
    verify_heap(0, 0);
    unsuspend_other_threads();
    return 0;
}
static int gc_cmd(char **ptr, __attribute__((unused)) iochannel_t io) {
    int last_gen = 0;
    extern generation_index_t verify_gens;
    if (more_p(ptr)) parse_number(ptr, &last_gen);
    gencgc_verbose = 2;
    pre_verify_gen_0 = 1;
    verify_gens = 0;
    suspend_other_threads();
    collect_garbage(last_gen);
    unsuspend_other_threads();
    return 0;
}

#ifdef LISP_FEATURE_IMMOBILE_SPACE
static int tlsf_cmd(__attribute__((unused)) char **ptr,
                    __attribute__((unused)) iochannel_t io) {
    tlsf_dump_pool(tlsf_control, tlsf_mem_start, "/dev/tty");
#ifdef TLSF_CONFIG_DEBUG
    tlsf_check(tlsf_control);
    tlsf_check_pool(tlsf_mem_start);
#endif
    return 0;
}
#endif

static struct cmd {
    char *cmd, *help;
    int (*fn)(char **ptr,iochannel_t);
} supported_cmds[] = {
    // Commands with no help string are all at-your-own-risk
    {"help", "Display this help information.", help_cmd},
    {"?", "(an alias for help)", help_cmd},
    {"backtrace", "Backtrace up to N frames.", backtrace_cmd},
    {"btthread", "Backtrace specified thread", threadbt_cmd},
    {"call", "Call FUNCTION with ARG1, ARG2, ...", call_cmd},
    {"catchers", "Print a list of all the active catchers.", catchers_cmd},
    {"context", "Print interrupt context number I.", print_context_cmd},
    {"dump", "Dump memory starting at ADDRESS for COUNT words.", dump_cmd},
    {"d", "(an alias for dump)", dump_cmd},
    {"exit", "Exit this instance of the monitor.", exit_cmd},
    {"findpath", "Find path to an object.", findpath_cmd},
    {"flush", "Flush all temp variables.", flush_cmd},
    {"hashtable", "Dump a hashtable in detail.", hashtable_cmd},
    {"layouts", "Dump LAYOUT instances.", layouts_cmd},
    {"print", "Print object at ADDRESS.", print_cmd},
    {"p", "(an alias for print)", print_cmd},
    {"pte", "Page table entry for address", pte_cmd},
    {"quit", "Quit.", quit_cmd},
    {"regs", "Display current Lisp registers.", regs_cmd},
    {"search", "Search heap for object.", search_cmd},
    {"save", 0, save_cmd}, // snapshot heap to file ("best effort" though)
    {"gc_and_save", 0, gc_and_save_cmd},
    {"threads", "List threads", threads_cmd},
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    {"tlsfdump", 0, tlsf_cmd}, // (unsafely) dump TLSF structures
#endif
    {"verify", "Check heap invariants", verify_cmd},
    {"gc", 0, gc_cmd}, // (this is not the right way to invoke GC)
    {NULL, NULL, NULL}
};

static int
visible(unsigned char c)
{
    if (c < ' ' || c > '~')
        return ' ';
    else
        return c;
}

static bool valid_widetag_p(unsigned char widetag) {
    // TODO: ensure that widetag is defined (not "unused") and is for a headered object
    // (i.e. is not CHARACTER_WIDETAG and not some other things)
    return other_immediate_lowtag_p(widetag);
}
static int NO_SANITIZE_MEMORY dump_cmd(char **ptr, iochannel_t io)
{
    static char *lastaddr = 0;
    static int lastcount = 20;

    char *addr = lastaddr;
    int count = lastcount, displacement;
    int force = 0, decode = 0;

    if (more_p(ptr)) {
        while (1) {
            if (!strncmp(*ptr, "-f ", 3)) {
              force = 1;
              *ptr += 3;
            } else if (!strncmp(*ptr, "-d ", 3)) {
              decode = 1;
              *ptr += 3;
            } else break;
        }
        if (!parse_addr(ptr, !force, &addr, io->out)) return 0;

        if (more_p(ptr) && !parse_number(ptr, &count)) return 0;
    }

    if (count == 0) {
        fprintf(io->out, "COUNT must be non-zero.\n");
        return 0;
    }

    lastcount = count;

    if (count > 0)
        displacement = N_WORD_BYTES;
    else {
        displacement = -N_WORD_BYTES;
        count = -count;
    }

    bool aligned = ((uword_t)addr & LOWTAG_MASK) == 0;
    if (decode && (!aligned || displacement < 0)) {
        fprintf(io->out, "Sorry, can only decode if aligned and stepping forward\n");
        decode = 0;
    }
    lispobj* next_object = decode ? (lispobj*)addr : 0;

    while (count-- > 0) {
        fprintf(io->out, "%p: ", (os_vm_address_t) addr);
        if (force || gc_managed_addr_p((lispobj)addr)) {
            unsigned long *lptr = (unsigned long *)addr;
            unsigned char *cptr = (unsigned char *)addr;

#if N_WORD_BYTES == 8
            fprintf(io->out, "0x%016lx | %c%c%c%c%c%c%c%c",
                   lptr[0],
                   visible(cptr[0]), visible(cptr[1]),
                   visible(cptr[2]), visible(cptr[3]),
                   visible(cptr[4]), visible(cptr[5]),
                   visible(cptr[6]), visible(cptr[7]));
#else
            unsigned short *sptr = (unsigned short *)addr;
            fprintf(io->out, "0x%08lx   0x%04x 0x%04x   "
                   "0x%02x 0x%02x 0x%02x 0x%02x    "
                   "%c%c"
                   "%c%c",
                   lptr[0], sptr[0], sptr[1],
                   cptr[0], cptr[1], cptr[2], cptr[3],
                   visible(cptr[0]), visible(cptr[1]),
                   visible(cptr[2]), visible(cptr[3]));
#endif
#ifdef LISP_FEATURE_GENERATIONAL
            if (aligned) {
                lispobj ptr = *(lispobj*)addr;
                int gen;
                if (is_lisp_pointer(ptr) && gc_managed_heap_space_p(ptr)
                    && (gen = gc_gen_of(ptr, 99)) != 99) { // say that static is 99
                    if (gen != 99) fprintf(io->out, " | %d", gen);
                } else {
                    fprintf(io->out, "    "); // padding to make MR part line up
                }
            }
#endif
#ifdef LISP_FEATURE_MARK_REGION_GC
            if (aligned && find_page_index(addr) != -1) {
                extern bool allocation_bit_marked(void*);
                fprintf(io->out, " %c", allocation_bit_marked(addr) ? '*' : ' ');
            }
#endif
            if (decode && addr == (char*)next_object) {
                lispobj word = *(lispobj*)addr;
                // ensure validity of widetag because crashing with
                // "no size function" would be worse than doing nothing
                if (word != 0 && !is_lisp_pointer(word)
                    && valid_widetag_p(header_widetag(word))) {
                    fprintf(io->out, " %s", widetag_names[header_widetag(word)>>2]);
                    next_object += headerobj_size2(next_object, word);
                } else if (!is_header(word)) {
                    next_object += CONS_SIZE;
                } else { // disable decoder if weirdness observed
                    decode = 0;
                }
            }
            putc('\n', io->out);
        }
        else
            fprintf(io->out, "invalid Lisp-level address\n");

        addr += displacement;
    }

    lastaddr = addr;
    return 0;
}

static int print_cmd(char **ptr, iochannel_t io)
{
    lispobj obj;
    if (parse_lispobj(ptr, &obj)) print_to_iochan(obj, io);
    return 0;
}

int verify_lisp_hashtable(__attribute__((unused)) struct hash_table* ht,
                          __attribute__((unused)) FILE* file)
{
    int errors = 0;
#if defined LISP_FEATURE_UNIX && defined LISP_FEATURE_64_BIT
    char *kinds[4] = {"EQ","EQL","EQUAL","EQUALP"};
    lispobj* data = VECTOR(ht->pairs)->data;
    uint32_t* hvdata = ht->hash_vector != NIL ?
      (uint32_t*)VECTOR(ht->hash_vector)->data : 0;
    struct vector* iv = VECTOR(ht->index_vector);
    uint32_t* ivdata = (void*)iv->data;
    uint32_t* nvdata = (void*)VECTOR(ht->next_vector)->data;
    unsigned ivmask = vector_len(iv) - 1;
    int hwm = KV_PAIRS_HIGH_WATER_MARK(data);
    sword_t state = (sword_t)ht->sw__hash_fun_state;

    if (file)
        fprintf(file,
                "Table %p Kind=%d=%s State=%ld Weak=%d Count=%d HWM=%d rehash=%d\n",
                ht, hashtable_kind(ht), kinds[hashtable_kind(ht)],
                state, hashtable_weakp(ht)?1:0,
                (int)fixnum_value(ht->_count), hwm, (int)data[1]);
    int j;
    // Flat hash tables are just vectors. Nothing to check.
    if (state != -1) {
        for (j = 1; j <= hwm; j++) {
            lispobj key = data[2*j];
            lispobj val = data[2*j+1];
            if (header_widetag(key) == UNBOUND_MARKER_WIDETAG ||
                header_widetag(val) == UNBOUND_MARKER_WIDETAG) {
                if (file) fprintf(file, "[%4d] %12lx %16lx\n", j, key, val);
                continue;
            }
            uint32_t h;
            if (hvdata && hvdata[j] != 0xFFFFFFFF) {
                h = hvdata[j];
                // print the as-stored hash
                if (file)
                    fprintf(file, "[%4d] %12lx %16lx  %08x %4x (",
                            j, key, val, h, h & ivmask);
            } else {
                // print the hash and then fuzzed hash;
                if (state >= 0) {
                    h = fixnum_value(funcall2(ht->hash_fun, key,
                                              make_fixnum(state)));
                } else {
                    h = fixnum_value(funcall1(ht->hash_fun, key));
                }
                if (file)
                    fprintf(file, "[%4d] %12lx %16lx %016lx (", j,
                            key, val, (unsigned long int)h);
            }
            // show the chain
            unsigned cell = ivdata[h & ivmask];
            while (cell) {
                if (file) fprintf(file, "%d", cell);
                lispobj matchp = funcall2(ht->test_fun, key, data[cell*2]);
                if (matchp != NIL) { if (file) fprintf(file, "\u2713"); break; }
                if ((cell = nvdata[cell]) != 0 && file) putc(' ', file);
            }
            if (!cell) ++errors;
            if (file) fprintf(file, cell ? ")\n" : ") *\n");
        }
    }
#endif
    return errors;
}

static int hashtable_cmd(char **ptr, iochannel_t io)
{
    lispobj obj;
    if (parse_lispobj(ptr, &obj)) {
        int errors = verify_lisp_hashtable((void*)native_pointer(obj),
                                           io->out);
        if (errors) fprintf(io->out, "Errors: %d\n", errors);
    }
    return 0;
}

static int pte_cmd(char **ptr, iochannel_t io)
{
    extern void gc_show_pte(lispobj, FILE*);
    lispobj obj;
    if (parse_lispobj(ptr, &obj)) gc_show_pte(obj, io->out);
    return 0;
}

static int regs_cmd(char __attribute__((unused)) **ptr, iochannel_t io)
{
    struct thread __attribute__((unused)) *thread = get_sb_vm_thread();

    fprintf(io->out, "CSP\t=\t%p   ", access_control_stack_pointer(thread));
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
    fprintf(io->out, "CFP\t=\t%p   ", access_control_frame_pointer(thread));
#endif

#ifdef reg_BSP
    fprintf(io->out, "BSP\t=\t%p\n", get_binding_stack_pointer(thread));
#else
    /* printf("BSP\t=\t%p\n", (void*)SymbolValue(BINDING_STACK_POINTER)); */
    fprintf(io->out, "\n");
#endif

#ifdef LISP_FEATURE_GENERATIONAL
    /* printf("DYNAMIC\t=\t%p\n", (void*)DYNAMIC_SPACE_START); */
#else
    fprintf(io->out, "STATIC\t=\t%p   ", static_space_free_pointer);
    fprintf(io->out, "RDONLY\t=\t%p   ", read_only_space_free_pointer);
    fprintf(io->out, "DYNAMIC\t=\t%p\n", (void*)current_dynamic_space);
#endif
    return 0;
}

static int call_cmd(char **ptr, iochannel_t io)
{
    lispobj thing;
    parse_lispobj(ptr, &thing);
    lispobj function, args[3];
    lispobj result = NIL;

    int numargs;

    if (lowtag_of(thing) == OTHER_POINTER_LOWTAG) {
        lispobj *obj = native_pointer(thing);
        switch (widetag_of(obj)) {
          case SYMBOL_WIDETAG:
              function = symbol_function((struct symbol*)obj);
              if (function == NIL) {
                  fprintf(io->out, "Symbol 0x%08lx is undefined.\n", (long unsigned)thing);
                  return 0;
              }
              break;
          case FDEFN_WIDETAG:
              function = FDEFN(thing)->fun;
              if (function == NIL) {
                  fprintf(io->out, "Fdefn 0x%08lx is undefined.\n", (long unsigned)thing);
                  return 0;
              }
              break;
          default:
              fprintf(io->out, "0x%08lx is not a function pointer, symbol, "
                     "or fdefn object.\n",
                     (long unsigned)thing);
              return 0;
        }
    }
    else if (lowtag_of(thing) != FUN_POINTER_LOWTAG) {
        fprintf(io->out, "0x%08lx is not a function pointer, symbol, or fdefn object.\n",
               (long unsigned)thing);
        return 0;
    }
    else
        function = thing;

    numargs = 0;
    while (more_p(ptr)) {
        if (numargs >= 3) {
            fprintf(io->out, "too many arguments (no more than 3 supported)\n");
            return 0;
        }
        parse_lispobj(ptr, &args[numargs++]);
    }

    switch (numargs) {
      case 0:
          result = funcall0(function);
          break;
      case 1:
          result = funcall1(function, args[0]);
          break;
      case 2:
          result = funcall2(function, args[0], args[1]);
          break;
      case 3:
          result = funcall3(function, args[0], args[1], args[2]);
          break;
      default:
          lose("unsupported arg count made it past validity check?!");
    }

    print_to_iochan(result, io);
    return 0;
}

static int
flush_cmd(char __attribute__((unused)) **ptr, __attribute__((unused)) iochannel_t io)
{
    flush_vars();
    return 0;
}

static int quit_cmd(char __attribute__((unused)) **ptr, iochannel_t io)
{
    char buf[10];

    fprintf(io->out, "Really quit? [y] ");
    fflush(io->out);
    if (fgets(buf, sizeof(buf), io->in)) {
        if (buf[0] == 'y' || buf[0] == 'Y' || buf[0] == '\n')
            exit(1);
    } else {
        fprintf(io->out, "\nUnable to read response, assuming y.\n");
        exit(1);
    }
    return 0;
}

static int help_cmd(char __attribute__((unused)) **ptr, iochannel_t io)
{
    struct cmd *cmd;

    for (cmd = supported_cmds; cmd->cmd != NULL; cmd++)
        if (cmd->help != NULL)
            fprintf(io->out, "%s\t%s\n", cmd->cmd, cmd->help);
    return 0;
}

static int
exit_cmd(char __attribute__((unused)) **ptr, __attribute__((unused)) iochannel_t io)
{
    return 1; // 'done' flag
}

static void print_context(os_context_t *context, iochannel_t io)
{
    int i;

    for (i = 0; i < NREGS; i++) {
        fprintf(io->out, "%s:\t", lisp_register_names[i]);
        brief_print((lispobj)(*os_context_register_addr(context,i)), io);

    }
#if defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_PPC)
    fprintf(io->out, "DAR:\t\t 0x%08lx\n", (unsigned long)(*os_context_register_addr(context, 41)));
    fprintf(io->out, "DSISR:\t\t 0x%08lx\n", (unsigned long)(*os_context_register_addr(context, 42)));
#endif
#ifndef REG_PC
    fprintf(io->out, "PC:\t\t  0x%08lx\n", (unsigned long)os_context_pc(context));
#endif
}

static int print_context_cmd(char **ptr, iochannel_t io)
{
    int free_ici;
    struct thread *thread = get_sb_vm_thread();

    free_ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,thread));

    FILE* f = io->out;
    if (more_p(ptr)) {
        int index;

        if (!parse_number(ptr, &index)) return 0;

        if ((index >= 0) && (index < free_ici)) {
            fprintf(f, "There are %d interrupt contexts.\n", free_ici);
            fprintf(f, "printing context %d\n", index);
            print_context(nth_interrupt_context(index, thread), io);
        } else {
            printf("There are %d interrupt contexts.\n", free_ici);
        }
    } else {
        if (free_ici == 0)
            fprintf(f, "There are no interrupt contexts.\n");
        else {
            fprintf(f, "There are %d interrupt contexts.\n", free_ici);
            fprintf(f, "printing context %d\n", free_ici - 1);
            print_context(nth_interrupt_context(free_ici - 1, thread), io);
        }
    }
    return 0;
}

static int backtrace_cmd(char **ptr, iochannel_t io)
{
    int n;

    if (more_p(ptr)) {
        if (!parse_number(ptr, &n)) return 0;
    } else
        n = 100;

    fprintf(io->out, "Backtrace:\n");
    print_lisp_backtrace(n, io->out);
    return 0;
}

/* Usage Example
 * =============
 * ldb> threads
 * (thread*, pthread, sb-thread:thread, name)
 * 0x7f1799000080 0x7f1798dff6c0 0x1000031ac0 "finalizer"
 * 0x7f1799600080 0x7f1799972240 0x1000b60000 "main thread"
 * ldb> btt 0x7f1799000080
 * Lisp thread @ 0x7f1799000080, tid 2355966 ("finalizer")
 *  interrupted @ PC 0x7f1799a2d1cc
 *  0x7f1799a2d1cc [__nptl_death_event]
 *  0x7f1799a2f930 [pthread_cond_wait]
 *  0x55e29ccb1caf [finalizer_thread_wait]
 *  0xb8006b3d11 [(LAMBDA () :IN SB-IMPL::FINALIZER-THREAD-START)]
 *  0xb800725d1b [(FLET SB-UNIX::BODY :IN SB-THREAD::RUN)]
 *  0xb800726474 [(FLET "WITHOUT-INTERRUPTS-BODY-" :IN SB-THREAD::RUN)]
 *  0xb8007258cb [(FLET SB-UNIX::BODY :IN SB-THREAD::RUN)]
 *  0xb80072663c [(FLET "WITHOUT-INTERRUPTS-BODY-" :IN SB-THREAD::RUN)]
 *  0xb8007256a9 [SB-THREAD::RUN]
 *  0x55e29cce6261 [call_into_lisp_]
 *  0x55e29ccaac1a [funcall1]
 *  0x55e29cccb978 [new_thread_trampoline]
 *  0x7f1799a306c2 [pthread_condattr_setpshared]
 *  0x7f1799aab128 [__clone]
 */
static int threadbt_cmd(char **ptr, iochannel_t io)
{
    char *addr = 0;
    __attribute__((unused)) int all = 0;
    if (!strncmp(*ptr, "all", 3)) all = 1;
    else if (!parse_addr(ptr, 1, &addr, io->out)) return 0;
#ifdef LISP_FEATURE_BACKTRACE_ON_SIGNAL
    extern void libunwind_backtrace(struct thread*, FILE*);
    struct thread* th;
    for_each_thread(th) {
        if (all || (char*)th == addr) {
            libunwind_backtrace(th, io->out);
            if (!all) return 0;
        }
    }
    if (!all) fprintf(io->out, "%p is not a thread\n", addr);
#else
    fprintf(io->out, "Unsupported\n");
#endif
    return 0;
}

static int search_cmd(char **ptr, iochannel_t io)
{
    char *addr;
    if (!parse_addr(ptr, 1, &addr, io->out)) return 0;
    lispobj *obj = search_all_gc_spaces((void*)addr);
    if(obj)
        fprintf(io->out, "#x%"OBJ_FMTX"\n", compute_lispobj(obj));
    else
        fprintf(io->out, "Not found\n");
    return 0;
}

static int catchers_cmd(char __attribute__((unused)) **ptr, iochannel_t io)
{
    struct catch_block *catch = (struct catch_block *)
        read_TLS(CURRENT_CATCH_BLOCK, get_sb_vm_thread());

    if (catch == NULL)
        fprintf(io->out, "There are no active catchers!\n");
    else {
        while (catch != NULL) {
            fprintf(io->out, "%p:\n\tuwp  : %p\n\tfp   : %p\n\t"
                   "code : %p\n\tentry: %p\n\ttag: ",
                   catch,
                   catch->uwp,
                   catch->cfp,
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || defined(LISP_FEATURE_ARM64)
                   component_ptr_from_pc((void*)catch->entry_pc),
#else
                   (void*)catch->code,
#endif
                   (void*)(catch->entry_pc));
            brief_print((lispobj)catch->tag, io);
            catch = catch->previous_catch;
        }
    }
    return 0;
}

struct layout_collection {
    struct cons* list;
    int passno;
    FILE* ostream;
};
static int count_layout_occurs(lispobj x, struct cons* list)
{
    int ct = 0;
    for ( ; list ; list = (void*)list->cdr ) if (list->car == x) ++ct;
    return ct;
}

static uword_t display_layouts(lispobj* where, lispobj* limit, void* arg)
{
    extern struct vector * classoid_name(lispobj * classoid);
    struct layout_collection *lc = arg;
    where = next_object(where, 0, limit); /* find first marked object */
    for ( ; where ; where = next_object(where, object_size(where), limit) ) {
        if (widetag_of(where) == INSTANCE_WIDETAG &&
            instance_layout(where) != 0 &&
            layout_depth2_id(LAYOUT(instance_layout(where))) == LAYOUT_LAYOUT_ID) {
            struct layout* l = (void*)where;
            struct classoid* c = (void*)native_pointer(l->classoid);
            if (lc->passno == 1) {
                // on the first pass, just collect the classoids;
                struct cons* cons = malloc(sizeof (struct cons));
                cons->car = (lispobj)c;
                cons->cdr = (lispobj)lc->list;
                lc->list = cons;
            } else {
                // print some information preceded by a '*' if more than one
                // layout points to the classoid of this layout
                int count = count_layout_occurs((lispobj)c, lc->list);
                struct vector* v = classoid_name((lispobj*)c);
                char* name =
                  header_widetag(v->header)==SIMPLE_BASE_STRING_WIDETAG ? (char*)v->data : "?";
                fprintf(lc->ostream, "%c %p %16" OBJ_FMTX " %16" OBJ_FMTX " %p %" OBJ_FMTX " %s\n",
                        count>1 ? '*' : ' ', l, l->clos_hash, l->uw_id_word0,
                        c, l->invalid, name);

            }
        }
    }
   return 0;
}

static int layouts_cmd(char __attribute__((unused)) **ptr, iochannel_t io)
{
    fprintf(io->out, "Dup, Layout, Hash, ID_Word, Classoid, Invalid, Name\n");
    struct layout_collection lc;
    lc.list = 0;
    lc.ostream = io->out;
    for (lc.passno = 1; lc.passno <= 2; ++lc.passno) {
        walk_generation(display_layouts, -1, &lc);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        display_layouts((lispobj*)FIXEDOBJ_SPACE_START, fixedobj_free_pointer, &lc);
#endif
    }
    struct cons* l = lc.list;
    while (l) {
        struct cons* next = (struct cons*)l->cdr;
        free(l);
        l = next;
    }
    return 0;
}

static int monitor_loop(struct iochannel);
extern FILE *gc_activitylog_file;
void
ldb_monitor(void)
{
    static struct iochannel io = {0,0};

    printf("Welcome to LDB, a low-level debugger for the Lisp runtime environment.\n");
    if (gc_active_p) printf("(GC in progress, oldspace=%d, newspace=%d)\n",
                            from_space, new_space);
    if (gc_activitylog_file) fflush(gc_activitylog_file);
    if (!io.out) {
        io.out = stderr;
        io.in = stdin;
#ifndef LISP_FEATURE_WIN32
        FILE* tty = fopen("/dev/tty","r+");
        if (tty) io.out = io.in = tty; else perror("Error opening /dev/tty");
#endif
    }

    if (!monitor_loop(io)) exit(1);
}
static int monitor_loop(struct iochannel io)
{
    struct cmd *cmd, *found;
    char buf[256];
    char *line, *ptr, *token;
    int ambig;

    while (1) {
        fprintf(io.out, "ldb> ");
        fflush(io.out);
        line = fgets(buf, sizeof(buf), io.in);
        if (line == NULL) {
            return 0;
        }
        ptr = line;
        if ((token = parse_token(&ptr)) == NULL)
            continue;
        ambig = 0;
        found = NULL;
        for (cmd = supported_cmds; cmd->cmd != NULL; cmd++) {
            if (strcmp(token, cmd->cmd) == 0) {
                found = cmd;
                ambig = 0;
                break;
            }
            else if (strncmp(token, cmd->cmd, strlen(token)) == 0) {
                if (found)
                    ambig = 1;
                else
                    found = cmd;
            }
        }
        if (ambig)
            fprintf(io.out, "``%s'' is ambiguous.\n", token);
        else if (found == NULL)
            fprintf(io.out, "unknown command: ``%s''\n", token);
        else {
            reset_printer();
            int done = (*found->fn)(&ptr, &io);
            if (done) return 1;
        }
    }
}

#ifdef START_LDB_SERVICE_THREAD
#include <sys/socket.h>
#include <netinet/in.h>
static int listener;
int ldb_service_port;
pthread_t ldb_service_thread;
static void* ldb_service_main(__attribute__((unused)) void* arg) {
    int peer;
    FILE* stream = 0;
    struct sockaddr_in sin;
    socklen_t addrlen = sizeof sin;
    while (1) {
        peer = accept(listener, (struct sockaddr*)&sin, &addrlen);
        if (peer < 0) {
          fprintf(stderr, "ldb: accept() failed\n");
          return 0;
        }
        stream = fdopen(peer, "r+");
        setlinebuf(stream);
        fprintf(stream, "LDB connected\n");
        struct iochannel io = {stream, stream};
        monitor_loop(io);
        fclose(stream);
    }
    return 0;
}
void init_ldb_service()
{
    struct sockaddr_in sin;
    memset(&sin, 0, sizeof sin);
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    listener = socket(AF_INET, SOCK_STREAM, 0);
    if (bind(listener, (struct sockaddr*)&sin, sizeof sin)) {perror("bind");exit(1);}
    socklen_t addrlen = sizeof sin;
    if (listen(listener, 1)) {perror("listen");exit(1);}
    pthread_attr_t thr_attr;
    pthread_attr_init(&thr_attr);
    pthread_attr_setdetachstate(&thr_attr, PTHREAD_CREATE_DETACHED);
    sigset_t mask, oldmask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGCHLD);
    pthread_sigmask(SIG_BLOCK, &mask, &oldmask);
    pthread_create(&ldb_service_thread, &thr_attr, ldb_service_main, 0);
    getsockname(listener, (struct sockaddr*)&sin, &addrlen);
    ldb_service_port = ntohs(sin.sin_port);
    fprintf(stderr, "NOTE: ldb service on port %d\n", ldb_service_port);
    pthread_attr_destroy(&thr_attr);
    pthread_sigmask(SIG_SETMASK, &oldmask, 0);
}
#endif

#ifdef STANDALONE_LDB
void gc_stop_the_world() { } // do nothing
void gc_start_the_world() { } // do nothing
#include <errno.h>
#include <setjmp.h>
#include "core.h"
struct lisp_startup_options lisp_startup_options;

static size_t checked_read(char *section, int fd, void* buf, size_t n)
{
    char label[8];
    if (read(fd, label, sizeof label) != 8 || strncmp(label, section, 8))
        lose("section messup: %.8s when expecting %s\n", label, section);
    if (n) {
        size_t result = read(fd, buf, n);
        if (result != n) { lose("read failed, errno=%d", errno); }
        fprintf(stderr, "%s: %zx bytes\n", section, n + sizeof label);
        maybe_show_contents(section, buf, n);
    }
    return n + sizeof label;
}

char *pagetypedesc(int type)
{
    static char what[4];
    switch (type) {
    case PAGE_TYPE_CODE: return "code";
    case PAGE_TYPE_BOXED: return "boxed";
    case PAGE_TYPE_UNBOXED: return "raw";
    case PAGE_TYPE_MIXED: return "mixed";
    default: snprintf(what, 4, "%d", type); return what;
    }
}

extern void gc_allocate_ptes();
extern void recompute_gen_bytes_allocated();
extern void print_generation_stats();
extern struct thread *alloc_thread_struct(void*);

/* For the time being at least, the standalone monitor requires that all spaces in
 * a dump file map exactly as requested. I'd prefer if that were not so, but I don't
 * see a way to perform the heap verification until fixing up the core as
 * it actually mapped. It could be impossible if the heap was messed up enough to
 * cause a crash in the first place */
#define LDB_SPACE_MOVABILITY 0

int load_gc_crashdump(char* pathname)
{
    int fd;
    os_context_t *contexts[10], *context;
    struct thread* threads = 0;
    fd = open(pathname, O_RDONLY);
    if (fd < 0) {
        fprintf(stderr, "can't open %s\n", pathname);
        exit(1);
    }
    struct crash_preamble preamble;
    struct crash_thread_preamble thread_preamble;
    checked_read("preamble", fd, &preamble, sizeof preamble);
    printf("static=%"OBJ_FMTX" nbytes=%x\n", preamble.static_start, (int)preamble.static_nbytes);
    printf("heap_start=%"OBJ_FMTX" npages=%d\n", preamble.dynspace_start,
           (int)preamble.dynspace_npages_used);
    // pin_dynspace_code is for display only. It gets recomputed as the
    // logical OR of all threads' values of *GC-PIN-CODE-PAGES*.
    printf("sprof_enabled=%d pin_dynspace_code=%d packages=%p\n",
           preamble.sprof_enabled, preamble.pin_dynspace_code,
           (void*)preamble.lisp_package_vector);
    lisp_package_vector = preamble.lisp_package_vector;
    sb_sprof_enabled = preamble.sprof_enabled;
    if (preamble.signature != CRASH_PREAMBLE_SIGNATURE)
        lose("Can't load crashdump: bad header (have %"OBJ_FMTX", expect %"OBJ_FMTX")",
             preamble.signature, (uword_t)CRASH_PREAMBLE_SIGNATURE);
    if (preamble.card_size != GENCGC_CARD_BYTES)
        lose("Can't load crashdump: memory parameters differ");
    gc_card_table_nbits = preamble.card_table_nbits;
    gc_card_table_mask = (1<<gc_card_table_nbits)-1;
#ifdef LISP_FEATURE_LINKAGE_SPACE
    linkage_space = (lispobj*)os_alloc_gc_space(0, 0, (char*)preamble.linkage_start,
                                                preamble.linkage_nbytes);
    checked_read("linkage", fd, (char*)linkage_space, preamble.linkage_nbytes);
#endif
    // static + readonly
#ifdef LISP_FEATURE_RELOCATABLE_STATIC_SPACE
    STATIC_SPACE_START =
        (uword_t)os_alloc_gc_space(STATIC_CORE_SPACE_ID, 0, (char*)preamble.static_start,
                                   STATIC_SPACE_SIZE + (1+gc_card_table_mask));
    printf("static: wanted %lx got %lx\n", preamble.static_start, STATIC_SPACE_START);
#endif
    checked_read("static", fd, (char*)STATIC_SPACE_START, preamble.static_nbytes);
    static_space_free_pointer =
        (lispobj*)(((char*)preamble.static_freeptr - (char*)preamble.static_start)
                   + (char*)STATIC_SPACE_START);
    if (!preamble.readonly_nbytes) {
        checked_read("R/O", fd, 0, 0);
    } else {
        void* actual =
            os_alloc_gc_space(READ_ONLY_CORE_SPACE_ID, LDB_SPACE_MOVABILITY,
                              (char*)preamble.readonly_start,
                              ALIGN_UP(preamble.readonly_nbytes, 4096));
        if (actual != (void*)preamble.readonly_start)
            fprintf(stderr, "WARNING: wanted R/O space @ %p but got %p\n",
                    (char*)preamble.readonly_start, actual);
        checked_read("R/O", fd, (char*)actual, preamble.readonly_nbytes);
#ifndef READ_ONLY_SPACE_START /* if non-constant */
        READ_ONLY_SPACE_START = (uword_t)actual;
        READ_ONLY_SPACE_END = READ_ONLY_SPACE_START + preamble.readonly_nbytes;
        read_only_space_free_pointer = (lispobj*)READ_ONLY_SPACE_END;
#endif
    }
    if (!preamble.permgen_nbytes) {
        checked_read("perm", fd, 0, 0);
    } else {
        void* actual =
            os_alloc_gc_space(PERMGEN_CORE_SPACE_ID, 0, (char*)preamble.permgen_start,
                              ALIGN_UP(preamble.permgen_nbytes, 4096));
        if (actual != (void*)preamble.permgen_start)
            lose("Couldn't map permgen as required (%p)", (char*)preamble.permgen_start);
        checked_read("perm", fd, (char*)preamble.permgen_start, preamble.permgen_nbytes);
        PERMGEN_SPACE_START = preamble.permgen_start;
        permgen_space_free_pointer = (lispobj*)(preamble.permgen_start + preamble.permgen_nbytes);
    }
    //
    page_table_pages = preamble.dynspace_npages_total;
    gc_allocate_ptes();
    dynamic_space_size = preamble.dynspace_npages_total * GENCGC_PAGE_BYTES;
    next_free_page = preamble.dynspace_npages_used;
    DYNAMIC_SPACE_START = preamble.dynspace_start;
    long dynspace_nbytes = preamble.dynspace_npages_used * GENCGC_PAGE_BYTES;
    char* dynspace = os_alloc_gc_space(DYNAMIC_CORE_SPACE_ID, LDB_SPACE_MOVABILITY,
                                       (char*)preamble.dynspace_start,
                                       DEFAULT_DYNAMIC_SPACE_SIZE);
    if (dynspace != (char*)preamble.dynspace_start
        && LDB_SPACE_MOVABILITY == 0) {
        lose("Didn't map dynamic space where expected: %p vs %p",
             dynspace, (char*)preamble.dynspace_start);
    }
    DYNAMIC_SPACE_START = (uword_t)dynspace;
    checked_read("dynamic", fd, dynspace, dynspace_nbytes);
    fprintf(stderr, "snapshot: %"PAGE_INDEX_FMT" pages in use (%ld bytes)\n",
            next_free_page, dynspace_nbytes);
    checked_read("PTE", fd, page_table, sizeof (struct page) * next_free_page);
    checked_read("cardmark", fd, gc_card_mark, 1+gc_card_table_mask);
    // mrgc_init needs to know the dynamic space size so that's why this is delayed
    // until after reading the crash preamble.
    gc_init();
#ifdef LISP_FEATURE_MARK_REGION_GC
    checked_read("allocated", fd, allocation_bitmap, bitmap_size(next_free_page));
    extern line_index_t line_count;
    checked_read("linemap", fd, line_bytemap, line_count);
#endif
    recompute_gen_bytes_allocated();
    print_generation_stats();
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    extern void gc_init_immobile(),
                calc_immobile_space_bounds(),
                write_protect_immobile_space();
    gc_assert(preamble.fixedobj_size == FIXEDOBJ_SPACE_SIZE);
    gc_assert(preamble.text_size == text_space_size);
    FIXEDOBJ_SPACE_START = preamble.fixedobj_start;
    TEXT_SPACE_START = preamble.text_start;
    fixedobj_free_pointer = (lispobj*)preamble.fixedobj_free_pointer;
    text_space_highwatermark = (lispobj*)preamble.text_space_highwatermark;
    os_alloc_gc_space(IMMOBILE_FIXEDOBJ_CORE_SPACE_ID, 0, (char*)FIXEDOBJ_SPACE_START,
                      FIXEDOBJ_SPACE_SIZE);
    os_alloc_gc_space(IMMOBILE_TEXT_CORE_SPACE_ID, 0, (char*)TEXT_SPACE_START,
                      text_space_size);
    gc_init_immobile(); // allocate the page tables
    calc_immobile_space_bounds();
    // Read fixedobj space
    int usage = (uword_t)fixedobj_free_pointer - FIXEDOBJ_SPACE_START;
    checked_read("fixedobj", fd, (char*)FIXEDOBJ_SPACE_START, usage);
    // Always read the whole page table regardless of the current space usage
    int total_npages = FIXEDOBJ_SPACE_SIZE / IMMOBILE_CARD_BYTES;
    checked_read("fixedobj_PTE", fd, fixedobj_pages, total_npages * sizeof sizeof(struct fixedobj_page));
    // Read text space
    usage = (uword_t)text_space_highwatermark - TEXT_SPACE_START;
    char *tlsf_memory_end = (char*)TEXT_SPACE_START + text_space_size;
    tlsf_mem_start = preamble.tlsf_mem_start;
    fprintf(stderr, "tlsf_mem_start=%p\n", tlsf_mem_start);
    int tlsf_memory_size = tlsf_memory_end - (char*)tlsf_mem_start;
    checked_read("text", fd, (char*)TEXT_SPACE_START, usage+3*N_WORD_BYTES);
    memcpy(tlsf_memory_end-3*N_WORD_BYTES, preamble.sentinel_block, 3*N_WORD_BYTES);
    total_npages = text_space_size / IMMOBILE_CARD_BYTES;
    int n_bitmap_elts = ALIGN_UP(total_npages, 32) / 32;
    checked_read("text_gen", fd, text_page_genmask, total_npages); // 1 byte per page
    checked_read("text_WP", fd, text_page_touched_bits, n_bitmap_elts * sizeof (int));
    tlsf_control = preamble.tlsf_control_address; // already mapped at a fixed address
    // TLSF control was mapped in gc_init_immobile()
    checked_read("TLSF_control", fd, tlsf_control, preamble.tlsf_control_size);
    int n_tlsf_pages = tlsf_memory_size / IMMOBILE_CARD_BYTES;
    fprintf(stderr, "%d TLSF pages\n", n_tlsf_pages);
    tlsf_page_sso = malloc(n_tlsf_pages * sizeof (short));
    checked_read("TLSF_sso", fd, tlsf_page_sso, n_tlsf_pages * sizeof (short));
    write_protect_immobile_space();
#endif
    fprintf(stderr, "%d threads:\n", (int)preamble.nthreads);
    int i;
    for(i=0; i<(int)preamble.nthreads; ++i) {
        struct thread* th = alloc_thread_struct(0);
        // Push it on the front
        th->prev = 0;
        th->next = threads;
        if (threads) threads->prev = th;
        threads = th;
        checked_read("thread", fd, &thread_preamble, sizeof thread_preamble);
        uword_t* stackptr = (uword_t*)((char*)th->control_stack_end - thread_preamble.control_stack_nbytes);
        context = contexts[i] = malloc(preamble.sizeof_context);
        nth_interrupt_context(0, th) = context;
        if (thread_preamble.has_context) {
            checked_read(" ctxt", fd, context, preamble.sizeof_context);
        }
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        *os_context_sp_addr(context) = (uword_t)stackptr;
#ifdef LISP_FEATURE_X86
        *os_context_register_addr(context,reg_SP) = (os_context_register_t)stackptr;
#endif
        gc_assert(*os_context_register_addr(context,reg_SP) == (os_context_register_t)stackptr);
#elif defined reg_CSP
        *os_context_register_addr(context, reg_CSP) = (uword_t)stackptr;
#endif
        checked_read(" stack", fd, stackptr, thread_preamble.control_stack_nbytes);
        checked_read(" bindings", fd, th->binding_stack_start, thread_preamble.binding_stack_nbytes);
        {
        // Capture the 'struct thread' portion of TLS so that things like
        // binding stack pointer, and prev,next links stay as they are,
        // after copying them back. This way the visual display of number of bytes
        // read comes out correctly
        struct thread preserve;
        memcpy(&preserve, th, sizeof *th);
        checked_read(" TLS", fd, th, preamble.tls_size);
        // Copy _into_ preserve the values of the allocation regions so that when
        // copied back into 'th' we have access to the regions as they were at dump.
        preserve.boxed_tlab = th->boxed_tlab;
        preserve.cons_tlab = th->cons_tlab;
        preserve.mixed_tlab = th->mixed_tlab;
        preserve.symbol_tlab = th->symbol_tlab; // not used yet
        preserve.sys_mixed_tlab = th->sys_mixed_tlab;
        preserve.sys_cons_tlab = th->sys_cons_tlab;
        memcpy(th, &preserve, sizeof *th - N_WORD_BYTES);
        }
        write_TLS(FREE_INTERRUPT_CONTEXT_INDEX, make_fixnum(1), th);
        char* cname = "(no lisp thread)";
        if (th->lisp_thread) {
            struct thread_instance* instance = (void*)(th->lisp_thread - INSTANCE_POINTER_LOWTAG);
            lispobj name = instance->_name;
            cname = gc_managed_addr_p(name) && simple_base_string_p(name)
                    ? vector_sap(name) : 0;
        }
        fprintf(stderr, "thread @ %p originally %p, %d bind_stk words, %d val_stk words '%s'\n",
                th, (void*)thread_preamble.address,
                (int)(thread_preamble.binding_stack_nbytes>>WORD_SHIFT),
                (int)(thread_preamble.control_stack_nbytes>>WORD_SHIFT),
                cname);
        // Scan thread stack looking for words which could be valid pointers,
        // but don't find an object when the heap is scanned.
        // Realizing that failure to find isn't necessarily an error,
        // there's nothing that we can do except show some information.
        int nwords = thread_preamble.control_stack_nbytes>>WORD_SHIFT, wordindex;
        int n_definitely_valid = 0, n_dangling = 0;
        for (wordindex = 0; wordindex < nwords; ++wordindex) {
            lispobj word = stackptr[wordindex];
            if (DYNAMIC_SPACE_START <= word && word < DYNAMIC_SPACE_START + dynamic_space_size
                && (is_lisp_pointer(word) ||
                    is_code(page_table[find_page_index((void*)word)].type))) {
                lispobj* found = search_dynamic_space((void*)word);
                if (found) {
                    __attribute__((unused)) page_index_t ind = find_page_index(found);
#if 0
                    fprintf(stderr, "   sp[%5d] = %"OBJ_FMTX" -> %p (g%d,%s)\n",
                            wordindex, word, found,
                            page_table[ind].gen, pagetypedesc(page_table[ind].type));
#endif
                    ++n_definitely_valid;
                } else {
                    fprintf(stderr, " ! sp[%5d] = %"OBJ_FMTX" (not found)\n",
                            wordindex, word);
                    ++n_dangling;
                }
            }
        }
        fprintf(stderr, "%d valid pointers", n_definitely_valid);
        if (n_dangling) fprintf(stderr, " (%d dangling)", n_dangling);
        putc('\n', stderr);
    }
    char signature[8];
    checked_read("sig", fd, signature, 8);
    gc_assert(!strncmp(signature, "SB.Crash", 8));
    gc_assert(read(fd, signature, 1) == 0);
    close(fd);
    all_threads = threads;
#if defined(LISP_FEATURE_GCC_TLS) && defined(LISP_FEATURE_SB_THREAD)
    current_thread = all_threads;
#endif
    return 0;
}

char *sbcl_runtime;
jmp_buf ldb_toplevel;
int main(int argc, char *argv[], char **envp)
{
    extern void calc_asm_routine_bounds();
    if (argc != 2) {
        fprintf(stderr, "Usage: ldb crashdump\n");
        return 1;
    }
    extern void sb_query_os_page_size();
    sb_query_os_page_size();
    bool have_hardwired_spaces = os_preinit(argv, envp);
    // Unlike in ordinary startup where we might try to call personality()
    // to disable ASLR, this can't proceed if the preinit fails.
    if (!have_hardwired_spaces) lose("failed to preinit");
    load_gc_crashdump(argv[1]);
    calc_asm_routine_bounds();
    gencgc_verbose = 1;
    if (setjmp(ldb_toplevel) != 0)
      fprintf(stderr, "Back in ldb, hope everything's Ok\n");
    ldb_monitor();
}
#endif
