
#include "sbcl.h"
#include "arch.h"
#include "runtime.h"
#include "lispregs.h"
#include "gc-internal.h"
#include "gc-private.h"
#include "code.h"
#include "genesis/closure.h"
#include "genesis/cons.h"
#include "genesis/constants.h"
#include "genesis/gc-tables.h"
#include "genesis/hash-table.h"
#include "genesis/instance.h"
#include "genesis/layout.h"
#include "genesis/package.h"
#include "genesis/vector.h"
#include "pseudo-atomic.h" // for get_alloc_pointer()
#include "search.h"
#include "genesis/avlnode.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#ifndef LISP_FEATURE_WIN32
#define HAVE_GETRUSAGE 1
#endif
#if HAVE_GETRUSAGE
#include <sys/resource.h> // for getrusage()
#endif

int heap_trace_verbose = 0;

extern generation_index_t gencgc_oldest_gen_to_gc;

/// Each "layer" is a set of objects reachable by tracing one reverse pointer
/// from any object in the previously built layer.
/// An object will not appear in more than one layer.
struct layer {
    struct __attribute((packed)) node {
        lispobj object; // With lowtag
        // Which 0-relative word in this object points to any object
        // in the next layer (closer to the intended target object).
        int wordindex;
    } *nodes;
    struct layer* next;
    int count;
};

/// memory in which to build the object lists comprising the
/// values in 'inverted_heap', the hashtable which maps each object
/// to a list of objects pointing to it.
struct scratchpad {
    char* base, *free, *end;
};

struct scan_state {
    long n_objects;
    long n_scanned_words;
    long n_immediates;
    long n_pointers;
    int record_ptrs;
    // A hashtable mapping each object to a list of objects pointing to it
    struct hopscotch_table inverted_heap;
    struct scratchpad scratchpad;
};

static int traceroot_gen_of(lispobj obj) {
    return gc_gen_of(obj, -1);
}

static const char* classify_obj(lispobj ptr)
{
    extern lispobj* instance_classoid_name(lispobj*);

    lispobj* name; // a Lisp string
    switch(lowtag_of(ptr)) {
    case INSTANCE_POINTER_LOWTAG:
        name = instance_classoid_name(native_pointer(ptr));
        if (widetag_of(name) == SIMPLE_BASE_STRING_WIDETAG)
          return (char*)(name + 2);
    case LIST_POINTER_LOWTAG:
        return "cons";
    case FUN_POINTER_LOWTAG:
    case OTHER_POINTER_LOWTAG:
        return widetag_names[widetag_of(native_pointer(ptr))>>2];
    }
    static char buf[8];
    sprintf(buf, "#x%x", widetag_of(native_pointer(ptr)));
    return buf;
}

static void add_to_layer(lispobj* obj, int wordindex,
                         struct layer* layer, int* capacity)
{
    // Resurrect the containing object's lowtag
    lispobj ptr = compute_lispobj(obj);
    int staticp = ptr <= STATIC_SPACE_END;
    int gen = staticp ? -1 : traceroot_gen_of(ptr);
    if (heap_trace_verbose>2)
      // Show the containing object, its type and generation, and pointee
        fprintf(stderr,
                "  add_to_layer(%p,%d) = %s,g%c -> %p\n",
                (void*)ptr, wordindex, classify_obj(ptr), (staticp ? 'S' : '0'+gen),
                (void*)obj[wordindex]);
    int count = layer->count;
    if (count >= *capacity) {
        *capacity = *capacity ? 2 * *capacity : 4;
        layer->nodes = realloc(layer->nodes, *capacity * sizeof (struct node));
    }
    layer->nodes[count].object = ptr;
    layer->nodes[count].wordindex = wordindex;
    ++layer->count;
}

/// If 'obj' is a simple-fun, return its code component,
/// otherwise return obj directly.
static lispobj canonical_obj(lispobj obj)
{
    if (functionp(obj) && widetag_of((lispobj*)(obj-FUN_POINTER_LOWTAG)) == SIMPLE_FUN_WIDETAG)
        return make_lispobj(fun_code_header(obj-FUN_POINTER_LOWTAG),
                            OTHER_POINTER_LOWTAG);
    return obj;
}

/* Return the word index of the pointer in 'source' which references 'target'.
 * Return -1 on failure. (This is an error if it happens)
 */
#define check_ptr(index,ptr) if(canonical_obj(ptr)==target) return index;
static int find_ref(lispobj* source, lispobj target)
{
    lispobj layout, bitmap;
    int scan_limit, i, j;

    lispobj header = *source;
    if (is_cons_half(header)) {
        check_ptr(0, header);
        check_ptr(1, source[1]);
        return -1;
    }
    int widetag = widetag_of(source);
    scan_limit = sizetab[widetag](source);
    switch (widetag) {
    case INSTANCE_WIDETAG:
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER
    case FUNCALLABLE_INSTANCE_WIDETAG:
#endif
        // mixed boxed/unboxed objects
        // Unlike in scav_instance where the slot loop is unswitched for
        // speed into three cases (no raw slots, fixnum bitmap, bignum bitmap),
        // here we just go for clarity by abstracting out logbitp.
        layout = instance_layout(source);
        check_ptr(0, layout);
        bitmap = layout ? LAYOUT(layout)->bitmap : make_fixnum(-1);
        for(i=1; i<scan_limit; ++i)
            if (layout_bitmap_logbitp(i-1, bitmap)) check_ptr(i, source[i]);
        // FIXME: check CUSTOM_GC_SCAVENGE in the header
        return -1;
#if FUN_SELF_FIXNUM_TAGGED
    case CLOSURE_WIDETAG:
        check_ptr(1, ((struct closure*)source)->fun - FUN_RAW_ADDR_OFFSET);
        break;
#endif
    case CODE_HEADER_WIDETAG:
        for_each_simple_fun(i, function_ptr, (struct code*)source, 0, {
            int wordindex = &function_ptr->name - source;
            for (j=0; j<4; ++j) check_ptr(wordindex+j, source[wordindex+j]);
        })
        scan_limit = code_header_words((struct code*)source);
        break;
    case FDEFN_WIDETAG:
        check_ptr(3, fdefn_callee_lispobj((struct fdefn*)source));
        scan_limit = 3;
        break;
    }
    for(i=1; i<scan_limit; ++i) check_ptr(i, source[i]);
    return -1;
}
#undef check_ptr

enum ref_kind { HEAP, CONTROL_STACK, BINDING_STACK, TLS };
char *ref_kind_name[4] = {"heap","C stack","bindings","TLS"};

static inline int interestingp(lispobj ptr, struct hopscotch_table* targets)
{
    return is_lisp_pointer(ptr) && hopscotch_containsp(targets, ptr);
}

#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
/* Try to find the call frame that contains 'addr', which is the address
 * in which a conservative root was seen.
 * Return the program counter associated with that frame. */
static char* NO_SANITIZE_MEMORY deduce_thread_pc(struct thread* th, void** addr)
{
    uword_t* fp = __builtin_frame_address(0);
    char* return_pc = 0;

    if (th != arch_os_get_current_thread()) {
        int i = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th));
        os_context_t *c = nth_interrupt_context(i-1, th);
        fp = (uword_t*)*os_context_register_addr(c,reg_FP);
    }
    while (1) {
        if ((uword_t*)addr < fp)
            return return_pc;
        uword_t prev_fp = fp[0];
        if (prev_fp == 0 || (uword_t*)prev_fp < fp || (lispobj*)prev_fp >= th->control_stack_end)
            return 0;
        return_pc = (void*)fp[1];
        fp = (uword_t*)prev_fp;
    }
}

static struct { void* pointer; boolean found; } pin_seek_state;
static void compare_pointer(void *addr) {
    if (addr == pin_seek_state.pointer)
        pin_seek_state.found = 1;
}

/* Figure out which thread's control stack contains 'pointer'
 * and the PC within the active function in the referencing frame  */
static struct thread* NO_SANITIZE_MEMORY
deduce_thread(void (*context_scanner)(), uword_t pointer, char** pc)
{
    struct thread *th;

    *pc = 0;
    pin_seek_state.found = 0;
    for_each_thread(th) {
        void **esp=(void **)-1;
        sword_t i,free;
        if (th == arch_os_get_current_thread())
            esp = (void **)((void *)&pointer);
        else {
            void **esp1;
            free = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th));
            for(i=free-1;i>=0;i--) {
                os_context_t *c = nth_interrupt_context(i, th);
                esp1 = (void **) *os_context_register_addr(c,reg_SP);
                if (esp1>=(void **)th->control_stack_start && esp1<(void **)th->control_stack_end) {
                    if(esp1<esp) esp=esp1;
                    pin_seek_state.pointer = (void*)pointer;
                    context_scanner(compare_pointer, c);
                    pin_seek_state.pointer = 0;
                    if (pin_seek_state.found) return th;
                }
            }
        }
        if (!esp || esp == (void*) -1)
            UNKNOWN_STACK_POINTER_ERROR("deduce_thread", th);
        void** where;
        for (where = ((void **)th->control_stack_end)-1; where >= esp;  where--)
            if ((uword_t)*where == pointer) {
                *pc = deduce_thread_pc(th, where);
                return th;
            }
    }
    return 0;
}
#endif

/* KNOWN BUG: stack reference to pinned large object or immobile object
 * won't be found in pins hashtable */
static lispobj examine_stacks(struct hopscotch_table* targets,
                              void (*context_scanner)(),
                              int n_pins, lispobj* pins,
                              enum ref_kind *root_kind,
                              struct thread** root_thread,
                              char** thread_pc,
                              lispobj *tls_index)
{
    struct thread *th;

    for_each_thread(th) {
        lispobj *where, *end;
#ifdef LISP_FEATURE_SB_THREAD
        // Examine thread-local storage
        *root_kind = TLS;
        where = (lispobj*)(th+1);
        end   = (lispobj*)((char*)th + SymbolValue(FREE_TLS_INDEX,0));
        for( ; where < end ; ++where)
            if (interestingp(*where, targets)) {
                *root_thread = th;
                *tls_index = (char*)where - (char*)th;
                return *where;
            }
#endif
        // Examine the binding stack
        *root_kind = BINDING_STACK;
        where = (lispobj*)th->binding_stack_start;
        end   = (lispobj*)get_binding_stack_pointer(th);
        for( ; where < end ; where += 2)
            if (interestingp(*where, targets)) {
                *root_thread = th;
                *tls_index = where[1];
                return *where;
            }
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        *root_kind = CONTROL_STACK;
        // Examine the control stack
        where = th->control_stack_start;
        end = access_control_stack_pointer(th);
        for ( ; where < end ; ++where)
            if (interestingp(*where, targets)) {
                *root_thread = th;
                *thread_pc = 0;
                return *where;
            }
        // Examine the explicit pin list
        lispobj pin_list = read_TLS(PINNED_OBJECTS,th);
        while (pin_list != NIL) {
            uword_t pin = CONS(pin_list)->car;
            if (interestingp(pin, targets)) {
                *root_thread = th;
                *thread_pc = 0;
                return *where;
            }
            pin_list = CONS(pin_list)->cdr;
        }
#endif
    }
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    // Look in the control stacks
    *root_kind = CONTROL_STACK;
    uword_t pin;
    int i;
    for (i=n_pins-1; i>=0; --i)
        // Bypass interestingp() to avoid one test - pins are known pointers.
        if (hopscotch_containsp(targets, pin = pins[i])) {
            boolean world_stopped = context_scanner != 0;
            if (world_stopped) {
                *root_thread = deduce_thread(context_scanner, pin, thread_pc);
            } else {
                *root_thread = 0;
                *thread_pc = 0;
                // Scan just the current thread's stack
                // (We don't know where the other stack pointers are)
                th = arch_os_get_current_thread();
                void **esp = __builtin_frame_address(0);
                void **where;
                for (where = ((void **)th->control_stack_end)-1; where >= esp;  --where)
                    if (*where == (void*)pin) {
                        *root_thread = th;
                        *thread_pc = deduce_thread_pc(th, where);
                        break;
                    }
            }
            return pin;
        }
#endif
    *root_kind = HEAP;
    return 0;
}

static void free_graph(struct layer* layer)
{
    while (layer) {
        free(layer->nodes);
        struct layer* next = layer->next;
        free(layer);
        layer = next;
    }
}

static struct node* find_node(struct layer* layer, lispobj ptr)
{
    int i;
    for(i=layer->count-1; i>=0; --i)
        if (layer->nodes[i].object == ptr)
            return &layer->nodes[i];
    return 0;
}

/// "Compressed" pointers are a huge win - they halve the amount
/// of space required to invert the heap.
static inline uint32_t encode_pointer(lispobj pointer)
{
    uword_t encoding;
    if (find_page_index((void*)pointer) >= 0) {
        // A dynamic space pointer is stored as a count in doublewords
        // from the heap base address. A 32GB range is representable.
        encoding = (pointer - DYNAMIC_SPACE_START) / (2*N_WORD_BYTES);
        gc_assert(encoding <= 0x7FFFFFFF);
        // Low bit of 1 signifies dynamic space compressed ptr.
        return (encoding<<1) | 1;
    } else
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (find_varyobj_page_index((void*)pointer) >= 0) {
        // A varyobj space pointer is stored as a count in doublewords
        // from the base address.
        encoding = (pointer - VARYOBJ_SPACE_START) / (2*N_WORD_BYTES);
        gc_assert(encoding <= 0x3FFFFFFF);
        // bit pattern #b10 signifies varyobj space compressed ptr.
        return (encoding<<2) | 2;
    } else
#endif
    {
        // Everything else is stored as-is.
        gc_assert(pointer <= 0xFFFFFFFF && !(pointer & 3));
        return pointer; // bit pattern #b00 signifies uncompressed ptr
    }
}

static inline lispobj decode_pointer(uint32_t encoding)
{
    if (encoding & 1)  // Compressed ptr to dynamic space
        return (encoding>>1)*(2*N_WORD_BYTES) + DYNAMIC_SPACE_START;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    else if ((encoding & 3) == 2) // Compressed ptr to varyobj space
        return (encoding>>2)*(2*N_WORD_BYTES) + VARYOBJ_SPACE_START;
#endif
    else
        return encoding; // Literal pointer
}

static struct simple_fun* simple_fun_from_pc(char* pc)
{
    struct code* code = (struct code*)component_ptr_from_pc((lispobj*)pc);
    if (!code) return 0;
    struct simple_fun* prev_fun = 0;
    for_each_simple_fun(i, fun, code, 1, {
        if (pc < (char*)fun) break;
        prev_fun = fun;
    })
    return prev_fun;
}

static void maybe_show_object_name(lispobj obj, FILE* stream)
{
    extern void safely_show_lstring(lispobj* string, int quotes, FILE *s);
    lispobj package, package_name;
    if (lowtag_of(obj)==OTHER_POINTER_LOWTAG)
        switch(widetag_of(native_pointer(obj))) {
        case SYMBOL_WIDETAG:
            putc(',', stream);
            if ((package = SYMBOL(obj)->package) == NIL) {
                fprintf(stream, "#:");
            } else {
                package_name = ((struct package*)native_pointer(package))->_name;
                safely_show_lstring(native_pointer(package_name), 0, stream);
                fputs("::", stream);
            }
            safely_show_lstring(native_pointer(SYMBOL(obj)->name), 0, stream);
            break;
        }
}

static boolean root_p(lispobj ptr, int criterion)
{
    if (ptr <= STATIC_SPACE_END) return 1; // always a root
    // 0 to 2 are in order of weakest to strongest condition for stopping,
    // i.e. criterion 0 implies that that largest number of objects
    // are considered roots.
    return criterion < 2
        && (traceroot_gen_of(ptr) > (criterion ? HIGHEST_NORMAL_GENERATION
                           : gencgc_oldest_gen_to_gc));
}

/// Find any shortest path to 'object' starting at a tenured object or a thread stack.
/// Return 1 if a path was found, 0 if not.
static int trace1(lispobj object,
                  struct hopscotch_table* targets,
                  struct hopscotch_table* visited,
                  struct hopscotch_table* inverted_heap,
                  struct scratchpad* scratchpad,
                  int n_pins, lispobj* pins, void (*context_scanner)(),
                   int criterion)
{
    struct node* anchor = 0;
    lispobj thread_ref;
    enum ref_kind root_kind;
    struct thread* root_thread;
    char* thread_pc = 0;
    lispobj tls_index = 0;
    lispobj target;
    int i;

    struct layer* top_layer = 0;
    int layer_capacity = 0;

    hopscotch_put(targets, object, 1);
    while ((thread_ref = examine_stacks(targets, context_scanner, n_pins, pins,
                                        &root_kind, &root_thread, &thread_pc,
                                        &tls_index)) == 0) {
        // TODO: preallocate layers to avoid possibility of malloc deadlock
        struct layer* layer = (struct layer*)malloc(sizeof (struct layer));
        layer->nodes   = 0;
        layer->count   = 0;
        layer->next    = top_layer;
        top_layer      = layer;
        layer_capacity = 0;
        if (heap_trace_verbose)
            printf("Next layer: Looking for %d object(s)\n", targets->count);
        for_each_hopscotch_key(i, target, (*targets)) {
            uint32_t list = hopscotch_get(inverted_heap, target, 0);
            if (heap_trace_verbose>1) {
                uint32_t list1 = list;
                fprintf(stderr, "target=%p srcs=", (void*)target);
                while (list1) {
                    uint32_t* cell = (uint32_t*)(scratchpad->base + list1);
                    lispobj* ptr = (lispobj*)decode_pointer(cell[0]);
                    if (hopscotch_containsp(visited, (lispobj)ptr))
                        fprintf(stderr, "%p ", ptr);
                    else {
                        lispobj word = *ptr;
                        int nwords = OBJECT_SIZE(word, ptr);
                        fprintf(stderr, "%p+%d ", ptr, nwords);
                    }
                    list1 = cell[1];
                }
                putc('\n',stderr);
            }
            while (list && !anchor) {
                uint32_t* cell = (uint32_t*)(scratchpad->base + list);
                lispobj ptr = decode_pointer(cell[0]);
                list = cell[1];
                if (hopscotch_containsp(visited, ptr))
                    continue;
                int wordindex = find_ref((lispobj*)ptr, target);
                if (wordindex == -1) {
                    fprintf(stderr, "Strange: no ref from %p to %p\n",
                            (void*)ptr, (void*)target);
                    continue;
                }
                hopscotch_insert(visited, ptr, 1);
                add_to_layer((lispobj*)ptr, wordindex,
                             top_layer, &layer_capacity);
                // Stop if the object at 'ptr' is tenured.
                if (root_p(ptr, criterion)) {
                    fprintf(stderr, "Stopping at %p: tenured\n", (void*)ptr);
                    anchor = &top_layer->nodes[top_layer->count-1];
                }
            }
        }
        if (!top_layer->count) {
            fprintf(stderr, "Failure tracing from %p. Current targets:\n", (void*)object);
            for_each_hopscotch_key(i, target, (*targets)) {
                fprintf(stderr, "%p", (void*)target);
                fprintf(stderr, "(g%d,", traceroot_gen_of(target));
                fputs(classify_obj(target), stderr);
                maybe_show_object_name(target, stderr);
                fprintf(stderr,") ");
            }
            putc('\n', stderr);
            free_graph(top_layer);
            return 0;
        }
        if (heap_trace_verbose>1)
            printf("Found %d object(s)\n", top_layer->count);
        // The top layer's last object if static or tenured
        // stops the scan. (And no more objects go in the top layer)
        if (anchor)
            break;
        // Transfer the top layer objects into 'targets'
        hopscotch_reset(targets);
        struct node* nodes = top_layer->nodes;
        for (i=top_layer->count-1 ; i>=0 ; --i) {
            lispobj ptr = nodes[i].object;
            hopscotch_put(targets, ptr, 1);
        }
    }

    FILE *file = stdout;
    if (thread_ref) {
        struct vector* lisp_thread_name(os_thread_t os_thread);
        extern void show_lstring(struct vector*, int, FILE*);
        struct vector* thread_name;
  #if 0
        fprintf(stderr,
                "%s pointed to by %s: %p\n",
                top_layer ? "Indirectly" : "Directly",
                ref_kind_name[root_kind],
                (void*)thread_ref);
  #endif
        if (top_layer) {
            // The thread indirectly points to a target.
            // The root object is whatever the thread pointed to,
            // which must be an object in the top layer. Find that object.
            anchor = find_node(top_layer, thread_ref);
            gc_assert(anchor);
        }
        putc('{', file);
        if (!root_thread)
            fprintf(file, "(unknown-thread)");
        else if ((thread_name = lisp_thread_name(root_thread->os_thread)) != 0)
            show_lstring(thread_name, 1, file);
        else
            fprintf(file, "thread=%p", root_thread);
        fprintf(file, ":%s:", ref_kind_name[root_kind]);
        if (root_kind==BINDING_STACK || root_kind==TLS) {
#ifdef LISP_FEATURE_SB_THREAD
            lispobj* symbol = (lispobj*)
              lisp_symbol_from_tls_index(tls_index);
#else
            lispobj* symbol = native_pointer(tls_index);
#endif
            if (symbol)
                show_lstring(symbol_name(symbol), 0, file);
            else
                fprintf(file, "%"OBJ_FMTX, tls_index);
        } else {
            struct simple_fun* fun = simple_fun_from_pc(thread_pc);
            if (fun) {
                fprintf(file, "fun=%p", (void*)make_lispobj(fun, FUN_POINTER_LOWTAG));
                if (is_lisp_pointer(fun->name) &&
                    widetag_of(native_pointer(fun->name)) == SYMBOL_WIDETAG) {
                    fprintf(file, "=");
                    show_lstring(VECTOR(SYMBOL(fun->name)->name), 0, file);
                }
            } else if (thread_pc)
                fprintf(file, "pc=%p", thread_pc);
        }
        fprintf(file, "}->");
    } else { // Stopped at (pseudo)static object
        fprintf(stderr, "Anchor object is @ %p. word[%d]\n",
                native_pointer(anchor->object), anchor->wordindex);
    }

    target = thread_ref;
    while (top_layer) {
        struct node next = *anchor;
        lispobj ptr = next.object;
        if (ptr <= STATIC_SPACE_END)
            fprintf(file, "(static,");
        else
            fprintf(file, "(g%d,", traceroot_gen_of(ptr));
        fputs(classify_obj(ptr), file);
        maybe_show_object_name(ptr, file);
        fprintf(file, ")#x%"OBJ_FMTX"[%d]->", ptr, next.wordindex);
        target = native_pointer(ptr)[next.wordindex];
        // Special-case a few combinations of <type,wordindex>
        switch (next.wordindex) {
        case 0:
            if (instancep(ptr) || functionp(ptr))
                target = instance_layout(native_pointer(ptr));
            break;
#if FUN_SELF_FIXNUM_TAGGED
        case 1:
            if (functionp(ptr) && widetag_of(native_pointer(ptr)) == CLOSURE_WIDETAG)
                target -= FUN_RAW_ADDR_OFFSET;
            break;
#endif
        case 3:
            if (lowtag_of(ptr) == OTHER_POINTER_LOWTAG &&
                widetag_of(&FDEFN(ptr)->header) == FDEFN_WIDETAG)
                target = fdefn_callee_lispobj((struct fdefn*)native_pointer(ptr));
            break;
        }
        target = canonical_obj(target);
        struct layer* next_layer = top_layer->next;
        free(top_layer->nodes);
        free(top_layer);
        top_layer = next_layer;
        if (top_layer) {
            anchor = find_node(top_layer, target);
            gc_assert(anchor);
        } else {
            gc_assert(object == target);
        }
    }
    fprintf(file, "#x%"OBJ_FMTX".\n", target);
    fflush(file);
    return 1;
}

static void record_ptr(lispobj* source, lispobj target,
                       struct scan_state* ss)
{
    // Add 'source' to the list of objects keyed by 'target' in the inverted heap.
    // Note that 'source' has no lowtag, and 'target' does.
    // Pointer compression occurs here as well: the linked list of source objects
    // is built using offsets into the scratchpad rather than absolute addresses.
    target = canonical_obj(target);
    uint32_t* new_cell = (uint32_t*)ss->scratchpad.free;
    uint32_t* next = new_cell + 2;
    gc_assert((char*)next <= ss->scratchpad.end);
    ss->scratchpad.free = (char*)next;
    new_cell[0] = encode_pointer((lispobj)source);
    new_cell[1] = hopscotch_get(&ss->inverted_heap, target, 0);
    hopscotch_put(&ss->inverted_heap, target,
                  (sword_t)((char*)new_cell - ss->scratchpad.base));
}

#define relevant_ptr_p(x) find_page_index(x)>=0||immobile_space_p((lispobj)x)

#define check_ptr(ptr) { \
    ++n_scanned_words; \
    if (!is_lisp_pointer(ptr)) ++n_immediates; \
    else if (relevant_ptr_p((void*)(ptr))) { \
      ++n_pointers; \
      if (record_ptrs) record_ptr(where,ptr,ss); \
    }}

static uword_t build_refs(lispobj* where, lispobj* end,
                          struct scan_state* ss)
{
    lispobj layout, bitmap;
    sword_t nwords, scan_limit, i, j;
    uword_t n_objects = 0, n_scanned_words = 0,
            n_immediates = 0, n_pointers = 0;

    boolean record_ptrs = ss->record_ptrs;
    for ( ; where < end ; where += nwords ) {
        ++n_objects;
        lispobj header = *where;
        if (is_cons_half(header)) {
            nwords = 2;
            check_ptr(header);
            check_ptr(where[1]);
            continue;
        }
        int widetag = widetag_of(where);
        nwords = scan_limit = sizetab[widetag](where);
        switch (widetag) {
        case INSTANCE_WIDETAG:
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER
        case FUNCALLABLE_INSTANCE_WIDETAG:
#endif
            // mixed boxed/unboxed objects
            layout = instance_layout(where);
            check_ptr(layout);
            // Partially initialized instance can't have nonzero words yet
            bitmap = layout ? LAYOUT(layout)->bitmap : make_fixnum(-1);
            // FIXME: check CUSTOM_GC_SCAVENGE in the header
            // If no raw slots, just scan without use of the bitmap.
            if (bitmap == make_fixnum(-1)) break;
            for(i=1; i<scan_limit; ++i)
                if (layout_bitmap_logbitp(i-1, bitmap)) check_ptr(where[i]);
            continue;
#if FUN_SELF_FIXNUM_TAGGED
        case CLOSURE_WIDETAG:
            check_ptr(((struct closure*)where)->fun - FUN_RAW_ADDR_OFFSET);
            break;
#endif
        case CODE_HEADER_WIDETAG:
            for_each_simple_fun(i, function_ptr, (struct code*)where, 0, {
                int wordindex = &function_ptr->name - where;
                for (j=0; j<4; ++j) check_ptr(where[wordindex+j]);
            })
            scan_limit = code_header_words((struct code*)where);
            break;
        case FDEFN_WIDETAG:
            check_ptr(fdefn_callee_lispobj((struct fdefn*)where));
            scan_limit = 3;
            break;
        case SIMPLE_VECTOR_WIDETAG:
            // For weak hash-table vectors, a <k,v> pair may participate in
            // a path from the root only if it does not involve whichever
            // object is definitely weak. This fails on weak key-OR-value
            // tables since we can't decide whether to allow the entry.
            if (is_vector_subtype(*where, VectorValidHashing)) {
                lispobj lhash_table = where[2];
                gc_assert(instancep(lhash_table));
                struct hash_table* hash_table =
                  (struct hash_table *)native_pointer(lhash_table);
                int weakness = hashtable_weakness(hash_table);
                if (hashtable_weakp(hash_table) &&
                    (weakness == 1 || weakness == 2)) { // 1=key, 2=value
                    // Skip the first 4 words which are:
                    //  header, length, vector->table backpointer, rehash-p
                    int start = (weakness == 1) ? 5 : 4;
                    for(i=start; i<scan_limit; i+=2) check_ptr(where[i]);
                    continue;
                }
            }
            if (is_vector_subtype(*where, VectorWeak))
                continue;
            break;
        default:
            if (!(other_immediate_lowtag_p(widetag) && lowtag_for_widetag[widetag>>2]))
              lose("Unknown widetag %x\n", widetag);
            // Skip irrelevant objects.
            if (unboxed_obj_widetag_p(widetag) ||
                (widetag == WEAK_POINTER_WIDETAG) || /* do not follow! */
                // These numeric types contain pointers, but are uninteresting.
                (widetag == COMPLEX_WIDETAG) ||
                (widetag == RATIO_WIDETAG))
                continue;
        }
        for(i=1; i<scan_limit; ++i) check_ptr(where[i]);
    }
    if (!record_ptrs) { // just count them
        ss->n_objects += n_objects;
        ss->n_scanned_words += n_scanned_words;
        ss->n_immediates += n_immediates;
        ss->n_pointers += n_pointers;
    }
    return 0;
}
#undef check_ptr

static void scan_spaces(struct scan_state* ss)
{
    build_refs((lispobj*)STATIC_SPACE_START, static_space_free_pointer, ss);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    build_refs((lispobj*)FIXEDOBJ_SPACE_START, fixedobj_free_pointer, ss);
    build_refs((lispobj*)VARYOBJ_SPACE_START, varyobj_free_pointer, ss);
#endif
    walk_generation((uword_t(*)(lispobj*,lispobj*,uword_t))build_refs,
                    -1, (uword_t)ss);
}

#define HASH_FUNCTION HOPSCOTCH_HASH_FUN_MIX

static void compute_heap_inverse(struct hopscotch_table* inverted_heap,
                                 struct scratchpad* scratchpad)
{
    struct scan_state ss;
    memset(&ss, 0, sizeof ss);
    fprintf(stderr, "Pass 1: Counting heap objects... ");
    scan_spaces(&ss);
    fprintf(stderr, "%ld objs, %ld ptrs, %ld immediates\n",
            ss.n_objects, ss.n_pointers,
            ss.n_scanned_words - ss.n_pointers);
    // Guess at the initial size of ~ .5 million objects.
    int size = 1<<19; // flsl(tot_n_objects); this would work if you have it
    while (ss.n_objects > size) size <<= 1;
    fprintf(stderr, "Pass 2: Inverting heap. Initial size=%d objects\n", size);
    hopscotch_create(&ss.inverted_heap, HASH_FUNCTION,
                     4, // XXX: half the word size if 64-bit
                     size /* initial size */, 0 /* default hop range */);
    // Add one pointer due to inability to use the first
    // two words of the scratchpad.
    uword_t scratchpad_min_size = (1 + ss.n_pointers) * 2 * sizeof (uint32_t);
    int pagesize = getpagesize();
    uword_t scratchpad_size = ALIGN_UP(scratchpad_min_size, pagesize);
    ss.scratchpad.base = os_allocate(scratchpad_size);
    gc_assert(ss.scratchpad.base);
    ss.scratchpad.free = ss.scratchpad.base + 2 * sizeof(uint32_t);
    ss.scratchpad.end  = ss.scratchpad.base + scratchpad_size;
    fprintf(stderr, "Scratchpad: %lu bytes\n", (long unsigned)scratchpad_size);
#if HAVE_GETRUSAGE
    struct rusage before, after;
    getrusage(RUSAGE_SELF, &before);
#endif
    ss.record_ptrs = 1;
    scan_spaces(&ss);
    *inverted_heap = ss.inverted_heap;
    *scratchpad = ss.scratchpad;
#if HAVE_GETRUSAGE
    getrusage(RUSAGE_SELF, &after);
    // We're done building the necessary structure. Show some memory stats.
#define timediff(b,a,field) \
    ((a.field.tv_sec-b.field.tv_sec)*1000000+(a.field.tv_usec-b.field.tv_usec))
    fprintf(stderr,
            "Inverted heap: ct=%d, cap=%d, LF=%f ET=%ld+%ld sys+usr\n",
            inverted_heap->count,
            1+hopscotch_max_key_index(*inverted_heap),
            100*(float)inverted_heap->count / (1+hopscotch_max_key_index(*inverted_heap)),
            timediff(before, after, ru_stime),
            timediff(before, after, ru_utime));
#endif
};

/* Find any shortest path from a thread or tenured object
 * to each of the specified objects.
 */
static int trace_paths(void (*context_scanner)(),
                       lispobj weak_pointers, int n_pins, lispobj* pins,
                       int criterion)
{
    int i;
    struct hopscotch_table inverted_heap;
    struct scratchpad scratchpad;
    // A hashset of all objects in the reverse reachability graph so far
    struct hopscotch_table visited;  // *Without* lowtag
    // A hashset of objects in the current graph layer
    struct hopscotch_table targets;  // With lowtag
    int n_found = 0; // how many objects had paths to them

    if (heap_trace_verbose) {
        fprintf(stderr, "%d pins:\n", n_pins);
        for(i=0;i<n_pins;++i)
          fprintf(stderr, " %p%s", (void*)pins[i],
                  ((i%8)==7||i==n_pins-1)?"\n":"");
    }
    compute_heap_inverse(&inverted_heap, &scratchpad);
    hopscotch_create(&visited, HASH_FUNCTION, 0, 32, 0);
    hopscotch_create(&targets, HASH_FUNCTION, 0, 32, 0);
    do {
        lispobj car = CONS(weak_pointers)->car;
        lispobj value = ((struct weak_pointer*)native_pointer(car))->value;
        weak_pointers = CONS(weak_pointers)->cdr;
        if (is_lisp_pointer(value)) {
            if (heap_trace_verbose)
                fprintf(stderr, "Target=%p (%s)\n", (void*)value, classify_obj(value));
            hopscotch_reset(&visited);
            hopscotch_reset(&targets);
            n_found += trace1(canonical_obj(value),
                              &targets, &visited,
                              &inverted_heap, &scratchpad,
                              n_pins, pins, context_scanner, criterion);
        }
    } while (weak_pointers != NIL);
    os_invalidate(scratchpad.base, scratchpad.end-scratchpad.base);
    hopscotch_destroy(&inverted_heap);
    hopscotch_destroy(&visited);
    hopscotch_destroy(&targets);
    return n_found;
}

/// Return number of sought objects that had paths to them.
/// Return -1 for invalid input.
int gc_prove_liveness(void(*context_scanner)(),
                      lispobj objects,
                      int n_pins, uword_t* pins,
                      int criterion)
{
    int n_watched = 0, n_live = 0, n_bad = 0, n_imm = 0;
    lispobj list;
    for (list = objects ; list != NIL && listp(list) ; list = CONS(list)->cdr) {
        ++n_watched;
        lispobj car = CONS(list)->car;
        if ((lowtag_of(car) != OTHER_POINTER_LOWTAG ||
             widetag_of(native_pointer(car)) != WEAK_POINTER_WIDETAG)) {
            ++n_bad;
            continue;
        }
        lispobj wpval = ((struct weak_pointer*)native_pointer(car))->value;
        if (is_lisp_pointer(wpval))
            ++n_live;
        else if (wpval != UNBOUND_MARKER_WIDETAG)
            ++n_imm;
    }
    if (!listp(list) || n_bad) {
        fprintf(stderr, "; Bad value in liveness tracker\n");
        return -1;
    }
    fprintf(stderr, "; Liveness tracking: %d/%d live watched objects",
            n_live, n_watched);
    if (n_imm)
        fprintf(stderr, " (ignored %d non-pointers)", n_imm);
    putc('\n', stderr);
    if (!n_live)
        return 0;
    // Put back lowtags on pinned objects, since wipe_nonpinned_words() removed
    // them. But first test whether lowtags were already repaired
    // in case prove_liveness() is called after gc_prove_liveness().
    if (n_pins>0 && !is_lisp_pointer(pins[0])) {
        int i;
        for(i=n_pins-1; i>=0; --i) {
            pins[i] = compute_lispobj((lispobj*)pins[i]);
        }
    }
    return trace_paths(context_scanner, objects, n_pins, (lispobj*)pins, criterion);
}

/* This should be called inside WITHOUT-GCING so that the set
 * of pins does not change out from underneath.
 */
int prove_liveness(lispobj objects, int criterion)
{
    extern struct hopscotch_table pinned_objects;
    extern int gc_n_stack_pins;
    return gc_prove_liveness(0, objects, gc_n_stack_pins, pinned_objects.keys, criterion);
}

// These are slot offsets in (DEFSTRUCT THREAD),
// not the C structure defined in genesis/thread.h
#define LISP_THREAD_NAME_SLOT INSTANCE_DATA_START+0
#define LISP_THREAD_OS_THREAD_SLOT INSTANCE_DATA_START+3

/* Perform exhaustive search on *ALL-THREADS* looking for a specific thread.
 * Efficiency is not a concern.
 */
static struct instance* find_thread(os_thread_t os_thread, lispobj tree)
{
    if (tree == NIL) return 0;
    struct avlnode* node = (struct avlnode*)native_pointer(tree);
    struct instance* lisp_thread = (struct instance*)native_pointer(node->data);
    if ((os_thread_t)lisp_thread->slots[LISP_THREAD_OS_THREAD_SLOT] == os_thread)
        return lisp_thread;
    if ((lisp_thread = find_thread(os_thread, node->left)) != NULL)
        return lisp_thread;
    if ((lisp_thread = find_thread(os_thread, node->right)) != NULL)
        return lisp_thread;
    return 0;
}

struct vector* lisp_thread_name(os_thread_t os_thread)
{
    static unsigned int hint;
    lispobj* sym = find_symbol("*ALL-THREADS*", "SB-THREAD", &hint);
    if (sym) {
        struct instance* lisp_thread =
            find_thread(os_thread, ((struct symbol*)sym)->value);
        if (lisp_thread)
            return VECTOR(lisp_thread->slots[LISP_THREAD_NAME_SLOT]);
    }
    return 0;
}
