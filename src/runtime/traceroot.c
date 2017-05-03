
#include "sbcl.h"
#include "arch.h"
#include "runtime.h"
#include "lispregs.h"
#include "gc-internal.h"
#include "genesis/closure.h"
#include "genesis/cons.h"
#include "genesis/constants.h"
#include "genesis/gc-tables.h"
#include "genesis/layout.h"
#include "pseudo-atomic.h" // for get_alloc_pointer()

#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/resource.h> // for getrusage()

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

static int gen_of(lispobj obj) {
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (immobile_space_p(obj))
        return immobile_obj_gen_bits(native_pointer(obj));
#endif
    int page = find_page_index((void*)obj);
    if (page >= 0) return page_table[page].gen;
    return -1;
}

const char* classify_obj(lispobj ptr)
{
    extern lispobj* instance_classoid_name(lispobj*);
    extern char *widetag_names[];

    lispobj* name; // a Lisp string
    switch(lowtag_of(ptr)) {
    case INSTANCE_POINTER_LOWTAG:
        name = instance_classoid_name(native_pointer(ptr));
        if (widetag_of(*name) == SIMPLE_BASE_STRING_WIDETAG)
          return (char*)(name + 2);
    case LIST_POINTER_LOWTAG:
        return "cons";
    case FUN_POINTER_LOWTAG:
    case OTHER_POINTER_LOWTAG:
        return widetag_names[widetag_of(*native_pointer(ptr))>>2];
    }
    static char buf[8];
    sprintf(buf, "#x%x", widetag_of(*native_pointer(ptr)));
    return buf;
}

static void add_to_layer(lispobj* obj, int wordindex,
                         struct layer* layer, int* capacity)
{
    // Resurrect the containing object's lowtag
    lispobj word = *obj;
    int lowtag = is_cons_half(word) ?
      LIST_POINTER_LOWTAG : lowtag_for_widetag[widetag_of(word)>>2];
    lispobj ptr = make_lispobj(obj, lowtag);
    int staticp = ptr <= STATIC_SPACE_START;
    int gen = staticp ? -1 : gen_of(ptr);
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
    if (lowtag_of(obj) == FUN_POINTER_LOWTAG &&
        widetag_of(*native_pointer(obj)) == SIMPLE_FUN_WIDETAG)
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
    int widetag = widetag_of(header);
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
        bitmap = ((struct layout*)native_pointer(layout))->bitmap;
        for(i=1; i<scan_limit; ++i)
            if (layout_bitmap_logbitp(i-1, bitmap)) check_ptr(i, source[i]);
        return -1;
    case CLOSURE_WIDETAG:
        check_ptr(1, ((struct closure*)source)->fun - FUN_RAW_ADDR_OFFSET);
        break;
    case CODE_HEADER_WIDETAG:
        for_each_simple_fun(i, function_ptr, (struct code*)source, 0, {
            int wordindex = &function_ptr->name - source;
            for (j=0; j<4; ++j) check_ptr(wordindex+j, source[wordindex+j]);
        })
        scan_limit = code_header_words(header);
        break;
#ifdef LISP_FEATURE_IMMOBILE_CODE
    case FDEFN_WIDETAG:
        check_ptr(3, fdefn_raw_referent((struct fdefn*)source));
        scan_limit = 3;
        break;
#endif
    }
    for(i=1; i<scan_limit; ++i) check_ptr(i, source[i]);
    return -1;
}
#undef check_ptr

enum ref_kind { HEAP, CONTROL_STACK, BINDING_STACK, TLS };
char *ref_kind_name[4] = {"heap","C stack","bindings","TLS"};

/// This unfortunately entails a heap scan,
/// but it's quite fast if the symbol is found in immobile space.
static lispobj* find_sym_by_tls_index(unsigned int tls_index)
{
    lispobj* where = 0;
    lispobj* end = 0;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    where = (lispobj*)IMMOBILE_SPACE_START;
    end = (lispobj*)SYMBOL(IMMOBILE_FIXEDOBJ_FREE_POINTER)->value;
#endif
    while (1) {
        while (where < end) {
            lispobj header = *where;
            int widetag = widetag_of(header);
            if (widetag == SYMBOL_WIDETAG &&
                tls_index_of(((struct symbol*)where)) == tls_index)
                return where;
            where += is_cons_half(header) ? 2 : sizetab[widetag](where);
        }
        if (where >= (lispobj*)DYNAMIC_SPACE_START)
            break;
        where = (lispobj*)DYNAMIC_SPACE_START;
        end = (lispobj*)get_alloc_pointer();
    }
    return 0;
}

static inline int interestingp(lispobj ptr, struct hopscotch_table* targets)
{
    return is_lisp_pointer(ptr) && hopscotch_containsp(targets, ptr);
}

/* Try to find the call frame that contains 'addr', which is the address
 * in which a conservative root was seen.
 * Return the program counter associated with that frame. */
static char* deduce_thread_pc(struct thread* th, void** addr)
{
    uword_t* fp = __builtin_frame_address(0);
    char* return_pc = 0;

    if (th != arch_os_get_current_thread()) {
        int i = fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX,th));
        os_context_t *c = th->interrupt_contexts[i-1];
#ifdef LISP_FEATURE_64_BIT
        fp = (uword_t*)*os_context_register_addr(c,reg_RBP);
#else
        fp = (uword_t*)*os_context_register_addr(c,reg_EBP);
#endif
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
static struct thread* deduce_thread(void (*context_scanner)(),
                                    uword_t pointer, char** pc)
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
            free = fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX,th));
            for(i=free-1;i>=0;i--) {
                os_context_t *c=th->interrupt_contexts[i];
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
            lose("deduce_thread: no SP known for thread %x (OS %x)", th, th->os_thread);
        void** where;
        for (where = ((void **)th->control_stack_end)-1; where >= esp;  where--)
            if ((uword_t)*where == pointer) {
                *pc = deduce_thread_pc(th, where);
                return th;
            }
    }
    return 0;
}

static lispobj examine_stacks(struct hopscotch_table* targets,
                              void (*context_scanner)(),
                              int n_pins, lispobj* pins,
                              enum ref_kind *root_kind,
                              struct thread** root_thread,
                              char** thread_pc,
                              unsigned int *tls_index)
{
    boolean world_stopped = context_scanner != 0;
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
    }
    // Look in the control stacks
    *root_kind = CONTROL_STACK;
    uword_t pin;
    int i;
    for (i=n_pins-1; i>=0; --i)
        // Bypass interestingp() to avoid one test - pins are known pointers.
        if (hopscotch_containsp(targets, pin = pins[i])) {
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
    *root_kind = HEAP;
    return 0;
}

void free_graph(struct layer* layer)
{
    while (layer) {
        free(layer->nodes);
        struct layer* next = layer->next;
        free(layer);
        layer = next;
    }
}

struct node* find_node(struct layer* layer, lispobj ptr)
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
    if (pointer >= DYNAMIC_SPACE_START) {
        // A dynamic space pointer is stored as a count in doublewords
        // from the heap base address. A 32GB range is representable.
        encoding = (pointer - DYNAMIC_SPACE_START) / (2*N_WORD_BYTES);
        gc_assert(encoding <= 0x7FFFFFFF);
        return (encoding<<1) | 1; // Low bit signifies compressed ptr.
    } else {
        // Non-dynamic-space pointers are stored as-is.
        gc_assert(pointer <= 0xFFFFFFFF && !(pointer & 1));
        return pointer; // Low bit 0 signifies literal pointer
    }
}

static inline lispobj decode_pointer(uint32_t encoding)
{
    if (encoding & 1)  // Compressed ptr
        return (encoding>>1)*(2*N_WORD_BYTES) + DYNAMIC_SPACE_START;
    else
        return encoding; // Literal pointer
}

struct simple_fun* simple_fun_from_pc(char* pc)
{
    struct code* code = (struct code*)component_ptr_from_pc((lispobj*)pc);
    struct simple_fun* prev_fun = (struct simple_fun*)
        ((char*)code + (code_header_words(code->header)<<WORD_SHIFT)
         + FIRST_SIMPLE_FUN_OFFSET(code));
    for_each_simple_fun(i, fun, code, 1, {
        if (pc < (char*)fun) break;
        prev_fun = fun;
    })
    return prev_fun;
}

/// Find any shortest path to 'object' starting at a tenured object or a thread stack.
static void trace1(lispobj object,
                   struct hopscotch_table* targets,
                   struct hopscotch_table* visited,
                   struct hopscotch_table* inverted_heap,
                   struct scratchpad* scratchpad,
                   int n_pins, lispobj* pins, void (*context_scanner)())
{
    struct node* anchor = 0;
    lispobj thread_ref;
    enum ref_kind root_kind;
    struct thread* root_thread;
    char* thread_pc;
    unsigned int tls_index;
    lispobj target, root;
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
            root = 0;
            if (heap_trace_verbose>1) {
                uint32_t list1 = list;
                fprintf(stderr, "target=%p srcs=", (void*)target);
                while (list1) {
                    uint32_t* cell = (uint32_t*)(scratchpad->base + list1);
                    lispobj ptr = decode_pointer(cell[0]);
                    fprintf(stderr, "%p ", (void*)ptr);
                    list1 = cell[1];
                }
                putc('\n',stderr);
            }
            while (list && !root) {
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
                if (ptr <= STATIC_SPACE_START || gen_of(ptr) >= 1+gencgc_oldest_gen_to_gc) {
                    fprintf(stderr, "Stopping at %p: tenured\n", (void*)ptr);
                    root = ptr;
                }
            }
        }
        if (!top_layer->count) {
            // Should print the graph we have so far.
            fprintf(stderr, "NO OBJECTS!\n");
            free_graph(top_layer);
            return;
        }
        if (heap_trace_verbose>1)
            printf("Found %d object(s)\n", top_layer->count);
        // The top layer's last object if static or tenured
        // stops the scan. (And no more objects go in the top layer)
        i = top_layer->count-1;
        lispobj ptr = top_layer->nodes[i].object;
        if (ptr <= STATIC_SPACE_START || gen_of(ptr) >= 1+gencgc_oldest_gen_to_gc)
            anchor = &top_layer->nodes[i];
        if (anchor) break;
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
            lispobj* symbol = find_sym_by_tls_index(tls_index);
            if (symbol)
                show_lstring(symbol_name(symbol), 0, file);
            else
                fprintf(file, "%x", tls_index);
        } else {
            struct simple_fun* fun = simple_fun_from_pc(thread_pc);
            if (fun) {
                fprintf(file, "fun=%p", (void*)make_lispobj(fun, FUN_POINTER_LOWTAG));
                if (is_lisp_pointer(fun->name) &&
                    widetag_of(*native_pointer(fun->name)) == SYMBOL_WIDETAG) {
                    fprintf(file, "=");
                    show_lstring((struct vector*)native_pointer(SYMBOL(fun->name)->name),
                                 0, file);
                }
            } else if (thread_pc)
                fprintf(file, "pc=%p", thread_pc);
        }
        fprintf(file, "}->");
    } else { // Stopped at (pseudo)static object
        fprintf(file, "Anchor object is @ %p. word[%d]\n",
                native_pointer(anchor->object), anchor->wordindex);
    }

    target = 0;
    while (top_layer) {
        struct node next = *anchor;
        lispobj ptr = next.object;
        fprintf(file, "(g%c,%s)%p[%d]->",
                (ptr <= STATIC_SPACE_START ? 'S' : '0'+gen_of(ptr)),
                classify_obj(ptr), (void*)ptr, next.wordindex);
        target = native_pointer(ptr)[next.wordindex];
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
    fprintf(file, "%p.\n", (void*)target);
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

#ifdef LISP_FEATURE_IMMOBILE_SPACE
#define relevant_ptr_p(x) find_page_index(x)>=0||find_immobile_page_index(x)>=0
#else
#define relevant_ptr_p(x) find_page_index(x)>=0
#endif

#define check_ptr(ptr) { \
    ++n_scanned_words; \
    if (!is_lisp_pointer(ptr)) ++n_immediates; \
    else if (relevant_ptr_p((void*)ptr)) { \
      ++n_pointers; \
      if (record_ptrs) record_ptr(where,ptr,ss); \
    }}

static uword_t build_refs(lispobj* where, lispobj* end,
                          struct scan_state* ss)
{
    lispobj layout, bitmap, fun;
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
        int widetag = widetag_of(header);
        nwords = scan_limit = sizetab[widetag](where);
        switch (widetag) {
        case INSTANCE_WIDETAG:
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER
        case FUNCALLABLE_INSTANCE_WIDETAG:
#endif
            // mixed boxed/unboxed objects
            layout = instance_layout(where);
            check_ptr(layout);
            bitmap = ((struct layout*)native_pointer(layout))->bitmap;
            // If no raw slots, just scan without use of the bitmap.
            if (bitmap == make_fixnum(-1)) break;
            for(i=1; i<scan_limit; ++i)
                if (layout_bitmap_logbitp(i-1, bitmap)) check_ptr(where[i]);
            continue;
        case CLOSURE_WIDETAG:
            fun = ((struct closure*)where)->fun - FUN_RAW_ADDR_OFFSET;
            check_ptr(fun);
            break;
        case CODE_HEADER_WIDETAG:
            for_each_simple_fun(i, function_ptr, (struct code*)where, 0, {
                int wordindex = &function_ptr->name - where;
                for (j=0; j<4; ++j) check_ptr(where[wordindex+j]);
            })
            scan_limit = code_header_words(header);
            break;
#ifdef LISP_FEATURE_IMMOBILE_CODE
        case FDEFN_WIDETAG:
            check_ptr(fdefn_raw_referent((struct fdefn*)where));
            scan_limit = 3;
            break;
#endif
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
    build_refs((lispobj*)STATIC_SPACE_START,
               (lispobj*)SYMBOL(STATIC_SPACE_FREE_POINTER)->value,
               ss);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    build_refs((lispobj*)IMMOBILE_SPACE_START,
               (lispobj*)SYMBOL(IMMOBILE_FIXEDOBJ_FREE_POINTER)->value,
               ss);
    build_refs((lispobj*)IMMOBILE_VARYOBJ_SUBSPACE_START,
               (lispobj*)SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value,
               ss);
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
    uword_t scratchpad_size = CEILING(scratchpad_min_size, pagesize);
    ss.scratchpad.base = os_validate(0, scratchpad_size);
    gc_assert(ss.scratchpad.base);
    ss.scratchpad.free = ss.scratchpad.base + 2 * sizeof(uint32_t);
    ss.scratchpad.end  = ss.scratchpad.base + scratchpad_size;
    fprintf(stderr, "Scratchpad: %lu bytes\n", (long unsigned)scratchpad_size);
    struct rusage before, after;
    getrusage(RUSAGE_SELF, &before);
    ss.record_ptrs = 1;
    scan_spaces(&ss);
    getrusage(RUSAGE_SELF, &after);
    *inverted_heap = ss.inverted_heap;
    *scratchpad = ss.scratchpad;
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
};

/* Find any shortest path from a thread or tenured object
 * to each of the specified objects.
 */
static void trace_paths(void (*context_scanner)(),
                        lispobj weak_pointers, int n_pins, lispobj* pins)
{
    int i;
    struct hopscotch_table inverted_heap;
    struct scratchpad scratchpad;
    // A hashset of all objects in the reverse reachability graph so far
    struct hopscotch_table visited;  // *Without* lowtag
    // A hashset of objects in the current graph layer
    struct hopscotch_table targets;  // With lowtag

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
        if (value != UNBOUND_MARKER_WIDETAG) {
            if (heap_trace_verbose)
                fprintf(stderr, "Target=%p (%s)\n", (void*)value, classify_obj(value));
            hopscotch_reset(&visited);
            hopscotch_reset(&targets);
            trace1(value, &targets, &visited,
                   &inverted_heap, &scratchpad,
                   n_pins, pins, context_scanner);
        }
    } while (weak_pointers != NIL);
    os_invalidate(scratchpad.base, scratchpad.end-scratchpad.base);
    hopscotch_delete(&inverted_heap);
    hopscotch_delete(&visited);
    hopscotch_delete(&targets);
}

void gc_prove_liveness(void(*context_scanner)(),
                       lispobj objects,
                       int n_pins, uword_t* pins)
{
    int n_watched = 0, n_live = 0, n_bad = 0;
    lispobj list = objects;
    while (list != NIL && lowtag_of(list) == LIST_POINTER_LOWTAG) {
        ++n_watched;
        lispobj car = CONS(list)->car;
        if ((lowtag_of(car) == OTHER_POINTER_LOWTAG
             && widetag_of(*native_pointer(car)) == WEAK_POINTER_WIDETAG)) {
            n_live += ((struct weak_pointer*)native_pointer(car))
                ->value != UNBOUND_MARKER_WIDETAG;
            list = CONS(list)->cdr;
        } else {
            ++n_bad;
        }
    }
    if (lowtag_of(list) != LIST_POINTER_LOWTAG || n_bad) {
        fprintf(stderr, "; Bad value in liveness tracker\n");
        return;
    }
    fprintf(stderr, "; Liveness tracking: %d/%d live watched objects\n",
            n_live, n_watched);
    if (!n_live)
        return;
    // Put back lowtags on pinned objects, since wipe_nonpinned_words() removed
    // them. But first test whether lowtags were already repaired
    // in case prove_liveness() is called after gc_prove_liveness().
    if (n_pins>0 && !is_lisp_pointer(pins[0])) {
        int i;
        for(i=n_pins-1; i>=0; --i) {
            lispobj* obj = (lispobj*)pins[i];
            lispobj header = *obj;
            int lowtag = is_cons_half(header) ? LIST_POINTER_LOWTAG
                : lowtag_for_widetag[widetag_of(header)>>2];
            pins[i] = make_lispobj(obj, lowtag);
        }
    }
    trace_paths(context_scanner, objects, n_pins, (lispobj*)pins);
}

/* This should be called inside WITHOUT-GCING so that the set
 * of pins does not change out from underneath.
 */
void prove_liveness(lispobj objects)
{
    extern struct hopscotch_table pinned_objects;
    extern int gc_n_stack_pins;
    gc_prove_liveness(0, objects, gc_n_stack_pins, pinned_objects.keys);
}

#include "genesis/package.h"
#include "genesis/instance.h"
#include "genesis/vector.h"

static boolean __attribute__((unused)) sym_stringeq(lispobj sym, const char *string, int len)
{
    struct vector* name = (struct vector*)native_pointer(SYMBOL(sym)->name);
    return widetag_of(name->header) == SIMPLE_BASE_STRING_WIDETAG
        && fixnum_value(name->length) == len
        && !strcmp((char*)name->data, string);
}

/* Return the value of SB-THREAD::*ALL-THREADS*
 * This does not need to be particularly efficient.
 */
static const char __attribute__((unused)) all_threads_sym[] = "*ALL-THREADS*";
static lispobj all_lisp_threads()
{
#ifdef ENTER_FOREIGN_CALLBACK
    // Starting with a known static symbol in SB-THREAD::, get the SB-THREAD package
    // and find *ALL-THREADS* (which isn't static). Fewer static symbols is better.
    struct symbol*   sym = SYMBOL(ENTER_FOREIGN_CALLBACK);
    struct package*  pkg = (struct package*)native_pointer(sym->package);
    struct instance* internals = (struct instance*)native_pointer(pkg->internal_symbols);
    struct vector*   cells = (struct vector*)
        native_pointer(internals->slots[INSTANCE_DATA_START]);
    int cells_length = fixnum_value(cells->length);
    static int index = 0;
    int initial_index = index;
    do {
        lispobj thing = cells->data[index];
        if (lowtag_of(thing) == OTHER_POINTER_LOWTAG
            && widetag_of(SYMBOL(thing)->header) == SYMBOL_WIDETAG
            && sym_stringeq(thing, all_threads_sym, strlen(all_threads_sym)))
          return SYMBOL(thing)->value;
        index = (index + 1) % cells_length;
    } while (index != initial_index);
    lose("Can't find *ALL-THREADS*");
#endif
    return NIL;
}

// These are slot offsets in (DEFSTRUCT THREAD),
// not the C structure defined in genesis/thread.h
#define LISP_THREAD_NAME_SLOT INSTANCE_DATA_START+0
#define LISP_THREAD_OS_THREAD_SLOT INSTANCE_DATA_START+3

struct vector* lisp_thread_name(os_thread_t os_thread)
{
    lispobj list = all_lisp_threads();
    while (list != NIL) {
        struct instance* lisp_thread = (struct instance*)native_pointer(CONS(list)->car);
        list = CONS(list)->cdr;
        if ((os_thread_t)lisp_thread->slots[LISP_THREAD_OS_THREAD_SLOT]
            == os_thread)
            return (struct vector*)
                native_pointer(lisp_thread->slots[LISP_THREAD_NAME_SLOT]);
    }
    return 0;
}
