#include "genesis/sbcl.h"
#include "arch.h"
#include "runtime.h"
#include "lispregs.h"
#include "gc.h"
#include "code.h"
#include "genesis/closure.h"
#include "genesis/cons.h"
#include "genesis/symbol.h"
#include "genesis/gc-tables.h"
#include "genesis/hash-table.h"
#include "genesis/instance.h"
#include "genesis/package.h"
#include "genesis/vector.h"
#include "search.h"
#include "genesis/sap.h"
#include "print.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#ifndef LISP_FEATURE_WIN32
#define HAVE_GETRUSAGE 1
#endif
#ifdef HAVE_GETRUSAGE
#include <sys/resource.h> // for getrusage()
#endif

#ifndef TRACEROOT_USE_ABSL_HASHMAP
#define TRACEROOT_USE_ABSL_HASHMAP 0
#endif

#if TRACEROOT_USE_ABSL_HASHMAP
extern void* new_absl_hashmap(int);
extern void absl_hashmap_destroy(void*);
extern unsigned absl_hashmap_get(void*,lispobj);
extern unsigned* absl_hashmap_get_ref(void*,lispobj);
extern int absl_hashmap_size(void*,int*);
typedef void* inverted_heap_t;
#define inverted_heap_get(graph, key) absl_hashmap_get(graph, key)
#define inverted_heap_get_ref(graph, key) absl_hashmap_get_ref(graph, key)
#else
typedef struct hopscotch_table* inverted_heap_t;
#define inverted_heap_get(graph, key) hopscotch_get(graph, key, 0)
#define inverted_heap_get_ref(graph, key) hopscotch_get_ref(graph, key)
#endif

int heap_trace_verbose = 0;

typedef uintptr_t traceroot_pointer;

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
    // A hashmap from object to list of objects pointing to it
    struct hopscotch_table* inverted_heap;
    struct scratchpad scratchpad;
    lispobj ignored_objects;
    int keep_leaves;
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
        if (widetag_of(name) == SIMPLE_BASE_STRING_WIDETAG) return (char*)(name + 2);
        break;
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
static inline lispobj canonical_obj(lispobj obj)
{
    if (functionp(obj) && widetag_of((lispobj*)FUNCTION(obj)) == SIMPLE_FUN_WIDETAG)
        return fun_code_tagged(FUNCTION(obj));
    return obj;
}

#define slot_index_of(tag_,slot_) offsetof(struct tag_,slot_)/N_WORD_BYTES

/* Return the word index of the pointer in 'source' which references 'target'.
 * Return -1 on failure. (This is an error if it happens)
 */
#define check_ptr(index,ptr) if(canonical_obj(ptr)==target) return index;
static int find_ref(lispobj* source, lispobj target)
{
    lispobj layout;
    sword_t scan_limit, i;

    lispobj word = *source;
    if (!is_header(word)) {
        check_ptr(0, source[0]);
        check_ptr(1, source[1]);
        return -1;
    }
    scan_limit = headerobj_size(source);
    int widetag = widetag_of(source);
    switch (widetag) {
    case INSTANCE_WIDETAG:
    case FUNCALLABLE_INSTANCE_WIDETAG:
        // Unlike in scav_instance where the slot loop is optimized for
        // certain special cases, here we opt for simplicity.
        layout = layout_of(source);
        check_ptr(0, layout);
        if (layout) {
            struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
            for(i=1; i<scan_limit; ++i)
                if (bitmap_logbitp(i-1, bitmap)) check_ptr(i, source[i]);
        }
        // FIXME: check lockfree_list_node_p() also
        return -1;
#if FUN_SELF_FIXNUM_TAGGED
    case CLOSURE_WIDETAG:
        check_ptr(1, fun_taggedptr_from_self(((struct closure*)source)->fun));
        break;
#endif
    case CODE_HEADER_WIDETAG:
        scan_limit = code_header_words((struct code*)source);
        break;
    case SYMBOL_WIDETAG: {
        struct symbol* sym = (void*)source;
        check_ptr(slot_index_of(symbol,value), sym->value);
        check_ptr(slot_index_of(symbol,info), sym->info);
        check_ptr(slot_index_of(symbol,fdefn), sym->fdefn);
        check_ptr(slot_index_of(symbol,name), decode_symbol_name(sym->name));
        return -1;
    }
    case FDEFN_WIDETAG:
        check_ptr(3, decode_fdefn_rawfun((struct fdefn*)source));
        scan_limit = 3;
        break;
    }
    for(i=1; i<scan_limit; ++i) check_ptr(i, source[i]);
    return -1;
}
#undef check_ptr

enum ref_kind { HEAP, CONTROL_STACK, BINDING_STACK, TLS };

static inline int interestingp(lispobj ptr, struct hopscotch_table* targets)
{
    return is_lisp_pointer(ptr) && hopscotch_containsp(targets, ptr);
}

/* This is not performance-critical */
static lispobj* valid_ambiguous_pointer_p(lispobj ptr, int registerp)
{
    lispobj *start = search_all_gc_spaces((void*)ptr);
    if (start == NULL) return NULL;
    // exact pointer is always a winner
    if (compute_lispobj(start) == ptr) return start;
    unsigned char widetag = widetag_of(start);
    // allow untagged and/or interior pointer to code, funcallable-instance
    if (widetag == CODE_HEADER_WIDETAG ||
        widetag == FUNCALLABLE_INSTANCE_WIDETAG)
        return start;
    // allow in-register untagged pointer to lockfree list node
    if (registerp && widetag == INSTANCE_WIDETAG && ptr == (lispobj)start
        && instance_layout(start) != 0
        && lockfree_list_node_layout_p(LAYOUT(instance_layout(start))))
        return start;
    return 0;
}

static uword_t cur_thread_stackptr_at_entry; // current thread has no sigcontext
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
static os_context_t* get_register_context(struct thread* th)
{
    if (th != get_sb_vm_thread()
        // don't give a damn about nested interrupts
        && read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th) == make_fixnum(1))
        return nth_interrupt_context(0, th);
    return 0;
}

static lispobj* get_stackptr(struct thread* th)
{
    if (th == get_sb_vm_thread()) return (lispobj*)cur_thread_stackptr_at_entry;
    os_context_t* context = get_register_context(th);
    if (context) return (lispobj*)(uword_t)*os_context_sp_addr(context);
    lose("No stack pointer for %p", th);
}

/* Try to find the call frame that contains 'addr', which is the address
 * in which an ambiguous root was seen.
 * Return the program counter associated with that frame. */
static char* NO_SANITIZE_MEMORY deduce_thread_pc(struct thread* th, void** addr)
{
    uword_t* fp = __builtin_frame_address(0);
    char* return_pc = 0;

    if (th != get_sb_vm_thread()) {
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

/* Figure out which thread's control stack contains 'pointer'.
 * Also guess the PC within the active function in the referencing frame.
 * BUG: what about context registers? */
static struct thread* NO_SANITIZE_ADDRESS NO_SANITIZE_MEMORY
deduce_thread(uword_t pointer, char** pc)
{
    struct thread *th;

    *pc = 0;
    for_each_thread(th) {
        lispobj* stackptr = get_stackptr(th);
        for ( ; stackptr < th->control_stack_end ; ++stackptr ) {
            lispobj* obj = valid_ambiguous_pointer_p(*stackptr, 0);
            if (obj && compute_lispobj(obj) == pointer) {
                *pc = deduce_thread_pc(th, (void**)stackptr);
                return th;
            }
        }
    }
    return 0;
}
#endif

static int non_nil_symbolp(lispobj x) {
    return lowtag_of(x) == OTHER_POINTER_LOWTAG
      && widetag_of((lispobj*)(x-OTHER_POINTER_LOWTAG)) == SYMBOL_WIDETAG;
}

static __attribute__((unused)) int tls_index_ok(lispobj tlsindex, struct vector* ignored_objects)
{
    if (ignored_objects) {
        int i;
        for (i = vector_len(ignored_objects)-1; i >= 0; --i) {
            lispobj x = ignored_objects->data[i];
            if (non_nil_symbolp(x) && tls_index_of(SYMBOL(x)) == tlsindex) return 0;
        }
    }
    return 1; // is OK
}

// Return any key in common between the two supplied tables
__attribute__((unused))
static uword_t table_intersect(struct hopscotch_table *tbl1,
                               struct hopscotch_table *tbl2)
{
    // Whichever table has fewer keys will be tbl1
    if (tbl2->count < tbl1->count) {
        struct hopscotch_table *temp;
        temp = tbl1;
        tbl1 = tbl2;
        tbl2 = temp;
    }
    int i;
    uword_t key;
    for_each_hopscotch_key(i, key, (*tbl1)) {
        if (hopscotch_containsp(tbl2, key)) return key;
    }
    return 0;
}

static lispobj examine_threads(struct hopscotch_table* targets,
                               struct hopscotch_table* stack_roots,
                               enum ref_kind *root_kind,
                               struct thread** root_thread,
                               char** thread_pc,
                               struct vector* ignored_objects,
                               lispobj *tls_index)
{
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    // Look in the control stacks
    uword_t root = table_intersect(targets, stack_roots);
    if (root) {
        *root_thread = deduce_thread(root, thread_pc);
        *root_kind = CONTROL_STACK;
        return root;
    }
#endif
    struct thread *th;
    for_each_thread(th) {
        lispobj *where, *end;
        // Examine thread-local storage
        *root_kind = TLS;
        where = &th->lisp_thread;
        end   = (lispobj*)((char*)th + SymbolValue(FREE_TLS_INDEX,0));
        for ( ; where < end ; ++where)
            if (interestingp(*where, targets)
                && tls_index_ok((char*)where - (char*)th, ignored_objects)) {
                *root_thread = th;
                *tls_index = (char*)where - (char*)th;
                return *where;
            }
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
#endif
    }
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

// No longer using compressed pointers by default
#if 1
static inline traceroot_pointer encode_pointer(lispobj pointer)
{
    return pointer;
}
static inline lispobj decode_pointer(traceroot_pointer encoding)
{
    return encoding;
}
#else
/// "Compressed" pointers are a huge win - they halve the amount
/// of space required to invert the heap.
static inline traceroot_pointer encode_pointer(lispobj pointer)
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
    if (find_text_page_index((void*)pointer) >= 0) {
        // A text space pointer is stored as a count in doublewords
        // from the base address.
        encoding = (pointer - TEXT_SPACE_START) / (2*N_WORD_BYTES);
        gc_assert(encoding <= 0x3FFFFFFF);
        // bit pattern #b10 signifies text space compressed ptr.
        return (encoding<<2) | 2;
    } else
#endif
    {
        // Everything else is stored as-is.
        gc_assert(pointer <= 0xFFFFFFFF && !(pointer & 3));
        return pointer; // bit pattern #b00 signifies uncompressed ptr
    }
}

static inline lispobj decode_pointer(traceroot_pointer encoding)
{
    if (encoding & 1)  // Compressed ptr to dynamic space
        return (encoding>>1)*(2*N_WORD_BYTES) + DYNAMIC_SPACE_START;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    else if ((encoding & 3) == 2) // Compressed ptr to text space
        return (encoding>>2)*(2*N_WORD_BYTES) + TEXT_SPACE_START;
#endif
    else
        return encoding; // Literal pointer
}
#endif

static void maybe_show_object_name(lispobj obj, FILE* stream)
{
    lispobj package, package_name;
    if (lowtag_of(obj)==OTHER_POINTER_LOWTAG)
        switch(widetag_of(native_pointer(obj))) {
        case SYMBOL_WIDETAG:
            putc(',', stream);
            package = symbol_package(SYMBOL(obj));
            if (package == NIL) {
                fprintf(stream, "#:");
            } else {
                package_name = ((struct package*)native_pointer(package))->_name;
                safely_show_lstring(VECTOR(package_name), 0, stream);
                fputs("::", stream);
            }
            safely_show_lstring(symbol_name(SYMBOL(obj)), 0, stream);
            break;
        }
}

static bool root_p(lispobj ptr, int criterion)
{
    if (ptr <= STATIC_SPACE_END) return 1; // always a root
    // 0 to 2 are in order of weakest to strongest condition for stopping,
    // i.e. criterion 0 implies that that largest number of objects
    // are considered roots.
    return criterion < 2
        && (traceroot_gen_of(ptr) > (criterion ? HIGHEST_NORMAL_GENERATION
                           : gencgc_oldest_gen_to_gc));
}

// Make a cons (or 'liststar2')
static lispobj mkcons(lispobj car, lispobj cdr)
{
    struct cons *cons = (struct cons*)
        gc_general_alloc(cons_region, sizeof(struct cons), PAGE_TYPE_CONS);
    cons->car = car;
    cons->cdr = cdr;
    return make_lispobj(cons, LIST_POINTER_LOWTAG);
}
static lispobj liststar3(lispobj x, lispobj y, lispobj z) {
  return mkcons(x, mkcons(y, z));
}
static lispobj make_sap(char* value)
{
    struct sap *sap = (struct sap*)
        gc_general_alloc(unboxed_region, sizeof(struct sap), PAGE_TYPE_UNBOXED);
    sap->header = (1<<N_WIDETAG_BITS) | SAP_WIDETAG;
    sap->pointer = value;
    return make_lispobj(sap, OTHER_POINTER_LOWTAG);
}

/// Find any shortest path to 'object' starting at a tenured object or a thread stack.
/// Return the path as a list, or 0 if a path could not be found.
static lispobj trace1(lispobj object,
                      struct hopscotch_table* targets,
                      struct hopscotch_table* visited,
                      inverted_heap_t graph,
                      struct scratchpad* scratchpad,
                      struct vector* ignored_objects,
                      struct hopscotch_table* stack_roots,
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
    while ((thread_ref = examine_threads(targets, stack_roots,
                                         &root_kind, &root_thread, &thread_pc,
                                         ignored_objects, &tls_index)) == 0) {
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
            traceroot_pointer list = inverted_heap_get(graph, target);
            if (heap_trace_verbose>1) {
                traceroot_pointer list1 = list;
                fprintf(stderr, "target=%p srcs=", (void*)target);
                while (list1) {
                    traceroot_pointer* cell = (traceroot_pointer*)(scratchpad->base + list1);
                    lispobj* ptr = (lispobj*)decode_pointer(cell[0]);
                    if (hopscotch_containsp(visited, (lispobj)ptr))
                        fprintf(stderr, "%p ", ptr);
                    else {
                        sword_t nwords = object_size(ptr);
                        fprintf(stderr, "%p+%d ", ptr, (int)nwords);
                    }
                    list1 = cell[1];
                }
                putc('\n',stderr);
            }
            while (list && !anchor) {
                traceroot_pointer* cell = (traceroot_pointer*)(scratchpad->base + list);
                lispobj ptr = decode_pointer(cell[0]);
                list = cell[1];
                if (hopscotch_containsp(visited, ptr))
                    continue;
                int wordindex = find_ref((lispobj*)ptr, target);
                if (wordindex == -1) {
                    if (heap_trace_verbose) {
                        fprintf(stderr, "Strange: no ref from %p to %p\n",
                                (void*)ptr, (void*)target);
                    }
                    continue;
                }
                hopscotch_insert(visited, ptr, 1);
                add_to_layer((lispobj*)ptr, wordindex,
                             top_layer, &layer_capacity);
                // Stop if the object at 'ptr' is tenured.
                if (root_p(ptr, criterion)) {
                    if (heap_trace_verbose) {
                        fprintf(stderr, "Stopping at %p: tenured\n", (void*)ptr);
                    }
                    anchor = &top_layer->nodes[top_layer->count-1];
                }
            }
        }
        if (!top_layer->count) {
            if (heap_trace_verbose) {
                fprintf(stderr, "Failure tracing from %p. Current targets:\n", (void*)object);
                for_each_hopscotch_key(i, target, (*targets)) {
                    fprintf(stderr, "%p", (void*)target);
                    fprintf(stderr, "(g%d,", traceroot_gen_of(target));
                    fputs(classify_obj(target), stderr);
                    maybe_show_object_name(target, stderr);
                    fprintf(stderr,") ");
                }
                putc('\n', stderr);
            }
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

    lispobj path_node = NIL;
    if (thread_ref) {
  #if 0
        char *ref_kind_name[4] = {"heap","C stack","bindings","TLS"};
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
        if (root_kind==BINDING_STACK || root_kind==TLS) {
            path_node = liststar3(make_fixnum(root_kind),
                                  (lispobj)root_thread,
                                  tls_index);
        } else {
            path_node = liststar3(make_fixnum(root_kind),
                                  (lispobj)root_thread,
                                  make_sap(thread_pc));
        }
    } else { // Stopped at (pseudo)static object
        if (heap_trace_verbose) {
            fprintf(stderr, "Anchor object is @ %p. word[%d]\n",
                    native_pointer(anchor->object), anchor->wordindex);
        }
        path_node = liststar3(0, 0, 0);
    }

    lispobj path = mkcons(path_node, NIL);
    target = thread_ref;
    while (top_layer) {
        struct node next = *anchor;
        lispobj ptr = next.object;
        path = mkcons(mkcons(ptr, make_fixnum(next.wordindex)), path);
        target = native_pointer(ptr)[next.wordindex];
        /* Special-case a few combinations of <type,wordindex>.
         * And don't assume that each special case is uniquely identified
         * by a wordindex. Coincidentally they are, but it would be incredibly
         * unmaintainable to assume that */
        if (next.wordindex == 0 && (instancep(ptr) || functionp(ptr))) {
            target = instance_layout(native_pointer(ptr));
        }
#if FUN_SELF_FIXNUM_TAGGED
        else if (next.wordindex == 1 && functionp(ptr)
                 && widetag_of(native_pointer(ptr)) == CLOSURE_WIDETAG) {
            target = fun_taggedptr_from_self(target);
        }
#endif
        else if (next.wordindex == 3 && lowtag_of(ptr) == OTHER_POINTER_LOWTAG &&
                 widetag_of(&FDEFN(ptr)->header) == FDEFN_WIDETAG) {
            target = decode_fdefn_rawfun((struct fdefn*)native_pointer(ptr));
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
    return path;
}

// Add 'source' to the list of objects keyed by 'target' in the inverted heap.
// Note that 'source' has no lowtag, and 'target' does.
// Pointer compression is used: the linked list of source objects
// is built using offsets into the scratchpad rather than absolute addresses.
// Return 1 if and only if 'target' was actually added to the graph.
static bool record_ptr(lispobj* source, lispobj target, struct scan_state* ss)
{
    if (!ss->keep_leaves) { // expected case
        if (!listp(target) &&
            leaf_obj_widetag_p(widetag_of(native_pointer(target)))) return 0;
    }
    target = canonical_obj(target);
    traceroot_pointer* new_cell = (traceroot_pointer*)ss->scratchpad.free;
    traceroot_pointer* next = new_cell + 2;
    gc_assert((char*)next <= ss->scratchpad.end);
    ss->scratchpad.free = (char*)next;
    if (ss->scratchpad.free > ss->scratchpad.end) lose("undersized scratchpad");
    new_cell[0] = encode_pointer((lispobj)source);
    traceroot_pointer* valref = inverted_heap_get_ref(ss->inverted_heap, target);
    new_cell[1] = *valref;
    *valref = (traceroot_pointer)((char*)new_cell - ss->scratchpad.base);
    return 1;
}

#define relevant_ptr_p(x) \
 (find_page_index((void*)(x))>=0||immobile_space_p((lispobj)(x))||readonly_space_p(x))

#define COUNT_POINTER(x) { ++n_scanned_words; \
      if (!is_lisp_pointer(x)) ++n_immediates; \
      else if (relevant_ptr_p(x)) ++n_pointers; }

#define check_ptr(x) { \
    if (count_only) COUNT_POINTER(x) \
    else if (is_lisp_pointer(x) && relevant_ptr_p(x)) record_ptr(where,x,ss); }

static bool ignorep(lispobj* base_ptr, lispobj ignored_objects)
{
    int i;
    for (i = vector_len(VECTOR(ignored_objects))-1; i >= 0; --i)
      if (native_pointer(VECTOR(ignored_objects)->data[i]) == base_ptr)
          return 1;
    return 0;
}

static uword_t build_refs(lispobj* where, lispobj* end,
                          struct scan_state* ss)
{
    lispobj layout;
    sword_t nwords, scan_limit, i;
    uword_t n_objects = 0, n_scanned_words = 0,
            n_immediates = 0, n_pointers = 0;

    bool count_only = !ss->record_ptrs;
    for (where = next_object(where, 0, end) ; where ; where = next_object(where, nwords, end)) {
        if (ss->ignored_objects && ignorep(where, ss->ignored_objects)) {
            nwords = object_size(where);
            continue;
        }
        ++n_objects;
        lispobj word = *where;
        if (!is_header(word)) {
            nwords = 2;
            check_ptr(where[0]);
            check_ptr(where[1]);
            continue;
        }
        nwords = scan_limit = headerobj_size2(where, word);
        int widetag = header_widetag(word);
        if (leaf_obj_widetag_p(widetag)) continue;
        switch (widetag) {
        case INSTANCE_WIDETAG:
        case FUNCALLABLE_INSTANCE_WIDETAG:
            // mixed boxed/unboxed objects
            layout = layout_of(where);
            check_ptr(layout);
            // Partially initialized instance can't have nonzero words yet
            if (layout) {
                struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
                // FIXME: check lockfree_list_node_p() also
                for(i=1; i<scan_limit; ++i)
                    if (bitmap_logbitp(i-1, bitmap)) check_ptr(where[i]);
            }
            continue;
#if FUN_SELF_FIXNUM_TAGGED
        case CLOSURE_WIDETAG:
            check_ptr(fun_taggedptr_from_self(((struct closure*)where)->fun));
            break;
#endif
        case CODE_HEADER_WIDETAG:
            scan_limit = code_header_words((struct code*)where);
            break;
        case SYMBOL_WIDETAG: {
            // I think it's OK to omit 'package'. It's seldom interesting
            // to discover a path involving a symbol back to its package.
            struct symbol* s = (void*)where;
            check_ptr(s->value); check_ptr(s->info); check_ptr(s->fdefn);
            check_ptr(decode_symbol_name(s->name));
            continue;
            }
        case FDEFN_WIDETAG:
            check_ptr(decode_fdefn_rawfun((struct fdefn*)where));
            scan_limit = 3;
            break;
        case SIMPLE_VECTOR_WIDETAG:
            // For weak hash-table vectors, a <k,v> pair may participate in
            // a path from the root only if it does not involve whichever
            // object is definitely weak. This fails on weak key-OR-value
            // tables since we can't decide whether to allow the entry.
            if (vector_flagp(*where, VectorWeak)) { // if any kind of weak vector
                // If not a hashtable, just skip it.
                if (!vector_flagp(*where, VectorHashing)) continue;
                lispobj* data = where + 2;
                int kv_vector_len = vector_len((struct vector*)where);
                lispobj lhash_table = data[kv_vector_len-1];
                gc_assert(instancep(lhash_table));
                struct hash_table* hash_table =
                  (struct hash_table *)native_pointer(lhash_table);
                int weakness = hashtable_weakness(hash_table);
                int high_water_mark = fixnum_value(data[0]);
                // FIXME: this is supposed to do what for weak-AND or weak-OR ?
                if (weakness == 1 || weakness == 2) { // 1=key, 2=value
                    // Skip the first 2 data words
                    int start = (weakness == 1) ? 3 : 2;
                    int end = 2 * (high_water_mark + 1);
                    for(i=start; i<end; i+=2) check_ptr(data[i]);
                    continue;
                }
            }
            break;
        case FILLER_WIDETAG: continue;
        default:
            if (!(other_immediate_lowtag_p(widetag) && LOWTAG_FOR_WIDETAG(widetag)))
              lose("Unknown widetag %x", widetag);
            // Skip irrelevant objects.
            if ((widetag == WEAK_POINTER_WIDETAG) || /* do not follow! */
                // These numeric types contain pointers, but are uninteresting.
                (widetag == COMPLEX_RATIONAL_WIDETAG) ||
                (widetag == RATIO_WIDETAG))
                continue;
        }
        if (widetag == SIMPLE_VECTOR_WIDETAG && ss->record_ptrs) {
            // Try to eliminate some duplicate edges in the reversed graph.
            // This is only a heuristic and will not eliminate all duplicate edges.
            // It helps for vectors which get initialized like #(#:FOO #:FOO ...)
            // by storing only one backpointer to #:FOO.
            // It is not particularly helpful for other objects.
            lispobj prev_interesting_ptr = 0;
            for(i=1; i<scan_limit; ++i) {
                lispobj pointer = where[i];
                if (is_lisp_pointer(pointer)
                    && relevant_ptr_p(pointer)
                    && pointer != prev_interesting_ptr
                    && record_ptr(where,pointer,ss))
                    prev_interesting_ptr = pointer;
            }
        } else {
            for(i=1; i<scan_limit; ++i) check_ptr(where[i]);
        }
    }
    if (count_only) {
        ss->n_objects += n_objects;
        ss->n_scanned_words += n_scanned_words;
        ss->n_immediates += n_immediates;
        ss->n_pointers += n_pointers;
    }
    return 0;
}
#undef check_ptr

#define show_tally(b,a,space) /* "before" and "after" */     \
  if(heap_trace_verbose && !ss->record_ptrs) \
    fprintf(stderr, space " space: %ld objs, %ld ptrs, %ld immediates\n", \
            a->n_objects - b.n_objects, a->n_pointers - b.n_pointers, \
            (a->n_scanned_words - a->n_pointers) - (b.n_scanned_words - b.n_pointers))

static void scan_spaces(struct scan_state* ss)
{
    struct scan_state old = *ss;
    build_refs((lispobj*)NIL_SYMBOL_SLOTS_START, (lispobj*)NIL_SYMBOL_SLOTS_END, ss);
    build_refs((lispobj*)STATIC_SPACE_OBJECTS_START, static_space_free_pointer, ss);
    show_tally(old, ss, "static");
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    old = *ss; build_refs((lispobj*)FIXEDOBJ_SPACE_START, fixedobj_free_pointer, ss);
    show_tally(old, ss, "fixedobj");
    old = *ss; build_refs((lispobj*)TEXT_SPACE_START, text_space_highwatermark, ss);
    show_tally(old, ss, "text");
#endif
    old = *ss;
    walk_generation((uword_t(*)(lispobj*,lispobj*,uword_t))build_refs,
                    -1, (uword_t)ss);
    show_tally(old, ss, "dynamic");
}

#define HASH_FUNCTION HOPSCOTCH_HASH_FUN_MIX

static void* compute_heap_inverse(bool keep_leaves,
                                  lispobj ignored_objects,
                                  struct scratchpad* scratchpad)
{
    struct scan_state ss;
    memset(&ss, 0, sizeof ss);
    ss.ignored_objects = ignored_objects;
    ss.keep_leaves = keep_leaves;
    if (heap_trace_verbose) fprintf(stderr, "Pass 1: Counting heap objects...\n");
    scan_spaces(&ss);
    // Guess at the initial size of ~ .5 million objects.
    int size = 1<<19; // flsl(tot_n_objects); this would work if you have it
    while (ss.n_objects > size) size <<= 1;
    if (heap_trace_verbose) {
        fprintf(stderr, "Pass 2: Inverting heap. Initial size=%d objects\n", size);
    }
#if TRACEROOT_USE_ABSL_HASHMAP
    void* inverted_heap = new_absl_hashmap(size);
    ss.inverted_heap = inverted_heap;
#else
    ss.inverted_heap = malloc(sizeof(struct hopscotch_table));
    hopscotch_create(ss.inverted_heap, HASH_FUNCTION,
                     sizeof(traceroot_pointer),
                     size /* initial size */, 0 /* default hop range */);
#endif
    // Add one pointer due to inability to use the first
    // two words of the scratchpad.
    uword_t scratchpad_min_size = (1 + ss.n_pointers) * 2 * sizeof (traceroot_pointer);
    int pagesize = os_reported_page_size;
    uword_t scratchpad_size = ALIGN_UP(scratchpad_min_size, pagesize);
    ss.scratchpad.base = os_allocate(scratchpad_size);
    gc_assert(ss.scratchpad.base);
    ss.scratchpad.free = ss.scratchpad.base + 2 * sizeof(traceroot_pointer);
    ss.scratchpad.end  = ss.scratchpad.base + scratchpad_size;
    if (heap_trace_verbose) {
        fprintf(stderr, "Scratchpad: %lu bytes\n", (long unsigned)scratchpad_size);
    }
#ifdef HAVE_GETRUSAGE
    struct rusage before, after;
    getrusage(RUSAGE_SELF, &before);
#endif
    ss.record_ptrs = 1;
    scan_spaces(&ss);
    *scratchpad = ss.scratchpad;
#ifdef HAVE_GETRUSAGE
    getrusage(RUSAGE_SELF, &after);
    // We're done building the necessary structure. Show some memory stats.
    if (heap_trace_verbose) {
        int count;
        int capacity;
#if TRACEROOT_USE_ABSL_HASHMAP
        count = absl_hashmap_size(ss.inverted_heap, &capacity);
#else
        count = ss.inverted_heap->count;
        capacity = 1+hopscotch_max_key_index(*ss.inverted_heap);
#endif
#define timediff(b,a,field) \
        ((a.field.tv_sec-b.field.tv_sec)*1000000+(a.field.tv_usec-b.field.tv_usec))
        float stime = timediff(before, after, ru_stime);
        float utime = timediff(before, after, ru_utime);
        fprintf(stderr,
                "Inverted heap: ct=%d, cap=%d, LF=%f ET=%f+%f sys+usr=%f\n",
                count, capacity, 100*(float)count / (capacity>0?capacity:1),
                stime/1000000, utime/1000000, (stime+utime)/1000000);
    }
#endif
    return ss.inverted_heap;
};

// FIXME: precise registers don't use this yet
__attribute__((unused)) static void add_to_roots(os_context_register_t word, void* arg)
{
    lispobj* obj = valid_ambiguous_pointer_p(word, 1);
    // fprintf(stderr, "  reg %lx -> %p\n", word, obj);
    if (obj) hopscotch_put(arg, compute_lispobj(obj), 1);
}

extern void visit_context_registers(void (*proc)(os_context_register_t, void*),
                                    os_context_t *context, void*);

/* Return number of sought objects that had paths to them.
 * Return -1 for invalid input.
 * This must be called inside WITHOUT-GCING. */
int NO_SANITIZE_ADDRESS NO_SANITIZE_MEMORY
gc_pathfind_aux(lispobj* stackptr, lispobj input, lispobj results,
                lispobj ignored_objects, int criterion)
{
    cur_thread_stackptr_at_entry = (uword_t)stackptr;
    int n_inputs = 0, n_live = 0, n_bad = 0, n_imm = 0;
    /* If 'inputs' does NOT contain any leaf object, then all leaf objects
     * can be omitted from the inverted heap as they can't point to anything.
     * Start by assuming that leaves need not be included in the search space.
     * This forces far more work that should be required, because if finding
     * a path to a leaf object, the inverse heap should include *only* the
     * pointerless objects that are in 'inputs', and not all such objects */
    int include_leaves = 0;
    lispobj list;
    for (list = input ; list != NIL && listp(list) ; list = CONS(list)->cdr) {
        ++n_inputs;
        lispobj car = CONS(list)->car;
        if ((lowtag_of(car) != OTHER_POINTER_LOWTAG ||
             widetag_of(native_pointer(car)) != WEAK_POINTER_WIDETAG)) {
            ++n_bad;
            continue;
        }
        lispobj wpval = ((struct weak_pointer*)native_pointer(car))->value;
        if (is_lisp_pointer(wpval)) {
            ++n_live;
            if (!listp(wpval) && leaf_obj_widetag_p(widetag_of(native_pointer(wpval))))
                include_leaves = 1;
        } else if (wpval != UNBOUND_MARKER_WIDETAG)
            ++n_imm;
    }
    if (!listp(list) || n_bad || lowtag_of(results) != OTHER_POINTER_LOWTAG
        || widetag_of(native_pointer(results)) != SIMPLE_VECTOR_WIDETAG) {
        if (heap_trace_verbose) {
            fprintf(stderr, "; Bad value in liveness tracker\n");
        }
        return -1;
    }
    if (heap_trace_verbose) {
        fprintf(stderr, "; Liveness tracking: %d/%d live objects", n_live, n_inputs);

        if (n_imm)
            fprintf(stderr, " (ignored %d non-pointers)", n_imm);
        putc('\n', stderr);
    }
    if (!n_live) return 0;
    struct thread *th;
    for_each_thread(th) gc_close_thread_regions(th, 0);
    ensure_region_closed(code_region, PAGE_TYPE_CODE);

    struct hopscotch_table roots;
    hopscotch_create(&roots, HOPSCOTCH_HASH_FUN_DEFAULT, 0, 32, 0);
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    th = get_sb_vm_thread();
    // Compute all the roots from stack (+ context registers)
    for_each_thread(th) {
        lispobj* stackptr = get_stackptr(th);
        os_context_t* context = get_register_context(th);
        /* fprintf(stderr, "thread %p stack range %p..%p\n", th, stackptr, th->control_stack_end); */
        if (context) visit_context_registers(add_to_roots, context, &roots);
        for ( ; stackptr < th->control_stack_end ; ++stackptr ) {
            lispobj word = *stackptr;
            lispobj *obj = valid_ambiguous_pointer_p(word, 0);
            if (obj) {
                if (heap_trace_verbose>3)
                    fprintf(stderr, "  %p: %"OBJ_FMTX" = %"OBJ_FMTX" %s\n", stackptr, word,
                            compute_lispobj(obj),
                            lowtag_of(compute_lispobj(obj))==LIST_POINTER_LOWTAG?"":
                            widetag_names[widetag_of(obj)>>2]);
                hopscotch_put(&roots, compute_lispobj(obj), 1);
            }
        }
    }
#else
    for_each_thread(th) {
        lispobj* where = th->control_stack_start;
        lispobj* end = access_control_stack_pointer(th);
        for ( ; where < end ; ++where) {
            lispobj word = *where;
            lispobj *obj = valid_ambiguous_pointer_p(word, 0);
            if (obj) {
                hopscotch_put(&roots, compute_lispobj(obj), 1);
            }
        }
    }
#endif
    // Compute the heap inverse
    struct scratchpad scratchpad;
    void* inverted_heap = compute_heap_inverse(include_leaves, ignored_objects,
                                               &scratchpad);

    // A hashset of all objects in the reverse reachability graph so far
    struct hopscotch_table visited;  // *Without* lowtag
    // A hashset of objects in the current graph layer
    struct hopscotch_table targets;  // With lowtag
    int n_found = 0; // how many objects had paths to them
    hopscotch_create(&visited, HASH_FUNCTION, 0, 32, 0);
    hopscotch_create(&targets, HASH_FUNCTION, 0, 32, 0);
    lispobj weak_pointers = input;
    int i = 0;
    do {
        gc_assert(i < vector_len(VECTOR(results)));
        lispobj car = CONS(weak_pointers)->car;
        lispobj value = ((struct weak_pointer*)native_pointer(car))->value;
        weak_pointers = CONS(weak_pointers)->cdr;
        if (is_lisp_pointer(value)) {
            if (heap_trace_verbose)
                fprintf(stderr, "Target=%p (%s)\n", (void*)value, classify_obj(value));
            hopscotch_reset(&visited);
            hopscotch_reset(&targets);
            lispobj path = trace1(canonical_obj(value),
                                  &targets, &visited,
                                  inverted_heap, &scratchpad,
                                  // could be NULL so don't use VECTOR() cast on it
                                  (struct vector*)native_pointer(ignored_objects),
                                  &roots, criterion);
            lispobj* elt = VECTOR(results)->data + i;
            vector_notice_pointer_store(elt);
            if ((*elt = path) != 0) ++n_found;
        }
        ++i;
    } while (weak_pointers != NIL);
    cur_thread_stackptr_at_entry = 0;

    ensure_region_closed(unboxed_region, PAGE_TYPE_UNBOXED);
    ensure_region_closed(cons_region, PAGE_TYPE_CONS);

    os_deallocate(scratchpad.base, scratchpad.end-scratchpad.base);
#if TRACEROOT_USE_ABSL_HASHMAP
    absl_hashmap_destroy(inverted_heap);
#else
    hopscotch_destroy(inverted_heap);
    free(inverted_heap);
#endif
    hopscotch_destroy(&visited);
    hopscotch_destroy(&targets);

    return n_found;
}

int gc_pathfind(lispobj input, lispobj results, lispobj ignored_objects,
                int criterion)
{
    // Approximate the stack pointer by using an incoming arg's address
    return gc_pathfind_aux(&input,
                           input, results, ignored_objects,
                           criterion);
}
