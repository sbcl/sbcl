#include "align.h"
#include "genesis/vector.h"
struct vector *symbol_name(struct symbol*);
lispobj symbol_package(struct symbol*);

#define decode_symbol_name(x) x
static inline void set_symbol_name(struct symbol*s, lispobj x) { s->name = x; }

static inline uint16_t symbol_package_id(struct symbol* s) {
#ifndef LISP_FEATURE_64_BIT
  return s->package_id >> N_FIXNUM_TAG_BITS;
#elif defined LISP_FEATURE_BIG_ENDIAN
  return *(uint16_t*)(4+(char*)s);
#elif defined LISP_FEATURE_X86_64
  return UNALIGNED_LOAD16(1+(char*)s);
# else
  return *(uint16_t*)(2+(char*)s);
#endif
}

#ifndef LISP_FEATURE_SB_THREAD
/* no threads: every symbol's tls_index is statically zero */
#  define tls_index_of(x) 0
#  define per_thread_value(sym, thread) sym->value
#else
#ifdef LISP_FEATURE_64_BIT
static inline unsigned int
tls_index_of(struct symbol *symbol) // untagged pointer
{
#ifdef LISP_FEATURE_LITTLE_ENDIAN
  return ((unsigned int*)symbol)[1];
#else
  return symbol->header >> 32;
#endif
}
#else
#  define tls_index_of(x) (x)->tls_index
#endif
#  define per_thread_value(sym,th) *(lispobj*)(tls_index_of(sym) + (char*)th)
#endif

#define NO_TLS_VALUE_MARKER (~(uword_t)0)

static inline lispobj
SymbolValue(lispobj tagged_symbol_pointer, void *thread)
{
    struct symbol *sym = SYMBOL(tagged_symbol_pointer);
    if (thread && tls_index_of(sym)) {
        lispobj r = per_thread_value(sym, thread);
        if (r != NO_TLS_VALUE_MARKER) return r;
    }
    return sym->value;
}

static inline void
SetSymbolValue(lispobj tagged_symbol_pointer,lispobj val, void *thread)
{
    struct symbol *sym = SYMBOL(tagged_symbol_pointer);
    if (thread && tls_index_of(sym)) {
        if (per_thread_value(sym, thread) != NO_TLS_VALUE_MARKER) {
            per_thread_value(sym, thread) = val;
            return;
        }
    }
    sym->value = val;
}

#ifdef LISP_FEATURE_SB_THREAD
/* write_TLS assigns directly into TLS causing the symbol to
 * be thread-local without saving a prior value on the binding stack. */
# define write_TLS(sym, val, thread) write_TLS_index(sym##_tlsindex, val, thread, _ignored_)
# define write_TLS_index(index, val, thread, sym) \
   *(lispobj*)(index + (char*)thread) = val
# define read_TLS(sym, thread) *(lispobj*)(sym##_tlsindex + (char*)thread)
# define bind_variable(sym, val, th) bind_tls_cell(sym##_tlsindex, val, th)
extern void bind_tls_cell(unsigned index, lispobj value, void *thread);
#else
# define write_TLS(sym, val, thread) SYMBOL(sym)->value = val
# define write_TLS_index(index, val, thread, sym) sym->value = val
# define read_TLS(sym, thread) SYMBOL(sym)->value
extern void bind_variable(lispobj symbol, lispobj value,void *thread);
#endif

extern void unbind(void *thread);
extern void unbind_to_here(lispobj *bsp,void *thread);

#if defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64 || defined LISP_FEATURE_ARM64
# define FUN_SELF_FIXNUM_TAGGED 1
# define fun_self_from_baseptr(simple_fun) (lispobj)simple_fun->insts
# define fun_self_from_taggedptr(funptr) funptr - FUN_POINTER_LOWTAG + 2*N_WORD_BYTES
# define fun_taggedptr_from_self(self) self - 2*N_WORD_BYTES + FUN_POINTER_LOWTAG
#else
# define FUN_SELF_FIXNUM_TAGGED 0
# define fun_self_from_baseptr(simple_fun) make_lispobj(simple_fun,FUN_POINTER_LOWTAG)
# define fun_self_from_taggedptr(funptr) funptr
# define fun_taggedptr_from_self(self) self
#endif

#ifdef LISP_FEATURE_LINKAGE_SPACE

#define LINKAGE_INDEX_MASK ((1<<N_LINKAGE_INDEX_BITS)-1)

static inline unsigned int fdefn_linkage_index(struct fdefn* f) {
    return (f->header >> 16) & LINKAGE_INDEX_MASK;
}
static inline unsigned int symbol_linkage_index(struct symbol* s) {
    // never look at NIL's hash slot's low bits
    if ((uword_t)s == (STATIC_SPACE_START + NIL_SYMBOL_SLOTS_OFFSET)) return 0;
    return s->hash & LINKAGE_INDEX_MASK;
}

static lispobj __attribute__((unused)) symbol_function(struct symbol* s) {
    return s->fdefn;
}
#define StaticSymbolFunction(x) fun_taggedptr_from_self(linkage_space[x##_fname_index])

#else

#include "genesis/static-symbols.h"
static lispobj __attribute__((unused)) symbol_function(struct symbol* symbol)
{
    if (symbol->fdefn && symbol->fdefn != NIL) return FDEFN(symbol->fdefn)->fun;
    return NIL;
}
#define StaticSymbolFunction(x) FdefnFun(x##_FDEFN)
/* Return 'fun' given a tagged pointer to an fdefn. */
static inline lispobj FdefnFun(lispobj fdefn) { return FDEFN(fdefn)->fun; }
#endif
extern lispobj decode_fdefn_rawfun(struct fdefn *fdefn);
