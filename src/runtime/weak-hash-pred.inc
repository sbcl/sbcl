/* -*- Mode: C -*- */

static int survived_gc_yet_KEY(lispobj key, lispobj __attribute__((unused)) value) {
    return !is_lisp_pointer(key) || pointer_survived_gc_yet(key);
}
static int survived_gc_yet_VALUE(lispobj __attribute__((unused)) key, lispobj value) {
    return !is_lisp_pointer(value) || pointer_survived_gc_yet(value);
}
static int survived_gc_yet_AND(lispobj key, lispobj value) {
    int key_nonpointer = !is_lisp_pointer(key);
    int val_nonpointer = !is_lisp_pointer(value);
    if (key_nonpointer && val_nonpointer) return 1;
    return (key_nonpointer || pointer_survived_gc_yet(key))
        && (val_nonpointer || pointer_survived_gc_yet(value));
}
static int survived_gc_yet_OR(lispobj key, lispobj value) {
    int key_nonpointer = !is_lisp_pointer(key);
    int val_nonpointer = !is_lisp_pointer(value);
    if (key_nonpointer || val_nonpointer) return 1;
    // Both MUST be pointers
    return pointer_survived_gc_yet(key) || pointer_survived_gc_yet(value);
}

/* magic number. cf WEAK-HASH-TABLE-KINDS in 'hash-table.lisp' */
#define WEAKNESS_KEY_AND_VALUE 0
int (*HT_ENTRY_LIVENESS_FUN_ARRAY_NAME[4])(lispobj,lispobj) = {
    survived_gc_yet_AND,
    survived_gc_yet_KEY,
    survived_gc_yet_VALUE,
    survived_gc_yet_OR
};
