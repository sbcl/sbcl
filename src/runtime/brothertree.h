#include "lispobj.h"

struct binary_node {
    lispobj header;
#ifndef LISP_FEATURE_COMPACT_INSTANCE_HEADER
    lispobj layout;
#endif
    uword_t key;
    lispobj left, right;
};
struct unary_node {
    lispobj header;
#ifndef LISP_FEATURE_COMPACT_INSTANCE_HEADER
    lispobj layout;
#endif
    lispobj child;
};

extern uword_t brothertree_find_lesseql(uword_t key, lispobj tree);
