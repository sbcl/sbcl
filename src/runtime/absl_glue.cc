#include "absl/container/flat_hash_map.h"
#include "murmur_hash.h"

typedef unsigned long lispobj;

struct Hash {
    size_t operator() (lispobj k) const { FMIX64(k); return k; }
};

typedef absl::flat_hash_map<lispobj,unsigned,Hash> inverted_heap;

extern "C" {
void* new_absl_hashmap(int size) {
    inverted_heap* hashmap = new inverted_heap;
    hashmap->reserve(size);
    return hashmap;
}
void absl_hashmap_destroy(inverted_heap* hashmap) {
    delete hashmap;
}
unsigned absl_hashmap_get(inverted_heap* hashmap, lispobj key) {
    return (*hashmap)[key];
}
unsigned* absl_hashmap_get_ref(inverted_heap* hashmap, lispobj key) {
    return &(*hashmap)[key];
}
int absl_hashmap_size(inverted_heap* hashmap, int* capacity) {
    *capacity = (*hashmap).bucket_count();
    return (*hashmap).size();
}

}
