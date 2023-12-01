#ifndef SBCL_INCLUDED_ALIGN_H
#define SBCL_INCLUDED_ALIGN_H

#include <stdint.h>
#include <string.h>
#include "genesis/sbcl.h"

#define ALIGN_UP(value,granularity) (((value)+((granularity)-1))&(~((granularity)-1)))
#define ALIGN_DOWN(value,granularity) (((value))&(~((granularity)-1)))
#define IS_ALIGNED(value,granularity) (0==(((value))&((granularity)-1)))

#define PTR_ALIGN_UP(pointer,granularity)                       \
    (typeof(pointer))ALIGN_UP((uintptr_t)pointer,granularity)

#define PTR_ALIGN_DOWN(pointer,granularity)                     \
    (typeof(pointer))ALIGN_DOWN((uintptr_t)pointer,granularity)

#define PTR_IS_ALIGNED(pointer,granularity)     \
    IS_ALIGNED((uintptr_t)pointer,granularity)

// For CPUs that can do unaligned memory operations, the C compiler
// is generally smart enough to not actually do a memcpy()
static inline uint16_t UNALIGNED_LOAD16(void* p) {
    uint16_t val;
    memcpy(&val, p, 2);
    return val;
}
static inline uint32_t UNALIGNED_LOAD32(void* p) {
    uint32_t val;
    memcpy(&val, p, 4);
    return val;
}
// 'volatile' works around a spurious GCC warning
static inline void UNALIGNED_STORE32(void* volatile p, uint32_t val) {
    memcpy(p, &val, 4);
}
#ifdef LISP_FEATURE_64_BIT
static inline void UNALIGNED_STORE64(void* p, uint64_t val) {
    memcpy(p, &val, 8);
}
#endif

#endif /* SBCL_INCLUDED_ALIGN_H */
