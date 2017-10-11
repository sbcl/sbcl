#ifndef SBCL_INCLUDED_ALIGN_H
#define SBCL_INCLUDED_ALIGN_H

#include <stdint.h>

#define ALIGN_UP(value,granularity) (((value)+(granularity-1))&(~(granularity-1)))
#define ALIGN_DOWN(value,granularity) (((value))&(~(granularity-1)))
#define IS_ALIGNED(value,granularity) (0==(((value))&(granularity-1)))

#define PTR_ALIGN_UP(pointer,granularity)                       \
    (typeof(pointer))ALIGN_UP((uintptr_t)pointer,granularity)

#define PTR_ALIGN_DOWN(pointer,granularity)                     \
    (typeof(pointer))ALIGN_DOWN((uintptr_t)pointer,granularity)

#define PTR_IS_ALIGNED(pointer,granularity)     \
    IS_ALIGNED((uintptr_t)pointer,granularity)

#endif /* SBCL_INCLUDED_ALIGN_H */
