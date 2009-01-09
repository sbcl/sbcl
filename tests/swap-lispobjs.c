#include "arch.h"
#include "genesis/config.h"
#include "genesis/constants.h"
#include "runtime.h"
#include "target-arch.h"

#if defined(LISP_FEATURE_X86) || defined (LISP_FEATURE_X86_64)

int
try_to_zero_with_swap_lispobjs(volatile lispobj *word)
{
    /* GCC with high enough optimization settings optimizes away the
     * whole assembly if it is not marked as volatile. */
    swap_lispobjs(word,0);
    if (*word==0) {
        return 0;
    } else {
        return 1;
    }
}

#endif
