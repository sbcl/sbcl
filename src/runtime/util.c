/*
 * miscellaneous utilities
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#include <stdlib.h>
#include "util.h"

/*
 * voidacc stuff
 *
 * The interface is documented in the header file.
 *
 * Note that we never end up needing to explicitly set errno, not even
 * for malloc failure, because Unix98 guarantees errno=ENOMEM for us
 * automatically in that case.
 */

int
voidacc_ctor(struct voidacc *va)
{
    va->n_used = 1;  /* We begin with 1 entry already for zero termination. */
    va->n_avail = 4; /* arbitrary initial value */
    va->result = (void**)calloc(sizeof(void*), va->n_avail);
    return va->result ? 0 : (-1);
}

int
voidacc_acc(struct voidacc *va, void* x)
{
    /* Ensure that we have enough space, or die. */
    if (va->n_used >= va->n_avail) { /* if we've run out of space */
        /* We need to allocate more space. */
        int new_n_avail = 1 + 2 * va->n_avail;
        void** new_result = (void**)calloc(sizeof(void*), new_n_avail);
        int i;
        if (!new_result) {
            return 1;
        }
        /* Copy old result into new space. */
        for (i = va->n_used; --i >= 0; ) {
            new_result[i] = va->result[i];
        }
        free(va->result);
        va->result = new_result;
        va->n_avail = new_n_avail;
    }

    /* If we get to this point, we have enough space to store x.
     *
     * Note that since we cleverly counted the 0 as part of the space
     * used, now we need to subtract one to get the correct offset to
     * write into.:-| */
    va->result[va->n_used++ - 1] = x;
    return 0;
}

void**
voidacc_give_away_result(struct voidacc *va)
{
    /* (We could do realloc(3) here to try to shrink the result down
     * to minimum size, but that's not really needed for the
     * directory-iteration application this was originally written
     * for, so for now we just do the simplest thing which could
     * possibly work.) */
    void **result_tmp = va->result;
    va->result = 0;
    return result_tmp;
}

void
voidacc_dtor(struct voidacc *va)
{
    /* if result has been allocated and hasn't been given away */
    if (va->result) {
        free(voidacc_give_away_result(va));
    }
}
