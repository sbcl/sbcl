/* -*- Mode: C -*- */

void gcbarrier_patch_code(void* where, int nbits)
{
    UNALIGNED_STORE32(where, ((1U<<nbits)-1));
}
