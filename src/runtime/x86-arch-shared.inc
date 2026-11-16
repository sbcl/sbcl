/* -*- Mode: C -*- */

char* memory_fault_emulation_trap;
void gcbarrier_patch_code(void* where, int nbits)
{
    UNALIGNED_STORE32(where, ((1U<<nbits)-1));
}
