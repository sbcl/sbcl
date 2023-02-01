#include <stdio.h>
#include "tlsf_utils.h"

int test_ffs_fls()
{
    /* Verify ffs/fls work properly. */
    int rv = 0;
    rv += (tlsf_ffs(0) == -1) ? 0 : 0x1;
    rv += (tlsf_fls(0) == -1) ? 0 : 0x2;
    rv += (tlsf_ffs(1) == 0) ? 0 : 0x4;
    rv += (tlsf_fls(1) == 0) ? 0 : 0x8;
    rv += (tlsf_ffs(0x80000000) == 31) ? 0 : 0x10;
    rv += (tlsf_ffs(0x80008000) == 15) ? 0 : 0x20;
    rv += (tlsf_fls(0x80000008) == 31) ? 0 : 0x40;
    rv += (tlsf_fls(0x7FFFFFFF) == 30) ? 0 : 0x80;

#if defined(TLSF_64BIT)
    rv += (tlsf_fls_sizet(0x80000000) == 31) ? 0 : 0x100;
    rv += (tlsf_fls_sizet(0x100000000) == 32) ? 0 : 0x200;
    rv += (tlsf_fls_sizet(0xffffffffffffffff) == 63) ? 0 : 0x400;
#endif

    if (rv)
        printf("test_ffs_fls: %x ffs/fls tests failed.\n", rv);
    return rv;
}

int main()
{
    return test_ffs_fls();
}
