#ifndef INCLUDED_tlsf_utils
#define INCLUDED_tlsf_utils

#include <stdint.h>

/*
** Architecture-specific bit manipulation routines.
**
** TLSF achieves O(1) cost for malloc and free operations by limiting
** the search for a free block to a free list of guaranteed size
** adequate to fulfill the request, combined with efficient free list
** queries using bitmasks and architecture-specific bit-manipulation
** routines.
**
** Most modern processors provide instructions to count leading zeroes
** in a word, find the lowest and highest set bit, etc. These
** specific implementations will be used when available, falling back
** to a reasonably efficient generic implementation.
**
** NOTE: TLSF spec relies on ffs/fls returning value 0..31.
** ffs/fls return 1-32 by default, returning 0 for error.
*/

/*
** Detect whether or not we are building for a 32- or 64-bit (LP/LLP)
** architecture. There is no reliable portable method at compile-time.
*/
#if defined(__alpha__) || defined(__ia64__) || defined(__x86_64__) || \
    defined(_WIN64) || defined(__LP64__) || defined(__LLP64__) ||     \
    defined(__aarch64__)
#define TLSF_64BIT
#endif

#if defined(__GNUC__)

/* count leading zeroes:
 *   clz(0) == 32, clz(0xf) == 28, clz(1 << 31) == 0
 */
static inline int clz(uint32_t x)
{
    return x ? __builtin_clz(x) : sizeof(x) * 8;
}

/* integer binary logarithm (rounding down):
 *   log2(0) == -1, log2(5) == 2
 */
static inline int __log2(uint32_t x)
{
    return sizeof(x) * 8 - clz(x) - 1;
}

/* find first set:
 *   __ffs(1) == 0, __ffs(0) == -1, __ffs(1<<31) == 31
 */
static inline int tlsf_ffs(unsigned int word)
{
    return __log2(word & (uint32_t)(-(uint32_t)word));
}

static inline int tlsf_fls(unsigned int word)
{
    const int bit = word ? 32 - __builtin_clz(word) : 0;
    return bit - 1;
}

#elif defined(_MSC_VER) && (_MSC_VER >= 1400) && \
    (defined(_M_IX86) || defined(_M_X64))
/* Microsoft Visual C++ support on x86/X64 architectures. */

#include <intrin.h>

#pragma intrinsic(_BitScanReverse)
#pragma intrinsic(_BitScanForward)

static inline int tlsf_fls(unsigned int word)
{
    unsigned long index;
    return _BitScanReverse(&index, word) ? index : -1;
}

static inline int tlsf_ffs(unsigned int word)
{
    unsigned long index;
    return _BitScanForward(&index, word) ? index : -1;
}

#elif defined(_MSC_VER) && defined(_M_PPC)
/* Microsoft Visual C++ support on PowerPC architectures. */

#include <ppcintrinsics.h>

static inline int tlsf_fls(unsigned int word)
{
    const int bit = 32 - _CountLeadingZeros(word);
    return bit - 1;
}

static inline int tlsf_ffs(unsigned int word)
{
    const unsigned int reverse = word & (~word + 1);
    const int bit = 32 - _CountLeadingZeros(reverse);
    return bit - 1;
}

#elif defined(__ARMCC_VERSION)
/* RealView Compilation Tools for ARM */

static inline int tlsf_ffs(unsigned int word)
{
    const unsigned int reverse = word & (~word + 1);
    const int bit = 32 - __clz(reverse);
    return bit - 1;
}

static inline int tlsf_fls(unsigned int word)
{
    const int bit = word ? 32 - __clz(word) : 0;
    return bit - 1;
}

#elif defined(__ghs__)
/* Green Hills support for PowerPC */

#include <ppc_ghs.h>

static inline int tlsf_ffs(unsigned int word)
{
    const unsigned int reverse = word & (~word + 1);
    const int bit = 32 - __CLZ32(reverse);
    return bit - 1;
}

static inline int tlsf_fls(unsigned int word)
{
    const int bit = word ? 32 - __CLZ32(word) : 0;
    return bit - 1;
}

#else
/* Fall back to generic implementation. */

static inline int tlsf_fls_generic(unsigned int word)
{
    int bit = 32;

    if (!word)
        bit -= 1;
    if (!(word & 0xffff0000)) {
        word <<= 16;
        bit -= 16;
    }
    if (!(word & 0xff000000)) {
        word <<= 8;
        bit -= 8;
    }
    if (!(word & 0xf0000000)) {
        word <<= 4;
        bit -= 4;
    }
    if (!(word & 0xc0000000)) {
        word <<= 2;
        bit -= 2;
    }
    if (!(word & 0x80000000)) {
        word <<= 1;
        bit -= 1;
    }

    return bit;
}

/* Implement ffs in terms of fls. */
static inline int tlsf_ffs(unsigned int word)
{
    return tlsf_fls_generic(word & (~word + 1)) - 1;
}

static inline int tlsf_fls(unsigned int word)
{
    return tlsf_fls_generic(word) - 1;
}

#endif

/* Possibly 64-bit version of tlsf_fls. */
#if defined(TLSF_64BIT)
static inline int tlsf_fls_sizet(size_t size)
{
    int high = (int)(size >> 32);
    int bits = 0;
    if (high) {
        bits = 32 + tlsf_fls(high);
    } else {
        bits = tlsf_fls((int)size & 0xffffffff);
    }
    return bits;
}
#else
#define tlsf_fls_sizet tlsf_fls
#endif

#endif /* INCLUDED_tlsf_utils */
