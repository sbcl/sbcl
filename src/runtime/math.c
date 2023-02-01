#include <stdlib.h>

/* Support for division by constant integers
 * using multiplication based on the the paper
 * "Division by Invariant Integers using Multiplication"
 * by Granlund and Montgomery.
 *
 * This particular expresion of the algorithms to find magic numbers is taken from
 * "The PowerPC Compiler Writer's Guide"
 * It can also be seen at
 * https://github.com/llvm/llvm-project/blob/main/llvm/lib/Support/DivisionByConstantInfo.cpp
 * which generalizes to arbitrary-precision integers as needed for C cross-compiling.
 * However, the formatting in that file is bad as it mistakes "2p" for "2^p"
 * so it's not the best reference on the actual algorithm's technical aspects.
 */

struct magics {
    int m; /* magic number */
    int s; /* shift amount */
};
struct magicu {
  unsigned int m; /* magic number */
  int a;          /* “add” indicator */
  int s;          /* shift amount */
};

void compute_sdiv_magic32(int d, struct magics* mag)
    /* must have 2 <= d <= 2^31-1 or -2^31 <= d <= -2 */
{
    int p;
    unsigned int ad, anc, delta, q1, r1, q2, r2, t;
    const unsigned int two31 = 2147483648; /* 2^31 */
    ad = abs(d);
    t = two31 + ((unsigned int)d >> 31);
    anc = t - 1 - t%ad;      /* absolute value of nc */
    p = 31;
    q1 = two31/anc;          /* initialize q1 = 2^p/abs(nc) */
    r1 = two31 - q1*anc;     /* initialize r1 = rem(2^p,abs(nc)) */
    q2 = two31/ad;           /* initialize q2 = 2^p/abs(d) */
    r2 = two31 - q2*ad;      /* initialize r2 = rem(2^p,abs(d)) */
    do {
        p = p + 1;
        q1 = 2*q1;           /* update q1 = 2^p/abs(nc) */
        r1 = 2*r1;           /* update r1 = rem(2^p/abs(nc)) */
        if (r1 >= anc) {     /* must be unsigned comparison */
            q1 = q1 + 1;
            r1 = r1 - anc;
        }
        q2 = 2*q2;           /* update q2 = 2^p/abs(d) */
        r2 = 2*r2;           /* update r2 = rem(2^p/abs(d)) */
        if (r2 >= ad) {
            q2 = q2 + 1;
            r2 = r2 - ad;
        }
        delta = ad - r2;
    } while (q1 < delta || (q1 == delta && r1 == 0));
    mag->m = q2 + 1;
    if (d < 0) mag->m = -mag->m; /* resulting magic number */
    mag->s = p - 32;             /* resulting shift */
}

void compute_udiv_magic32(unsigned int d, struct magicu* magu)
        /* must have 1 <= d <= 2^32-1 */
{
    int p;
    unsigned int nc, delta, q1, r1, q2, r2;
    magu->a = 0; /* initialize “add” indicator */
    nc = - 1 - (-d)%d;
    p = 31;
    q1 = 0x80000000/nc;       /* initialize q1 = 2^p/nc */
    r1 = 0x80000000 - q1*nc;  /* initialize r1 = rem(2^p,nc) */
    q2 = 0x7FFFFFFF/d;        /* initialize q2 = (2^p-1)/d */
    r2 = 0x7FFFFFFF - q2*d;   /* initialize r2 = rem((2^p-1),d) */
    do {
        p = p + 1;
        if (r1 >= nc - r1 ) {
            q1 = 2*q1 + 1;
            r1 = 2*r1 - nc;
        } else {
            q1 = 2*q1;
            r1 = 2*r1;
        }
        if (r2 + 1 >= d - r2) {
            if (q2 >= 0x7FFFFFFF) magu->a = 1;
            q2 = 2*q2 + 1;
            r2 = 2*r2 + 1 - d;
        } else {
            if (q2 >= 0x80000000) magu->a = 1;
            q2 = 2*q2;
            r2 = 2*r2 + 1;
        }
        delta = d - 1 - r2;
    } while (p < 64 && (q1 < delta || (q1 == delta && r1 == 0)));
    magu->m = q2 + 1; /* resulting magic number */
    magu->s = p - 32; /* resulting shift */
}

