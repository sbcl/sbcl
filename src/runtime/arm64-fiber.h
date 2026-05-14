#ifndef SBCL_FIBER_ARM64_H
#define SBCL_FIBER_ARM64_H

struct fiber_context {
    void   *sp;     /* 0x00 */
    void   *fp;     /* 0x08  x29 */
    void   *lr;     /* 0x10  x30 */
    void   *x19;    /* 0x18 */
    void   *x20;    /* 0x20 */
    void   *x21;    /* 0x28 */
    void   *x22;    /* 0x30 */
    void   *x23;    /* 0x38 */
    void   *x24;    /* 0x40 */
    void   *x25;    /* 0x48 */
    void   *x26;    /* 0x50 */
    void   *x27;    /* 0x58 */
    void   *x28;    /* 0x60 */
    double  d8;     /* 0x68 */
    double  d9;     /* 0x70 */
    double  d10;    /* 0x78 */
    double  d11;    /* 0x80 */
    double  d12;    /* 0x88 */
    double  d13;    /* 0x90 */
    double  d14;    /* 0x98 */
    double  d15;    /* 0xa0 */
};

#endif
