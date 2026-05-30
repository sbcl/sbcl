#ifndef SBCL_FIBER_X86_64_H
#define SBCL_FIBER_X86_64_H

#include <stdint.h>

/* Callee-saved register set: RBX, RBP, R12-R15, MXCSR. RSP is first
 * because the fallback switcher fiber-swap-regs is reached via CALL,
 * so the callee's return PC sits at [RSP] when the save happens. The
 * %swap-regs VOP pushes RESUME before saving so the same shape works.
 */
struct sb_fiber_regs {
    void    *rsp;     /* 0x00 */
    void    *rbx;     /* 0x08 */
    void    *rbp;     /* 0x10 */
    void    *r12;     /* 0x18 */
    void    *r13;     /* 0x20 */
    void    *r14;     /* 0x28 */
    void    *r15;     /* 0x30 */
    uint32_t mxcsr;   /* 0x38 */
    uint32_t pad_;    /* 0x3C */
};

#endif /* SBCL_FIBER_X86_64_H */
