#if defined LISP_FEATURE_DARWIN
#if defined __ASSEMBLER__
#define REG(num) r##num
#define FREG(num) f##num
#else
#define REG(num) num
#define FREG(num) num
#endif
#else
#define REG(num) num
#define FREG(num) num
#endif

#define NREGS 32

// We should probably eliminate uses of the wired 0 register and use r0
// as the NIL value. It's not wired to 0 in hardware, and we seldom use it in
// ways that would be sensitive to its unusual nature (effectively a wired 0)
// regarding load/store and some other instructions.
#define reg_ZERO      REG(0)    /* Should always contain 0 in lisp */
#define reg_NSP       REG(1)    /* The number/C stack pointer */
#define reg_TOC       REG(2)    /* ABI-reserved GOT + small data pointer */
#define reg_NL0       REG(3)    /* FF param/result 1 */
#define reg_NL1       REG(4)    /* FF param/result 2 */
#define reg_NL2       REG(5)    /* FF param 3 */
#define reg_NL3       REG(6)
#define reg_NL4       REG(7)
#define reg_NL5       REG(8)
#define reg_NL6       REG(9)    /* Last (7th) FF param */
#define reg_FDEFN     REG(10)   /* was NL7 until recently -dan */
#define reg_NARGS     REG(11)
#define reg_CFUNC     REG(12)   /* Silly to blow a reg on FF-name */
// r13 is as if it does not exist
#define reg_BSP       REG(14)   /* Binding stack pointer */
#define reg_CFP       REG(15)   /* Control/value stack frame pointer */
#define reg_CSP       REG(16)   /* Control/value stack top */
#define reg_ALLOC     REG(17)   /* (Global) dynamic free pointer */
#define reg_NULL      REG(18)   /* NIL and globals nearby */
#define reg_CODE      REG(19)   /* Current function object */
#define reg_NFP       REG(20)   /* Lisp may save around FF-call */
#define reg_LEXENV    REG(21)   /* And why burn a register for this ? */
#define reg_OCFP      REG(22)   /* The caller's reg_CFP */
#define reg_LRA       REG(23)   /* Tagged lisp return address */
#define reg_A0        REG(24)   /* First function arg/return value */
#define reg_A1        REG(25)   /* Second. */
#define reg_A2        REG(26)   /*  */
#define reg_A3        REG(27)   /* Last of (only) 4 arg regs */
#define reg_L0        REG(28)   /* Tagged temp regs */
#define reg_L1        REG(29)
#define reg_THREAD    REG(30)   /* TLS block pointer */
#define reg_LIP       REG(31)   /* Lisp Interior Pointer, e.g., locative */
