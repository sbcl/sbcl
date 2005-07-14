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

#define NREGS   (32)

#ifdef LANGUAGE_ASSEMBLY
#ifdef linux
#define REG(num) $##num
#else
#define REG(num) $/**/num
#endif /* linux */
#else
#define REG(num) num
#endif
                                /* "traditional" register name and use */
                                /* courtesy of <alpha/regdef.h> */
#define reg_LIP REG(0)          /* v0 */
#define reg_A0 REG(1)           /* t0 - temporary (caller-saved) */
#define reg_A1 REG(2)           /* t1 */
#define reg_A2 REG(3)           /* t2 */
#define reg_A3 REG(4)           /* t3 */
#define reg_A4 REG(5)           /* t4 */
#define reg_A5 REG(6)           /* t5 */
#define reg_L0 REG(7)           /* t6 */
#define reg_NARGS REG(8)        /* t7 */
#define reg_CSP REG(9)          /* s0 - saved (callee-saved) */
#define reg_CFP REG(10)         /* s1 */
#define reg_OCFP REG(11)        /* s2 */
#define reg_BSP REG(12)         /* s3 */
#define reg_LEXENV REG(13)      /* s4 */
#define reg_CODE REG(14)        /* s5 */
#define reg_NULL REG(15)        /* s6 = fp (frame pointer) */
#define reg_NL0 REG(16)         /* a0 - argument (caller-saved) */
#define reg_NL1 REG(17)         /* a1 */
#define reg_NL2 REG(18)         /* a2 */
#define reg_NL3 REG(19)         /* a3 */
#define reg_NL4 REG(20)         /* a4 */
#define reg_NL5 REG(21)         /* a5 */
#define reg_ALLOC REG(22)       /* t8 - more temps (caller-saved) */
#define reg_FDEFN REG(23)       /* t9 */
#define reg_CFUNC REG(24)       /* t10 */
#define reg_NFP REG(25)         /* t11 */
#define reg_LRA REG(26)         /* ra - return address */
#define reg_L1 REG(27)          /* t12, or pv - procedure variable */
#define reg_L2 REG(28)          /* at - assembler temporary */
#define reg_GP REG(29)          /* global pointer */
#define reg_NSP REG(30)         /* sp - stack pointer */
#define reg_ZERO REG(31)        /* reads as zero, writes are noops */


#define REGNAMES \
    "LIP", "A0", "A1", "A2", "A3", "A4", "A5", "L0", "NARGS",  \
    "CSP", "CFP", "OCFP", "BSP", "LEXENV", "CODE", "NULL", \
    "NL0", "NL1", "NL2", "NL3", "NL4", "NL5", "ALLOC", "FDEFN", \
    "CFUNC", "NFP", "LRA", "L1", "L2", "GP", "NSP", "ZERO"

#define BOXED_REGISTERS { \
    reg_CODE, reg_FDEFN, reg_LEXENV, reg_NARGS, reg_OCFP, reg_LRA, \
    reg_A0, reg_A1, reg_A2, reg_A3, reg_A4, reg_A5, \
    reg_L0, reg_L1, reg_L2 \
}

#define call_into_lisp_LRA_page 0x10000
