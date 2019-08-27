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


#define NREGS (32)

#ifdef __ASSEMBLER__
#define GREG(num) %g##num
#define OREG(num) %o##num
#define LREG(num) %l##num
#define IREG(num) %i##num

#else

#define GREG(num) (num)
#define OREG(num) ((num)+8)
#define LREG(num) ((num)+16)
#define IREG(num) ((num)+24)

#endif

#define reg_ZERO        GREG(0)
#define reg_ALLOC       GREG(1)
#define reg_NULL        GREG(2)
#define reg_CSP         GREG(3)
#define reg_CFP         GREG(4)
#define reg_BSP         GREG(5)
/* %g6 and %g7 are supposed to be reserved for the system */

#define reg_NL0         OREG(0)
#define reg_NL1         OREG(1)
#define reg_NL2         OREG(2)
#define reg_NL3         OREG(3)
#define reg_NL4         OREG(4)
#define reg_NL5         OREG(5)
#define reg_NSP         OREG(6)
#define reg_NARGS       OREG(7)

#define reg_A0          LREG(0)
#define reg_A1          LREG(1)
#define reg_A2          LREG(2)
#define reg_A3          LREG(3)
#define reg_A4          LREG(4)
#define reg_A5          LREG(5)
#define reg_OCFP        LREG(6)
#define reg_LRA         LREG(7)

#define reg_CNAME       IREG(0)
#define reg_LEXENV      IREG(1)
#define reg_L0          IREG(2)
#define reg_NFP         IREG(3)
#define reg_CFUNC       IREG(4)
#define reg_CODE        IREG(5)
#define reg_LIP         IREG(7)
