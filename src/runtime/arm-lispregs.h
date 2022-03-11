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


#define NREGS (16)

#ifdef __ASSEMBLER__
#    define REG(num) r##num
#else
#    define REG(num) (num)
#endif

#define reg_R0          REG(0)
#define reg_R1          REG(1)
#define reg_R2          REG(2)
#define reg_LEXENV      REG(3)
#define reg_NL2         REG(4)
#define reg_CODE        REG(5)
#define reg_NL3         REG(6)
#define reg_OCFP        REG(7)
#define reg_R8          REG(8)
#define reg_NFP         REG(9)
#define reg_NULL        REG(10)
#define reg_CFP         REG(11)
#define reg_NARGS       REG(12)
#define reg_NSP         REG(13)
#define reg_LR          REG(14)
#define reg_PC          REG(15)
