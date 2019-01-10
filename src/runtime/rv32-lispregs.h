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

#ifdef LANGUAGE_ASSEMBLY
#define REG(num) $ ## num
#else
#define REG(num) num
#endif

#define NREGS (32)

#define reg_ZERO     REG(0)
#define reg_LR       REG(1)
#define reg_NSP      REG(2)
#define reg_LRA      REG(3)
#define reg_CFP      REG(4)
#define reg_OCFP     REG(5)
#define reg_NFP      REG(6)
#define reg_CSP      REG(7)
#define reg_A0       REG(8)
#define reg_NL0      REG(9)
#define reg_A1       REG(10)
#define reg_NL1      REG(11)
#define reg_A2       REG(12)
#define reg_NL2      REG(13)
#define reg_A3       REG(14)
#define reg_NL3      REG(15)
#define reg_L0       REG(16)
#define reg_PA_FLAG  REG(17)
#define reg_LEXENV   REG(27)
#define reg_NULL     REG(28)
#define reg_CODE     REG(29)
#define reg_LIP      REG(30)
#define reg_NARGS    REG(31)

#define REGNAMES \
        "ZERO",         "LR",           "NSP",          "LRA", \
        "CFP",          "OCFP",         "NFP",          "CSP", \
        "A0",           "NL0",          "A1",           "NL1", \
        "A2",           "NL2",          "A3",           "NL3", \
        "L0",           "PA_FLAG",      "x18",          "x19", \
        "x20",          "x21",          "x22",          "x23", \
        "x24",          "x25",          "x26",          "LEXENV", \
        "NULL",         "CODE",         "LIP",          "NARGS"

#define BOXED_REGISTERS { \
  reg_A0, reg_A1, reg_A2, reg_A3, reg_LEXENV, \
    reg_NFP, reg_OCFP, reg_LRA, reg_L0, reg_CODE \
}
