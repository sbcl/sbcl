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

#ifdef LANGUAGE_ASSEMBLY
#    define REG(num) x##num
#else
#    define REG(num) (num)
#endif

#define reg_NL0          REG(0)
#define reg_NL1          REG(1)
#define reg_NL2          REG(2)
#define reg_NL3          REG(3)
#define reg_NL4          REG(4)
#define reg_NL5          REG(5)
#define reg_NL6          REG(6)
#define reg_NL7          REG(7)
#define reg_NL8          REG(8)
#define reg_NL9          REG(9)

#define reg_R0         REG(10)
#define reg_R1         REG(11)
#define reg_R2         REG(12)
#define reg_R3         REG(13)
#define reg_R4         REG(14)
#define reg_R5         REG(15)
#define reg_R6         REG(16)
#define reg_R7         REG(17)
#define reg_R8         REG(18)
#define reg_R9         REG(19)

#ifdef LISP_FEATURE_SB_THREAD
#define reg_THREAD      REG(20)
#else
#define reg_R10         REG(20)
#endif
#define reg_LEXENV      REG(21)

#define reg_NARGS       REG(22)
#define reg_NFP         REG(23)
#define reg_OCFP        REG(24)
#define reg_CFP         REG(25)
#define reg_CSP         REG(26)
#define reg_TMP         REG(27)
#define reg_wTMP        w27
#define reg_NULL        REG(28)
#define reg_wNULL       w28
#define reg_CODE        REG(29)
#define reg_LR          REG(30)
#define reg_NSP         REG(31)

#ifdef LISP_FEATURE_SB_THREAD
#define REG10_NAME "THREAD"
#else
#define REG10_NAME "R10"
#endif

#define REGNAMES \
    "NL0", "NL1", "NL2", "NL3", "NL4", "NL5", "NL6", "NL7", "NL8", "NL9", \
    "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", REG10_NAME, \
    "LEXENV", "NARGS", "NFP", "OCFP", "CFP", "CSP", "TMP", "NULL", \
    "CODE", "LR", "NSP"

#define BOXED_REGISTERS { \
    reg_R0, reg_R1, reg_R2, reg_R3, reg_R4, reg_R5, reg_R6, \
    reg_R7, reg_R8, reg_R9, REG(20), reg_LEXENV, reg_CODE   \
}

