#define NREGS   (32)

#ifdef LANGUAGE_ASSEMBLY
#define REG(num) num
#else
#define REG(num) num
#endif

#define reg_ZERO REG(0)
#define reg_NFP REG(1)
#define reg_CFUNC REG(2)
#define reg_CSP REG(3)
#define reg_CFP REG(4)
#define reg_BSP REG(5)
#define reg_NULL REG(6)
#define reg_ALLOC REG(7)
#define reg_CODE REG(8)
#define reg_FDEFN REG(9)
#define reg_LEXENV REG(10)
#define reg_NARGS REG(11)
#define reg_OCFP REG(12)
#define reg_LRA REG(13)
#define reg_A0 REG(14)
#define reg_A1 REG(15)
#define reg_A2 REG(16)
#define reg_A3 REG(17)
#define reg_A4 REG(18)
#define reg_A5 REG(19)
#define reg_L0 REG(20)
#define reg_L1 REG(21)
#define reg_L2 REG(22)
#define reg_NL3 REG(23)
#define reg_NL2 REG(24)
#define reg_NL1 REG(25)
#define reg_NL0 REG(26)
#define reg_DP REG(27)
#define reg_NL4 REG(28)
#define reg_NL5 REG(29)
#define reg_NSP REG(30)
#define reg_LIP REG(31)


#define REGNAMES \
    "ZERO", "NFP", "CFUNC", "CSP", "CFP", "BSP", "NULL", "ALLOC", \
    "CODE", "FDEFN", "LEXENV", "NARGS", "OCFP", "LRA", "A0", "A1", \
    "A2", "A3", "A4", "A5", "L0", "L1", "L2", "NL3", \
    "NL2", "NL1", "NL0", "DP", "NL4", "NL5", "NSP", "LIP"

#define BOXED_REGISTERS { \
    reg_CODE, reg_FDEFN, reg_LEXENV, reg_OCFP, reg_LRA, \
    reg_A0, reg_A1, reg_A2, reg_A3, reg_A4, reg_A5, \
    reg_L0, reg_L1, reg_L2, reg_NFP \
}

