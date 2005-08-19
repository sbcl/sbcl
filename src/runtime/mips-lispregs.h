#ifdef LANGUAGE_ASSEMBLY
#define REG(num) $ ## num
#else
#define REG(num) num
#endif

#define NREGS   (32)

#define reg_ZERO    REG(0)
#define reg_NL3     REG(1)
#define reg_CFUNC   REG(2)
#define reg_NL4     REG(3)
#define reg_NL0     REG(4)
#define reg_NL1     REG(5)
#define reg_NL2     REG(6)
#define reg_NARGS   REG(7)
#define reg_A0      REG(8)
#define reg_A1      REG(9)
#define reg_A2      REG(10)
#define reg_A3      REG(11)
#define reg_A4      REG(12)
#define reg_A5      REG(13)
#define reg_FDEFN   REG(14)
#define reg_LEXENV  REG(15)
#define reg_NFP     REG(16)
#define reg_OCFP    REG(17)
#define reg_LRA     REG(18)
#define reg_L0      REG(19)
#define reg_NIL     REG(20)
#define reg_BSP     REG(21)
#define reg_CFP     REG(22)
#define reg_CSP     REG(23)
#define reg_L1      REG(24)
#define reg_ALLOC   REG(25)
#define reg_NSP     REG(29)
#define reg_CODE    REG(30)
#define reg_LIP     REG(31)

#define REGNAMES \
        "ZERO",         "NL3",          "CFUNC",        "NL4", \
        "NL0",          "NL1",          "NL2",          "NARGS", \
        "A0",           "A1",           "A2",           "A3", \
        "A4",           "A5",           "FDEFN",        "LEXENV", \
        "NFP",          "OCFP",         "LRA",          "L0", \
        "NIL",          "BSP",          "CFP",          "CSP", \
        "L1",           "ALLOC",        "K0",           "K1", \
        "GP",           "NSP",          "CODE",         "LIP"


#define BOXED_REGISTERS { \
    reg_A0, reg_A1, reg_A2, reg_A3, reg_A4, reg_A5, reg_FDEFN, reg_LEXENV, \
    reg_NFP, reg_OCFP, reg_LRA, reg_L0, reg_L1, reg_CODE \
}
