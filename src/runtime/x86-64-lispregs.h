/*
 * These register names and offsets correspond to definitions in
 * compiler/x86/vm.lisp. They map into accessors in the OS-dependent
 * POSIX signal context structure os_context_t via the
 * os_context_register_addr(..) OS-dependent function.
 */

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

/* the number of registers visible as registers in the virtual machine
 * (excludes stuff like segment registers) */
#define NREGS   (16)

#ifdef LANGUAGE_ASSEMBLY
#define REG(num) $ ## num
#else
#define REG(num) num
#endif

#define reg_RAX REG( 0)
#define reg_RCX REG( 2)
#define reg_RDX REG( 4)
#define reg_RBX REG( 6)
#define reg_RSP REG( 8)
#define reg_RBP REG(10)
#define reg_RSI REG(12)
#define reg_RDI REG(14)
#define reg_R8  REG(16)
#define reg_R9  REG(18)
#define reg_R10 REG(20)
#define reg_R11 REG(22)
#define reg_R12 REG(24)
#define reg_R13 REG(26)
#define reg_R14 REG(28)
#define reg_R15 REG(30)

#define REGNAMES "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI", \
        "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"

/* classification of registers
 *
 * reg_SP = the register used by Lisp as stack pointer
 * reg_FP = the register used by Lisp as frame pointer
 * BOXED_REGISTERS =
 *   the registers which may contain Lisp object pointers */
#define reg_SP reg_RSP
#define reg_FP reg_RBP
#define BOXED_REGISTERS {\
  reg_RAX, reg_RCX, reg_RDX, reg_RBX, reg_RSI, reg_RDI \
}
