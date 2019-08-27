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

#ifdef __ASSEMBLER__
#define REG(num) $ ## num
#else
#define REG(num) num
#endif

#define reg_RAX REG( 0)
#define reg_RCX REG( 1)
#define reg_RDX REG( 2)
#define reg_RBX REG( 3)
#define reg_RSP REG( 4)
#define reg_RBP REG( 5)
#define reg_RSI REG( 6)
#define reg_RDI REG( 7)
#define reg_R8  REG( 8)
#define reg_R9  REG( 9)
#define reg_R10 REG(10)
#define reg_R11 REG(11)
#define reg_R12 REG(12)
#define reg_R13 REG(13)
#define reg_R14 REG(14)
#define reg_R15 REG(15)

/* reg_SP = stack pointer, reg_FP = frame pointer */
#define reg_SP reg_RSP
#define reg_FP reg_RBP
