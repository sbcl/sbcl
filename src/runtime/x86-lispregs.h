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
#define NREGS   (8)

#ifdef __ASSEMBLER__
#define REG(num) $ ## num
#else
#define REG(num) num
#endif

#define reg_EAX REG( 0)
#define reg_ECX REG( 2)
#define reg_EDX REG( 4)
#define reg_EBX REG( 6)
#define reg_ESP REG( 8)
#define reg_EBP REG(10)
#define reg_ESI REG(12)
#define reg_EDI REG(14)
#define reg_UESP REG(16)

#define REGNAMES "EAX", "ECX", "EDX", "EBX", "ESP", "EBP", "ESI", "EDI", "UESP"

/* reg_SP = stack pointer, reg_FP = frame pointer */
#define reg_SP reg_ESP
#define reg_FP reg_EBP
