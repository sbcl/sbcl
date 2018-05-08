#ifndef _MIPS_ARCH_H
#define _MIPS_ARCH_H

#define ALIEN_STACK_GROWS_DOWNWARD

unsigned int arch_get_fp_control(void);
void arch_set_fp_control(unsigned int fp);

#endif /* _MIPS_ARCH_H */
