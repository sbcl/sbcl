#ifndef _MIPS_ARCH_H
#define _MIPS_ARCH_H


static inline void
get_spinlock(volatile lispobj *word, long value)
{
#ifdef LISP_FEATURE_SB_THREAD
    unsigned long __old = (volatile lispobj)*word;
    unsigned long __prev;
    int __cmp;

    __asm__ __volatile__ (
        "       .set push\n"
        "       .set mips2\n"
        "       .set noreorder\n"
        "1:     ll      %[__prev],%[__mem]\n"
        "       bne     %[__prev],%[__old],2f\n"
        "        li     %[__cmp],0\n"
        "       move    %[__cmp],%[__new]\n"
        "       sc      %[__cmp],%[__mem]\n"
        "       beqz    %[__cmp],1b\n"
        "        nop\n"
        "       sync\n"
        "2:\n"
        "       .set pop"
        : [__prev] "=&r" (__prev),
          [__cmp] "=&r" (__cmp)
        : [__mem] "R" (*word),
          [__old] "r" (__old),
          [__new] "r" (value)
        : "memory");

    if (!cmp)
        lose("recursive get_spinlock: 0x%x,%d\n", word, value);
#else /* LISP_FEATURE_SB_THREAD */
    *word=value;
#endif
}

static inline void
release_spinlock(volatile lispobj *word)
{
#ifdef LISP_FEATURE_SB_THREAD
    __asm__ __volatile__ (
        "       .set push\n"
        "       .set mips2\n"
        "       .set noreorder\n"
        "       sw      $0,%[__mem]\n"
        "       sync\n"
        "       .set pop"
        :
        : [__mem] "R" (*word)
        : "memory");
#else /* LISP_FEATURE_SB_THREAD */
    *word=0;
#endif
}

unsigned int arch_get_fp_control(void);
void arch_set_fp_control(unsigned int fp);

#endif /* _MIPS_ARCH_H */
