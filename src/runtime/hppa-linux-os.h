#ifndef _HPPA_LINUX_OS_H
#define _HPPA_LINUX_OS_H

typedef struct ucontext os_context_t;
/* FIXME: This will change if the parisc-linux people implement
   wide-sigcontext for 32-bit kernels */
typedef unsigned long os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

unsigned long os_context_fp_control(os_context_t *context);
void os_restore_fp_control(os_context_t *context);

#define SC_REG(sc, n) (((unsigned long *)((sc)->sc_ap))[n])
#define SC_PC(sc) ((sc)->sc_pcoqh)
#define SC_NPC(sc) ((sc)->sc_pcoqt)

#endif /* _HPPA_LINUX_OS_H */
