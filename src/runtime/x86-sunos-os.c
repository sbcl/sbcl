#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include "sbcl.h"
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef LISP_FEATURE_SB_THREAD
#include <sys/segment.h>
#include <sys/sysi86.h>
#endif

#include "validate.h"

#ifdef LISP_FEATURE_SB_THREAD
pthread_mutex_t modify_ldt_lock = PTHREAD_MUTEX_INITIALIZER;

static int
ldt_index_selector (int index) {
  return index << 3 | 7;
}

static int
find_free_ldt_index () {
  struct ssd ssd;
  int usage[65536/sizeof(int)];
  int i;
  FILE *fp;

  memset(usage, 0, sizeof(usage));

  fp = fopen("/proc/self/ldt", "r");

  if (fp == NULL) {
    lose("Couldn't open /proc/self/ldt");
  }

  while (fread(&ssd, sizeof(ssd), 1, fp) == 1) {
    int index = ssd.sel >> 3;
    if (index >= 65536) {
      lose("segment selector index too large: %d", index);
    }

    usage[index / sizeof(int)] |= 1 << (index & (sizeof(int)-1));
  }

  fclose(fp);

  /* Magic number 7 is the first LDT index that Solaris leaves free. */
  for (i = 7; i < 65536; i++) {
    if (~usage[i / sizeof(int)] & (1 << (i & (sizeof(int)-1)))) {
      return i;
    }
  }

  lose("Couldn't find a free LDT index");
}

static int
install_segment (unsigned long start, unsigned long size) {
    int selector;

    ignore_value(mutex_acquire(&modify_ldt_lock));

    selector = ldt_index_selector(find_free_ldt_index());
    struct ssd ssd = { selector,
                       start,
                       size,
                       0xf2,
                       0x4};
    if (sysi86(SI86DSCR, &ssd) < 0) {
        lose("Couldn't install segment for thread-local data");
    }

    ignore_value(mutex_release(&modify_ldt_lock));

    return selector;
}
#endif

int arch_os_thread_init(struct thread *thread) {
  stack_t sigstack;

#ifdef LISP_FEATURE_SB_THREAD
  int sel = install_segment((unsigned long) thread, dynamic_values_bytes);

  FSHOW_SIGNAL((stderr, "/ TLS: Allocated LDT %x\n", sel));
  __asm__ __volatile__ ("mov %0, %%fs" : : "r"(sel));

  thread->tls_cookie = sel;
#endif

#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp    = calc_altstack_base(thread);
    sigstack.ss_flags = 0;
    sigstack.ss_size  = calc_altstack_size(thread);
    sigaltstack(&sigstack,0);
#endif
     return 1;                   /* success */
}

int arch_os_thread_cleanup(struct thread *thread) {
#if defined(LISP_FEATURE_SB_THREAD)
    int n = thread->tls_cookie;
    struct ssd delete = { n, 0, 0, 0, 0};

    /* Set the %%fs register back to 0 and free the ldt by setting it
     * to NULL.
     */
    FSHOW_SIGNAL((stderr, "/ TLS: Freeing LDT %x\n", n));

    __asm__ __volatile__ ("mov %0, %%fs" : : "r"(0));

    ignore_value(mutex_acquire(&modify_ldt_lock));
    if (sysi86(SI86DSCR, &delete) < 0) {
      lose("Couldn't remove segment");
    }
    ignore_value(mutex_release(&modify_ldt_lock));
#endif
    return 1;                   /* success */
}

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    /* Solaris x86 holds %esp value in UESP */
    switch(offset) {
    case reg_EAX: return &context->uc_mcontext.gregs[11];
    case reg_ECX: return &context->uc_mcontext.gregs[10];
    case reg_EDX: return &context->uc_mcontext.gregs[9];
    case reg_EBX: return &context->uc_mcontext.gregs[8];
    case reg_ESP: return &context->uc_mcontext.gregs[17]; /* REG_UESP */
    case reg_EBP: return &context->uc_mcontext.gregs[6];
    case reg_ESI: return &context->uc_mcontext.gregs[5];
    case reg_EDI: return &context->uc_mcontext.gregs[4];
    default: return 0;
    }
    return &context->uc_mcontext.gregs[offset];
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &(context->uc_mcontext.gregs[14]); /* REG_EIP */
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return &(context->uc_mcontext.gregs[17]); /* REG_UESP */
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &(context->uc_sigmask);
}

void os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    int *state = context->uc_mcontext.fpregs.fp_reg_set.fpchip_state.state;
    /* The STATE array is in the format used by the x86 instruction FNSAVE,
     * so the FPU control word is in the first 16 bits */
    int cw = (state[0] & 0xffff);
    int sw = context->uc_mcontext.fpregs.fp_reg_set.fpchip_state.status;
    return (cw ^ 0x3f) | (sw << 16);
}
