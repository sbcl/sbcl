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

#include <stdio.h>

#include "sbcl.h"
#include "arch.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "interrupt.h"
#include "lispregs.h"
#include "signal.h"
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"
#include "alloc.h"

#if defined(LISP_FEATURE_GENCGC)
#include "gencgc-alloc-region.h"
#endif

#ifdef LISP_FEATURE_SB_THREAD
#include "getallocptr.h"
#endif

  /* The header files may not define PT_DAR/PT_DSISR.  This definition
     is correct for all versions of ppc linux >= 2.0.30

     As of DR2.1u4, MkLinux doesn't pass these registers to signal
     handlers correctly; a patch is necessary in order to (partially)
     correct this.

     Even with the patch, the DSISR may not have its 'write' bit set
     correctly (it tends not to be set if the fault was caused by
     something other than a protection violation.)

     Caveat callers.  */

#if defined (LISP_FEATURE_DARWIN) || defined(LISP_FEATURE_LINUX)
#ifndef PT_DAR
#define PT_DAR          41
#endif

#ifndef PT_DSISR
#define PT_DSISR        42
#endif
#endif

#ifdef LISP_FEATURE_64_BIT
#define TRAP_INSTRUCTION(trap) ((2<<26) | (1 << 21) | reg_NULL << 16 | (trap))
#else
#define TRAP_INSTRUCTION(trap) ((3<<26) | (6 << 21) | (trap))
#endif


os_vm_address_t
arch_get_bad_addr(int sig, siginfo_t *code, os_context_t *context)
{
    os_vm_address_t addr;

#if defined(LISP_FEATURE_NETBSD) || defined(LISP_FEATURE_OPENBSD)
    addr = (os_vm_address_t) (code->si_addr);
#else
    addr = (os_vm_address_t) (*os_context_register_addr(context,PT_DAR));
#endif
    return addr;
}


void
arch_skip_instruction(os_context_t *context)
{
    char** pcptr;
    pcptr = (char**) os_context_pc_addr(context);
    *pcptr += 4;
}

unsigned char *
arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char *)(*os_context_pc_addr(context)+4);
}


boolean
arch_pseudo_atomic_atomic(os_context_t *context)
{
#ifdef LISP_FEATURE_SB_THREAD
    struct thread *thread = get_sb_vm_thread();

    if (foreign_function_call_active_p(thread)) {
        return get_pseudo_atomic_atomic(thread);
    } else return
#else
    /* FIXME: this foreign_function_call_active test is dubious at
     * best. If a foreign call is made in a pseudo atomic section
     * (?) or more likely a pseudo atomic section is in a foreign
     * call then an interrupt is executed immediately. Maybe it
     * has to do with C code not maintaining pseudo atomic
     * properly. MG - 2005-08-10
     *
     * The foreign_function_call_active used to live at each call-site
     * to arch_pseudo_atomic_atomic, but this seems clearer.
     * --NS 2007-05-15 */
    return (!foreign_function_call_active_p(get_sb_vm_thread())) &&
#endif
        ((*os_context_register_addr(context,reg_ALLOC)) & flag_PseudoAtomic);
}

void
arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
#ifdef LISP_FEATURE_SB_THREAD
    struct thread *thread = get_sb_vm_thread();

    if (foreign_function_call_active_p(thread)) {
        set_pseudo_atomic_interrupted(thread);
    } else
#endif
        *os_context_register_addr(context,reg_ALLOC)
            |= flag_PseudoAtomicInterrupted;
}

void
arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
#ifdef LISP_FEATURE_SB_THREAD
    struct thread *thread = get_sb_vm_thread();

    if (foreign_function_call_active_p(thread)) {
        clear_pseudo_atomic_interrupted(thread);
    } else
#endif
        *os_context_register_addr(context,reg_ALLOC)
            &= ~flag_PseudoAtomicInterrupted;
}

unsigned int
arch_install_breakpoint(void *pc)
{
    unsigned int *ptr = (unsigned int *)pc;
    unsigned int result = *ptr;
    *ptr = TRAP_INSTRUCTION(trap_Breakpoint);
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));
    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    *(unsigned int *)pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));
}

/*
 * Perform the instruction that we overwrote with a breakpoint.  As we
 * don't have a single-step facility, this means we have to:
 * - put the instruction back
 * - put a second breakpoint at the following instruction,
 *   set after_breakpoint and continue execution.
 *
 * When the second breakpoint is hit (very shortly thereafter, we hope)
 * sigtrap_handler gets called again, but follows the AfterBreakpoint
 * arm, which
 * - puts a bpt back in the first breakpoint place (running across a
 *   breakpoint shouldn't cause it to be uninstalled)
 * - replaces the second bpt with the instruction it was meant to be
 * - carries on
 *
 * Clear?
 */
static unsigned int *skipped_break_addr, displaced_after_inst;
static sigset_t orig_sigmask;

static boolean
should_branch(os_context_t *context, unsigned int orig_inst)
{
    /* orig_inst is a conditional branch instruction.  We need to
     * know if the branch will be taken if executed in context. */
    int ctr = *os_context_ctr_addr(context);
    int cr = *os_context_cr_addr(context);
    int bo_field = (orig_inst >> 21) & 0x1f;
    int bi_field = (orig_inst >> 16) & 0x1f;
    int ctr_ok;

    if (!(bo_field & 4)) ctr--; /* Decrement CTR if necessary. */

    ctr_ok = (bo_field & 4) || ((ctr == 0) == ((bo_field & 2) == 2));
    return ctr_ok && ((bo_field & 0x10) ||
                      !(((cr >> (31-bi_field)) ^ (bo_field >> 3)) & 1));
}

static sword_t sign_extend(uword_t word, int n_bits) {
  return (sword_t)(word<<(N_WORD_BITS-n_bits)) >> (N_WORD_BITS-n_bits);
}

void
arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    /* not sure how we ensure that we get the breakpoint reinstalled
     * after doing this -dan */
    unsigned int *pc = (unsigned int *)(*os_context_pc_addr(context));
    unsigned int *next_pc;
    int op = orig_inst >> 26;
    int sub_op = (orig_inst & 0x7fe) >> 1;  /* XL-form sub-opcode */

    orig_sigmask = *os_context_sigmask_addr(context);
    sigaddset_blockable(os_context_sigmask_addr(context));

    *pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));
    skipped_break_addr = pc;

    /* Figure out where we will end up after running the displaced
     * instruction by defaulting to the next instruction in the stream
     * and then checking for branch instructions.  FIXME: This will
     * probably screw up if it attempts to step a trap instruction. */
    next_pc = pc + 1;

    if (op == 18) {
        /* Branch  I-form */
        sword_t displacement = sign_extend(orig_inst & 0x03fffffc, 26);
        if (orig_inst & 2) { /* Absolute Address */
            next_pc = (unsigned int *)displacement;
        } else {
            next_pc = (unsigned int *)((uword_t)pc + displacement);
        }
    } else if ((op == 16)
               && should_branch(context, orig_inst)) {
        /* Branch Conditional  B-form */
        sword_t displacement = sign_extend(orig_inst & 0x0000fffc, 16);
        if (orig_inst & 2) { /* Absolute Address */
            next_pc = (unsigned int *)displacement;
        } else {
            next_pc = (unsigned int *)((uword_t)pc + displacement);
        }
    } else if ((op == 19) && (sub_op == 16)
               && should_branch(context, orig_inst)) {
        /* Branch Conditional to Link Register  XL-form */
        next_pc = (unsigned int *)
            ((*os_context_lr_addr(context)) & ~3);
    } else if ((op == 19) && (sub_op == 528)
               && should_branch(context, orig_inst)) {
        /* Branch Conditional to Count Register  XL-form */
        next_pc = (unsigned int *)
            ((*os_context_ctr_addr(context)) & ~3);
    }

    /* Set the "after" breakpoint. */
    displaced_after_inst = *next_pc;
    *next_pc = TRAP_INSTRUCTION(trap_AfterBreakpoint);
    os_flush_icache((os_vm_address_t)next_pc, sizeof(unsigned int));
}

#define INLINE_ALLOC_DEBUG 0
#ifdef LISP_FEATURE_CHENEYGC
#define handle_allocation_trap(x) (0)
#else
/*
 * Return non-zero if the current instruction is an allocation trap
 */
static int
allocation_trap_p(os_context_t * context)
{
    /*
     * First, the instruction has to be "Tx {LGT|LGE} temp, NL3, which has the
     * format.
     * | 6|  5| 5 | 5 | 10|1|  field width
     * ----------------------
     * |31| TO|dst|src|  4|0|  TW - trap word
     * |31| TO|dst|src| 68|0|  TD - trap doubleword
     *
     *   TO = #b00001 for LGT
     *        #b00101 for LGE
     */
    unsigned *pc = (unsigned int *) *os_context_pc_addr(context);
    unsigned inst = *pc;
    unsigned opcode = inst >> 26;
    unsigned src = (inst >> 11) & 0x1f;
    // unsigned dst = (inst >> 16) & 0x1f;
    unsigned to = (inst >> 21) & 0x1f;
    unsigned subcode = inst & 0x7ff;

    if (opcode == 31 && (to == 1 || to == 5) && src == reg_NL3
        && (subcode == 4<<1 || subcode == 68<<1)) {
        /* It doesn't much matter which trap option we pick for "could be large"
         * but I've chosen "TGE" because of the choices, that one is subject to spurious
         * failure, and I'd prefer not to spuriously fail on a 2-word cons.
         * (Spurious failure occurs when the EQ condition is met, meaning the allocation
         * would have worked, but the trap happens regardless) */
        int success = (to == 5) ? 1 : -1; // 1 = single-object page ok, -1 = not ok

        /*
         * We got the instruction.  Now, look back to make sure it was
         * proceeded by what we expected.  The previous instruction
         * should be an ADD or ADDI instruction.
         */
        unsigned int add_inst;

        add_inst = pc[-1];
        opcode = add_inst >> 26;
        if ((opcode == 31) && (266 == ((add_inst >> 1) & 0x1ff))) {
            return success;
        } else if ((opcode == 14)) {
            return success;
        } else {
            fprintf(stderr,
                    "Whoa! Got allocation trap but could not find ADD or ADDI instruction: 0x%08x in the proper place\n",
                    add_inst);
        }
    }
    return 0;
}

static int
handle_allocation_trap(os_context_t * context)
{
    int alloc_trap_p = allocation_trap_p(context);

    if (!alloc_trap_p) return 0;

    struct thread* thread = get_sb_vm_thread();
    gc_assert(!foreign_function_call_active_p(thread));
    if (gencgc_alloc_profiler && thread->state_word.sprof_enable)
        record_backtrace_from_context(context, thread);
    fake_foreign_function_call(context);
    unsigned int *pc = (unsigned int *) (*os_context_pc_addr(context));

    /*
     * Go back and look at the add/addi instruction.  The second src arg
     * is the size of the allocation.  Get it and call alloc to allocate
     * new space.
     */

    unsigned int inst = pc[-1];
    int target = (inst >> 21) & 0x1f;
    unsigned int opcode = inst >> 26;
#if INLINE_ALLOC_DEBUG
    fprintf(stderr, "  add inst  = 0x%08x, opcode = %d\n", inst, opcode);
#endif
    sword_t size = 0;
    if (opcode == 14) {
        /*
         * ADDI temp-tn, alloc-tn, size
         *
         * Extract the size
         */
        size = (inst & 0xffff);
    } else if (opcode == 31) {
        /*
         * ADD temp-tn, alloc-tn, size-tn
         *
         * Extract the size
         */
        int reg;

        reg = (inst >> 11) & 0x1f;
#if INLINE_ALLOC_DEBUG
        fprintf(stderr, "  add, reg = %s\n", lisp_register_names[reg]);
#endif
        size = *os_context_register_addr(context, reg);

    }

#if INLINE_ALLOC_DEBUG
    fprintf(stderr, "Alloc %d to %s\n", size, lisp_register_names[target]);
#endif

    /*
     * alloc-tn was incremented by size.  Need to decrement it by size
     * to restore its original value. This is not true on GENCGC
     * anymore. d_s_f_p and reg_alloc get out of sync, but the p_a
     * bits stay intact and we set it to the proper value when it
     * needs to be. Keep this comment here for the moment in case
     * somebody tries to figure out what happened here.
     */
    /*    dynamic_space_free_pointer =
        (lispobj *) ((long) dynamic_space_free_pointer - size);
    */

    char *memory;
    {
        extern lispobj *alloc(sword_t), *alloc_list(sword_t);
        struct interrupt_data *data = &thread_interrupt_data(thread);
        data->allocation_trap_context = context;
        memory = (char*)(alloc_trap_p < 0 ? alloc_list(size) : alloc(size));
        data->allocation_trap_context = 0;
    }

#if INLINE_ALLOC_DEBUG
    fprintf(stderr, "alloc returned %p\n", memory);
    fprintf(stderr, "free_pointer = %p\n", dynamic_space_free_pointer);
#endif

    /*
     * The allocation macro wants the result to point to the end of the
     * object!
     */
    memory += size;

#if INLINE_ALLOC_DEBUG
    fprintf(stderr, "object end at %p\n", memory);
#endif

    *os_context_register_addr(context, target) = (unsigned long) memory;
#ifndef LISP_FEATURE_SB_THREAD
    /* This is handled by the fake_foreign_function_call machinery on
     * threaded targets. */
    *os_context_register_addr(context, reg_ALLOC) =
      (unsigned long) dynamic_space_free_pointer
      | (*os_context_register_addr(context, reg_ALLOC)
         & LOWTAG_MASK);
#endif

    undo_fake_foreign_function_call(context);

    // Skip 2 instructions: the trap, and the writeback of free pointer
    (*os_context_pc_addr(context)) = (uword_t)(pc + 2);
    return 1; // handled
}
#endif

#if defined LISP_FEATURE_SB_THREAD
static int
handle_tls_trap(os_context_t * context, uword_t pc, unsigned int code)
{
#ifdef LISP_FEATURE_PPC64
# ifdef LISP_FEATURE_LITTLE_ENDIAN
#  define TLS_INDEX_FIELD_DISPLACEMENT (4-OTHER_POINTER_LOWTAG)
# else
#  define TLS_INDEX_FIELD_DISPLACEMENT (-OTHER_POINTER_LOWTAG)
# endif
#else
# define TLS_INDEX_FIELD_DISPLACEMENT \
    (offsetof(struct symbol,tls_index)-OTHER_POINTER_LOWTAG)
#endif

    /* LWZ ra,-k(rb) followed by TWI :eq ra,0 is the "unassigned symbol TLS" trap.
     * LWZ and TWI coincidentally have a similar format as follows:
     *      | 6|  5|  5| 16|  field width
     *      ----------------------
     * LWZ: |32| RT| RA|  D|
     * TWI: | 3| TO| RA|imm|  TO bits: EQ = 4
     *
     */

    boolean handle_it = 0;
    unsigned prev_inst;
    if ((code & ~(31 << 16)) == ((3<<26)|(4<<21))) { // mask out RA for test
        prev_inst= ((uint32_t*)pc)[-1];
        int16_t disp = (prev_inst & 0xFFFF);
        handle_it = (prev_inst >> 26) == 32 // a load
          // RT of the load must be RA of the trap
          && ((prev_inst >> 21) & 31) == ((code >> 16) & 31)
          && (disp == TLS_INDEX_FIELD_DISPLACEMENT);
    }
    if (!handle_it) return 0;

    struct thread *thread = get_sb_vm_thread();
    set_pseudo_atomic_atomic(thread);

    int symbol_reg = (prev_inst >> 16) & 31;
    struct symbol *specvar =
      SYMBOL(*os_context_register_addr(context, symbol_reg));
    struct symbol *free_tls_index = SYMBOL(FREE_TLS_INDEX);

    // *FREE-TLS-INDEX* value is [lock][tls-index]
    uword_t* pvalue = &free_tls_index->value;
    uword_t old;
    const uword_t spinlock_bit = (uword_t)1<<31;
    do {
        old = __sync_fetch_and_or(pvalue, spinlock_bit);
        if (old & spinlock_bit) sched_yield(); else break;
    } while (1);
    // sync_fetch_and_or acts as a barrier which prevents
    // speculatively loading tls_index_of().
    uint32_t tls_index = tls_index_of(specvar);
    if (tls_index != 0) { // someone else assigned
        free_tls_index->value = old; // just release the spinlock
        // fprintf(stderr, "TLS index trap: special var = %p, data race\n", specvar);
    } else {
        tls_index = old;
        // XXX: need to be careful here if GC uses any bits of the header
        // for concurrent marking. Would need to do a 4-byte write in that case.
        // This is simpler because it works for either endianness.
#ifdef LISP_FEATURE_PPC64
        specvar->header |= (uword_t)tls_index << 32;
#else
        specvar->tls_index = tls_index;
#endif
        // A barrier here ensures that nobody else thinks this symbol
        // doesn't have a TLS index.  compare-and-swap is the barrier.
        // It doesn't really need to be a CAS, because we hold the spinlock.
        int res = __sync_bool_compare_and_swap(pvalue,
                                               old | spinlock_bit,
                                               old + N_WORD_BYTES);
        gc_assert(res);
        // fprintf(stderr, "TLS index trap: special var = %p, assigned %x\n", specvar, tls_index);
    }
    // This is actually always going to be 0 for 64-bit code
    int tlsindex_reg = (code >> 16) & 31; // the register we trapped on
    *os_context_register_addr(context, tlsindex_reg) = tls_index;
    clear_pseudo_atomic_atomic(thread);
    return 1; // handled this signal
}
#endif

void
arch_handle_breakpoint(os_context_t *context)
{
    handle_breakpoint(context);
}

void
arch_handle_fun_end_breakpoint(os_context_t *context)
{
    *os_context_pc_addr(context) = (uword_t)handle_fun_end_breakpoint(context);
}

void
arch_handle_after_breakpoint(os_context_t *context)
{
    *skipped_break_addr = TRAP_INSTRUCTION(trap_Breakpoint);
    os_flush_icache((os_vm_address_t) skipped_break_addr,
                    sizeof(unsigned int));
    skipped_break_addr = NULL;
    *(unsigned int *)*os_context_pc_addr(context)
        = displaced_after_inst;
    *os_context_sigmask_addr(context)= orig_sigmask;
    os_flush_icache((os_vm_address_t) *os_context_pc_addr(context),
                    sizeof(unsigned int));
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    unsigned int code = *((uint32_t *)(*os_context_pc_addr(context)));
    int register_offset = code >> 8 & 0x1f;
    handle_single_step_trap(context, trap, register_offset);
    arch_skip_instruction(context);
}

#if 0
static void dump_cpu_state(char *reason, os_context_t* context)
{
    int i,j,r=0;
    sigset_t cur_sigset;
    char buf[100];
    sigset_t *sigset = os_context_sigmask_addr(context); sigset_tostring(sigset, buf, sizeof buf);
    fprintf(stderr, "%s\n", reason);
    fprintf(stderr, " oldmask=%s\n", buf);
    pthread_sigmask(0, 0, &cur_sigset); sigset_tostring(&cur_sigset, buf, sizeof buf);
    fprintf(stderr, " curmask=%s\n", buf);
    fprintf(stderr, "  $pc=%16lx  $lr=%16lx $ctr=%16lx  $cr=%16lx\n",
            *os_context_pc_addr(context),
            *os_context_lr_addr(context),
            *os_context_ctr_addr(context),
            *os_context_cr_addr(context));
    for(i=0; i<4; ++i) {
        for(j=0; j<8 && r<32; ++j,++r)
            fprintf(stderr, " %s%d=%16lx",
                    r<10 ? " $r" : "$r", r,
                    *os_context_register_addr(context, r));
        putchar('\n');
    }
}
#endif

static void
sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
  uword_t pc = *os_context_pc_addr(context);
  unsigned int code = *(uint32_t*)pc;

#ifdef LISP_FEATURE_SIGILL_TRAPS
    if (signal == SIGILL) {
        if (code == 0x7C0002A6) { // allocation region overflow trap
            // there is an actual trap instruction located 2 instructions later.
            // pretend the trap happened there.
            *os_context_pc_addr(context) = pc + 8;
            if (handle_allocation_trap(context)) return;
        }
        if (code == 0x7C2002A6) { // pending interrupt
            arch_clear_pseudo_atomic_interrupted(context);
            arch_skip_instruction(context);
            interrupt_handle_pending(context);
            return;
        }
        lose("sigill traps enabled but got unexpected sigill");
    }
#endif
    if (signal == SIGTRAP && handle_allocation_trap(context)) return;

#ifdef LISP_FEATURE_SB_THREAD
    if (signal == SIGTRAP && handle_tls_trap(context, pc, code)) return;
#endif

    if (code == ((3 << 26) | (0x18 << 21) | (reg_NL3 << 16))||
        /* trap instruction from do_pending_interrupt */
        code == 0x7fe00008) {
        arch_clear_pseudo_atomic_interrupted(context);
        arch_skip_instruction(context);
        /* interrupt or GC was requested in PA; now we're done with the
           PA section we may as well get around to it */
        interrupt_handle_pending(context);
        return;
    }
#ifdef LISP_FEATURE_64_BIT
    /* TDI LGT,$NULL,code */
    if ((code >> 16) == ((2 << 10) | (1 << 5) | reg_NULL)) {
        int trap = code & 0xff;
        handle_trap(context,trap);
        return;
    }
#else
    if ((code >> 16) == ((3 << 10) | (6 << 5))) {
        /* twllei reg_ZERO,N will always trap if reg_ZERO = 0 */
        int trap = code & 0xff;
        handle_trap(context,trap);
        return;
    }
#endif

    /* twi :ne ... or twi ... nargs */
    if (((code >> 26) == 3) && (((code >> 21) & 31) == 24
                                || ((code >> 16) & 31) == reg_NARGS
        )) {
        interrupt_internal_error(context, 0);
        return;
    }

    interrupt_handle_now(signal, (siginfo_t *)code, context);
}


void arch_install_interrupt_handlers()
{
    ll_install_handler(SIGILL, sigtrap_handler);
    ll_install_handler(SIGTRAP, sigtrap_handler);
}

void
ppc_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
  os_vm_address_t end = PTR_ALIGN_UP(address+length, 32);
  extern void ppc_flush_cache_line(os_vm_address_t);

  while (address < end) {
    ppc_flush_cache_line(address);
    address += 32;
  }
}

/* Linkage tables for PowerPC
 *
 * Linkage entry size is 16, because we need at least 4 instructions to
 * implement a jump.
 */

/*
 * Define the registers to use in the linkage jump table. Can be the
 * same. Some care must be exercised when choosing these. It has to be
 * a register that is not otherwise being used. reg_NFP is a good
 * choice. call_into_c trashes reg_NFP without preserving it, so we can
 * trash it in the linkage jump table.
 */
#define LINKAGE_TEMP_REG        reg_NFP
#define LINKAGE_ADDR_REG        reg_NFP

/*
 * Insert the necessary jump instructions at the given address.
 */
void
arch_write_linkage_table_entry(int index, void *target_addr, int datap)
{
  char *reloc_addr = (char*)LINKAGE_TABLE_SPACE_START + index * LINKAGE_TABLE_ENTRY_SIZE;
  if (datap) {
    *(unsigned long *)reloc_addr = (unsigned long)target_addr;
    return;
  }

#if defined LISP_FEATURE_64_BIT
  extern long call_into_c; // actually a function entry address,
  // but trick the compiler into thinking it isn't, so that it does not
  // indirect through a descriptor, but instead we get its logical address.
  if (target_addr != &call_into_c) {
#ifdef LISP_FEATURE_LITTLE_ENDIAN
      int* inst_ptr;
      unsigned long a0,a16,a32,a48;
      unsigned int inst;

      inst_ptr = (int*) reloc_addr;

      a48 = (unsigned long) target_addr >> 48 & 0xFFFF;
      a32 = (unsigned long) target_addr >> 32 & 0xFFFF;
      a16 = (unsigned long) target_addr >> 16 & 0xFFFF;
      a0 =  (unsigned long) target_addr       & 0xFFFF;


      /* addis 12, 0, a48 */

      inst = (15 << 26) | (12 << 21) | (0 << 16) | a48;
      *inst_ptr++ = inst;

      /* ori 12, 12, a32 */

      inst = (24 << 26) | (12 << 21) | (12 << 16) | a32;
      *inst_ptr++ = inst;

      /* sldi 12, 12, 32 */
      inst = (30 << 26) | (12 << 21) | (12 << 16) | 0x07C6;
      *inst_ptr++ = inst;

      /* oris 12, 12, a16 */

      inst = (25 << 26) | (12 << 21) | (12 << 16) | a16;
      *inst_ptr++ = inst;

      /* ori 12, 12, a0 */

      inst = (24 << 26) | (12 << 21) | (12 << 16) | a0;
      *inst_ptr++ = inst;

      /*
       * mtctr 12
       */

      inst = (31 << 26) | (12 << 21) | (9 << 16) | (467 << 1);
      *inst_ptr++ = inst;

      /*
       * bctr
       */

      inst = (19 << 26) | (20 << 21) | (528 << 1);
      *inst_ptr++ = inst;

      os_flush_icache((os_vm_address_t) reloc_addr, (char*) inst_ptr - reloc_addr);

#else
      // Could use either ABI, but we're assuming v1
      /* In the 64-bit v1 ABI, function pointers are alway passed around
       * as "function descriptors", not directly the jump target address.
       * A descriptor is 3 words:
       *   word 0 = address to jump to
       *   word 1 = value to place in r2
       *   word 2 = value to place in r11
       * For foreign calls, the value that we hand off to call_into_c
       * is therefore a function descriptor. To make things consistent,
       * this linkage table entry itself has to look like a function descriptor.
       * We can just copy the real descriptor to here, except in one case:
       * call_into_c is not itself an ABI-compatible call. It really should be
       * a lisp assembly routine, but then we have a turtles-all-the-way-down problem:
       * it's tricky to access C global data from lisp assembly routines.
       */
      memcpy(reloc_addr, target_addr, 24);
#endif
      return;
  }
  // Can't encode more than 32 bits of jump address
  gc_assert(((unsigned long) target_addr >> 32) == 0);
#endif

  /*
   * Make JMP to function entry.
   *
   * The instruction sequence is:
   *
   *        addis 13, 0, (hi part of addr)
   *        ori   13, 13, (low part of addr)
   *        mtctr 13
   *        bctr
   *
   */
  int* inst_ptr;
  unsigned long hi;                   /* Top 16 bits of address */
  unsigned long lo;                   /* Low 16 bits of address */
  unsigned int inst;

  inst_ptr = (int*) reloc_addr;

  hi = (unsigned long) target_addr;
  lo = hi & 0xffff;
  hi >>= 16;

  /*
   * addis 13, 0, (hi part)
   */

  inst = (15 << 26) | (LINKAGE_TEMP_REG << 21) | (0 << 16) | hi;
  *inst_ptr++ = inst;

  /*
   * ori 13, 13, (lo part)
   */

  inst = (24 << 26) | (LINKAGE_TEMP_REG << 21) | (LINKAGE_TEMP_REG << 16) | lo;
  *inst_ptr++ = inst;

  /*
   * mtctr 13
   */

  inst = (31 << 26) | (LINKAGE_TEMP_REG << 21) | (9 << 16) | (467 << 1);
  *inst_ptr++ = inst;

  /*
   * bctr
   */

  inst = (19 << 26) | (20 << 21) | (528 << 1);
  *inst_ptr++ = inst;

  os_flush_icache((os_vm_address_t) reloc_addr, (char*) inst_ptr - reloc_addr);
}

void gcbarrier_patch_code(void* where, int nbits)
{
#ifdef LISP_FEATURE_64_BIT
    int m_operand = 64 - nbits;
    // the M field has a kooky encoding
    int m_encoded = ((m_operand & 0x1F) << 1) | (m_operand >> 5);
    unsigned int* pc = where;
    unsigned int inst = *pc;
    // .... ____ _xxx xxx_ ____ = 0x7E0;
    //                  ^ deposit it here, in (BYTE 6 5) of the instruction.
    *pc = (inst & ~0x7E0) | (m_encoded << 5);
#else
    lose("can't patch rldicl in 32-bit"); // illegal instruction
#endif
}
