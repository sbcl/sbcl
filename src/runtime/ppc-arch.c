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
#include "pseudo-atomic.h"
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
    struct thread *thread = arch_os_get_current_thread();

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
    return (!foreign_function_call_active_p(arch_os_get_current_thread())) &&
#endif
        ((*os_context_register_addr(context,reg_ALLOC)) & flag_PseudoAtomic);
}

void
arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
#ifdef LISP_FEATURE_SB_THREAD
    struct thread *thread = arch_os_get_current_thread();

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
    struct thread *thread = arch_os_get_current_thread();

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
    unsigned int *pc;
    unsigned inst;
    unsigned opcode;
    unsigned src;
    unsigned __attribute__((unused)) dst;

    /*
     * First, the instruction has to be a TWLGT temp, NL3, which has the
     * format.
     * | 6| 5| 5 | 5 | 10|1|  field width
     * |31| 1|dst|src|  4|0|  value
     */
    pc = (unsigned int *) (*os_context_pc_addr(context));
    inst = *pc;

#if INLINE_ALLOC_DEBUG
    fprintf(stderr, "allocation_trap_p at %p:  inst = 0x%08x\n", pc, inst);
#endif

    opcode = inst >> 26;
    src = (inst >> 11) & 0x1f;
    dst = (inst >> 16) & 0x1f;
    if ((opcode == 31) && (src == reg_NL3) && (1 == ((inst >> 21) & 0x1f))
        && (4 == ((inst >> 1) & 0x3ff))) {
        /*
         * We got the instruction.  Now, look back to make sure it was
         * proceeded by what we expected.  The previous instruction
         * should be an ADD or ADDI instruction.
         */
        unsigned int add_inst;

        add_inst = pc[-1];
#if INLINE_ALLOC_DEBUG
        fprintf(stderr, "   add inst at %p:  inst = 0x%08x\n",
                pc - 1, add_inst);
#endif
        opcode = add_inst >> 26;
        if ((opcode == 31) && (266 == ((add_inst >> 1) & 0x1ff))) {
            return 1;
        } else if ((opcode == 14)) {
            return 1;
        } else {
            fprintf(stderr,
                    "Whoa! Got allocation trap but could not find ADD or ADDI instruction: 0x%08x in the proper place\n",
                    add_inst);
        }
    }
    return 0;
}

#ifndef boxed_region
#define boxed_region gc_alloc_region[0]
#endif

static int
handle_allocation_trap(os_context_t * context)
{
    unsigned int *pc;
    unsigned int opcode;
    sword_t size = 0;
    char *memory;

    if (!allocation_trap_p(context)) return 0;
    gc_assert(!foreign_function_call_active_p(arch_os_get_current_thread()));
    fake_foreign_function_call(context);

    /*
     * Look at current instruction: TWNE temp, NL3. We're here because
     * temp > NL3 and temp is the end of the allocation, and NL3 is
     * current-region-end-addr.
     *
     * We need to adjust temp and alloc-tn.
     */

    pc = (unsigned int *) (*os_context_pc_addr(context));
#if 0
    inst = pc[0];
    end_addr = (inst >> 11) & 0x1f;
    target = (inst >> 16) & 0x1f;

    target_ptr = *os_context_register_addr(context, target);
#endif

#if INLINE_ALLOC_DEBUG
    fprintf(stderr, "handle_allocation_trap at %p:\n", pc);
    fprintf(stderr, "boxed_region.free_pointer: %p\n", boxed_region.free_pointer);
    fprintf(stderr, "boxed_region.end_addr: %p\n", boxed_region.end_addr);
    fprintf(stderr, "target reg: %d, end_addr reg: %ld\n", target, end_addr);
    fprintf(stderr, "target: %"OBJ_FMTX"\n",
            (uword_t)*os_context_register_addr(context, target));
    fprintf(stderr, "end_addr: %"OBJ_FMTX"\n",
            (uword_t)*os_context_register_addr(context, end_addr));
    fprintf(stderr, "trap inst = 0x%08x\n", inst);
    fprintf(stderr, "target reg = %s\n", lisp_register_names[target]);
#endif

    /*
     * Go back and look at the add/addi instruction.  The second src arg
     * is the size of the allocation.  Get it and call alloc to allocate
     * new space.
     */

    unsigned int inst = pc[-1];
    int target = (inst >> 21) & 0x1f;
    opcode = inst >> 26;
#if INLINE_ALLOC_DEBUG
    fprintf(stderr, "  add inst  = 0x%08x, opcode = %d\n", inst, opcode);
#endif
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
    if ((((unsigned long)boxed_region.end_addr + size) / GENCGC_CARD_BYTES) ==
        (((unsigned long)boxed_region.end_addr) / GENCGC_CARD_BYTES)) {
      fprintf(stderr,"*** possibly bogus trap allocation of %d bytes at %p\n",
              size, (void*)target_ptr);
      fprintf(stderr, "    dynamic_space_free_pointer: %p, boxed_region.end_addr %p\n",
              dynamic_space_free_pointer, boxed_region.end_addr);
    }
    fprintf(stderr, "Ready to alloc\n");
    fprintf(stderr, "free_pointer = %p\n", dynamic_space_free_pointer);
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

    {
        extern lispobj* alloc(sword_t);
        struct interrupt_data *data =
            arch_os_get_current_thread()->interrupt_data;
        data->allocation_trap_context = context;
        memory = (char *) alloc(size);
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

    /* Skip the allocation trap and the write of the updated free
     * pointer back to the allocation region.  This is either two
     * instructions or four instructions. */
#if defined LISP_FEATURE_SB_THREAD || defined LISP_FEATURE_64_BIT
    (*os_context_pc_addr(context)) = (uword_t)(pc + 2);
#else
    (*os_context_pc_addr(context)) = (uword_t)(pc + 4);
#endif
    return 1; // handled
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
    unsigned int code = *((u32 *)(*os_context_pc_addr(context)));
    int register_offset = code >> 8 & 0x1f;
    handle_single_step_trap(context, trap, register_offset);
    arch_skip_instruction(context);
}

static void
sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    if (signal == SIGTRAP && handle_allocation_trap(context)) return;

    unsigned int code;

    code=*((u32 *)(*os_context_pc_addr(context)));
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
    undoably_install_low_level_interrupt_handler(SIGILL, sigtrap_handler);
    undoably_install_low_level_interrupt_handler(SIGTRAP, sigtrap_handler);
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

#ifdef LISP_FEATURE_LINKAGE_TABLE

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
arch_write_linkage_table_entry(char *reloc_addr, void *target_addr, int datap)
{
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
       * (so that we need not distinguish between #+/-dynamic-core or other reasons)
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
#endif
