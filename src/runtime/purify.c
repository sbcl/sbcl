/*
 * C-level stuff to implement Lisp-level PURIFY
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

#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>

#include "runtime.h"
#include "os.h"
#include "sbcl.h"
#include "globals.h"
#include "validate.h"
#include "interrupt.h"
#include "purify.h"
#include "interr.h"
#ifdef GENCGC
#include "gencgc.h"
#endif

#undef PRINTNOISE

#if defined(ibmrt) || defined(__i386__)
static lispobj *dynamic_space_free_pointer;
#endif

#define gc_abort() \
  lose("GC invariant lost, file \"%s\", line %d", __FILE__, __LINE__)

#if 1
#define gc_assert(ex) do { \
	if (!(ex)) gc_abort(); \
} while (0)
#else
#define gc_assert(ex)
#endif


/* These hold the original end of the read_only and static spaces so
 * we can tell what are forwarding pointers. */

static lispobj *read_only_end, *static_end;

static lispobj *read_only_free, *static_free;

static lispobj *pscav(lispobj *addr, int nwords, boolean constant);

#define LATERBLOCKSIZE 1020
#define LATERMAXCOUNT 10

static struct later {
    struct later *next;
    union {
        lispobj *ptr;
        int count;
    } u[LATERBLOCKSIZE];
} *later_blocks = NULL;
static int later_count = 0;

#define CEILING(x,y) (((x) + ((y) - 1)) & (~((y) - 1)))
#define NWORDS(x,y) (CEILING((x),(y)) / (y))

#ifdef sparc
#define RAW_ADDR_OFFSET 0
#else
#define RAW_ADDR_OFFSET (6*sizeof(lispobj) - type_FunctionPointer)
#endif

static boolean
forwarding_pointer_p(lispobj obj)
{
    lispobj *ptr;

    ptr = (lispobj *)obj;

    return ((static_end <= ptr && ptr <= static_free) ||
            (read_only_end <= ptr && ptr <= read_only_free));
}

static boolean
dynamic_pointer_p(lispobj ptr)
{
#ifndef __i386__
    /* KLUDGE: This has an implicit dependence on the ordering of
     * address spaces, and is therefore basically wrong. I'd fix it,
     * but I don't have a non-386 port to test it on. Porters are
     * encouraged to fix it. -- WHN 2000-10-17 */
    return (ptr >= (lispobj)DYNAMIC_SPACE_START);
#else
    /* Be more conservative, and remember, this is a maybe. */
    return (ptr >= (lispobj)DYNAMIC_SPACE_START
	    &&
	    ptr < (lispobj)dynamic_space_free_pointer);
#endif
}


#ifdef __i386__

#ifdef WANT_CGC
/* original x86/CGC stack scavenging code by Paul Werkowski */

static int
maybe_can_move_p(lispobj thing)
{
  lispobj *thingp,header;
  if (dynamic_pointer_p(thing)) { /* in dynamic space */
    thingp = (lispobj*)PTR(thing);
    header = *thingp;
    if(Pointerp(header) && forwarding_pointer_p(header))
      return -1;		/* must change it */
    if(LowtagOf(thing) == type_ListPointer)
      return type_ListPointer;	/* can we check this somehow */
    else if (thing & 3) {	/* not fixnum */
      int kind = TypeOf(header);
      /* printf(" %x %x",header,kind); */
      switch (kind) {		/* something with a header */
      case type_Bignum:
      case type_SingleFloat:
      case type_DoubleFloat:
#ifdef type_LongFloat
      case type_LongFloat:
#endif
      case type_Sap:
      case type_SimpleVector:
      case type_SimpleString:
      case type_SimpleBitVector:
      case type_SimpleArrayUnsignedByte2:
      case type_SimpleArrayUnsignedByte4:
      case type_SimpleArrayUnsignedByte8:
      case type_SimpleArrayUnsignedByte16:
      case type_SimpleArrayUnsignedByte32:
#ifdef type_SimpleArraySignedByte8
      case type_SimpleArraySignedByte8:
#endif
#ifdef type_SimpleArraySignedByte16
      case type_SimpleArraySignedByte16:
#endif
#ifdef type_SimpleArraySignedByte30
      case type_SimpleArraySignedByte30:
#endif
#ifdef type_SimpleArraySignedByte32
      case type_SimpleArraySignedByte32:
#endif
      case type_SimpleArraySingleFloat:
      case type_SimpleArrayDoubleFloat:
#ifdef type_SimpleArrayLongFloat
      case type_SimpleArrayLongFloat:
#endif
#ifdef type_SimpleArrayComplexSingleFloat
      case type_SimpleArrayComplexSingleFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
      case type_SimpleArrayComplexDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexLongFloat
      case type_SimpleArrayComplexLongFloat:
#endif
      case type_CodeHeader:
      case type_FunctionHeader:
      case type_ClosureFunctionHeader:
      case type_ReturnPcHeader:
      case type_ClosureHeader:
      case type_FuncallableInstanceHeader:
      case type_InstanceHeader:
      case type_ValueCellHeader:
      case type_ByteCodeFunction:
      case type_ByteCodeClosure:
      case type_WeakPointer:
      case type_Fdefn:
	return kind;
	break;
      default:
	return 0;
      }}}
  return 0;
}

static int pverbose=0;
#define PVERBOSE pverbose
static void
carefully_pscav_stack(lispobj*lowaddr, lispobj*base)
{
  lispobj*sp = lowaddr;
  while (sp < base)
    { int k;
      lispobj thing = *sp;
      if((unsigned)thing & 0x3)	/* may be pointer */
	{
	  /* need to check for valid float/double? */
	  k = maybe_can_move_p(thing);
	  if(PVERBOSE)printf("%8x %8x %d\n",sp, thing, k);
	  if(k)
	    pscav(sp, 1, 0);
	}
      sp++;
    }
}
#endif

#ifdef GENCGC
/*
 * Enhanced x86/GENCGC stack scavenging by Douglas Crosher.
 *
 * Scavenging the stack on the i386 is problematic due to conservative
 * roots and raw return addresses. Here it is handled in two passes:
 * the first pass runs before any objects are moved and tries to
 * identify valid pointers and return address on the stack, the second
 * pass scavenges these.
 */

static unsigned pointer_filter_verbose = 0;

static int
valid_dynamic_space_pointer(lispobj *pointer, lispobj *start_addr)
{
  /* If it's not a return address then it needs to be a valid Lisp
   * pointer. */
  if (!Pointerp((lispobj)pointer))
    return 0;

  /* Check that the object pointed to is consistent with the pointer
   * low tag. */
  switch (LowtagOf((lispobj)pointer)) {
  case type_FunctionPointer:
    /* Start_addr should be the enclosing code object, or a closure
     * header. */
    switch (TypeOf(*start_addr)) {
    case type_CodeHeader:
      /* This case is probably caught above. */
      break;
    case type_ClosureHeader:
    case type_FuncallableInstanceHeader:
    case type_ByteCodeFunction:
    case type_ByteCodeClosure:
      if ((int)pointer != ((int)start_addr+type_FunctionPointer)) {
	if (pointer_filter_verbose) {
	  fprintf(stderr,"*Wf2: %x %x %x\n", (unsigned int) pointer, 
		  (unsigned int) start_addr, *start_addr);
	}
	return 0;
      }
      break;
    default:
      if (pointer_filter_verbose) {
	fprintf(stderr,"*Wf3: %x %x %x\n", (unsigned int) pointer, 
		(unsigned int) start_addr, *start_addr);
      }
      return 0;
    }
    break;
  case type_ListPointer:
    if ((int)pointer != ((int)start_addr+type_ListPointer)) {
      if (pointer_filter_verbose)
	fprintf(stderr,"*Wl1: %x %x %x\n", (unsigned int) pointer, 
		(unsigned int) start_addr, *start_addr);
      return 0;
    }
    /* Is it plausible cons? */
    if((Pointerp(start_addr[0])
	|| ((start_addr[0] & 3) == 0) /* fixnum */
	|| (TypeOf(start_addr[0]) == type_BaseChar)
	|| (TypeOf(start_addr[0]) == type_UnboundMarker))
       && (Pointerp(start_addr[1])
	   || ((start_addr[1] & 3) == 0) /* fixnum */
	   || (TypeOf(start_addr[1]) == type_BaseChar)
	   || (TypeOf(start_addr[1]) == type_UnboundMarker))) {
      break;
    } else {
      if (pointer_filter_verbose) {
	fprintf(stderr,"*Wl2: %x %x %x\n", (unsigned int) pointer, 
		(unsigned int) start_addr, *start_addr);
      }
      return 0;
    }
  case type_InstancePointer:
    if ((int)pointer != ((int)start_addr+type_InstancePointer)) {
      if (pointer_filter_verbose) {
	fprintf(stderr,"*Wi1: %x %x %x\n", (unsigned int) pointer, 
		(unsigned int) start_addr, *start_addr);
      }
      return 0;
    }
    if (TypeOf(start_addr[0]) != type_InstanceHeader) {
      if (pointer_filter_verbose) {
	fprintf(stderr,"*Wi2: %x %x %x\n", (unsigned int) pointer, 
		(unsigned int) start_addr, *start_addr);
      }
      return 0;
    }
    break;
  case type_OtherPointer:
    if ((int)pointer != ((int)start_addr+type_OtherPointer)) {
      if (pointer_filter_verbose) {
	fprintf(stderr,"*Wo1: %x %x %x\n", (unsigned int) pointer, 
		(unsigned int) start_addr, *start_addr);
      }
      return 0;
    }
    /* Is it plausible?  Not a cons. X should check the headers. */
    if(Pointerp(start_addr[0]) || ((start_addr[0] & 3) == 0)) {
      if (pointer_filter_verbose) {
	fprintf(stderr,"*Wo2: %x %x %x\n", (unsigned int) pointer, 
		(unsigned int) start_addr, *start_addr);
      }
      return 0;
    }
    switch (TypeOf(start_addr[0])) {
    case type_UnboundMarker:
    case type_BaseChar:
      if (pointer_filter_verbose) {
	fprintf(stderr,"*Wo3: %x %x %x\n", (unsigned int) pointer, 
		(unsigned int) start_addr, *start_addr);
      }
      return 0;

      /* only pointed to by function pointers? */
    case type_ClosureHeader:
    case type_FuncallableInstanceHeader:
    case type_ByteCodeFunction:
    case type_ByteCodeClosure:
      if (pointer_filter_verbose) {
	fprintf(stderr,"*Wo4: %x %x %x\n", (unsigned int) pointer, 
		(unsigned int) start_addr, *start_addr);
      }
      return 0;

    case type_InstanceHeader:
      if (pointer_filter_verbose) {
	fprintf(stderr,"*Wo5: %x %x %x\n", (unsigned int) pointer, 
		(unsigned int) start_addr, *start_addr);
      }
      return 0;

      /* the valid other immediate pointer objects */
    case type_SimpleVector:
    case type_Ratio:
    case type_Complex:
#ifdef type_ComplexSingleFloat
    case type_ComplexSingleFloat:
#endif
#ifdef type_ComplexDoubleFloat
    case type_ComplexDoubleFloat:
#endif
#ifdef type_ComplexLongFloat
    case type_ComplexLongFloat:
#endif
    case type_SimpleArray:
    case type_ComplexString:
    case type_ComplexBitVector:
    case type_ComplexVector:
    case type_ComplexArray:
    case type_ValueCellHeader:
    case type_SymbolHeader:
    case type_Fdefn:
    case type_CodeHeader:
    case type_Bignum:
    case type_SingleFloat:
    case type_DoubleFloat:
#ifdef type_LongFloat
    case type_LongFloat:
#endif
    case type_SimpleString:
    case type_SimpleBitVector:
    case type_SimpleArrayUnsignedByte2:
    case type_SimpleArrayUnsignedByte4:
    case type_SimpleArrayUnsignedByte8:
    case type_SimpleArrayUnsignedByte16:
    case type_SimpleArrayUnsignedByte32:
#ifdef type_SimpleArraySignedByte8
    case type_SimpleArraySignedByte8:
#endif
#ifdef type_SimpleArraySignedByte16
    case type_SimpleArraySignedByte16:
#endif
#ifdef type_SimpleArraySignedByte30
    case type_SimpleArraySignedByte30:
#endif
#ifdef type_SimpleArraySignedByte32
    case type_SimpleArraySignedByte32:
#endif
    case type_SimpleArraySingleFloat:
    case type_SimpleArrayDoubleFloat:
#ifdef type_SimpleArrayLongFloat
    case type_SimpleArrayLongFloat:
#endif
#ifdef type_SimpleArrayComplexSingleFloat
    case type_SimpleArrayComplexSingleFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
    case type_SimpleArrayComplexDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexLongFloat
    case type_SimpleArrayComplexLongFloat:
#endif
    case type_Sap:
    case type_WeakPointer:
      break;

    default:
      if (pointer_filter_verbose) {
	fprintf(stderr,"*Wo6: %x %x %x\n", (unsigned int) pointer, 
		(unsigned int) start_addr, *start_addr);
      }
      return 0;
    }
    break;
  default:
    if (pointer_filter_verbose) {
      fprintf(stderr,"*W?: %x %x %x\n", (unsigned int) pointer, 
	      (unsigned int) start_addr, *start_addr);
    }
    return 0;
  }

  /* looks good */
  return 1;
}

#define MAX_STACK_POINTERS 256
lispobj *valid_stack_locations[MAX_STACK_POINTERS];
unsigned int num_valid_stack_locations;

#define MAX_STACK_RETURN_ADDRESSES 128
lispobj *valid_stack_ra_locations[MAX_STACK_RETURN_ADDRESSES];
lispobj *valid_stack_ra_code_objects[MAX_STACK_RETURN_ADDRESSES];
unsigned int num_valid_stack_ra_locations;

/* Identify valid stack slots. */
static void
setup_i386_stack_scav(lispobj *lowaddr, lispobj *base)
{
  lispobj *sp = lowaddr;
  num_valid_stack_locations = 0;
  num_valid_stack_ra_locations = 0;
  for (sp = lowaddr; sp < base; sp++) {
    lispobj thing = *sp;
    /* Find the object start address */
    lispobj *start_addr = search_dynamic_space((void *)thing);
    if (start_addr) {
      /* We need to allow raw pointers into Code objects for return
       * addresses. This will also pick up pointers to functions in code
       * objects. */
      if (TypeOf(*start_addr) == type_CodeHeader) {
	gc_assert(num_valid_stack_ra_locations < MAX_STACK_RETURN_ADDRESSES);
	valid_stack_ra_locations[num_valid_stack_ra_locations] = sp;
	valid_stack_ra_code_objects[num_valid_stack_ra_locations++] =
	  (lispobj *)((int)start_addr + type_OtherPointer);
      } else {
	if (valid_dynamic_space_pointer((void *)thing, start_addr)) {
	  gc_assert(num_valid_stack_locations < MAX_STACK_POINTERS);
	  valid_stack_locations[num_valid_stack_locations++] = sp;
	}
      }
    }
  }
  if (pointer_filter_verbose) {
    fprintf(stderr, "number of valid stack pointers = %d\n",
	    num_valid_stack_locations);
    fprintf(stderr, "number of stack return addresses = %d\n",
	    num_valid_stack_ra_locations);
  }
}

static void
pscav_i386_stack(void)
{
  int i;

  for (i = 0; i < num_valid_stack_locations; i++)
    pscav(valid_stack_locations[i], 1, 0);

  for (i = 0; i < num_valid_stack_ra_locations; i++) {
    lispobj code_obj = (lispobj)valid_stack_ra_code_objects[i];
    pscav(&code_obj, 1, 0);
    if (pointer_filter_verbose) {
      fprintf(stderr,"*C moved RA %x to %x; for code object %x to %x\n",
	      *valid_stack_ra_locations[i],
	      (int)(*valid_stack_ra_locations[i])
	      - ((int)valid_stack_ra_code_objects[i] - (int)code_obj),
	      (unsigned int) valid_stack_ra_code_objects[i], code_obj);
    }
    *valid_stack_ra_locations[i] =
      ((int)(*valid_stack_ra_locations[i])
       - ((int)valid_stack_ra_code_objects[i] - (int)code_obj));
  }
}
#endif
#endif


static void
pscav_later(lispobj *where, int count)
{
    struct later *new;

    if (count > LATERMAXCOUNT) {
        while (count > LATERMAXCOUNT) {
            pscav_later(where, LATERMAXCOUNT);
            count -= LATERMAXCOUNT;
            where += LATERMAXCOUNT;
        }
    }
    else {
        if (later_blocks == NULL || later_count == LATERBLOCKSIZE ||
            (later_count == LATERBLOCKSIZE-1 && count > 1)) {
            new  = (struct later *)malloc(sizeof(struct later));
            new->next = later_blocks;
            if (later_blocks && later_count < LATERBLOCKSIZE)
                later_blocks->u[later_count].ptr = NULL;
            later_blocks = new;
            later_count = 0;
        }

        if (count != 1)
            later_blocks->u[later_count++].count = count;
        later_blocks->u[later_count++].ptr = where;
    }
}

static lispobj ptrans_boxed(lispobj thing, lispobj header, boolean constant)
{
    int nwords;
    lispobj result, *new, *old;

    nwords = 1 + HeaderValue(header);

    /* Allocate it */
    old = (lispobj *)PTR(thing);
    if (constant) {
        new = read_only_free;
        read_only_free += CEILING(nwords, 2);
    }
    else {
        new = static_free;
        static_free += CEILING(nwords, 2);
    }

    /* Copy it. */
    bcopy(old, new, nwords * sizeof(lispobj));

    /* Deposit forwarding pointer. */
    result = (lispobj)new | LowtagOf(thing);
    *old = result;

    /* Scavenge it. */
    pscav(new, nwords, constant);

    return result;
}

/* We need to look at the layout to see whether it is a pure structure
 * class, and only then can we transport as constant. If it is pure, we can
 * ALWAYS transport as a constant. */
static lispobj ptrans_instance(lispobj thing, lispobj header, boolean constant)
{
    lispobj layout = ((struct instance *)PTR(thing))->slots[0];
    lispobj pure = ((struct instance *)PTR(layout))->slots[15];

    switch (pure) {
    case T:
	return (ptrans_boxed(thing, header, 1));
    case NIL:
	return (ptrans_boxed(thing, header, 0));
    case 0:
	{
	    /* Substructure: special case for the COMPACT-INFO-ENVs, where
	     * the instance may have a point to the dynamic space placed
	     * into it (e.g. the cache-name slot), but the lists and arrays
	     * at the time of a purify can be moved to the RO space. */
	    int nwords;
	    lispobj result, *new, *old;

	    nwords = 1 + HeaderValue(header);

	    /* Allocate it */
	    old = (lispobj *)PTR(thing);
	    new = static_free;
	    static_free += CEILING(nwords, 2);

	    /* Copy it. */
	    bcopy(old, new, nwords * sizeof(lispobj));

	    /* Deposit forwarding pointer. */
	    result = (lispobj)new | LowtagOf(thing);
	    *old = result;

	    /* Scavenge it. */
	    pscav(new, nwords, 1);

	    return result;
	}
    default:
	gc_abort();
	return NIL; /* dummy value: return something ... */
    }
}

static lispobj ptrans_fdefn(lispobj thing, lispobj header)
{
    int nwords;
    lispobj result, *new, *old, oldfn;
    struct fdefn *fdefn;

    nwords = 1 + HeaderValue(header);

    /* Allocate it */
    old = (lispobj *)PTR(thing);
    new = static_free;
    static_free += CEILING(nwords, 2);

    /* Copy it. */
    bcopy(old, new, nwords * sizeof(lispobj));

    /* Deposit forwarding pointer. */
    result = (lispobj)new | LowtagOf(thing);
    *old = result;

    /* Scavenge the function. */
    fdefn = (struct fdefn *)new;
    oldfn = fdefn->function;
    pscav(&fdefn->function, 1, 0);
    if ((char *)oldfn + RAW_ADDR_OFFSET == fdefn->raw_addr)
        fdefn->raw_addr = (char *)fdefn->function + RAW_ADDR_OFFSET;

    return result;
}

static lispobj ptrans_unboxed(lispobj thing, lispobj header)
{
    int nwords;
    lispobj result, *new, *old;

    nwords = 1 + HeaderValue(header);

    /* Allocate it */
    old = (lispobj *)PTR(thing);
    new = read_only_free;
    read_only_free += CEILING(nwords, 2);

    /* Copy it. */
    bcopy(old, new, nwords * sizeof(lispobj));

    /* Deposit forwarding pointer. */
    result = (lispobj)new | LowtagOf(thing);
    *old = result;

    return result;
}

static lispobj ptrans_vector(lispobj thing, int bits, int extra,
			     boolean boxed, boolean constant)
{
    struct vector *vector;
    int nwords;
    lispobj result, *new;

    vector = (struct vector *)PTR(thing);
    nwords = 2 + (CEILING((fixnum_value(vector->length)+extra)*bits,32)>>5);

    if (boxed && !constant) {
        new = static_free;
        static_free += CEILING(nwords, 2);
    }
    else {
        new = read_only_free;
        read_only_free += CEILING(nwords, 2);
    }

    bcopy(vector, new, nwords * sizeof(lispobj));

    result = (lispobj)new | LowtagOf(thing);
    vector->header = result;

    if (boxed)
        pscav(new, nwords, constant);

    return result;
}

#ifdef __i386__
static void
apply_code_fixups_during_purify(struct code *old_code, struct code *new_code)
{
  int nheader_words, ncode_words, nwords;
  void  *constants_start_addr, *constants_end_addr;
  void  *code_start_addr, *code_end_addr;
  lispobj fixups = NIL;
  unsigned  displacement = (unsigned)new_code - (unsigned)old_code;
  struct vector *fixups_vector;

  /* Byte compiled code has no fixups. The trace table offset will be
   * a fixnum if it's x86 compiled code - check. */
  if (new_code->trace_table_offset & 0x3)
    return;

  /* Else it's x86 machine code. */
  ncode_words = fixnum_value(new_code->code_size);
  nheader_words = HeaderValue(*(lispobj *)new_code);
  nwords = ncode_words + nheader_words;

  constants_start_addr = (void *)new_code + 5*4;
  constants_end_addr = (void *)new_code + nheader_words*4;
  code_start_addr = (void *)new_code + nheader_words*4;
  code_end_addr = (void *)new_code + nwords*4;

  /* The first constant should be a pointer to the fixups for this
   * code objects. Check. */
  fixups = new_code->constants[0];

  /* It will be 0 or the unbound-marker if there are no fixups, and
   * will be an other-pointer to a vector if it is valid. */
  if ((fixups==0) || (fixups==type_UnboundMarker) || !Pointerp(fixups)) {
#ifdef GENCGC
    /* Check for a possible errors. */
    sniff_code_object(new_code,displacement);
#endif
    return;
  }

  fixups_vector = (struct vector *)PTR(fixups);

  /* Could be pointing to a forwarding pointer. */
  if (Pointerp(fixups) && (dynamic_pointer_p(fixups))
      && forwarding_pointer_p(*(lispobj *)fixups_vector)) {
    /* If so then follow it. */
    fixups_vector = (struct vector *)PTR(*(lispobj *)fixups_vector);
  }

  if (TypeOf(fixups_vector->header) == type_SimpleArrayUnsignedByte32) {
    /* We got the fixups for the code block. Now work through the vector,
     * and apply a fixup at each address. */
    int length = fixnum_value(fixups_vector->length);
    int i;
    for (i=0; i<length; i++) {
      unsigned offset = fixups_vector->data[i];
      /* Now check the current value of offset. */
      unsigned  old_value = *(unsigned *)((unsigned)code_start_addr + offset);

      /* If it's within the old_code object then it must be an
       * absolute fixup (relative ones are not saved) */
      if ((old_value>=(unsigned)old_code)
	  && (old_value<((unsigned)old_code + nwords*4)))
	/* So add the dispacement. */
	*(unsigned *)((unsigned)code_start_addr + offset) = old_value
	  + displacement;
      else
	/* It is outside the old code object so it must be a relative
	 * fixup (absolute fixups are not saved). So subtract the
	 * displacement. */
	*(unsigned *)((unsigned)code_start_addr + offset) = old_value
	  - displacement;
    }
  }

  /* No longer need the fixups. */
  new_code->constants[0] = 0;

#ifdef GENCGC
  /* Check for possible errors. */
  sniff_code_object(new_code,displacement);
#endif
}
#endif

static lispobj ptrans_code(lispobj thing)
{
    struct code *code, *new;
    int nwords;
    lispobj func, result;

    code = (struct code *)PTR(thing);
    nwords = HeaderValue(code->header) + fixnum_value(code->code_size);

    new = (struct code *)read_only_free;
    read_only_free += CEILING(nwords, 2);

    bcopy(code, new, nwords * sizeof(lispobj));

#ifdef __i386__
    apply_code_fixups_during_purify(code,new);
#endif

    result = (lispobj)new | type_OtherPointer;

    /* Stick in a forwarding pointer for the code object. */
    *(lispobj *)code = result;

    /* Put in forwarding pointers for all the functions. */
    for (func = code->entry_points;
         func != NIL;
         func = ((struct function *)PTR(func))->next) {

        gc_assert(LowtagOf(func) == type_FunctionPointer);

        *(lispobj *)PTR(func) = result + (func - thing);
    }

    /* Arrange to scavenge the debug info later. */
    pscav_later(&new->debug_info, 1);

    if(new->trace_table_offset & 0x3)
#if 0
      pscav(&new->trace_table_offset, 1, 0);
#else
      new->trace_table_offset = NIL; /* limit lifetime */
#endif

    /* Scavenge the constants. */
    pscav(new->constants, HeaderValue(new->header)-5, 1);

    /* Scavenge all the functions. */
    pscav(&new->entry_points, 1, 1);
    for (func = new->entry_points;
         func != NIL;
         func = ((struct function *)PTR(func))->next) {
        gc_assert(LowtagOf(func) == type_FunctionPointer);
        gc_assert(!dynamic_pointer_p(func));

#ifdef __i386__
	/* Temporarly convert the self pointer to a real function
           pointer. */
	((struct function *)PTR(func))->self -= RAW_ADDR_OFFSET;
#endif
        pscav(&((struct function *)PTR(func))->self, 2, 1);
#ifdef __i386__
	((struct function *)PTR(func))->self += RAW_ADDR_OFFSET;
#endif
        pscav_later(&((struct function *)PTR(func))->name, 3);
    }

    return result;
}

static lispobj ptrans_func(lispobj thing, lispobj header)
{
    int nwords;
    lispobj code, *new, *old, result;
    struct function *function;

    /* Thing can either be a function header, a closure function
     * header, a closure, or a funcallable-instance. If it's a closure
     * or a funcallable-instance, we do the same as ptrans_boxed.
     * Otherwise we have to do something strange, 'cause it is buried
     * inside a code object. */

    if (TypeOf(header) == type_FunctionHeader ||
        TypeOf(header) == type_ClosureFunctionHeader) {

	/* We can only end up here if the code object has not been
         * scavenged, because if it had been scavenged, forwarding pointers
         * would have been left behind for all the entry points. */

        function = (struct function *)PTR(thing);
        code = (PTR(thing)-(HeaderValue(function->header)*sizeof(lispobj))) |
            type_OtherPointer;

        /* This will cause the function's header to be replaced with a 
         * forwarding pointer. */
        ptrans_code(code);

        /* So we can just return that. */
        return function->header;
    }
    else {
	/* It's some kind of closure-like thing. */
        nwords = 1 + HeaderValue(header);
        old = (lispobj *)PTR(thing);

	/* Allocate the new one. */
	if (TypeOf(header) == type_FuncallableInstanceHeader) {
	    /* FINs *must* not go in read_only space. */
	    new = static_free;
	    static_free += CEILING(nwords, 2);
	}
	else {
	    /* Closures can always go in read-only space, 'cause they
	     * never change. */

	    new = read_only_free;
	    read_only_free += CEILING(nwords, 2);
	}
        /* Copy it. */
        bcopy(old, new, nwords * sizeof(lispobj));

        /* Deposit forwarding pointer. */
        result = (lispobj)new | LowtagOf(thing);
        *old = result;

        /* Scavenge it. */
        pscav(new, nwords, 0);

        return result;
    }
}

static lispobj ptrans_returnpc(lispobj thing, lispobj header)
{
    lispobj code, new;

    /* Find the corresponding code object. */
    code = thing - HeaderValue(header)*sizeof(lispobj);

    /* Make sure it's been transported. */
    new = *(lispobj *)PTR(code);
    if (!forwarding_pointer_p(new))
        new = ptrans_code(code);

    /* Maintain the offset: */
    return new + (thing - code);
}

#define WORDS_PER_CONS CEILING(sizeof(struct cons) / sizeof(lispobj), 2)

static lispobj ptrans_list(lispobj thing, boolean constant)
{
    struct cons *old, *new, *orig;
    int length;

    if (constant)
        orig = (struct cons *)read_only_free;
    else
        orig = (struct cons *)static_free;
    length = 0;

    do {
        /* Allocate a new cons cell. */
        old = (struct cons *)PTR(thing);
        if (constant) {
            new = (struct cons *)read_only_free;
            read_only_free += WORDS_PER_CONS;
        }
        else {
            new = (struct cons *)static_free;
            static_free += WORDS_PER_CONS;
        }

        /* Copy the cons cell and keep a pointer to the cdr. */
        new->car = old->car;
        thing = new->cdr = old->cdr;

        /* Set up the forwarding pointer. */
        *(lispobj *)old = ((lispobj)new) | type_ListPointer;

        /* And count this cell. */
        length++;
    } while (LowtagOf(thing) == type_ListPointer &&
             dynamic_pointer_p(thing) &&
             !(forwarding_pointer_p(*(lispobj *)PTR(thing))));

    /* Scavenge the list we just copied. */
    pscav((lispobj *)orig, length * WORDS_PER_CONS, constant);

    return ((lispobj)orig) | type_ListPointer;
}

static lispobj ptrans_otherptr(lispobj thing, lispobj header, boolean constant)
{
    switch (TypeOf(header)) {
      case type_Bignum:
      case type_SingleFloat:
      case type_DoubleFloat:
#ifdef type_LongFloat
      case type_LongFloat:
#endif
#ifdef type_ComplexSingleFloat
      case type_ComplexSingleFloat:
#endif
#ifdef type_ComplexDoubleFloat
      case type_ComplexDoubleFloat:
#endif
#ifdef type_ComplexLongFloat
      case type_ComplexLongFloat:
#endif
      case type_Sap:
        return ptrans_unboxed(thing, header);

      case type_Ratio:
      case type_Complex:
      case type_SimpleArray:
      case type_ComplexString:
      case type_ComplexVector:
      case type_ComplexArray:
        return ptrans_boxed(thing, header, constant);
	
      case type_ValueCellHeader:
      case type_WeakPointer:
        return ptrans_boxed(thing, header, 0);

      case type_SymbolHeader:
        return ptrans_boxed(thing, header, 0);

      case type_SimpleString:
        return ptrans_vector(thing, 8, 1, 0, constant);

      case type_SimpleBitVector:
        return ptrans_vector(thing, 1, 0, 0, constant);

      case type_SimpleVector:
        return ptrans_vector(thing, 32, 0, 1, constant);

      case type_SimpleArrayUnsignedByte2:
        return ptrans_vector(thing, 2, 0, 0, constant);

      case type_SimpleArrayUnsignedByte4:
        return ptrans_vector(thing, 4, 0, 0, constant);

      case type_SimpleArrayUnsignedByte8:
#ifdef type_SimpleArraySignedByte8
      case type_SimpleArraySignedByte8:
#endif
        return ptrans_vector(thing, 8, 0, 0, constant);

      case type_SimpleArrayUnsignedByte16:
#ifdef type_SimpleArraySignedByte16
      case type_SimpleArraySignedByte16:
#endif
        return ptrans_vector(thing, 16, 0, 0, constant);

      case type_SimpleArrayUnsignedByte32:
#ifdef type_SimpleArraySignedByte30
      case type_SimpleArraySignedByte30:
#endif
#ifdef type_SimpleArraySignedByte32
      case type_SimpleArraySignedByte32:
#endif
        return ptrans_vector(thing, 32, 0, 0, constant);

      case type_SimpleArraySingleFloat:
        return ptrans_vector(thing, 32, 0, 0, constant);

      case type_SimpleArrayDoubleFloat:
        return ptrans_vector(thing, 64, 0, 0, constant);

#ifdef type_SimpleArrayLongFloat
      case type_SimpleArrayLongFloat:
#ifdef __i386__
        return ptrans_vector(thing, 96, 0, 0, constant);
#endif
#ifdef sparc
        return ptrans_vector(thing, 128, 0, 0, constant);
#endif
#endif

#ifdef type_SimpleArrayComplexSingleFloat
      case type_SimpleArrayComplexSingleFloat:
        return ptrans_vector(thing, 64, 0, 0, constant);
#endif

#ifdef type_SimpleArrayComplexDoubleFloat
      case type_SimpleArrayComplexDoubleFloat:
        return ptrans_vector(thing, 128, 0, 0, constant);
#endif

#ifdef type_SimpleArrayComplexLongFloat
      case type_SimpleArrayComplexLongFloat:
#ifdef __i386__
        return ptrans_vector(thing, 192, 0, 0, constant);
#endif
#ifdef sparc
        return ptrans_vector(thing, 256, 0, 0, constant);
#endif
#endif

      case type_CodeHeader:
        return ptrans_code(thing);

      case type_ReturnPcHeader:
        return ptrans_returnpc(thing, header);

      case type_Fdefn:
	return ptrans_fdefn(thing, header);

      default:
        /* Should only come across other pointers to the above stuff. */
        gc_abort();
	return NIL;
    }
}

static int pscav_fdefn(struct fdefn *fdefn)
{
    boolean fix_func;

    fix_func = ((char *)(fdefn->function+RAW_ADDR_OFFSET) == fdefn->raw_addr);
    pscav(&fdefn->name, 1, 1);
    pscav(&fdefn->function, 1, 0);
    if (fix_func)
        fdefn->raw_addr = (char *)(fdefn->function + RAW_ADDR_OFFSET);
    return sizeof(struct fdefn) / sizeof(lispobj);
}

#ifdef __i386__
/* now putting code objects in static space */
static int
pscav_code(struct code*code)
{
    int nwords;
    lispobj func;
    nwords = HeaderValue(code->header) + fixnum_value(code->code_size);

    /* pw--The trace_table_offset slot can contain a list pointer. This
     * occurs when the code object is a top level form that initializes
     * a byte-compiled function. The fact that PURIFY was ignoring this
     * slot may be a bug unrelated to the x86 port, except that TLF's
     * normally become unreachable after the loader calls them and
     * won't be seen by PURIFY at all!! */
    if(code->trace_table_offset & 0x3)
#if 0
      pscav(&code->trace_table_offset, 1, 0);
#else
      code->trace_table_offset = NIL; /* limit lifetime */
#endif

    /* Arrange to scavenge the debug info later. */
    pscav_later(&code->debug_info, 1);

    /* Scavenge the constants. */
    pscav(code->constants, HeaderValue(code->header)-5, 1);

    /* Scavenge all the functions. */
    pscav(&code->entry_points, 1, 1);
    for (func = code->entry_points;
         func != NIL;
         func = ((struct function *)PTR(func))->next) {
        gc_assert(LowtagOf(func) == type_FunctionPointer);
        gc_assert(!dynamic_pointer_p(func));

#ifdef __i386__
	/* Temporarly convert the self pointer to a real function
	 * pointer. */
	((struct function *)PTR(func))->self -= RAW_ADDR_OFFSET;
#endif
        pscav(&((struct function *)PTR(func))->self, 2, 1);
#ifdef __i386__
	((struct function *)PTR(func))->self += RAW_ADDR_OFFSET;
#endif
        pscav_later(&((struct function *)PTR(func))->name, 3);
    }

    return CEILING(nwords,2);
}
#endif

static lispobj *pscav(lispobj *addr, int nwords, boolean constant)
{
    lispobj thing, *thingp, header;
    int count = 0; /* (0 = dummy init value to stop GCC warning) */
    struct vector *vector;

    while (nwords > 0) {
        thing = *addr;
        if (Pointerp(thing)) {
            /* It's a pointer. Is it something we might have to move? */
            if (dynamic_pointer_p(thing)) {
                /* Maybe. Have we already moved it? */
		thingp = (lispobj *)PTR(thing);
                header = *thingp;
                if (Pointerp(header) && forwarding_pointer_p(header))
                    /* Yep, so just copy the forwarding pointer. */
                    thing = header;
                else {
                    /* Nope, copy the object. */
                    switch (LowtagOf(thing)) {
                      case type_FunctionPointer:
                        thing = ptrans_func(thing, header);
                        break;

                      case type_ListPointer:
                        thing = ptrans_list(thing, constant);
                        break;

                      case type_InstancePointer:
                        thing = ptrans_instance(thing, header, constant);
                        break;

                      case type_OtherPointer:
                        thing = ptrans_otherptr(thing, header, constant);
                        break;

                      default:
                        /* It was a pointer, but not one of them? */
                        gc_abort();
                    }
                }
                *addr = thing;
            }
            count = 1;
        }
        else if (thing & 3) {
            /* It's an other immediate. Maybe the header for an unboxed */
            /* object. */
            switch (TypeOf(thing)) {
              case type_Bignum:
              case type_SingleFloat:
              case type_DoubleFloat:
#ifdef type_LongFloat
              case type_LongFloat:
#endif
              case type_Sap:
                /* It's an unboxed simple object. */
                count = HeaderValue(thing)+1;
                break;

              case type_SimpleVector:
                if (HeaderValue(thing) == subtype_VectorValidHashing)
                    *addr = (subtype_VectorMustRehash<<type_Bits) |
                        type_SimpleVector;
                count = 1;
                break;

              case type_SimpleString:
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length)+1,4)+2,2);
                break;

              case type_SimpleBitVector:
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),32)+2,2);
                break;

              case type_SimpleArrayUnsignedByte2:
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),16)+2,2);
                break;

              case type_SimpleArrayUnsignedByte4:
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),8)+2,2);
                break;

              case type_SimpleArrayUnsignedByte8:
#ifdef type_SimpleArraySignedByte8
              case type_SimpleArraySignedByte8:
#endif
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),4)+2,2);
                break;

              case type_SimpleArrayUnsignedByte16:
#ifdef type_SimpleArraySignedByte16
              case type_SimpleArraySignedByte16:
#endif
                vector = (struct vector *)addr;
                count = CEILING(NWORDS(fixnum_value(vector->length),2)+2,2);
                break;

              case type_SimpleArrayUnsignedByte32:
#ifdef type_SimpleArraySignedByte30
              case type_SimpleArraySignedByte30:
#endif
#ifdef type_SimpleArraySignedByte32
              case type_SimpleArraySignedByte32:
#endif
                vector = (struct vector *)addr;
                count = CEILING(fixnum_value(vector->length)+2,2);
                break;

              case type_SimpleArraySingleFloat:
                vector = (struct vector *)addr;
                count = CEILING(fixnum_value(vector->length)+2,2);
                break;

              case type_SimpleArrayDoubleFloat:
#ifdef type_SimpleArrayComplexSingleFloat
              case type_SimpleArrayComplexSingleFloat:
#endif
                vector = (struct vector *)addr;
                count = fixnum_value(vector->length)*2+2;
                break;

#ifdef type_SimpleArrayLongFloat
              case type_SimpleArrayLongFloat:
                vector = (struct vector *)addr;
#ifdef __i386__
                count = fixnum_value(vector->length)*3+2;
#endif
#ifdef sparc
                count = fixnum_value(vector->length)*4+2;
#endif
                break;
#endif

#ifdef type_SimpleArrayComplexDoubleFloat
              case type_SimpleArrayComplexDoubleFloat:
                vector = (struct vector *)addr;
                count = fixnum_value(vector->length)*4+2;
                break;
#endif

#ifdef type_SimpleArrayComplexLongFloat
              case type_SimpleArrayComplexLongFloat:
                vector = (struct vector *)addr;
#ifdef __i386__
                count = fixnum_value(vector->length)*6+2;
#endif
#ifdef sparc
                count = fixnum_value(vector->length)*8+2;
#endif
                break;
#endif

              case type_CodeHeader:
#ifndef __i386__
                gc_abort(); /* no code headers in static space */
#else
		count = pscav_code((struct code*)addr);
#endif
                break;

              case type_FunctionHeader:
              case type_ClosureFunctionHeader:
              case type_ReturnPcHeader:
                /* We should never hit any of these, 'cause they occur
                 * buried in the middle of code objects. */
                gc_abort();
		break;

#ifdef __i386__
	      case type_ClosureHeader:
	      case type_FuncallableInstanceHeader:
	      case type_ByteCodeFunction:
	      case type_ByteCodeClosure:
		/* The function self pointer needs special care on the
		 * x86 because it is the real entry point. */
		{
		  lispobj fun = ((struct closure *)addr)->function
		    - RAW_ADDR_OFFSET;
		  pscav(&fun, 1, constant);
		  ((struct closure *)addr)->function = fun + RAW_ADDR_OFFSET;
		}
		count = 2;
		break;
#endif

              case type_WeakPointer:
                /* Weak pointers get preserved during purify, 'cause I
		 * don't feel like figuring out how to break them. */
                pscav(addr+1, 2, constant);
                count = 4;
                break;

	      case type_Fdefn:
		/* We have to handle fdefn objects specially, so we
		 * can fix up the raw function address. */
		count = pscav_fdefn((struct fdefn *)addr);
		break;

              default:
                count = 1;
                break;
            }
        }
        else {
            /* It's a fixnum. */
            count = 1;
        }

        addr += count;
        nwords -= count;
    }

    return addr;
}

int purify(lispobj static_roots, lispobj read_only_roots)
{
    lispobj *clean;
    int count, i;
    struct later *laters, *next;

#ifdef PRINTNOISE
    printf("[doing purification:");
    fflush(stdout);
#endif

    if (fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)) != 0) {
	/* FIXME: 1. What does this mean? 2. It shouldn't be reporting
	 * its error simply by a. printing a string b. to stdout instead
	 * of stderr. */
        printf(" Ack! Can't purify interrupt contexts. ");
        fflush(stdout);
        return 0;
    }

#if defined(ibmrt) || defined(__i386__)
    dynamic_space_free_pointer =
      (lispobj*)SymbolValue(ALLOCATION_POINTER);
#endif

    read_only_end = read_only_free =
        (lispobj *)SymbolValue(READ_ONLY_SPACE_FREE_POINTER);
    static_end = static_free =
        (lispobj *)SymbolValue(STATIC_SPACE_FREE_POINTER);

#ifdef PRINTNOISE
    printf(" roots");
    fflush(stdout);
#endif

#ifdef GENCGC
    gc_assert((lispobj *)CONTROL_STACK_END > ((&read_only_roots)+1));
    setup_i386_stack_scav(((&static_roots)-2), (lispobj *)CONTROL_STACK_END);
#endif

    pscav(&static_roots, 1, 0);
    pscav(&read_only_roots, 1, 1);

#ifdef PRINTNOISE
    printf(" handlers");
    fflush(stdout);
#endif
    pscav((lispobj *) interrupt_handlers,
          sizeof(interrupt_handlers) / sizeof(lispobj),
          0);

#ifdef PRINTNOISE
    printf(" stack");
    fflush(stdout);
#endif
#ifndef __i386__
    pscav((lispobj *)control_stack,
	  current_control_stack_pointer - (lispobj *)CONTROL_STACK_START,
	  0);
#else
#ifdef GENCGC
    pscav_i386_stack();
#endif
#ifdef WANT_CGC
    gc_assert((lispobj *)control_stack_end > ((&read_only_roots)+1));
    carefully_pscav_stack(((&read_only_roots)+1),
			  (lispobj *)CONTROL_STACK_END);
#endif
#endif

#ifdef PRINTNOISE
    printf(" bindings");
    fflush(stdout);
#endif
#if !defined(ibmrt) && !defined(__i386__)
    pscav( (lispobj *)BINDING_STACK_START,
	  (lispobj *)current_binding_stack_pointer - (lispobj *)BINDING_STACK_START,
	  0);
#else
    pscav( (lispobj *)BINDING_STACK_START,
	  (lispobj *)SymbolValue(BINDING_STACK_POINTER) -
	  (lispobj *)BINDING_STACK_START,
	  0);
#endif

#ifdef SCAVENGE_READ_ONLY_SPACE
    if (SymbolValue(SCAVENGE_READ_ONLY_SPACE) != type_UnboundMarker
	&& SymbolValue(SCAVENGE_READ_ONLY_SPACE) != NIL) {
      unsigned  read_only_space_size =
	  (lispobj *)SymbolValue(READ_ONLY_SPACE_FREE_POINTER) -
	  (lispobj *)READ_ONLY_SPACE_START;
      fprintf(stderr,
	      "scavenging read only space: %d bytes\n",
	      read_only_space_size * sizeof(lispobj));
      pscav( (lispobj *)READ_ONLY_SPACE_START, read_only_space_size, 0);
    }
#endif

#ifdef PRINTNOISE
    printf(" static");
    fflush(stdout);
#endif
    clean = (lispobj *)STATIC_SPACE_START;
    do {
        while (clean != static_free)
            clean = pscav(clean, static_free - clean, 0);
        laters = later_blocks;
        count = later_count;
        later_blocks = NULL;
        later_count = 0;
        while (laters != NULL) {
            for (i = 0; i < count; i++) {
                if (laters->u[i].count == 0) {
                    ;
                } else if (laters->u[i].count <= LATERMAXCOUNT) {
                    pscav(laters->u[i+1].ptr, laters->u[i].count, 1);
                    i++;
                } else {
                    pscav(laters->u[i].ptr, 1, 1);
		}
            }
            next = laters->next;
            free(laters);
            laters = next;
            count = LATERBLOCKSIZE;
        }
    } while (clean != static_free || later_blocks != NULL);

#ifdef PRINTNOISE
    printf(" cleanup");
    fflush(stdout);
#endif

#if defined(WANT_CGC) && defined(X86_CGC_ACTIVE_P)
    if(SymbolValue(X86_CGC_ACTIVE_P) != T) {
	os_zero((os_vm_address_t) DYNAMIC_SPACE_START,
		(os_vm_size_t) DYNAMIC_SPACE_SIZE);
    }
#else
    os_zero((os_vm_address_t) DYNAMIC_SPACE_START,
            (os_vm_size_t) DYNAMIC_SPACE_SIZE);
#endif

    /* Zero the stack. Note that the stack is also zeroed by SUB-GC
     * calling SCRUB-CONTROL-STACK - this zeros the stack on the x86. */
#ifndef __i386__
    os_zero((os_vm_address_t) current_control_stack_pointer,
            (os_vm_size_t) (CONTROL_STACK_SIZE -
                            ((current_control_stack_pointer -
			      (lispobj *)CONTROL_STACK_START) *
                             sizeof(lispobj))));
#endif

#if defined(WANT_CGC) && defined(STATIC_BLUE_BAG)
    {
      lispobj bag = SymbolValue(STATIC_BLUE_BAG);
      struct cons*cons = (struct cons*)static_free;
      struct cons*pair = cons + 1;
      static_free += 2*WORDS_PER_CONS;
      if(bag == type_UnboundMarker)
	bag = NIL;
      cons->cdr = bag;
      cons->car = (lispobj)pair | type_ListPointer;
      pair->car = (lispobj)static_end;
      pair->cdr = (lispobj)static_free;
      bag = (lispobj)cons | type_ListPointer;
      SetSymbolValue(STATIC_BLUE_BAG, bag);
    }
#endif

    /* It helps to update the heap free pointers so that free_heap can
     * verify after it's done. */
    SetSymbolValue(READ_ONLY_SPACE_FREE_POINTER, (lispobj)read_only_free);
    SetSymbolValue(STATIC_SPACE_FREE_POINTER, (lispobj)static_free);

#if !defined(ibmrt) && !defined(__i386__)
    dynamic_space_free_pointer = DYNAMIC_SPACE_START;
#else
#if defined(WANT_CGC) && defined(X86_CGC_ACTIVE_P)
    /* X86 using CGC */
    if(SymbolValue(X86_CGC_ACTIVE_P) != T)
	SetSymbolValue(ALLOCATION_POINTER, (lispobj)DYNAMIC_SPACE_START);
    else
	cgc_free_heap();
#else
#if defined GENCGC
    gc_free_heap();
#else
    /* ibmrt using GC */
    SetSymbolValue(ALLOCATION_POINTER, (lispobj)DYNAMIC_SPACE_START);
#endif
#endif
#endif

#ifdef PRINTNOISE
    printf(" done]\n");
    fflush(stdout);
#endif

    return 0;
}
