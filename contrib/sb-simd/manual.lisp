(in-package :sb-manual)

(defsection @sb-simd (:title "sb-simd")
  "The `SB-SIMD` module provides a convenient interface for SIMD
  programming in SBCL. It provides one package per SIMD instruction
  set, plus functions and macros for querying whether an instruction
  set is available and what functions and data types it exports."
  (@data-types section)
  (@casts section)
  (@constructors section)
  (@unpackers section)
  (@reinterpret-casts section)
  (@associatives section)
  (@reducers section)
  (@rounding section)
  (@comparisons section)
  (@conditionals section)
  (@loads-and-stores section)
  (@specialized-scalar-operations section)
  (@instruction-set-dispatch section))

(defsection @data-types (:title "Data Types")
  "The central data type in sb-simd is the SIMD pack. A SIMD pack
  is very similar to a specialized vector, except that its length must
  be a particular power of two that depends on its element type and
  the underlying hardware. The set of element types that are supported
  for SIMD packs is similar to that of SBCL's specialized array
  element types, except that there is currently no support for SIMD
  packs of complex numbers or characters.

  The supported scalar types are `F32`, `F64`, `S<N>`, and `U<N>`,
  where `<N>` is either 8, 16, 32, or 64. These scalar types are
  abbreviations for the Common Lisp types SINGLE-FLOAT, DOUBLE-FLOAT,
  SIGNED-BYTE, and UNSIGNED-BYTE, respectively. For each scalar data
  type `X`, there exists one or more SIMD data type `X.Y` with `Y`
  elements. For example, in AVX there are two supported SIMD data
  types with element type `F64`, namely `F64.2` (128 bit) and
  `F64.4` (256 bit).

  SIMD packs are regular Common Lisp objects that have a type, a
  class, and can be passed as function arguments. The price for this
  is that SIMD packs have both a boxed and an unboxed representation.
  The unboxed representation of a SIMD pack has zero overhead and fits
  into a CPU register but can only be used within a function and when
  the compiler can statically determine the SIMD pack's type.
  Otherwise, the SIMD pack is boxed, i.e. spilled to the heap together
  with its type information. In practice, boxing of SIMD packs can
  usually be avoided via inlining, or by loading and storing them to
  specialized arrays instead of passing them around as function
  arguments.")

(defsection @casts (:title "Casts")
  "For each scalar data type `X`, there is a function named `X`
  that is equivalent to `(LAMBDA (V) (COERCE V 'X))`. For each SIMD
  data type `X.Y`, there is a function named `X.Y` that ensures that
  its argument is of type `X.Y`, or, if the argument is a number,
  calls the cast function of `X` and broadcasts the result.

  All functions provided by sb-simd (apart from the casts themselves)
  implicitly cast each argument to its expected type. So, to add the
  number five to each single float in a SIMD pack `X` of type `F32.8`,
  it is sufficient to write `(F32.8+ X 5)`. We don't mention this
  implicit conversion explicitly in the following sections, so if any
  function description states that an argument must be of type `X.Y`,
  the argument can actually be of any type that is a suitable argument
  of the cast function named `X.Y`.")

(defsection @constructors (:title "Constructors")
  "For each SIMD data type `X.Y`, there is a constructor named
  `MAKE-X.Y` that takes `Y` arguments of type `X` and returns a SIMD
  pack whose elements are the supplied values.")

(defsection @unpackers (:title "Unpackers")
  "For each SIMD data type `X.Y`, there is a function named
  `X.Y-VALUES` that returns, as `Y` multiple values, the elements of
  the supplied SIMD pack of type `X.Y`.")

(defsection @reinterpret-casts (:title "Reinterpret Casts")
  "For each SIMD data type `X.Y`, there is a function named
  `X.Y!` that takes any SIMD pack or scalar datum and interprets its
  bits as a SIMD pack of type `X.Y`. If the supplied datum has more
  bits than the resulting value, the excess bits are discarded. If the
  supplied datum has less bits than the resulting value, the missing
  bits are assumed to be zero.")

(defsection @associatives (:title "Associatives")
  "For each associative binary function, e.g. `TWO-ARG-X.Y-OP`, there
  is a function `X.Y-OP` that takes any number of arguments and
  combines them with this binary function in a tree-like fashion. If
  the binary function has an identity element, it is possible to call
  the function with zero arguments, in which case the identity element
  is returned. If there is no identity element, the function must
  receive at least one argument.

  Examples of associative functions are `SB-SIMD-AVX:F32.8+`, for
  summing any number of 256 bit packs of single floats, and
  `SB-SIMD-FMA:U8.32-MAX`, for computing the element-wise maximum of
  one or more 256 bit packs of 8 bit integers.")

(defsection @reducers (:title "Reducers")
  "For binary functions `TWO-ARG-X.Y-OP` that are not associative but
  have a neutral element, there are functions `X.Y-OP` that take any
  positive number of arguments and return the reduction of all
  arguments with the binary function. In the special case of a single
  supplied argument, the binary function is invoked on the neutral
  element and that argument. Reducers have been introduced to generate
  Lisp-style subtraction and division functions.

  Examples of reducers are `SB-SIMD-AVX:F32.8/`, for successively
  dividing a pack of 32 bit single floats by all further supplied
  packs of 32 bit single floats, or `SB-SIMD-FMA:U32.8-` for
  subtracting any number of supplied packs of 32 bit unsigned integers
  from the first supplied one, except in the case of a single
  argument, where `SB-SIMD-FMA:U32.8-` simply negates all values in
  the pack.")

(defsection @rounding (:title "Rounding")
  "For each floating-point SIMD data type `X.Y`, there are several
  functions that round the values of a supplied SIMD pack to nearby
  floating-point values whose fractional digits are all zero. Those
  functions are `X.Y-ROUND`, `X.Y-FLOOR`, `X.Y-CEILING`, and
  `X.Y-TRUNCATE`, and they have the same semantics as the one argument
  versions of CL:ROUND, CL:FLOOR, CL:CEILING, and CL:TRUNCATE,
  respectively.")

(defsection @comparisons (:title "Comparisons")
  "For each SIMD data type `X.Y`, there exist conversion functions
  `X.Y<`, `X.Y<=`, `X.Y>`, `X.Y>=`, and `X.Y=` that check whether the
  supplied arguments are strictly monotonically increasing,
  monotonically increasing, strictly monotonically decreasing,
  monotonically decreasing, equal, or nowhere equal, respectively. In
  contrast to the Common Lisp functions `<`, `<=`, `>`, `>=`, `=`, and
  `/=`, the SIMD comparison functions don't return a generalized
  boolean but a SIMD pack of unsigned integers with `Y` elements.
  The bits of each unsigned integer are either all one, if the values
  of the arguments at that position satisfy the test, or all zero, if
  they don't. We call a SIMD packs of such unsigned integers a mask.")

(defsection @conditionals (:title "Conditionals")
  "The SIMD paradigm is inherently incompatible with fine-grained control
  flow. A piece of code containing an IF special form cannot be
  vectorized in a straightforward way, because doing so would require
  as many instruction pointers and processor states as there are
  values in the desired SIMD data type. Instead, most SIMD instruction
  sets provide an operator for selecting values from one of two
  supplied SIMD packs based on a mask. The mask is a SIMD pack with as
  many elements as the other two arguments, but whose elements are
  unsigned integers whose bits must be either all zeros or all ones.
  This selection mechanism can be used to emulate the effect of an IF
  special form, at the price that both operands have to be computed
  each time.

  In sb-simd, all conditional operations and comparisons emit suitable
  mask fields, and there is a `X.Y-IF` function for each SIMD data
  type with element type `X` and number of elements `Y` whose first
  arguments must be a suitable mask, whose second and third argument
  must be objects that can be converted to the SIMD data type `X.Y`,
  and that returns a value of type `X.Y` where each element is from
  the second operand if the corresponding mask bits are set, and from
  the third operand if the corresponding mask bits are not set.")

(defsection @loads-and-stores (:title "Loads and Stores")
  "In practice, a SIMD pack `X.Y` is usually not constructed by
  calling its constructor but by loading `Y` consecutive elements from
  a specialized array with element type `X`. The functions for doing
  so are called `X.Y-AREF` and `X.Y-ROW-MAJOR-AREF`, and have similar
  semantics as Common Lisp's AREF and ROW-MAJOR-AREF. In addition to
  that, some instruction sets provide the functions
  `X.Y-NON-TEMPORAL-AREF` and `X.Y-NON-TEMPORAL-ROW-MAJOR-AREF`, for
  accessing a memory location without loading the referenced values
  into the CPU's cache.

  For each function `X.Y-FOO` for loading SIMD packs from an array,
  there also exists a corresponding function `(SETF X.Y-FOO)` for
  storing a SIMD pack in the specified memory location. An exception
  to this rule is that some instruction sets (e.g., SSE) only provide
  functions for non-temporal stores but not for the corresponding
  non-temporal loads.

  One difficulty when treating the data of a Common Lisp array as a
  SIMD pack is that some hardware instructions require a particular
  alignment of the address being referenced. Luckily, most
  architectures provide instructions for unaligned loads and stores
  that are, at least on modern CPUs, not slower than their aligned
  equivalents. So by default we translate all array references as
  unaligned loads and stores. An exception are the instructions for
  non-temporal loads and stores, that always require a certain
  alignment. We do not handle this case specially, so without special
  handling by the user, non-temporal loads and stores will only work
  on certain array indices that depend on the actual placement of that
  array in memory.")

(defsection @specialized-scalar-operations
    (:title "Specialized Scalar Operations")
  "Finally, for each SIMD function `X.Y-OP` that applies a certain
  operation `OP` element-wise to the `Y` elements of type `X`, there
  exists also a functions `X-OP` for applying that operation only to a
  single element. For example, the SIMD function `F64.4+` has a
  corresponding function `F64+` that differs from `CL:+` in that it
  only accepts arguments of type double float, and that it adds its
  supplied arguments in a fixed order that is the same as the one used
  by `F64.4`.

  There are good reasons for exporting scalar functions from a SIMD
  library, too. The most obvious one is that they obey the same naming
  convention and hence make it easier to locate the correct functions.
  Another benefit is that the semantics of each scalar operation is
  precisely the same as that of the corresponding SIMD function, so
  they can be used to write reference implementations for testing. A
  final reason is that these scalar functions can be used to simplify
  the life of tools for automatic vectorization.")

(defsection @instruction-set-dispatch (:title "Instruction Set Dispatch")
  "One challenge that is unique to image-based programming systems such as
  Lisp is that a program can run on one machine, be dumped as an image,
  and then resumed on another machine.  While nobody expects this feature
  to work across machines with different architectures, it is quite likely
  that the machine where the image is dumped and the one where execution
  is resumed provide different instruction set extensions.

  As a practical example, consider a game developer that develops software
  on an x86-64 machine with all SIMD extensions up to AVX2, but then dumps
  it as an image and ships it to a customer whose machine only supports
  SIMD extensions up to SSE2.  Ideally, the image should contain multiple
  optimized versions of all crucial functions, and dynamically select the
  most appropriate version based on the instruction set extensions that
  are actually available.

  This kind of run time instruction set dispatch is explicitly
  supported by means of the SB-SIMD-INTERNALS:INSTRUCTION-SET-CASE
  macro. The code resulting from an invocation of this macro compiles
  to an efficient jump table whose index is recomputed on each startup
  of the Lisp image.")
