(in-package :sb-manual)

(defsection @efficiency (:title "Efficiency")
  (@slot-access section)
  (@stack-allocation section)
  (@modular-arithmetic section)
  (@recognized-idioms section)
  (@global-and-always-bound-variables section)
  (@miscellaneous-efficiency-issues section))

(defsection @slot-access (:title "Slot Access")
  (@structure-object-slot-access section)
  (@standard-object-slot-access section))

(defsection @structure-object-slot-access
    (:title "Structure Object Slot Access")
  "Structure slot accessors are efficient only if the compiler is
  able to open code them: compiling a call to a structure slot
  accessor before the structure is defined, declaring one NOTINLINE,
  or passing it as a functional argument to another function causes
  severe performance degradation.")

(defsection @standard-object-slot-access
    (:title "Standard Object Slot Access")
  "The most efficient way to access a slot of a STANDARD-OBJECT is
  by using SLOT-VALUE with a constant slot name argument inside a
  DEFMETHOD body, where the variable holding the instance is a
  specializer parameter of the method and is never assigned to. The
  cost is roughly 1.6 times that of an open coded structure slot
  accessor.

  Second most efficient way is to use a CLOS slot accessor, or
  SLOT-VALUE with a constant slot name argument, but in circumstances
  other than specified above. This may be up to 3 times as slow as the
  method described above.

  Example:

      (defclass foo () ((bar)))

      ;; Fast: specializer and never assigned to
      (defmethod quux ((foo foo) new)
        (let ((old (slot-value foo 'bar)))
          (setf (slot-value foo 'bar) new)
          old))

      ;; Slow: not a specializer
      (defmethod quux ((foo foo) new)
        (let* ((temp foo)
               (old (slot-value temp 'bar)))
          (setf (slot-value temp 'bar) new)
          old))

      ;; Slow: assignment to FOO
      (defmethod quux ((foo foo) new)
        (let ((old (slot-value foo 'bar)))
          (setf (slot-value foo 'bar) new)
          (setf foo new)
          old))

  Note that when profiling code such as this, the first few calls to the
  generic function are not representative, as the dispatch mechanism is
  lazily set up during those calls.")

(defsection @stack-allocation (:title "Stack Allocation")
  "SBCL has fairly extensive support for performing allocations on the
  stack when a variable or function is declared DYNAMIC-EXTENT. The
  DYNAMIC-EXTENT declarations are not verified but are simply trusted
  as long as SB-EXT:*STACK-ALLOCATE-DYNAMIC-EXTENT* is true."
  (sb-ext:*stack-allocate-dynamic-extent* variable)
  "SBCL recognizes any value which a variable declared DYNAMIC-EXTENT
  can take on as having dynamic extent. This means that, in addition
  to the value a variable is bound to initially, a value assigned to a
  variable by SETQ is also recognized as having dynamic extent when
  the variable is declared DYNAMIC-EXTENT. Users can thus build
  complex structures on the stack using iteration and SETQ.

  At present, SBCL implements stack allocation for the following kinds
  of values when they are recognized as having dynamic extent:

  - &REST lists;

  - the results of CONS, LIST, LIST*, and VECTOR;

  - the result of simple forms of MAKE-ARRAY: stack allocation is
    possible only if the resulting array is known to be both simple
    and one-dimensional, and has a constant :ELEMENT-TYPE;

      > __Warning__: Stack space is limited, so allocation of a large
      > vector may cause stack overflow. Stack overflow checks are
      > done except in zero SAFETY policies.

  - closures defined with FLET or LABELS with a bound DYNAMIC-EXTENT
    declaration;

  - anonymous closures defined with LAMBDA;

  - user-defined structures when the structure constructor defined using
    DEFSTRUCT has been declared INLINE;

      > _Note_: Structures with _raw_ slots can currently be
      > stack-allocated only on x86 and x86-64. A raw slot is one
      > whose declared type is a subtype of exactly one of:
      > DOUBLE-FLOAT, SINGLE-FLOAT, `(COMPLEX
      > DOUBLE-FLOAT)`, `(COMPLEX SINGLE-FLOAT)`, or SB-EXT:WORD; but
      > as an exception to the preceding, any subtype of FIXNUM is not
      > stored as raw despite also being a subtype of SB-EXT:WORD.

  - otherwise-inaccessible parts of objects recognized to be dynamic
    extent. The support for detecting when this applies is very
    sophisticated. The compiler can do this detection when any value
    form for a variable contains conditional allocations, function
    calls, inlined functions, anonymous closures, or even other
    variables. This allows stack allocation of complex structures.

  Examples:

      ;;; Declaiming a structure constructor inline before definition makes
      ;;; stack allocation possible.
      (declaim (inline make-thing))
      (defstruct thing obj next)

      ;;; Stack allocation of various objects bound to DYNAMIC-EXTENT
      ;;; variables.
      (let* ((list (list 1 2 3))
             (nested (cons (list 1 2) (list* 3 4 (list 5))))
             (vector (make-array 3 :element-type 'single-float))
             (thing (make-thing :obj list
                                :next (make-thing :obj (make-array 3))))
             (closure (let ((y ...)) (lambda () y))))
        (declare (dynamic-extent list nested vector thing closure))
        ...)

      ;;; Stack allocation of objects assigned to DYNAMIC-EXTENT variables.
      (let ((x nil))
        (declare (dynamic-extent x))
        (setq x (list 1 2 3))
        (dotimes (i 10)
          (setq x (cons i x)))
        ...)

      ;;; Stack allocation of arguments to a local function is equivalent
      ;;; to stack allocation of local variable values.
      (flet ((f (x)
               (declare (dynamic-extent x))
               ...))
        ...
        (f (list 1 2 3))
        (f (cons (cons 1 2) (cons 3 4)))
        ...)

      ;;; Stack allocation of &REST lists
      (defun foo (&rest args)
        (declare (dynamic-extent args))
        ...)

  As a notable exception to recognizing otherwise inaccessible parts
  of other recognized dynamic extent values, SBCL does not as of
  1.0.48.21 propagate dynamic-extentness through &REST arguments --
  but another conforming implementation might, so portable code should
  not rely on this.

      (declaim (inline foo))
      (defun foo (fun &rest arguments)
        (declare (dynamic-extent arguments))
        (apply fun arguments))

      (defun bar (a)
        ;; SBCL will heap allocate the result of (LIST A), and stack
        ;; allocate only the spine of the &rest list -- so this is
        ;; safe but unportable.
        ;;
        ;; Another implementation, including earlier versions of SBCL
        ;; might consider (LIST A) to be otherwise inaccessible and
        ;; stack-allocate it as well!
        (foo #'car (list a)))

  If dynamic extent constraints specified in the Common Lisp standard
  are violated, the best that can happen is for the program to have
  garbage in variables and return values; more commonly, the system
  will crash.

  In particular, it is important to realize that this can interact in
  suprising ways with the otherwise inaccessible parts criterion:

      (let* ((a (list 1 2 3))
             (b (cons a a)))
         (declare (dynamic-extent b))
         ;; Unless A is accessed elsewhere as well, SBCL will consider
         ;; it to be otherwise inaccessible -- it can only be accessed
         ;; through B, after all -- and stack allocate it as well.
         ;;
         ;; Hence returning (CAR B) here is unsafe.
         ...)

  SBCL also performs sophisticated escape analysis to enable automatic
  stack allocation of local functions without any bound dynamic extent
  declarations in many situations where the compiler can prove that no
  uses escape (traditional Lisp terminology names this situation \"all
  uses are downward funargs\"). For example, in the following
  function, the local function `#'PREDICATEP` is stack allocated,
  because the compiler understands that the built-in function
  POSITION-IF only uses its first argument as a downward funarg:

      (let ((acc 0))
        (flet ((predicatep (num) (plusp (+ num off))))
          (dotimes (i 10)
            (incf acc (position-if #'predicatep array)))
          (if (plusp off)
              (incf acc (if (positivep acc) 10 3))
              (incf acc (position-if #'predicatep array))))
        acc)

  Users can also declare that their own functions take downward
  funargs by adding bound dynamic extent declarations on the function
  arguments.

      (defun trivial-hof (fun arg)
        (declare (dynamic-extent fun))
        (funcall fun 3 arg))

  Currently, such dynamic extent declarations only cause stack
  allocation of downward funargs at call sites on sufficiently unsafe
  policy. This is partly because the compiler is currently not able to
  detect incorrect usage of dynamic extent declarations.

      (defun autodxclosure1 (&optional (x 4))
        ;; Calling a higher-order function will only implicitly
        ;; stack-allocate a funarg if the callee is trusted (a CL:
        ;; function) or the caller is unsafe.
        (declare (optimize speed (safety 0) (debug 0)))
        (trivial-hof (lambda (a b) (+ a b x)) 92))")

(defsection @modular-arithmetic (:title "Modular Arithmetic")
  "Some numeric functions have a property: n lower bits of the
  result depend only on n lower bits of (all or some) arguments. If
  the compiler sees an expression of form `(LOGAND <EXPR> <MASK>)`,
  where `<EXPR>` is a tree of such _good_ functions and `<MASK>` is
  known to be of type `(UNSIGNED-BYTE <W>)`, where `<W>` is a _good_
  width, all intermediate results will be cut to `<W>` bits (but it is
  not done for variables and constants!). This often results in an
  ability to use simple machine instructions for the functions.

  Consider this example:

      (defun i (x y)
        (declare (type (unsigned-byte 32) x y))
        (ldb (byte 32 0) (logxor x (lognot y))))

  The result of `(LOGNOT Y)` will be negative and of type
  `(SIGNED-BYTE 33)`, so a naive implementation on a 32-bit platform
  is unable to use 32-bit arithmetic here. But modular arithmetic
  optimizer is able to do it: because the result is cut down to 32
  bits, the compiler will replace LOGXOR and LOGNOT with versions
  cutting results to 32 bits, and because terminals (here, expressions
  `X` and `Y`) are also of type `(UNSIGNED-BYTE 32)`, 32-bit machine
  arithmetic can be used.

  As of SBCL 0.8.5 good functions are `+`, `-`, LOGAND, LOGIOR,
  LOGXOR, LOGNOT and their combinations; and ASH with the positive
  second argument. Good widths are 32 on 32-bit CPUs and 64 on 64-bit
  CPUs. While it is possible to support smaller widths as well,
  currently this is not implemented."
  (@signed-modular-arithmetic section))

(defsection @signed-modular-arithmetic (:title "Signed Modular Arithmetic")
  "Sign-extending the result in the following way will be
  translated into signed modular arithmetic:

      (defun add (a b)
        (declare (type (signed-byte 64) a b))
        (let ((u (ldb (byte 64 0) (+ a b))))
          (logior u (- (mask-field (byte 1 63) u)))))")

(defsection @recognized-idioms (:title "Recognized Idioms")
  "Common Lisp doesn't directly expose all features present in
  modern hardware. Some code patterns are recognized and turned into
  more efficient hardware instructions without requiring the use of
  internal features."
  (@count-trailing-zeros section))

(defsection @count-trailing-zeros (:title "Count Trailing Zeros")
  "    (defun ctz (n)
        (declare (type (unsigned-byte 64) n))
        (integer-length (ldb (byte 64 0) (lognor n (- n)))))

  is turned into hardware instructions on arm64 and x86-64. It returns
  64 when `N` is 0. `N` can also be `(SIGNED-BYTE 64)` or FIXNUM.")

(defsection @global-and-always-bound-variables
    (:title "Global and Always-bound Variables")
  (sb-ext:defglobal macro)
  "- [__declaration__] SB-EXT:GLOBAL

      Syntax: `(SB-EXT:GLOBAL &REST SYMBOLS)`

      Only valid as a global proclamation.

      Specifies that the named symbols cannot be proclaimed or locally
      declared SPECIAL. Proclaiming an already special or constant
      variable name as SB-EXT:GLOBAL signal an error. Allows more
      efficient value lookup in threaded environments in addition to
      expressing programmer intention.

  - [__declaration__] SB-EXT:ALWAYS-BOUND

      Syntax: `(SB-EXT:ALWAYS-BOUND &REST SYMBOLS)`

      Only valid as a global proclamation.

      Specifies that the named symbols are always bound. Inhibits
      MAKUNBOUND of the named symbols. Proclaiming an unbound symbol
      as SB-EXT:ALWAYS-BOUND signals an error. Allows the compiler to
      elide boundness checks from value lookups.")

(defsection @miscellaneous-efficiency-issues
    (:title "Miscellaneous Efficiency Issues")
  "FIXME: The material in the CMUCL manual about getting good
  performance from the compiler should be reviewed, reformatted in
  Texinfo, lightly edited for SBCL, and substituted into this
  manual. In the meantime, the original CMUCL manual is still 95+%
  correct for the SBCL version of the Python compiler. See the
  sections

  - Advanced Compiler Use and Efficiency Hints
  - Advanced Compiler Introduction
  - More About Types in Python
  - Type Inference
  - Source Optimization
  - Tail Recursion
  - Local Call
  - Block Compilation
  - Inline Expansion
  - Object Representation
  - Numbers
  - General Efficiency Hints
  - Efficiency Notes

  Besides this information from the CMUCL manual, there are a few other
  points to keep in mind.

  - The CMUCL manual doesn't seem to state it explicitly, but Python
    has a mental block about type inference when assignment is
    involved. Python is very aggressive and clever about inferring the
    types of values bound with LET, LET*, inline function call, and so
    forth. However, it's much more passive and dumb about inferring
    the types of values assigned with SETQ, SETF, and friends. It
    would be nice to fix this, but in the meantime don't expect that
    just because it's very smart about types in most respects it will
    be smart about types involved in assignments. (This doesn't affect
    its ability to benefit from explicit type declarations involving
    the assigned variables, only its ability to get by without
    explicit type declarations.)"
  ;; FIXME: Python dislikes assignments but not in type inference. The
  ;; real problems are loop induction, closed over variables and
  ;; aliases.
  "- Since the time the CMUCL manual was written, CMUCL (and thus SBCL)
    has gotten a generational garbage collector. This means that there
    are some efficiency implications of various patterns of memory
    usage which aren't discussed in the CMUCL manual. (Some new
    material should be written about this.)

  - SBCL has some important known efficiency problems. Perhaps the
    most important are

      - The garbage collector is not particularly efficient, at least
        on platforms without the generational collector (as of SBCL
        0.8.9, all except x86).

      - Various aspects of the PCL implementation of CLOS are more
        inefficient than necessary.

  Finally, note that Common Lisp defines many constructs which, in the
  infamous phrase, \"could be compiled efficiently by a sufficiently
  smart compiler\". The phrase is infamous because making a compiler
  which actually is sufficiently smart to find all these optimizations
  systematically is well beyond the state of the art of current
  compiler technology. Instead, they're optimized on a case-by-case
  basis by hand-written code, or not optimized at all if the
  appropriate case hasn't been hand-coded. Some cases where no such
  hand-coding has been done as of SBCL version 0.6.3 include

  - `(REDUCE #'F X)` where the type of `X` is known at compile time,

  - various bit vector operations, e.g. `(POSITION 0 SOME-BIT-VECTOR)`,

  - specialized sequence idioms, e.g. `(REMOVE ITEM LIST :COUNT 1)`,

  - cases where local compilation policy does not require excessive
    type checking, e.g. `(LOCALLY (DECLARE (SAFETY 1)) (ASSOC ITEM LIST))`
    (which currently performs safe ENDP checking internal to ASSOC).

  If your system's performance is suffering because of some construct
  which could in principle be compiled efficiently, but which the SBCL
  compiler can't in practice compile efficiently, consider writing a
  patch to the compiler and submitting it for inclusion in the main
  sources. Such code is often reasonably straightforward to write;
  search the sources for the string `deftransform` to find many
  examples (some straightforward, some less so).")
