(in-package :sb-manual)

(defsection @efficiency (:title "Efficiency"
                         :concepts ("efficicency"))
  "In SBCL as with any language on any computer, the path to efficient
  code starts with good algorithms and sensible programming
  techniques, but to avoid inefficiency pitfalls, you need to know
  some of this implementation's quirks and features. This chapter is
  mostly a fairly long and detailed overview of what optimizations the
  compiler does. Although there are the usual negative suggestions of
  inefficient features to avoid, the main emphasis is on describing
  the things that programmers can count on being efficient.

  The optimizations described here can have the effect of speeding up
  existing programs written in conventional styles, but the potential
  for new programming styles that are clearer and less error-prone is
  at least as significant. For this reason, several sections end with
  a discussion of the implications of these optimizations for
  programming style.

  In this section, we discuss SBCL's support for __types__, which is
  unusual in three major ways:

  - @PRECISE-TYPE-CHECKING encourages the specific use of type
    declarations as a form of run-time consistency checking. This
    speeds development by localizing type errors and giving more
    meaningful error messages. The compiler produces completely safe
    code; @TYPE-CHECK-OPTIMIZATION maintains reasonable efficiency.

  - Comprehensive support for the Common Lisp type system makes
    complex type specifiers useful. Using type specifiers such as OR
    and MEMBER has both efficiency and robustness advantages (see
    @MORE-ABOUT-TYPES).

  - @TYPE-INFERENCE eliminates the need for some declarations and also
    aids compile-time detection of type errors. Given detailed type
    declarations, type inference can often eliminate type checks and
    enable more efficient object representations and code sequences.
    Checking all types results in fewer type checks.

  Then, we move on to the topic of __optimization__. The main barrier
  to writing efficient Lisp programs is that it's hard to find to the
  most efficient solution among the many semantically equivalent
  \"reasonable\" ways to code a given problem.
  __@SOURCE-OPTIMIZATION__ greatly reduces the variance in performance
  across equivalent solutions, freeing the programmer from a class of
  low-level considerations.

  The next important topic is __function call__ efficiency. This is
  especially important in Lisp due local functions and recursion. For
  this reason, Lisp implementations strive to make the general (or
  full) function call as inexpensive as possible. SBCL goes beyond
  this by providing two good alternatives to full call:

  - __@LOCAL-CALL__ resolves function references at compile time,
    allowing better calling sequences and optimization across function
    calls.

      - Control transfer is a direct jump.

      - The closure environment is passed in registers rather than
        heap allocated.

      - Keyword arguments and multiple values are implemented more
        efficiently.

  - __@INLINE-EXPANSION__ totally eliminates call overhead and allows
    many context-dependent optimizations. This provides a safe and
    efficient implementation of operations with function semantics,
    eliminating the need for error-prone macro definitions or manual
    case analysis.

  Generally, the compiler provides simple implementations for simple
  uses of function call, rather than having only a single calling
  convention. These features allow a more natural programming style:

  - Proper __@TAIL-CALL__s

  - Relatively efficient closures

  - A FUNCALL that is as efficient as normal named call

  We also discuss how the compiler handles the tension between
  efficient, natural @REPRESENTATION-OF-OBJECTS and the dynamic nature
  of Common Lisp. @EFFICIENCY-NOTES open a window into the compiler's
  view."
  ;; I'm not sure the SBCL user manual is the place for the following
  ;; soap box. -- MG, 2026-10-25
  "Finally, we can only touch upon some of the most important concepts
  discussed later before this introduction gets too long, so let's
  focus on the big picture. Writing efficient code that works is a
  complex and prolonged process. It is important not to get so
  involved in the pursuit of efficiency that you lose sight of what
  the original problem demands. Remember that:

  - The program should be correct: it doesn't matter how quickly you
    get the wrong answer.

  - Both the programmer and the user will make errors, so the program
    must be robust: it must detect errors in a way that allows easy
    correction.

  - A small portion of the program will consume most of the resources,
    with the bulk of the code being virtually irrelevant to efficiency
    considerations. Even experienced programmers familiar with the
    problem area cannot reliably predict where these hot spots will
    be.

  The best way to get efficient code that is still worth using, is to
  separate coding from tuning. During coding, you should:

  - Use a coding style that aids correctness and robustness without
    being incompatible with efficiency.

  - Choose appropriate data structures that allow efficient algorithms
    and object representations. Try to make interfaces abstract enough
    so that you can change to a different representation if @PROFILING
    reveals a need.

  - Whenever you make an assumption about a function argument or
    global data structure, add consistency assertions, either with
    type declarations or explicit uses of ASSERT, ECASE, etc.

  During tuning, you should:

  - Follow the @GENERAL-EFFICIENCY-HINTs.

  - Declare @SPEED-critical code and pay attention to
    @EFFICIENCY-NOTES.

  - Identify the hot spots in the program through @PROFILING.

  - Identify inefficient constructs in the hot spot with efficiency
    notes, more profiling, or manual inspection of the source and with
    judicious use of DISASSEMBLE.

  - Add declarations and consider the application of discussed in the
    section.

  - If all else fails, consider algorithm or data structure changes.
    If you did a good job coding, changes will be easy to introduce."
  (@more-about-types section)
  (@type-inference section)
  (@source-optimization section)
  (@tail-call section)
  (@local-call section)
  (@block-compilation section)
  (@inline-expansion section)
  (@slot-access section)
  (@stack-allocation section)
  (@representation-of-objects section)
  (@efficiency-notes section)
  (@recognized-idioms section)
  (@global-and-always-bound-variables section)
  (@miscellaneous-efficiency-issues section))

(defsection @more-about-types (:title "More About Types"
                               :concepts (("types")))
  "This section goes into more detail describing what types and
  declarations are recognized by SBCL. Some of the most important
  features of SBCL are related to types:

  - @PRECISE-TYPE-CHECKING helps to find bugs at run time.

  - Compile-time type checking helps to find bugs at compile time (see
    @DYNAMIC-TYPE-INFERENCE for an example).

  - @TYPE-INFERENCE minimizes the need for generic operations and also
    increases the efficiency of run-time type checking and the
    effectiveness of compile-time type checking.

  - Support for detailed types provides a wealth of opportunity for
    operation-specific type inference and optimization."
  (@more-types-meaningful section)
  (@type-canonicalization section)
  (@member-types section)
  (@union-types section)
  (@the-empty-type section)
  (@function-types section)
  (@the-values-declaration section)
  (@structure-types section)
  (@type-restrictions section)
  (@type-style-recommendations section))

(defsection @more-types-meaningful (:title "More Types Meaningful")
  "Common Lisp has a very powerful type system, but conventional
  implementations typically only recognize the small set of types
  special in that implementation. In these systems, there is an
  unfortunate paradox: a declaration for a relatively general type
  like FIXNUM will be recognized by the compiler, but a highly
  specific declaration such as `(INTEGER 3 17)` may be totally
  ignored.

  This is obviously a problem, since the user has to know how to
  specify the type of an object in the way the compiler wants it. A
  very minimal (but rarely satisfied) criterion for type system
  support is that it be no worse to make a specific declaration than
  to make a general one. SBCL goes beyond this by exploiting a number
  of advantages obtained from detailed type information.

  Using more restrictive types in declarations allows the compiler to
  do better type inference and more compile-time type checking. Also,
  when type declarations are considered to be consistency assertions
  that should be verified (conditional on policy), then complex types
  are useful for making more detailed assertions.

  SBCL \"understands\" the list-style OR, MEMBER, FUNCTION, ARRAY and
  number type specifiers. Understanding means that:

  - If the type contains more information than is used in a particular
    context, then the extra information is simply ignored, rather than
    derailing type inference.

  - In many contexts, the extra information from these type specifier
    is used to good effect. In particular, with SBCL's
    @PRECISE-TYPE-CHECKING, these complex types can be used in
    declarations to make interesting assertions about functions and
    data structures. More specific declarations also aid type
    inference and reduce the cost for type checking (see
    @TYPE-CHECK-OPTIMIZATION)."
  ;; FIXME: once we have these sections: "For related information, see
  ;; @NUMERIC-TYPES and @ARRAY-TYPES."
  )

(defsection @type-canonicalization (:title "Type Canonicalization"
                                    :concepts (("types," "equivalence")
                                               ("canonicalization" "of types")
                                               ("equivalence" "of types")))
  "When given a type specifier, SBCL will often rewrite it into a
  different (but equivalent) type. This is the mechanism that the
  compiler uses for detecting type equivalence. For example, in
  its canonical representation,

      (or list (member :end))

  is equivalent to

      (or cons (member nil :end))

  This has two implications for the user:

  - The standard symbol type specifiers (e.g. NULL and LIST) are
    in no way magical. The NULL type is actually defined as
    `(MEMBER NIL)` and LIST as `(OR CONS NULL)`.

  - When the compiler prints out a type, it may not look like the type
    specifier that originally appeared in the program. This is
    generally not a problem, but it must be taken into consideration
    when reading compiler error messages.")

(defsection @member-types (:title "MEMBER Types"
                           :concepts (("member types")
                                      ("types," "member")))
  "The MEMBER type specifier can be used to represent
  \"symbolic\" values, analogous to the enumerated types of Pascal.
  For example, the second value of FIND-SYMBOL has type

      (member :internal :external :inherited nil)

  Member types are very useful for expressing consistency constraints on data
  structures, for example:

      (defstruct ice-cream
        (flavor :vanilla :type (member :vanilla :chocolate :strawberry)))

  Member types are also useful in @TYPE-INFERENCE, as the number of
  members can sometimes be pared down to one, in which case the value
  is a known constant.")

(defsection @union-types (:title "Union Types"
                          :concepts (("union types")
                                     ("or types")
                                     ("types," "union")
                                     ("types," "or")))
  "The OR (union) type specifier is understood and is
  meaningfully applied in many contexts. The use of OR allows
  assertions to be made about types in dynamically typed programs. For
  example:

      (defstruct box
        (next nil :type (or box null))
        (top :removed :type (or box-top (member :removed))))

  The type assertion on the `TOP` slot ensures that an error will be
  signaled when there is an attempt to store an illegal value (such as
  `:RMVD`.) Although somewhat weak, these union type assertions
  provide a useful input into type inference, allowing the cost of
  type checking to be reduced. For example, this loop is safely
  compiled with no type checks:

      (defun find-box-with-top (box)
        (declare (type (or box null) box))
        (do ((current box (box-next current)))
            ((null current))
          (unless (eq (box-top current) :removed)
            (return current))))

  Union types are also useful in type inference for representing types
  that are partially constrained. For example, the result of

      (if foo
          (logior x y)
          (list x y))

  can be expressed as `(OR INTEGER CONS)`.")

(defsection @the-empty-type (:title "The Empty Type"
                             :concepts (("empty type")
                                        ("types," "empty")
                                        ("NIL type")
                                        ("types," "NIL")))
  "The type NIL is also called the empty type, since no object is of that
  type. The union of no types, `(OR)`, is also empty. The compiler's
  interpretation of an expression whose type is NIL is that the
  expression never yields any value but rather fails to terminate or
  exits non-locally. For example, the type of a call to ERROR or a use
  of RETURN is NIL. When the type of an expression is empty,
  compile-time type warnings about its value are suppressed;
  presumably somebody else is signaling an error. If a function is
  declared to have return type NIL but does in fact return, then (in
  [safe code][ @type-declaration-checking]) an error will be signaled.")

(defsection @function-types (:title "Function Types"
                             :concepts (("function" "types")
                                        ("types," "function")))
  "Function types (FTYPE declarations) are understood in the restrictive
  sense, specifying:

  - _The argument syntax that the function must be called with_

      This is information about what argument counts are acceptable,
      and which keyword arguments are recognized. In SBCL, warnings
      about argument syntax are a consequence of function type
      checking.

  - _The types of the argument values that the caller must pass_

      If the compiler can prove that some argument to a call is of a
      type disallowed by the called function's type, then it will give
      a compile-time type warning. In addition to being used for
      compile-time type checking, these type assertions are also used
      as output type assertions in code generation. For example, if
      `FOO` is declared to have a FIXNUM argument, then the 1+
      in `(FOO (1+ X))` is compiled with knowledge that the result
      must be a FIXNUM.

  - _The types the values that will be bound to argument variables in
    the function's definition_

      Declaring a function's type with FTYPE implicitly declares the
      types of the arguments in the definition. The compiler checks
      for consistency between the definition and the FTYPE
      declaration. Because of @PRECISE-TYPE-CHECKING, an error will be
      signaled when a function is called with an argument of the wrong
      type.

  - _The type of return value(s) that the caller can expect_

      This information is a useful input to @TYPE-INFERENCE at call
      sites. For example, if a function is declared to return a
      FIXNUM, then when a call to that function appears in an
      expression, the expression will be compiled with knowledge that
      the call will return a FIXNUM (see @TYPE-DECLARATION-CHECKING).

  - _The type of return value(s) that the definition must return_

      The result type in an FTYPE declaration is treated like an
      implicit THE wrapped around the body of the definition. If the
      definition returns a value of the wrong type, an error will be
      signaled. If the compiler can prove that the function returns
      the wrong type, then it will give a compile-time warning.

  This is consistent with the new interpretation of function types and
  the FTYPE declaration in the [proposed X3J13
  function-type-argument-type-semantics cleanup][
  issue:function-type-argument-type-semantics clhs].

  See also @GLOBAL-FUNCTION-TYPE-INFERENCE.")

(defsection @the-values-declaration (:title "The VALUES declaration"
                                     :concepts (("values declaration")))
  "SBCL supports the VALUES declaration as an extension to Common Lisp."
  (values declaration)
  "This declaration is semantically equivalent to a THE form wrapped
  around the body of the special form in which the VALUES declaration
  appears. The advantage of VALUES over THE is purely syntactic: it
  doesn't introduce more indentation. For example,

      (defun foo (x)
        (declare (values single-float))
        (ecase x
          (:this ...)
          (:that ...)
          (:the-other ...)))

  is equivalent to

      (defun foo (x)
        (the single-float
             (ecase x
               (:this ...)
               (:that ...)
               (:the-other ...))))

  and

      (defun floor (number &optional (divisor 1))
        (declare (values integer real))
        ...)

  is equivalent to

      (defun floor (number &optional (divisor 1))
        (the (values integer real)
             ...))

  In addition to being recognized by LAMBDA (and hence by DEFUN), the
  VALUES declaration is recognized by all the other special forms with
  bodies and declarations: LET, LET*, LABELS and FLET. Macros with
  declarations usually splice the declarations into one of the above
  forms, so they will accept this declaration too, but the exact
  effect of a VALUES declaration will depend on the macro.

  If you declare the types of all arguments to a function and also
  declare the return value types with VALUES, you have described the
  type of the function. The compiler will use this argument and result
  type information to derive a function type that will then be applied
  to calls of the function (see @FUNCTION-TYPES). This provides a way
  to declare the types of functions that is much less syntactically
  awkward than using the FTYPE declaration with a FUNCTION type
  specifier.

  Although the VALUES declaration is non-standard, it is relatively
  harmless to use it in otherwise portable code, since any warning in
  non-SBCL implementations can be suppressed with the standard
  DECLARATION proclamation:

      (declaim (declaration values))")

(defsection @structure-types (:title "Structure Types"
                              :concepts (("structure types")
                                         ("defstruct types")
                                         ("types," "structure")))
  "Structure accessors have @PRECISE-TYPE-CHECKING:

  - The structure argument to structure accessors is precisely checked
    -- if you call `FOO-A` on a `BAR`, an error will be signaled.

  - The types of slot values are precisely checked -- if you pass
    the wrong type argument to a constructor or a slot setter, then an
    error will be signaled.

  An additional advantage of checking structure types and enforcing
  slot types is that the compiler can [safely][
  @type-declaration-checking] trust slot type declarations. The
  compiler effectively moves the type checking from the slot access to
  the slot setter or constructor call. This is more efficient since
  caller of the setter or constructor often knows the type of the
  value, entirely eliminating the need to check the value's type.
  Consider this example:

      (defstruct coordinate
        (x nil :type single-float)
        (y nil :type single-float))

      (defun make-it ()
        (make-coordinate :x 1.0 :y 1.0))

      (defun use-it (it)
        (declare (type coordinate it))
        (sqrt (expt (coordinate-x it) 2) (expt (coordinate-y it) 2)))

  `MAKE-IT` and `USE-IT` are compiled with no checking on the types of
  the float slots, yet `USE-IT` can use SINGLE-FLOAT arithmetic with
  perfect safety. Note that `MAKE-COORDINATE` must still check the
  values of `X` and `Y` unless the call is in @BLOCK-COMPILATION or
  undergoes @INLINE-EXPANSION. But even without this advantage, it is
  almost always more efficient to check slot values on structure
  initialization, since slots are usually written once and read many
  times.

  When redefining a structure, one can choose to whether to invalidate
  previously compiled code (the CONTINUE restart) or not (the
  `RECKLESSLY-CONTINUE` restart). With CONTINUE, previously compiled
  calls to accessors will signal an error in [safe code][
  @type-declaration-checking]. With `RECKLESSLY-CONTINUE`, safety is
  compromised."
  ;; Note that RECKLESSLY-CONTINUE is not exported. It's purely for
  ;; interactive purposes apparently.
  (sb-ext:freeze-type declaration))

;;; FIXME: This section is misnamed. Also, it's not very precise, as
;;; this clearly works:
;;;
;;; (defun foo (x)
;;;   (declare (type (and fixnum (satisfies evenp)) x)
;;;            (optimize speed))
;;;   (1+ x))
(defsection @type-restrictions (:title "Type Restrictions"
                                :concepts (("types," "restrictions on")))
  "Be careful with AND, NOT and SATISFIES types in declarations, since
  @TYPE-INFERENCE has problems with them. When these types do appear
  in a declaration, they are still checked precisely, but the type
  information may be of limited use to the compiler.

  AND types are effective as long as the intersection can be
  canonicalized to a type that doesn't use AND. For example,

      (and fixnum unsigned-byte (not (member 7)))

  is fine, since it is the same as

      (or (integer 0 6) (integer 8 most-positive-fixnum))

  but this type:

      (and symbol (not (member :end)))

  will not be fully understood by type interference since the AND
  can't be removed by canonicalization.

  Using any of these type specifiers in a type test with TYPEP or
  TYPECASE is fine, since as tests, these types can be translated into
  the AND macro, the NOT function or a call to the satisfies
  predicate.")

(defsection @type-style-recommendations (:title "Type Style Recommendations"
                                         :concepts (("style recommendations")))
  "With SBCL it is desirable to make declarations as precise as possible,
  but @TYPE-INFERENCE also makes some declarations unnecessary. Here
  are some general guidelines for maximum robustness and efficiency:

  - Declare the types of all function arguments and structure slots as
    precisely as possible (while avoiding NOT, AND and SATISFIES). Put
    these declarations in during initial coding so that type
    assertions can find bugs for you during debugging.

  - Durining development, prefer declaring function argument types
    with with DECLARE within the function to avoid having to redefine
    FTYPEs, which can lead to safety problems (see
    @TYPE-DECLARATION-CHECKING).

  - Use the MEMBER type specifier where there are a small number of
    possible symbol values, for example, `(MEMBER :RED :BLUE :GREEN)`.

  - Use the OR type specifier in situations where the type is not
    certain, but there are only a few possibilities, for example,
    `(OR LIST VECTOR)`.

  - Declare integer types with the tightest bounds that you can, such
    as `(INTEGER 3 7)`.

  - Put DEFTYPE or DEFSTRUCT definitions before they are used.
    Definition after use is legal (producing no undefined type
    warnings), but type tests and structure operations will be
    compiled much less efficiently.

  - Use the SB-EXT:FREEZE-TYPE declaration to speed up type testing
    for structure types that won't have new subtypes added later.

  - In addition to declaring the array element type and simpleness,
    also declare the dimensions if they are fixed. For example,

          (simple-array single-float (1024 1024))

    The bounds information allows array indexing for multi-dimensional
    arrays to be compiled much more efficiently and may also allow
    array bounds checking to be done at compile time.

  - Avoid use of the THE declaration within expressions. Not only does
    it clutter the code, but it is often unnecesary under [safe
    policies][ @type-declaration-checking]. If the need for a return
    type assertion is revealed by @EFFICIENCY-NOTES during tuning,
    then you can consider THE, but it is preferable to constrain the
    argument types more, allowing the compiler to prove the desired
    result type.

  - Don't bother declaring the type of LET or other non-argument
    variables unless the type is non-obvious. If you declare function
    return types and structure slot types, then the type of a variable
    is often obvious both to the programmer and to the compiler. An
    important case where the type isn't obvious, and a declaration is
    appropriate, is when the value for a variable is pulled out of
    untyped structure (e.g. the result of CAR), from a CLASS, or comes
    from some weakly typed function, such as READ.

  - Declarations are sometimes necessary for integer loop variables,
    since the compiler can't always prove that the value is of a good
    integer type. These declarations are best added during tuning,
    when an @EFFICIENCY-NOTE indicates the need.")

(defsection @type-inference (:title "Type Inference"
                             :concepts (("type inference")
                                        ("derivation of types")))
  "Type inference is the process by which the compiler tries to figure
  out the types of expressions and variables, given an inevitable lack
  of complete type information. Although SBCL does more type inference
  than most Common Lisp compilers, remember that the more precise and
  comprehensive type declarations are, the more type inference will be
  able to do."
  (@variable-type-inference section)
  (@local-function-type-inference section)
  (@global-function-type-inference section)
  (@operation-specific-type-inference section)
  (@dynamic-type-inference section)
  (@type-check-optimization section))

(defsection @variable-type-inference
    (:title "Variable Type Inference"
     :concepts (("variable type inference")
                ("type inference," "variable")))
  "The type of a variable is the union of the types of all the
  definitions. In the degenerate case of a LET, the type of the
  variable is the type of the initial value. This inferred type is
  intersected with any declared type and is then propagated to all the
  variable's references. The types of MULTIPLE-VALUE-BIND variables
  are similarly inferred from the types of the individual values of
  the values form.

  If multiple type declarations apply to a single variable, then all
  the declarations must be correct; it is as though all the types were
  intersected producing a single [AND][(clhs type)] type specifier. In
  this example

      (defmacro my-dotimes ((var count) &body body)
        `(do ((,var 0 (1+ ,var)))
             ((>= ,var ,count))
           (declare (type (integer 0 *) ,var))
           ,@body))

      (my-dotimes (i ...)
        (declare (fixnum i))
        ...)

  the two declarations for `I` are intersected, which yields that it
  must be a non-negative fixnum.

  In practice, this type inference is limited to
  @LOCAL-CALLs (including @LET-CALLs), since the compiler can't
  analyze all the calls to a global function. But type inference works
  well enough on local variables so that it is often unnecessary to
  declare the type of local variables. This is especially likely when
  function result types and structure slot types are declared. The
  main areas where type inference breaks down are:

  - When the initial value of a variable is an untyped expression,
    such as `(CAR X)`, and

  - When the type of one of the variable's definitions is a function
    of the variable's current value, as in `(SETQ X (1+ X))`.")

(defsection @local-function-type-inference
    (:title "Local Function Type Inference"
     :concepts (("local call" "type inference")))
  "The types of arguments to local functions are inferred in the same way
  as any other local variable; the type is the union of the argument
  types across all the calls to the function, intersected with the
  declared type. If there are any assignments to the argument
  variables, the type of the assigned value is unioned in as well.

  The result type of a local function is computed in a special way
  that takes @TAIL-RECURSION into consideration. The result type is
  the union of all possible return values that aren't tail recursive.
  For example, the compiler will infer that the result type of this
  function is INTEGER:

      (defun ! (n res)
        (declare (integer n res))
        (if (zerop n)
            res
            (! (1- n) (* n res))))

  Although this is a rather obvious result, it becomes somewhat less
  trivial in the presence of mutual tail recursion of multiple
  functions. Local function result type inference interacts with the
  mechanisms for ensuring proper tail calls mentioned in
  @LOCAL-CALL-RETURN-VALUES.")

(defsection @global-function-type-inference
    (:title "Global Function Type Inference"
     :concepts (("function," "type inference")))
  "As described in @TYPE-DECLARATION-CHECKING, global function
  type (FTYPE) declarations are trusted by the compiler at call sites
  unless the @DEBUG optimization quality is 3. This can result in
  unsafe propagation of the type constraints in the callee based on
  the declared types of its arguments and return values.

  For example, if we compile this:

      (declaim (ftype (function () cons) foo))
      (defun foo ()
        (cons 1 2))

      (defun bar ()
        (car (foo)))

  and subsequently redefine `FOO` as

      (declaim (ftype (function () t) foo))
      (defun foo ()
        1)

  we get a somewhat innocent looking style warning:

      ; caught STYLE-WARNING:
      ;   The new FTYPE proclamation for COMMON-LISP-USER::FOO
      ;     (FUNCTION NIL T)
      ;   does not match the old FTYPE proclamation
      ;     (FUNCTION NIL (VALUES CONS &REST T))

  However, here the consequences are on the harsher side of undefined
  behavior:

      CL-USER> (bar)
      Unhandled memory fault at #xFFFFFFFFFFFFFFFB.

  This happens because when compiling `BAR`, the compiler applied type
  arithmetic to the active FTYPE of `FOO` (`cons`). Because a `cons`
  satisfies `car`'s type requirement with 100% certainty, the compiler
  completely eliminated the runtime type check. Upon redefinition, `BAR`
  executes a raw memory pointer dereference on the integer `1`, triggering
  an immediate hardware-level segmentation fault.

  This happens because when compiling `BAR`, the compiler trusted the
  then active FTYPE of `FOO`, with return value type CONS, and thus
  omitted the type check for CONS from the inlined call to CAR.

  > _Note_: A similar example can be constructed based on declared
  > argument types.

  > _Note_: Without explicit FTYPE declarations and with
  > SB-EXT:*DERIVE-FUNCTION-TYPES* NIL, COMPILE-FILE itself does not
  > trust the derived function types, which does a lot to mitigate the
  > safety issue."
  (sb-ext:*derive-function-types* variable))

(defsection @operation-specific-type-inference
    (:title "Operation-specific Type Inference"
     :concepts (("operation-specific type inference")
            ("arithmetic type inference")
            ("numeric" "type inference")))
  "Many of the standard Common Lisp functions have special type inference
  procedures that determine the result type as a function of the
  argument types. For example, the result type of AREF is the array
  element type. Here are some other examples of type inferences:

      (logand x #xFF) => (unsigned-byte 8)

      (+ (the (integer 0 12) x) (the (integer 0 1) y)) => (integer 0 13)

      (ash (the (unsigned-byte 16) x) -8) => (unsigned-byte 8)")

(defsection @dynamic-type-inference (:title "Dynamic Type Inference"
                                     :concepts (("dynamic type inference")
                                                ("conditional type inference")
                                                ("type inference," "dynamic")))
  "SBCL uses flow analysis to infer types in dynamically typed
  programs. For example:

      (ecase x
        (list (length x))
        ...)

  Here, the compiler knows the type of the argument to LENGTH because
  LENGTH is called when X is a LIST. The most significant efficiency
  effect of inference from assertions is usually in type check
  optimization.

  A similar optimization is possible for

      (+ (car x) (length x))

  Dynamic type inference has two inputs: explicit conditionals and
  implicit or explicit type assertions. Flow analysis propagates these
  constraints on variable type to any code that can be executed only
  after passing through the constraint. Explicit type constraints come
  from IFs where the test is either a lexical variable or a function
  of lexical variables and constants, where the function is either a
  type predicate, a numeric comparison or EQ.

  If there is an EQ or EQL test, then the compiler will actually
  substitute one argument for the other in the true branch. For
  example,

      (when (eq x :yow!)
        (return x))

  becomes

      (when (eq x :yow!)
        (return :yow!))

  This substitution is done when one argument is a constant or one
  argument has better type information than the other. This
  transformation reveals opportunities for constant folding or
  type-specific optimizations. If the test is against a constant, then
  the compiler can prove that the variable is not that constant value
  in the false branch or `(NOT (MEMBER :YOW!))` in the example above.
  This can eliminate redundant tests. For example,

      (if (eq x nil)
          ...
          (if x a b))

  is transformed to

      (if (eq x nil)
          ...
          a)

  Variables appearing as IF tests are interpreted as
  `(NOT (EQ VAR NIL))` tests. The compiler also converts `=` into EQL
  where possible. It is difficult to do inference directly on = since
  it does implicit coercions.

  When there is an explicit `<` or `>` test on numeric variables, the
  compiler makes inferences about the ranges the variables can assume
  in the true and false branches. This is mainly useful when it proves
  that the values are small enough in magnitude to allow open-coding
  of arithmetic operations. For example, in many uses of DOTIMES with
  a FIXNUM repeat count, the compiler proves that fixnum arithmetic
  can be used.

  Implicit type assertions are quite common, especially if you declare
  function argument types. Dynamic inference from implicit type
  assertions sometimes helps to disambiguate programs to a useful
  degree but is most noticeable when it detects a dynamic type error.
  For example,

      (defun foo (x)
        (+ (car x) x))

  results in this warning:

      ; in: DEFUN FOO
      ;     (+ (CAR X) X)
      ;
      ; note: deleting unreachable code
      ;
      ; caught WARNING:
      ;   Derived type of X is
      ;     (VALUES LIST &OPTIONAL),
      ;   conflicting with its asserted type
      ;     NUMBER.")

(defsection @type-check-optimization
    (:title "Type Check Optimization"
     :concepts (("type checking" "optimization")
                ("optimization," "type checking")
                ("complemented type check")
                ("optimization," "complemented type check")))
  "SBCL backs up its support for @PRECISE-TYPE-CHECKING by minimizing
  the cost of run-time type checking. This is done both through
  @TYPE-INFERENCE and through optimizations of type checking itself.

  Type inference often allows the compiler to prove that a value is of
  the correct type, and thus no type check is necessary. For example:

      (defstruct foo a b c)
      (defstruct link
        (foo (required-argument) :type foo)
        (next nil :type (or link null)))

      (foo-a (link-foo x))

  Here, there is no need to check that the result of `LINK-FOO` is a
  `FOO` since it always is. Even when some type checks are necessary,
  type inference can often reduce the number:

      (defun test (x)
        (let ((a (foo-a x))
              (b (foo-b x))
              (c (foo-c x)))
          ...))

  In this above example, only one `(FOO-P X)` check is needed. This
  applies to a lesser degree in list operations, such as

      (if (eql (car x) 3)
          (cdr x)
          y)

  where we only have to check that `X` is a list once.

  Since the compiler recognizes explicit type tests, code that
  explicitly protects itself against type errors has little introduced
  overhead due to implicit type checking. For example, this loop
  compiles with no type checks for CAR and CDR due to the termination
  condition involving ATOM:

      (defun memq (e l)
        (do ((current l (cdr current)))
            ((atom current) nil)
          (when (eq (car current) e) (return current))))

  The compiler reduces the cost of checks that must be done through an
  optimization called _complementing_. A complemented check for
  `\\TYPE` is simply a check that the value is not of the type
  `(NOT TYPE)`. This is only interesting when something is known about
  the actual type, in which case we can test for the complement of
  `(AND KNOWN-TYPE (NOT TYPE))` or the difference between the known
  type and the assertion. An example:

      (link-foo (link-next x))

  Here, we change the type check for `LINK-FOO` from a test for `FOO`
  to a test for:

      (not (and (or foo null)
                (not foo)))

  or more simply `(NOT NULL)`. This is probably the most important use
  of complementing, since the situation is fairly common and a NULL
  test is much cheaper than a structure type test.

  Here is a more complicated example that illustrates the combination of
  complementing with dynamic type inference:

      (defun find-a (a x)
        (declare (type (or link null) x))
        (do ((current x (link-next current)))
            ((null current) nil)
          (let ((foo (link-foo current)))
            (when (eq (foo-a foo) a) (return foo)))))

  This loop can be compiled with no type checks. The `LINK` test for
  `LINK-FOO` and `LINK-NEXT` is complemented to `(NOT NULL)` and then
  deleted because of the explicit NULL test. As before, no check is
  necessary for `FOO-A`, since `LINK-FOO` is always a `FOO`. This sort
  of situation shows how @PRECISE-TYPE-CHECKING combined with precise
  declarations can actually result in reduced type checking.")

(defsection @source-optimization (:title "Source Optimization"
                                  :concepts (("source optimization")))
  "This section describes source-level transformations that the SBCL
  compiler does on programs in an attempt to make them more efficient.
  Although source-level optimizations can make existing programs more
  efficient, the biggest advantage of this sort of optimization is
  that it makes it easier to write efficient programs. If a clean,
  straightforward implementation can be transformed into an efficient
  one, then there is no need for tricky and dangerous hand
  optimization.

  Source-level optimization eliminates the need for macros to optimize
  their expansion, and also increases the effectiveness of
  @INLINE-EXPANSION."
  (@let-optimization section)
  (@constant-folding section)
  (@unused-expression-elimination section)
  (@control-optimization section)
  (@unreachable-code-deletion section)
  (@multiple-values-optimization section)
  (@style-recommendations section))

(defsection @let-optimization (:title "Let Optimization"
                               :concepts (("let" "optimization")
                                          ("optimization," "let")))
  "The primary optimization of let variables is to delete them when they
  are unnecessary. Whenever the value of a let variable is a constant,
  a constant variable or a constant (local or non-notinline) function,
  the variable is deleted, and references to the variable are replaced
  with references to the constant expression. This is useful primarily
  in the expansion of macros or inline functions, where argument
  values are often constant in any given call but are, in general,
  non-constant expressions that must be bound to preserve order of
  evaluation. Let variable optimization eliminates the need for macros
  to carefully avoid spurious bindings and also makes inline functions
  just as efficient as macros.

  A particularly interesting class of constant is a local function.
  Substituting for lexical variables that are bound to a function can
  substantially improve the efficiency of functional programming
  styles. For example,

      (let ((a #'(lambda (x) (zow x))))
        (funcall a 3))

  effectively transforms to

      (zow 3)

  This transformation is done even when the function is a closure, as in

      (let ((a (let ((y (zug)))
                 #'(lambda (x) (zow x y)))))
        (funcall a 3))

  becoming

      (zow 3 (zug))

  A constant variable is a lexical variable that is never assigned to,
  always keeping its initial value. Whenever possible, avoid setting
  lexical variables -- instead bind a new variable to the new value.
  Except for loop variables, it is almost always possible to avoid
  setting lexical variables. This form:

      (let ((x (f x)))
        ...)

  is more efficient than

      (setq x (f x))
      ...

  Setting variables makes the program more difficult to understand,
  both to the compiler and to the programmer. SBCL compiles
  assignments very efficiently, but most let optimizations are only
  done on constant variables.

  Constant variables with only a single use are also optimized away,
  even when the initial value is not constant (modulo side effects).
  For example, this expansion of INCF:

      (let ((#:g3 (+ x 1)))
        (setq x #:g3))

  becomes

      (setq x (+ x 1))

  The type semantics of this transformation are more important than
  the elimination of the variable itself. Consider what happens when
  `X` is declared to be a FIXNUM. After the transformation, the
  compiler can infer that a non-fixnum result would be a type error.
  This allows it to completely eliminate the code path for BIGNUMs,
  restricting the math exclusively to fast, register-level
  operations (though an overflow check may still be retained depending
  on @TYPE-DECLARATION-CHECKING).

  Another variable optimization deletes any variable that is never
  read. This causes the initial value and any assigned values to be
  unused, allowing those expressions to be deleted if they have no
  side-effects.

  Note that a LET is actually a degenerate case of @LOCAL-CALL, and
  that let optimization can be done on calls that weren't created by a
  LET. Also, local call allows an applicative style of iteration that
  is totally assignment-free.")

(defsection @constant-folding (:title "Constant Folding"
                               :concepts (("constant folding")
                                          ("optimization," "constant folding")))
  "Constant folding is an optimization that replaces a call of constant
  arguments with the constant result of that call. Constant folding is
  done on all standard functions for which it is legal. Inline
  expansion allows folding of any constant parts of the definition and
  can be done even on functions that have side-effects.

  It is convenient to rely on constant folding when programming, as in
  this example:

      (defconstant limit 42)

      (defun foo ()
        (... (1- limit) ...))

  Constant folding is also helpful when writing macros or inline
  functions, since it usually eliminates the need to write a macro
  that special-cases constant arguments.")

(defsection @unused-expression-elimination
    (:title "Unused Expression Elimination"
     :concepts (("unused expression elimination")
                ("dead code elimination")
                ("optimization," "unused expression elimination")
                ("optimization," "dead code elimination")))
  "If the value of any expression is not used, and the expression has no
  side-effects, then it is deleted. As with @CONSTANT-FOLDING, this
  optimization applies most often when cleaning up after inline
  expansion and other optimizations.

  Note that the compiler will eliminate parts of unused expressions
  known to be side-effect free, even if there are other unknown parts.
  For example, LIST has no side-effects, so

      (let ((a (list (foo) (bar))))
        (if t
            (zow)
            (raz a)))

  becomes

      (progn (foo) (bar))
      (zow)")

(defsection @control-optimization (:title "Control Optimization"
                                   :concepts (("control optimization")
                                              ("optimization," "control")))
  "The most important optimization of control is recognizing when an
  IF test is known at compile time, then deleting the IF, the test
  expression, and the unreachable branch. This can be considered a
  special case of constant folding, although the test doesn't have to
  be truly constant as long as it is definitely false or definitely
  not false. Note also, that @TYPE-INFERENCE propagates the result of
  an IF test to the true and false branches."
  ;; FIXME: add "(see @CONSTRAINT-PROPAGATION)" once that exists
  "A related IF optimization transforming

      (if (if a b c) x y)

  into

      (if a
          (if b x y)
          (if c x y))

  The opportunity for this sort of optimization usually results from a
  conditional macro. For example,

      (if (not a) x y)

  is actually implemented as

      (if (if a nil t) x y)

  which is transformed to

      (if a
          (if nil x y)
          (if t x y))

  which is then optimized to

      (if a y x)

  Note that due to the compiler's internal representations, the IF-IF
  situation will be recognized even if other forms are wrapped around
  the inner IF as in

      (if (let ((g ...))
            (loop
              ...
              (return (not g))
              ...))
          x y)

  In SBCL, all the Common Lisp macros really are macros written in
  terms of IF, BLOCK and TAGBODY, so user-defined control macros can
  be just as efficient as the standard ones. The compiler emits basic
  blocks using a heuristic that minimizes the number of unconditional
  branches. The code in a TAGBODY will not be emitted in the order it
  appeared in the source, so there is no point in arranging the code
  to make control drop through to the target.")

(defsection @unreachable-code-deletion
    (:title "Unreachable Code Elimination"
     :concepts (("unreachable code deletion")
                ("dead code elimination")
                ("optimization," "unreachable code deletion")
                ("optimization," "dead code elimination")))
  "The compiler will delete code whenever it can prove that the code can
  never be executed. Code becomes unreachable when

  - an IF is optimized away, or

  - there is an explicit unconditional control transfer such as GO or
    RETURN-FROM, or

  - the last reference to a local function is deleted (or there never
    was any reference).

  When code that appeared in the original source is deleted, the
  compiler prints a note to indicate a possible problem (or at least
  unnecessary code.) For example:

      (defun foo ()
        (if t
            (write-line \"True.\")
            (write-line \"False.\")))

  will result in this note:

      ; in: DEFUN FOO
      ;    (WRITE-LINE \"False.\")
      ;
      ; note: deleting unreachable code

  It is important to pay attention to unreachable code notes, since
  they often indicate a subtle type error. For example,

      (defstruct foo a b)

      (defun lose (x)
        (let ((a (foo-a x))
              (b (if x (foo-b x) \"never\")))
          (values a b)))

  results in this note:

      ; in: DEFUN LOSE
      ;     (IF X
      ;         (FOO-B X)
      ;         \"never\")
      ; ==>
      ;   \"never\"
      ;
      ; note: deleting unreachable code

  The `\"never\"` branch is unreachable because type inference knows
  that the argument to `FOO-A` must be a `FOO` and thus can't be NIL.
  Presumably the programmer forgot that `X` could be NIL when writing
  the binding for `A`.

  Here is an example with an incorrect declaration:

      (defun count-a (string)
        (do ((pos 0 (position #\\a string :start (1+ pos)))
             (count 0 (1+ count)))
            ((null pos) count)
          (declare (fixnum pos))))

  This time our note is:

      ; in: DEFUN COUNT-A
      ;     (DO ((POS 0 (POSITION #\\a STRING :START (1+ POS)))
      ;          (COUNT 0 (1+ COUNT)))
      ;         ((NULL POS) COUNT)
      ;       (DECLARE (FIXNUM POS)))
      ; --> BLOCK LET TAGBODY RETURN-FROM PROGN
      ; ==>
      ;   COUNT
      ;
      ; note: deleting unreachable code

  The problem here is that `POS` can never be null since it is
  declared a FIXNUM.

  It takes some experience with unreachable code notes to be able to
  tell what they are trying to say. In non-obvious cases, the best
  thing to do is to call the function in a way that should cause the
  unreachable code to be executed. Either you will get a type error,
  or you will find that there truly is no way for the code to be
  executed.

  Not all unreachable code results in a note:

  - A note is only given when the unreachable code textually appears
    in the original source. This prevents spurious notes due to the
    optimization of macros and inline functions but sometimes also
    foregoes a note that would have been useful.

  - Since accurate source information is not available for non-list
    forms, there is an element of heuristic in determining whether or
    not to give a note about an atom. Spurious notes may be given when
    a macro or inline function defines a variable that is also present
    in the calling function. Notes about easily introduced constants
    are never given since it is too easy to confuse them in expanded
    code with ones in the original source. These constants are NIL, T,
    keywords and unboxed values such as FIXNUMs, CHARACTERs, and
    SINGLE-FLOATs on 64-bit platforms.

  - Notes are only given about code unreachable due to control flow.
    There is no note when an expression is deleted because its value
    is unused, since this is a common consequence of other
    optimizations.")

(defsection @multiple-values-optimization
    (:title "Multiple Values Optimization"
     :concepts (("multiple values optimization")
                ("optimization," "multiple values")))
  "Within a function, the compiler implements uses of multiple values
  particularly efficiently. Multiple values can be kept in arbitrary
  registers, so using multiple values doesn't imply stack manipulation
  and representation conversion. For example,

      (let ((a (if x (foo x) u))
            (b (if x (bar x) v)))
        ...)

  is actually more efficient when written as

      (multiple-value-bind (a b)
          (if x
              (values (foo x) (bar x))
              (values u v))
        ...)

  Also, see @LOCAL-CALL-RETURN-VALUES for information on how local
  call provides efficient support for multiple function return values.")

(defsection @style-recommendations (:title "Style Recommendations")
  "Source level optimization makes possible a clearer and more relaxed
  programming style:

  - Don't use macros purely to avoid function call. If you want an
    inline function, write it as a function and declare it INLINE.
    It's clearer, less error-prone, and works just as well.

  - Don't write macros that try to optimize their expansion in trivial
    ways such as avoiding binding variables for simple expressions.
    The compiler does these optimizations too and is less likely to
    make a mistake.

  - Make use of local functions (i.e. LABELS and FLET) and @TAIL-CALLs
    in places where it is clearer. @LOCAL-CALL is faster than full
    call.

  - Avoid setting local variables when possible. Binding a new LET
    variable is at least as efficient as setting an existing variable
    and is easier to understand, both for the compiler and the
    programmer.

  - Instead of writing similar code over and over again so that it can
    be hand customized for each use, define a macro or inline
    function, and let the compiler do the work.")

(defsection @tail-call (:title "Tail Call"
                        :concepts (~tail-call))
  "A call to a function is a tail call if nothing has to be done after
  the call returns. That is, when the call returns, the returned
  values are immediately returned from the calling function. Tail
  calls can be compiled to much more efficient code than normal code.
  See <https://en.wikipedia.org/wiki/Tail_call> for more.

  Note that both @LOCAL-CALLs and full calls can be tail calls."
  (@tail-call-optimization section)
  (@tail-position section)
  (@tail-recursion section))

(defsection @tail-call-optimization (:title "Tail Call Optimization"
                                     :concepts (~tail-call-optimization))
  "SBCL performs tail call optimization (TCO) although this is not
  required by the Common Lisp standard.

  In Lisp terms, you can think of TCO as converting a function call to
  a PSETQ that assigns the argument values to the called function's
  variables, followed by a GO to the start of the called function.
  This makes clear an inherent efficiency advantage of tail calls: in
  addition to not having to allocate a stack frame, there is no need
  to prepare for the call to return (e.g. by computing a return PC).

  Consider the following trivial example:

      (defun fun1 (x)
        (fun2 x x))

  FUN1 is compiled to native code that modifies its frame
  (to have the arguments of `FUN2` in the right places) and control is
  transferred via a single jump instruction to `FUN2`.

  In general, a call is compiled to a tail call if it is in
  @TAIL-POSITION and the @DEBUG optimization quality is at most 2.
  Thus, by default, all calls in tail position are compiled to tail
  calls. See @DEBUGGING-TAIL-CALLS.")

(defsection @tail-position (:title "Tail Position")
  "A call is said to be in tail position unless it is enclosed by at
  least one of the following:

  - a special binding (e.g. by LET, PROGV),

  - a CATCH,

  - an UNWIND-PROTECT,

  - a BLOCK and the block name has been closed over,

  - a TAGBODY and a GO tag has been closed over.

  These inhibit @TAIL-CALL-OPTIMIZATION because they require cleanups
  when the stack is unwound. ~TAIL-CALL-OPTIMIZATION")

(defsection @tail-recursion (:title "Tail Recursion"
                             :concepts (~tail-recursion))
  "Tail recursion is when a function makes a @TAIL-CALL to
  itself. In this example, the recursive call to `MYFUN` is
  tail-recursive:

      (defun myfun (x)
        (if (oddp (random x))
            (isqrt x)
            (myfun (1- x))))

  Because it is a @TAIL-CALL, tail recursion can be compiled to much
  more efficient code than general recursion. In fact, tail recursion
  is equivalent to iteration both semantically and in efficiency.

  Why would you want to write a program recursively when you can write
  it using a loop? Well, the main answer is that recursion is a more
  general mechanism, so it can express some solutions simply that are
  awkward to write as a loop. Some programmers also feel that
  recursion is a stylistically preferable way to write loops because
  it avoids assigning variables. For example, instead of writing

      (defun fun1 (x)
        (something x))

      (defun fun2 (y)
        (other-thing y))

      (do ((x 7 (fun2 (fun1 x))))
          (nil))

  ... you can write

      (defun fun1 (x)
        (fun2 (something x)))

      (defun fun2 (y)
        (fun1 (other-thing y)))

      (fun1 7)

  The tail-recursive definition is actually more efficient, in
  addition to being (arguably) clearer. As the number of functions and
  the complexity of their call graph increases, the simplicity of
  using recursion becomes compelling. Consider the advantages of
  writing a large finite-state machine with separate tail-recursive
  functions instead of using a single huge PROG.")

(defsection @local-call (:title "Local Call"
                         :concepts (~local-call ~full-call))

  "The compiler supports two kinds of function call: full call and local
  call. Full call is the standard calling convention; its late binding
  and generality make Lisp what it is but create unavoidable
  overheads. When the compiler can compile the calling function and
  the called function simultaneously, it can use local call to avoid
  some of this overhead. Local call is really a collection of
  compilation strategies that omit some aspect of full call. In some
  cases, local call can be totally free.

  Local call makes the use of the lexical function binding forms FLET
  and LABELS much more efficient. A local call is always faster than a
  full call, and in many cases it is much faster. Similarly, local
  call is a natural approach to block compilation (see COMPILE-FILE),
  a compilation technique that resolves function references at compile
  time. Block compilation speeds up function calls but increases
  compilation times and prevents function redefinition."
  (@local-self-recursive-call section)
  (@let-call section)
  (@local-calls-to-closures section)
  (@local-tail-call section)
  (@local-call-return-values section))

(defsection @local-self-recursive-call (:title "Local Self-recursive Call"
                                        :concepts (~local-self-recursive-call))
  "@LOCAL-CALL is used when a function defined by DEFUN calls
  itself and block compilation is used (see COMPILE-FILE). For
  example:

      (defun fact (n)
        (if (zerop n)
            1
            (* n (fact (1- n)))))

  This use of local call speeds recursion but can also complicate
  debugging since TRACE will only show the first call to FACT and not
  the recursive calls. This is because the recursive calls directly
  jump to the start of the function and don't indirect through
  SYMBOL-FUNCTION. This is similar to @DEBUGGING-TAIL-CALLS.")

(defsection @let-call (:title "Let Call"
                       :concepts (~let-call))
  "Because @LOCAL-CALL avoids unnecessary call overheads, the compiler
  internally uses local call to implement some macros and special
  forms that are not normally thought of as involving a function call.
  For example, this LET:

      (let ((a (foo))
            (b (bar)))
        ...)

  is internally represented as though it was macroexpanded into:

      (funcall #'(lambda (a b)
                   ...)
               (foo)
               (bar))

  which results in just as good code as compiling LET directly, but it
  also benefits local calls that are semantically the same as LET. For
  example, these definitions are all the same as far as the compiler
  is concerned:

      (defun foo ()
        ...some other stuff...
        (let ((a something))
          ...some stuff...))

      (defun foo ()
        (flet ((localfun (a)
                 ...some stuff...))
          ...some other stuff...
          (localfun something)))

      (defun foo ()
        (let ((funvar #'(lambda (a)
                          ...some stuff...)))
          ...some other stuff...
          (funcall funvar something)))

  Although local call is most efficient when the function is called
  only once, a call doesn't have to be equivalent to a LET to be more
  efficient than full call. All local calls avoid the overhead of
  argument count checking and keyword argument parsing, and there are
  a number of other advantages that apply in many common situations.
  See @LET-OPTIMIZATION for a discussion of the optimizations done on
  let calls.")

(defsection @local-calls-to-closures (:title "Local Calls to Closures"
                                      :concepts (("call" "closure" )
                                                 ("closure," "call")))
  "@LOCAL-CALL allows for much more efficient use of @CLOSUREs, since the
  closure environment doesn't need to be allocated on the heap or even
  stored in memory at all. In this example, there is no penalty for
  `LOCALFUN` referencing `A` and `B`:

      (defun foo (a b)
        (flet ((localfun (x)
                 (1+ (* a b x))))
          (if (= a b)
              (localfun (- a))
              (localfun a))))

  In local call, the compiler effectively passes closed-over values as
  extra arguments, so there is no need for you to optimize `LOCALFUN`
  by explicitly passing in lexically visible values. Closures may also
  be subject to @LET-OPTIMIZATION.

  > _Note_: indirect value cells are currently always allocated on the
  > heap when a variable is both assigned to (with SETQ or SETF) and
  > closed over, regardless of whether the closure is a local function
  > or not. This is another reason to avoid setting variables when you
  > don't have to.")

(defsection @local-tail-call (:title "Local Tail Call"
                              :concepts (~local-tail-call))
  "Local tail calls are particularly efficient because they combine the
  benefits of @LOCAL-CALLs (simplified calling convention) and
  @TAIL-CALLs ~TAIL-CALL (reusing the stack frame). Scheme programmers
  write loops with tail-recursive local calls instead of using the
  imperative GO and SETQ. This has not caught on in the Common Lisp
  community for historical reasons and because the standard does not
  require tail calls. In SBCL, users can choose to write loops such as

      (defun recurse-fact (n)
        (labels ((loop* (n total)
                   (if (zerop n)
                       total
                       (loop* (1- n) (* n total)))))
          (loop* n 1)))

  which is compiled to just as efficient code as

      (defun loop-fact (n)
        (let ((r 1))
          (loop for i downfrom n above 0
                do (setq r (* i r)))
           r))")

(defsection @local-call-return-values (:title "Local Call Return Values")
  "~LOCAL-CALL
  One of the more subtle costs of full call comes from allowing
  arbitrary numbers of @RETURN-VALUES. This overhead can be avoided in
  @LOCAL-CALLs to functions that always return the same
  number of values. For efficiency reasons (as well as stylistic
  ones), you should write functions so that they always return the
  same number of values. This may require passing extra NIL arguments
  to VALUES in some cases, but the result is more efficient, not less
  so.")

(defsection @block-compilation (:title "Block Compilation"
                                :concepts (("block compilation")
                                           ("compilation," "block")))
  "Block compilation allows calls to global functions defined by
  DEFUN to be compiled as @LOCAL-CALLs. The function call can be in a
  different top-level form than the DEFUN or even in a different file.

  In addition, block compilation allows the declaration of the _entry
  points_ to the block-compiled portion. An entry point is a function
  that may be called from outside the block-compiled unit. If a
  function is not an entry point, then it can be compiled more
  efficiently since all calls are known at compile time. In
  particular, if a function is only called in one place, then it will
  be let converted. This effectively inline expands the function but
  without the code duplication that results from defining the function
  normally and then declaring it inline.

  The main advantage of block compilation is that it preserves
  efficiency in programs even when (for readability and syntactic
  convenience) they are broken up into many small functions. There is
  absolutely no overhead for calling a non-entry point function that
  is defined purely for modularity (i.e. called only in one place).

  Block compilation also allows for more unboxed and @NON-DESCRIPTOR
  arguments and @RETURN-VALUES in non-trivial programs (see
  @NON-DESCRIPTORS-IN-LOCAL-CALL)."
  (@block-compilation-semantics section)
  (@block-compilation-declarations section)
  (@block-compilation-difficulties section))

(defsection @block-compilation-semantics (:title "Block Compilation Semantics")
  "The effect of block compilation can be envisioned as the compiler
  turning all the DEFUNs in the block-compiled unit into a single
  LABELS form. For example,

      (declaim (sb-ext:start-block fun1 fun3))

      (defun fun1 ()
        ...)

      (defun fun2 ()
        ...
        (fun1)
        ...)

      (defun fun3 (x)
        (if x
            (fun1)
            (fun2)))

      (declaim (sb-ext:end-block))

   becomes

      (labels ((fun1 ()
                 ...)
               (fun2 ()
                 ...
                 (fun1)
                 ...)
               (fun3 (x)
                 (if x
                     (fun1)
                     (fun2))))
        (setf (fdefinition 'fun1) #'fun1)
        (setf (fdefinition 'fun3) #'fun3))

  Calls between the block-compiled functions are @LOCAL-CALLs, so
  changing the global definition of `FUN1` will have no effect on what
  `FUN2` does; `FUN2` will keep calling the old `FUN1`.

  The entry points `FUN1` and `FUN3` are still installed in
  SYMBOL-FUNCTION as the global definitions of the functions, so a
  @FULL-CALL to an entry point works just as before. However, `FUN2`
  is not an entry point, so it is not globally defined. In addition,
  `FUN2` is only called in one place, so it will be let converted.")

(defsection @block-compilation-declarations
    (:title "Block Compilation Declarations"
     :concepts (("block compilation" "declarations")))
  "The following declarations allow fine-grained control of block
  compilation. They are only legal as global declarations (DECLAIM or
  PROCLAIM)."
  (sb-ext:start-block declaration)
  (sb-ext:end-block declaration))

(defsection @block-compilation-difficulties
    (:title "Block Compilation Difficulties")
  "The main problem with block compilation is that the compiler uses
  large amounts of memory when it is block compiling. This places an
  upper limit on the amount of code that can be block compiled as a
  unit. To make best use of block compilation, it is necessary to
  locate the parts of the program containing many internal calls, and
  then add the appropriate SB-EXT:START-BLOCK declarations. When
  writing new code, it is a good idea to put in block compilation
  declarations from the very beginning, since writing block
  declarations correctly requires accurate knowledge of the program's
  function call structure. If you want to initially develop code with
  full incremental redefinition, you can compile with
  SB-EXT:*BLOCK-COMPILE-DEFAULT* set to NIL (the default).

  Note that if a DEFUN appears in a non-null lexical environment, then
  calls to it cannot be block compiled.

  Unless files are very small, it is probably impractical to block
  compile multiple files as a unit by specifying a list of files to
  COMPILE-FILE. @SEMI-INLINE-EXPANSION provides another way to extend
  block compilation across file boundaries.")

(defsection @inline-expansion (:title "Inline Expansion"
                               :concepts (("inline" "expansion")
                                          ("inlining")
                                          ("expansion," "inline")
                                          ("call," "inline")
                                          ("function call," "inline")
                                          ("optimization," "inlining")))

  "SBCL can expand almost any function inline, including functions
  with keyword arguments. The only restrictions are that keyword
  argument keywords in the call must be constant, and that global
  function definitions (DEFUN) must be done in a null lexical
  environment (not nested in a LET or other binding form.) Local
  functions (e.g. FLET) can be inline expanded in any environment.
  Combined with source-level optimization, inline expansion can be
  used for things that would otherwise require macros for efficient
  implementation. In SBCL, macros don't have any efficiency advantage,
  so they need only be used where a macro's syntactic flexibility is
  required.

  Inline expansion is a compiler optimization technique that reduces
  the overhead of a function call by simply not doing the call:
  instead, the compiler effectively rewrites the program to appear as
  though the definition of the called function was inserted at each
  call site. In Lisp, this is straightforwardly expressed by inserting
  the LAMBDA corresponding to the original definition. Consider

      (declaim (inline my-1+))
      (defun my-1+ (x)
        (+ x 1))

  With this definition, `(MY-1+ SOMEVAL)` is equivalent
  to `((LAMBDA (X) (+ X 1)) SOMEVAL)`."
  (sb-ext:*inline-expansion-limit* variable)
  "Regardless of this limit, expanding a large function inline can
  increase the size of a program considerably. If the program becomes
  too large, inline expansion hurts speed rather than helping it,
  since hardware resources such as physical memory and cache will be
  exhausted. Inline expansion is called for:

  - When profiling has shown that a relatively simple function is
    called so often that a large amount of time is being wasted in the
    calling of that function (as opposed to running in that function).
    If a function is complex, it will take a long time to run relative
    the time spent in call, so the speed advantage of inline expansion
    is diminished at the same time the space cost of inline expansion
    is increased. Of course, if a function is rarely called, then the
    overhead of calling it is also insignificant.

  - With functions so simple that they take less space to inline
    expand than would be taken to call the function (such as `MY-1+`
    above). It would require intimate knowledge of the compiler to be
    certain when inline expansion would reduce space, but it is
    generally safe to inline expand functions whose definition is a
    single function call, or a few calls to simple functions.

  In addition to this speed/space tradeoff from inline expansion's
  avoidance of the call, inline expansion can also reveal
  opportunities for optimization. SBCL's extensive source-level
  optimization can make use of context information from the caller to
  tremendously simplify the code resulting from the inline expansion
  of a function.

  The main form of caller context is local information about the
  actual argument values: what the argument types are and whether the
  arguments are constant. Knowledge about argument types can eliminate
  run-time type tests (e.g. for generic arithmetic). Constant
  arguments in a call provide opportunities for constant folding
  optimization after inline expansion.

  A hidden way that constant arguments are often supplied to functions
  is through the defaulting of unsupplied optional or keyword arguments.
  There can be a huge efficiency advantage to inline expanding functions
  that have complex keyword-based interfaces, such as this definition of
  the MEMBER function:

      (declaim (inline member))
      (defun member (item list &key
                     (key #'identity)
                     (test #'eql testp)
                     (test-not nil notp))
        (do ((list list (cdr list)))
            ((null list) nil)
          (let ((car (car list)))
            (if (cond (testp
                       (funcall test item (funcall key car)))
                      (notp
                       (not (funcall test-not item (funcall key car))))
                      (t
                       (funcall test item (funcall key car))))
                (return list)))))

  After inline expansion,

      (member a l :key #'foo-a :test #'char=)

  is simplified to the obvious code:

      (do ((list list (cdr list)))
          ((null list) nil)
        (let ((car (car list)))
          (if (char= item (foo-a car))
              (return list))))

  In this example, there could easily be more than an order of
  magnitude improvement in speed. In addition to eliminating the
  original call to MEMBER, inline expansion also allows the calls to
  CHAR= and `FOO-A` to be open-coded. We go from a loop with three
  tests and two calls to a loop with one test and no calls.

  See also @SOURCE-OPTIMIZATION."
  (@inline-expansion-recording section)
  (@semi-inline-expansion section))

(defsection @inline-expansion-recording
    (:title "Inline Expansion Recording"
     :concepts (("inline expansion," "recording")))
  "Inline expansion requires that the source for the inline expanded
  function to be available when calls to the function are compiled.
  The compiler doesn't remember the inline expansion of every
  function, since that would take an excessive amount of space.
  Instead, the programmer must tell the compiler to record the inline
  expansion before the definition of the inline expanded function is
  compiled. This is done by globally declaring the function inline
  before the function is defined, by using the INLINE and
  SB-EXT:MAYBE-INLINE declarations.

  In addition to recording the inline expansion of inline functions at
  the time the function is compiled, COMPILE-FILE also puts the inline
  expansion in the output file. When the output file is loaded, the
  inline expansion is made available for subsequent compilations;
  there is no need to compile the definition again to record the
  inline expansion.

  If a function is declared inline, but no expansion is recorded, then
  the compiler will give style warning like

  ```
  ; caught STYLE-WARNING:
  ;   Call to MY-1+ could not be inlined because its source code
  ;   was not saved. A global INLINE or SB-EXT:MAYBE-INLINE proclamation must be
  ;   in effect to save function definitions for inlining.
  ```

  If you get this, check that the INLINE declaration appears before
  the DEFUN, which in turn is before the calls that are to be inline
  expanded. Alternatively, the style warning can be silenced by
  declaring the function NOTINLINE at the call site.

  This style warning will also be given if the inline expansion could
  not be recorded because the DEFUN was in a non-null lexical
  environment.")

(defsection @semi-inline-expansion (:title "Semi-inline Expansion"
                                    :concepts (@semi-inline))
  "Semi-inline expansion shares a single copy of a function across all
  the calls in a component by converting the inline expansion into a
  @LOCAL-CALL. This takes up less space when there are multiple calls
  but also provides less opportunity for context-dependent
  optimization. When there is only one call, the result is identical
  to normal @INLINE-EXPANSION.

  The inlining decision takes into account the inlining declaration of
  the function to be called and the optimization qualities in effect,
  where both are taken at the call site.

  - SB-EXT:MAYBE-INLINE: Semi-inlined if @SPACE is `0`; else, not
    inlined at all.

  - INLINE: Semi-inlined if (< @SPEED @SPACE) or (< @SPEED
    @COMPILATION-SPEED); else, inlined normally."
  (sb-ext:maybe-inline declaration)
  "Note that the use of the SB-EXT:MAYBE-INLINE declaration followed by
  the DEFUN is preferable to the standard idiom of

      (declaim (inline myfun))
      (defun myfun () ...)
      (declaim (notinline myfun))

      ;; Calls to MYFUN here are not inline expanded.

      (defun somefun ()
        (declare (inline myfun))
        ;; Calls to MYFUN here are inline expanded.
        ...)

  The problem with using NOTINLINE in this way is that it does more
  than just suppress inline expansion: it also forbids the compiler to
  use any knowledge of `MYFUN` until a later INLINE declaration
  overrides the NOTINLINE. This prevents compiler warnings about
  incorrect calls to the function and also prevents
  @BLOCK-COMPILATION.

  When the goal is merely to control whether inline expansion is done
  by default, it is preferable to use SB-EXT:MAYBE-INLINE rather than
  NOTINLINE. The NOTINLINE declaration should be reserved for those
  special occasions when a function may be redefined at run-time, so
  the compiler must be told that the obvious definition of a function
  is not necessarily the one that will be in effect at the time of the
  call.")

(defsection @slot-access (:title "Slot Access"
                          :concepts (("slot" "access")))
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

  Second most efficient way is to use a CLOS slot accessor or
  SLOT-VALUE with a constant slot name argument but in circumstances
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
    and one-dimensional, and it has a constant :ELEMENT-TYPE;

      > __Warning__: Stack space is limited, so allocation of a large
      > vector may cause stack overflow. Stack overflow checks are
      > done except in 0 @SAFETY policies.

  - @CLOSUREs defined with FLET or LABELS with a bound DYNAMIC-EXTENT
    declaration;

  - anonymous @CLOSUREs defined with LAMBDA;

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
    calls, inlined functions, anonymous @CLOSUREs, or even other
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
  uses escape (traditional Lisp terminology names this situation _all
  uses are downward funargs_). For example, in the following function,
  the local function `#'PREDICATEP` is stack allocated, because the
  compiler understands that the built-in function POSITION-IF only
  uses its first argument as a downward funarg:

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

(defsection @representation-of-objects
    (:title "Representation of Objects"
     :concepts (("representation" "of objects")
                ("object representation")))
  (@descriptor section)
  (@non-descriptor section)
  (@non-descriptor-variables section)
  (@non-descriptors-in-local-call section)
  (@specialized-arrays section)
  (@specialized-structure-slots section)
  (@characters section)
  (@generic-arithmetic section)
  (@fixnums section)
  (@word-integers section)
  (@modular-arithmetic section)
  (@floating-point section))

(defsection @descriptor (:title "Descriptor"
                         :concepts (("descriptor representation")
                                    ("representation," "descriptor")))
  "To support dynamic typing and garbage collection, all Lisp objects
  can be represented with a one-word object, known as a _descriptor_,
  in which a few _tag_ bits carry information about the object's type and
  allocation.

  There are two kinds of descriptors:

  - _Immediate descriptor_ (or _tagged value_)

      For example, @FIXNUMS and @CHARACTERS fit into a machine word
      along with the tag bits. Immediate descriptors are unboxed. The
      presence of the tag bits has a very slight performance impact on
      operations.

  - _Pointer descriptor_ (or _tagged pointer_)

      Objects larger than one word (such as CONSes) or objects that do
      not fit into one word due to the tag bits (such as
      @WORD-INTEGERS and SINGLE-FLOATs) are represented as aligned
      pointers to a separately allocated memory area, where the
      alignment allows some low bits to be used as the tag. Pointer
      descriptors are boxed.

      This boxing has two major disadvantages:

      - The memory pointed to must be allocated on the heap, so it
        must eventually be freed by the garbage collector. Excessive
        @CONSING is inefficient in several ways.

      - Representing an object in memory requires the compiler to emit
        additional instructions to read the actual value in from
        memory, and then to write the value back after operating on
        it.")

(defsection @non-descriptor (:title "Non-descriptor"
                             :concepts (("non-descriptor representation")
                                        ("representation," "non-descriptor")))
  "We saw that the standard @DESCRIPTOR representation has efficiency
  problems, the worst being the @CONSING caused by boxing. SBCL tries
  to avoid these by using _non-descriptor_ representations, which are
  basically untagged immediates. For example, this function does not
  cons numbers:

      (defun multby (vec n)
        (declare (type (simple-array single-float (*)) vec)
                 (single-float n))
        (dotimes (i (length vec))
          (setf (aref vec i)
                (* n (aref vec i)))))

  If a descriptor representation were used, each iteration of the loop
  might cons two floats and do three times as many memory references.

  As its negative definition suggests, the range of possible
  non-descriptor representations is large. The performance improvement
  from non-descriptor representation depends upon both the number of
  types that have non-descriptor representations and the number of
  contexts in which the compiler is forced to use a descriptor
  representation.

  SBCL supports non-descriptor representations for

  - float types (see @FLOATING-POINT),

  - @WORD-INTEGERS,

  - characters (see @REPRESENTATION-OF-CHARACTERS),

  - system area pointers (unconstrained pointers, see @UNTYPED-MEMORY),

  - local variables (see @NON-DESCRIPTOR-VARIABLES),

  - array elements (section @SPECIALIZED-ARRAYS),

  - arguments and return values in @LOCAL-CALL (see
    @NON-DESCRIPTORS-IN-LOCAL-CALL), and

  - @SPECIALIZED-STUCTURE-SLOTS.")

(defsection @non-descriptor-variables
    (:title "Non-descriptor Variables"
     :concepts (("variables," "non-descriptor")
                ("type declarations," "variable")))
  "In order to use a @NON-DESCRIPTOR representation for a variable or
  expression intermediate value, the compiler must be able to prove
  that the value is always of a particular type having a
  non-descriptor representation. @TYPE-INFERENCE often needs some help
  from user-supplied declarations. The best kind of type declaration
  is a variable type declaration placed at the binding point:

      (let ((x (car l)))
        (declare (single-float x))
        ...)

  Use of THE or of variable declarations not at the binding form is
  insufficient to allow non-descriptor representation of the variable
  -- with these declarations it is not certain that all values of the
  variable are of the right type. It is sometimes useful to introduce
  a gratuitous binding that allows the compiler to change to a
  non-descriptor representation, like

      (etypecase x
        ((signed-byte 32)
         (let ((x x))
           (declare (type (signed-byte 32) x))
           ...))
        ...)

  The declaration on the inner `X` is necessary here due to a phase
  ordering problem. Although the compiler will eventually prove that
  the outer `X` is a `(SIGNED-BYTE 32)` within that ETYPECASE branch,
  the inner `X` would have been optimized away by that time. Declaring
  the type makes let optimization more cautious."
  ;; FIXME: ^^^ check the above example
  "Note that storing a value into a global variable always forces a
  descriptor representation. Wherever possible, you should operate
  only on local variables, binding any referenced globals to local
  variables at the beginning of the function, and doing any global
  assignments at the end.

  @EFFICIENCY-NOTES signal use of inefficient representations, so
  programmers needn't continuously worry about the details of
  representation selection (see @REPRESENTATION-EFF-NOTE.)")

(defsection @non-descriptors-in-local-call
    (:title "Non-descriptors in Local Call"
     :concepts (("local call," "non-descriptors")
                ("non-descriptor," "in local call")))
  "@LOCAL-CALL has many advantages; one relevant to our discussion here
  is that local call extends the usefulness of @NON-DESCRIPTOR
  representations. If the compiler knows from the argument type that
  an argument has a non-descriptor representation, then the argument
  will be passed in that representation. The easiest way to ensure
  that the argument type is known at compile time is to always declare
  the argument type in the called function, like

      (defun 2+f (x)
        (declare (single-float x))
        (+ x 2.0))

  The advantages of passing arguments and return values in a
  @NON-DESCRIPTOR representation are the same as for non-descriptor
  representations in general: reduced consing and memory access. This
  extends the applicative programming styles (see @LOCAL-CALL) to
  numeric code. Also, if source files are kept reasonably small, block
  compilation can be used to reduce number consing to a minimum.

  Note that non-descriptor return values can only be used with the
  known return convention (see @LOCAL-CALL-RETURN-VALUES). If the
  compiler can't prove that a function always returns the same number
  of values, then it must use the unknown values return convention,
  which requires a descriptor representation. Pay attention to the
  known return @EFFICIENCY-NOTES to avoid number consing.")

(defsection @specialized-arrays (:title "Specialized Arrays"
                                 :concepts (("specialized array types")
                                            ("array types," "specialized")
                                            ("types," "specialized array")))
  "Common Lisp supports specialized array element types through the
  :ELEMENT-TYPE argument to MAKE-ARRAY. When an array has a
  specialized element type, only elements of that type can be stored
  in the array. From this restriction comes two major efficiency
  advantages:

  - A specialized array can save space by packing multiple elements
    into a single word. For example, on 64-bit platforms, a BASE-CHAR
    array can have 8 elements per word, and a BIT array can have 64.
    This space-efficient representation is possible because it is not
    necessary to separately indicate the type of each element.

  - The elements in a specialized array can be given the same
    non-descriptor representation as the one used in registers and on
    the stack, eliminating the need for representation conversions
    when reading and writing array elements. For objects with pointer
    descriptor representations (such as floats and word integers)
    there is also a substantial consing reduction because it is not
    necessary to allocate a new object every time an array element is
    modified.

  These are the specialized element types currently supported on
  64-bit platforms:

      nil
      bit
      (unsigned-byte 2)
      (unsigned-byte 4)
      (unsigned-byte 7)
      (unsigned-byte 8)
      (unsigned-byte 15)
      (unsigned-byte 16)
      (unsigned-byte 31)
      (unsigned-byte 32)
      (unsigned-byte 63)
      (unsigned-byte 64)
      (signed-byte 8)
      (signed-byte 16)
      (signed-byte 30)
      (signed-byte 32)
      (signed-byte 64)
      single-float
      double-float
      (complex single-float)
      (complex double-float)
      base-char
      character

  Although a SIMPLE-VECTOR can hold any type of object, T should still
  be considered a specialized array type, since arrays with element
  type T are specialized to hold descriptors.

  When using non-descriptor representations, it is particularly
  important to make sure that array accesses are open-coded, since in
  addition to the generic operation overhead, efficiency is lost when
  the array element is converted to a descriptor so that it can be
  passed to (or from) the generic access routine. You can detect
  inefficient array accesses by enabling @EFFICIENCY-NOTES.")

(defsection @specialized-structure-slots
    (:title "Specialized Structure Slots"
     :concepts (("structure types" "numeric slots")
                ("specialized structure slots")))
  "Structure slots declared by the :TYPE DEFSTRUCT slot option to have
  certain known numeric types are also given @NON-DESCRIPTOR
  representations. On 64-bit platforms, these types (and subtypes of
  these types) are supported:

      (unsigned-byte 64)
      (signed-byte 64)
      single-float
      double-float
      (complex single-float)
      (complex double-float)

  The primary advantage of specialized slot representations is a large
  reduction in spurious memory allocation and access overhead of
  programs that intensively use these types.")

(defsection @characters (:title "Characters"
                         :concepts (("characters")))
  "SBCL also uses a @NON-DESCRIPTOR representation for characters when
  convenient. This improves the efficiency of string manipulation but
  is otherwise pretty invisible; characters have an immediate
  @DESCRIPTOR representation, so there is not a great penalty for
  converting a character to a descriptor. Nonetheless, it may
  sometimes be helpful to declare character-valued variables as
  BASE-CHAR.")

(defsection @generic-arithmetic (:title "Generic Arithmetic"
                                 :concepts (("generic arithmetic")
                                            ("arithmetic," "generic")))
  "In Common Lisp, arithmetic operations are _generic_. For example, the
  `+` function can be passed FIXNUMs, BIGNUMs, RATIOs, and various
  kinds of FLOATs and COMPLEXes, in any combination.

  > _Note_: As Steele notes in CLTL II, this generic conception of
  > _generic_ is not to be confused with the CLOS concept of a generic
  > function.

  In addition to the inherent complexity of BIGNUM and RATIO
  operations, there is also a lot of overhead in just figuring out
  which operation to do and what contagion and canonicalization rules
  apply. The complexity of generic arithmetic is so great that it is
  inconceivable to open code it. Instead, the compiler does a function
  call to a generic arithmetic routine, consuming many instructions
  before the actual computation even starts.

  This is ridiculous, since even Lisp programs do a lot of arithmetic,
  and the hardware is capable of doing operations on small integers
  and floats with a single instruction. To get acceptable efficiency,
  the compiler special-cases uses of generic arithmetic that are
  directly implemented in the hardware. In order to open code
  arithmetic, several constraints must be met:

  - All the arguments must be known to be a _good type_ of number.

  - The result must be known to be a good type of number.

  - Any intermediate values such as the result of `(+ A B)` in the
    call `(+ A B C)` must be known to be a good type of number.

  - All the above numbers with good types must be of the _same_ good
    type. Don't try to mix integers and floats or different float
    formats.

  The good types on 64-bit platforms are

      (unsigned-byte 64)
      (signed-byte 64)
      single-float
      double-float
      (complex single-float)
      (complex double-float)

  See @FIXNUMS, @WORD-INTEGERS and @FLOATING-POINT for more discussion
  of good numeric types.

  FLOAT is not a good type, since it might mean either SINGLE-FLOAT or
  DOUBLE-FLOAT. INTEGER is not a good type, since it might mean
  BIGNUM. RATIONAL is not a good type, since it might mean RATIO. Note
  however that these types are still useful in declarations, since
  @TYPE-INFERENCE may be able to strengthen a weak declaration into a
  good one, when it would be at a loss if there was no declaration at
  all. The INTEGER and UNSIGNED-BYTE (or non-negative integer) types
  are especially useful in this regard, since they can often be
  strengthened to a good integer type."
  ;; Arithmetic with COMPLEX numbers is inefficient in comparison to
  ;; float and integer arithmetic. Complex numbers are always
  ;; represented with a pointer descriptor (causing consing overhead),
  ;; and complex arithmetic is always closed coded using the general
  ;; generic arithmetic
  "As noted above, SBCL has support for `(COMPLEX SINGLE-FLOAT)`
  and `(COMPLEX DOUBLE-FLOAT)`. These can be unboxed and, thus, are
  quite efficient. However, arithmetic with complex types such
  as `(COMPLEX FLOAT)` and `(COMPLEX FIXNUM)` will be significantly
  slower than the good complex types but is still faster than BIGNUM
  or RATIO arithmetic, since the implementation is much simpler.

  > _Note_: don't use the `/` function to divide integers unless you
  > want the overhead of rational arithmetic. Use TRUNCATE even when
  > you know that the arguments divide evenly.

  You don't need to remember all the rules for how to get open-coded
  arithmetic, since @EFFICIENCY-NOTES will tell you when and where
  there is a problem.")

(defsection @fixnums (:title "Fixnums" :concepts (("fixnums")
                                                  ("bignums")))
  "A fixnum is a _FIXed-precision NUMber_. In modern Lisp
  implementations, fixnums can be represented with an immediate
  @DESCRIPTOR, so operating on fixnums requires no consing or memory
  references. Clever choice of representations also allows some
  arithmetic operations to be done on fixnums using hardware supported
  word-integer instructions, somewhat reducing the speed penalty for
  using an unnatural integer representation.

  It is useful to distinguish the FIXNUM type from the fixnum
  representation of integers. In SBCL, there is absolutely nothing
  magical about the FIXNUM type in comparison to other finite integer
  types. FIXNUM is equivalent to `(SIGNED-BYTE 63)` on all 64-bit
  platforms but ppc64, where it is `(SIGNED-BYTE 61)`. On 32-bit
  platforms, a FIXNUM is `(SIGNED-BYTE 29)`. FIXNUM is simply the
  largest subset of integers that _can be represented_ using an
  immediate fixnum descriptor.

  Unlike in other Common Lisp compilers, it is in no way desirable to
  use the FIXNUM type in declarations in preference to more
  restrictive integer types such as BIT, `(INTEGER -43 7)`
  and `(UNSIGNED-BYTE 8)`. Since SBCL does understand these integer
  types, it is preferable to use the more restrictive type, as it
  allows better @TYPE-INFERENCE (see @MORE-TYPES-MEANINGFUL).

  The small, efficient fixnum is contrasted with bignum, or _BIG
  NUMber_. This is another descriptor representation for integers, but
  this time a pointer representation that allows for arbitrarily large
  integers. Bignum operations are less efficient than fixnum
  operations, both because of the consing and memory reference
  overheads of a pointer descriptor, and also because of the inherent
  complexity of extended precision arithmetic. While fixnum operations
  can often be done with a single instruction, bignum operations are
  so complex that they are always done using generic arithmetic.

  A crucial point is that the compiler will use generic arithmetic if
  it can't prove that all the arguments, intermediate values, and
  results are fixnums. With bounded integer types such as FIXNUM, the
  result type proves to be especially problematical, since these types
  are not closed under common arithmetic operations such as
  [`+`][function], [`-`][function], [`*`][function] and
  [`/`][function]. For example, `(1+ (THE FIXNUM X))` does not
  necessarily evaluate to a FIXNUM. Bignums were added to Lisp to get
  around this problem, but they really just transform the correctness
  problem _if this add overflows, you will get the wrong answer_ to
  the efficiency problem _if this add overflows, then your program
  will run slowly_ (because of generic arithmetic).

  There is just no getting around the fact that the hardware only
  directly supports short integers. To get the most efficient open
  coding, the compiler must be able to prove that the result is a good
  integer type. This is an argument in favor of using more restrictive
  integer types: `(1+ (THE FIXNUM X))` may not always be a FIXNUM,
  but `(1+ (THE (UNSIGNED-BYTE 8) X))` always is. Of course, you can
  also assert the result type by putting in lots of THE declarations
  and then compiling with @SAFETY `0` (see
  @TYPE-DECLARATION-CHECKING).")

(defsection @word-integers (:title "Word Integers"
                            :concepts (("word integers")))
  "SBCL provides an efficient implementation of arithmetic
  on full-word integers through @NON-DESCRIPTOR representations and
  open coding. Arithmetic on any subtype of

      (signed-byte 64)
      (unsigned-byte 64)

  on 64-bit platforms or the 32-bit equivalents on 32-bit platforms is
  reasonably efficient, although subtypes of FIXNUM remain somewhat
  more efficient.

  If a word integer must be represented as a @DESCRIPTOR, then the
  BIGNUM representation is used, with its associated consing overhead.
  The support for word integers in no way changes the language
  semantics, it just makes arithmetic on small bignums vastly more
  efficient. It is fine to do arithmetic operations with mixed FIXNUM
  and word integer operands; just declare the most specific integer
  type you can, and let the compiler decide what representation to
  use.

  In fact, to most users, the greatest advantage of word integer
  arithmetic is that it effectively provides a few guard bits on the
  fixnum representation. If there are missing assertions on
  intermediate values in a fixnum expression, the intermediate results
  can usually be proved to fit in a word. After the whole expression
  is evaluated, there will often be a fixnum assertion on the final
  result, allowing creation of a fixnum result without even checking
  for overflow.

  The remarks in @FIXNUMS about fixnum result type also apply to word
  integers; you must be careful to give the compiler enough
  information to prove that the result is still a word integer. This
  time, though, when we blow out of word integers we land in into
  generic bignum arithmetic, which is much worse than sleazing from
  FIXNUMs to word integers. Note that mixing unsigned word
  integers (e.g. `(UNSIGNED-BYTE 64)`) arguments with arguments of any
  signed type (such as FIXNUM) is a no-no, since the result might not
  be unsigned.")

(defsection @modular-arithmetic (:title "Modular Arithmetic"
                                 :concepts ("modular arithmetic"
                                            ("arithmetic," "modular")
                                            ("arithmetic," "hardware")))
  "Some numeric functions have a property: n lower bits of the
  result depend only on n lower bits of (all or some) arguments. If
  the compiler sees an expression of form `(LOGAND <EXPR> <MASK>)`,
  where `<EXPR>` is a tree of such _good_ functions and `<MASK>` is
  known to be of type `(UNSIGNED-BYTE <W>)`, where `<W>` is a _good_
  width, all intermediate results will be cut to `<W>` bits (but this
  is _not_ done for variables and constants). This often results in an
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
  arithmetic can be used."
  "- Good functions are [`+`][function], [`-`][function], LOGAND,
    LOGIOR, LOGXOR, LOGNOT, ASH with a positive second
    argument, and their combinations.

  - Good widths are 64 on 64-bit CPUs and 32 on 32-bit CPUs. While it
    is possible to support smaller widths as well, currently this is
    not implemented."
  (@signed-modular-arithmetic section))

(defsection @signed-modular-arithmetic (:title "Signed Modular Arithmetic")
  "Sign-extending the result in the following way will be
  translated into signed modular arithmetic:

      (defun add (a b)
        (declare (type (signed-byte 64) a b))
        (let ((u (ldb (byte 64 0) (+ a b))))
          (logior u (- (mask-field (byte 1 63) u)))))")

(defsection @floating-point (:title "Floating Point"
                             :concepts (("floating point efficiency")))
  "Arithmetic on objects of type SINGLE-FLOAT and DOUBLE-FLOAT is
  efficiently implemented using @NON-DESCRIPTOR representations and
  open coding. As for integer arithmetic, the arguments must be known
  to be of the same float type. Unlike for integer arithmetic, the
  results and intermediate values usually take care of themselves due
  to the rules of float contagion, i.e. `(1+ (THE SINGLE-FLOAT X))` is
  always a SINGLE-FLOAT.

  Although they are not specially implemented, SHORT-FLOAT and
  LONG-FLOAT are also acceptable in declarations, since they are
  synonyms for the SINGLE-FLOAT and DOUBLE-FLOAT types, respectively.

  In SBCL, compound float type specifiers such as
  `(SINGLE-FLOAT 0.0 1.0)` will be used to good effect. For example,
  in this function

      (defun square (x)
        (declare (type (single-float 0f0 10f0)))
        (* x x))

  SBCL can deduce that the return type of the function `SQUARE`
  is `(SINGLE-FLOAT 0F0 100F0)`.

  Many union types are also supported so that

      (+ (the (or (integer 1 1) (integer 5 5)) x)
         (the (or (integer 10 10) (integer 20 20)) y))

  has the inferred type

      (or (integer 11 11) (integer 15 15)
          (integer 21 21) (integer 25 25))

  This also works for floating-point numbers. @MEMBER-TYPES are also
  supported.

  SBCL can also infer types for many mathematical functions including
  square root, exponential and logarithmic functions, trigonometric
  functions and their inverses, and hyperbolic functions and their
  inverses. For numeric code, this can greatly enhance efficiency by
  allowing the compiler to use specialized versions of the functions
  instead of the generic versions. The greatest benefit of this type
  inference is determining that the result of the function is a
  real-valued number instead of possibly being a complex-valued
  number.

  For example, consider the function

      (defun fun (x)
        (declare (type (single-float (0f0) 100f0) x))
        (values (sqrt x) (log x)))

  With this declaration, the compiler can determine that the argument
  to SQRT and LOG are always non-negative so that the result is always
  a SINGLE-FLOAT. In fact, the return type for this function is
  derived to be `(VALUES (SINGLE-FLOAT 0F0 10F0) (SINGLE-FLOAT *
  2F0))`.

  If the declaration were reduced to just `(DECLARE (SINGLE-FLOAT
  X))`, the argument to SQRT and LOG could be negative. This forces
  the use of the generic versions of these functions because the
  result could be a complex number.

  We note, however, that proper interval arithmetic is not fully
  implemented in the compiler so the inferred types may be slightly in
  error due to round-off errors. This round-off error could accumulate
  to cause the compiler to erroneously deduce the result type and
  cause code to be removed as being unreachable. Thus, the
  declarations should only be precise enough for the compiler to
  deduce that a real-valued argument to a function would produce a
  real-valued result. The @EFFICIENCY-NOTES from the compiler will
  guide you on what declarations might be useful.

  When a float must be represented as a @DESCRIPTOR, a pointer
  representation is used, creating @CONSING overhead. For this reason,
  you should try to avoid situations (such as full call and
  non-specialized data structures) that force a descriptor
  representation. See @SPECIALIZED-ARRAYS, @SPECIALIZED-STUCTURE-SLOTS
  and @NON-DESCRIPTORS-IN-LOCAL-CALL."
  ;; FIXME: See @IEEE-FLOAT for information on the extensions to
  ;; support IEEE floating point.
  (@signed-zeroes-and-special-functions section))

(defsection @signed-zeroes-and-special-functions
    (:title "Signed Zeroes and Special Functions")
  "SBCL supports IEEE signed zeroes. In typical usage, the signed
  zeroes are not a problem and can be treated as an unsigned zero.
  However, some of the special functions have branch points at zero, so
  care must be taken.

  For example, suppose we have the function

      (defun fun (x)
        (declare (type (single-float 0f0) x))
        (log x))

  The derived result of the function is `(OR SINGLE-FLOAT
  (COMPLEX SINGLE-FLOAT))` because the declared values for `X`
  includes both `-0.0` and `0.0` and `(LOG -0.0)` is actually a
  complex number. Because of this, the generic complex log routine is
  used.

  If the declaration for `X` were `(SINGLE-FLOAT (0F0))` so `+0.0` is
  not included or `(OR (SINGLE-FLOAT (0F0)) (MEMBER 0F0))` so `+0.0`
  is included but not `-0.0`, the derived type would be SINGLE-FLOAT
  for both cases. By declaring `X` this way, the log can be
  implemented using a fast real-valued log routine instead of the
  generic log routine.

  SBCL implements the branch cuts and values given by Kahan (_Branch
  Cuts for Complex Elementary Functions, or Much Ado About Nothing's
  Sign Bit_ in Iserles and Powell (eds.) _The State of the Art in
  Numerical Analysis_, pp. 165-211, Clarendon Press, 1987).")

(defsection @efficiency-notes (:title "Efficiency Notes"
                               :concepts (("efficiency notes")
                                          ("notes," "efficiency")
                                          ("tuning")))
  "Efficiency notes are messages that alert the user that the compiler
  has chosen a relatively inefficient implementation for some
  operation. Usually an efficiency note reflects the compiler's desire
  for more type information. If the type of the values concerned is
  known to the programmer, then additional declarations can be used to
  get a more efficient implementation.

  The main cause of inefficiency is the compiler's lack of adequate
  information about the types of function argument and result values.

  Efficiency notes are enabled whenever @SPEED is greater than its
  default of `1`. See the deprecated SB-EXT:INHIBIT-WARNINGS
  optimization quality in @COMPILER-POLICY for the full picture. See
  also @CONTROLLING-VERBOSITY for some related knobs."
  (@operation-efficiency-notes section)
  (@representation-efficiency-notes section))

(defsection @operation-efficiency-notes (:title "Operation Efficiency Notes")
  "Many important operations have an inefficient general case (see
  e.g. @GENERIC-ARITHMETIC) but have efficient implementations that
  can usually be used if there is sufficient argument type
  information.

  Compiling this function with an obscure missing declaration:

      (defun eff-note (x y z)
        (declare (fixnum x y z)
                 (optimize speed))
        (zerop (mod (+ x y z) 3)))

      ; in: DEFUN EFF-NOTE
      ;     (+ X Y Z)
      ;
      ; note: forced to do +/SIGNED=>S128 (cost 8)
      ;       unable to do inline fixnum arithmetic (cost 2) because:
      ;       The first argument is a (INTEGER -9223372036854775808 9223372036854775806), not a FIXNUM.
      ;       The result is a (VALUES
      ;                        (INTEGER -13835058055282163712 13835058055282163709)
      ;                        &OPTIONAL), not a (VALUES FIXNUM &OPTIONAL).

  This rather cryptic efficiency note is trying to tell us that the
  result of the intermediate computation `(+ X Y)`, which adds two
  fixnums, can overflow the fixnum range, so the addition of the
  intermediate sum to `Z` must be done less efficiently. This might be
  fixed by providing more type information about the result of `(+ X
  Y)`:

      (defun eff-note (x y z)
        (declare (fixnum x y z)
                 (optimize speed))
        (zerop (mod (+ (the fixnum (+ x y)) z) 3)))")

(defsection @representation-efficiency-notes
    (:title "Representation Efficiency Notes"
     :concepts (("representation efficiency notes")
                ("efficiency notes," "representation")))
  "When operating on values that have @NON-DESCRIPTOR representations,
  there can be a substantial time and @CONSING penalty for converting
  to and from @DESCRIPTOR representations. For this reason, the
  compiler gives an efficiency note whenever it is forced to do a
  representation coercion more expensive than
  SB-EXT:*EFFICIENCY-NOTE-COST-THRESHOLD*.

  Inefficient representation coercions may be due to type uncertainty,
  as in this example on a 64-bit platform:

      (defun cf+ (x y)
        (declare (double-float x y)
                 (optimize speed))
        (cons (+ x y) t))

      ; in: DEFUN CF+
      ;     (CONS (+ X Y) T)
      ;
      ; note: doing float to pointer coercion (cost 13), for:
      ;       the first argument of CONS

  The issue here is that a CONS can only hold @DESCRIPTORs. Thus, the
  64-bit float result of `(+ X Y)` needs to be boxed (see
  @FLOATING-POINT).

  Inefficient representation conversions are often not due to type
  uncertainty -- instead, they result from evaluating a non-descriptor
  expression in a context that requires a descriptor result:

  - assignment to or initialization of any data structure other than
    @SPECIALIZED-ARRAYS, or

  - assignment to a SPECIAL variable, or

  - passing as an argument or returning as a value in any function
    call that is not a local call (see
    @NON-DESCRIPTORS-IN-LOCAL-CALL).

  If such inefficient coercions appear in a hot spot in the program,
  data structure redesign or program reorganization may be necessary
  to improve efficiency. See e.g. @BLOCK-COMPILATION,
  @REPRESENTATION-OF-OBJECTS and @PROFILING.")

(defsection @recognized-idioms (:title "Recognized Idioms"
                                :concepts ("modular arithmetic"
                                           ("arithmetic," "modular")
                                           ("arithmetic," "hardware")))
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
  (sb-ext:global declaration)
  (sb-ext:always-bound declaration))

(defsection @miscellaneous-efficiency-issues
    (:title "Miscellaneous Efficiency Issues")
  "The SB-EXT:PURIFY function (available with @CONSERVATIVE-GC) causes
  SBCL first to collect all garbage, then to mark all uncollected
  objects as permanent, never again attempting to collect them as
  garbage. This can cause a large increase in efficiency when using a
  primitive garbage collector, or a more moderate increase in
  efficiency when using a more sophisticated garbage collector which
  is well suited to the program's memory usage pattern. It also allows
  permanent code to be frozen at fixed addresses, a precondition for
  using copy-on-write to share code between multiple Lisp processes.
  This is less important with modern @GENERATIONAL-GC, but not all
  SBCL platforms use such a garbage collector.

  The SB-EXT:TRULY-THE special form declares the type of the result of
  the operations, producing its argument; the declaration is not
  checked. In short: don't use it."
  ;; FIXME: It's not a macro.
  #+nil
  (sb-ext:truly-the macro)
  "FIXME: The remaining material in the CMUCL manual about getting good
  performance from the compiler should be reviewed, and added to this
  section. In the meantime, the original CMUCL manual is still 95+%
  correct for the SBCL's compiler. See the sections

  - Advanced Compiler Use and Efficiency Hints
      - General Efficiency Hints

  Besides this information from the CMUCL manual, there are a few other
  points to keep in mind.

  - The CMUCL manual doesn't seem to state it explicitly, but
    Python (the CMUCL compiler, not the language) has a mental block
    about type inference when assignment is involved. Python is very
    aggressive and clever about inferring the types of values bound
    with LET, LET*, inline function call, and so forth. However, it's
    much more passive and dumb about inferring the types of values
    assigned with SETQ, SETF, and friends. It would be nice to fix
    this, but in the meantime don't expect that just because it's very
    smart about types in most respects it will be smart about types
    involved in assignments. (This doesn't affect its ability to
    benefit from explicit type declarations involving the assigned
    variables, only its ability to get by without explicit type
    declarations.)"
  ;; FIXME: Python dislikes assignments but not in type inference. The
  ;; real problems are loop induction, closed over variables and
  ;; aliases.
  "- Since the time the CMUCL manual was written, CMUCL (and thus SBCL)
    has gotten a @GENERATIONAL-GC. This means that there are some
    efficiency implications of various patterns of memory usage which
    aren't discussed in the CMUCL manual. (Some new material should be
    written about this.)

  - SBCL has some important known efficiency problems. Perhaps the
    most important are

      - The garbage collector is not particularly efficient, at least
        on platforms without the generational collector (as of SBCL
        0.8.9, all except x86).

      - Various aspects of the PCL implementation of CLOS are more
        inefficient than necessary.

  Finally, note that Common Lisp defines many constructs which, in the
  infamous phrase, _could be compiled efficiently by a sufficiently
  smart compiler_. The phrase is infamous because making a compiler
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

  If your system's performance is suffering due to some construct that
  SBCL fails to compile efficiently, even though it could be in
  principle, consider writing a patch to the compiler and submitting
  it for inclusion in the main sources. Such code is often reasonably
  straightforward to write; search the sources for the string
  `deftransform` to find many examples (some straightforward, some
  less so).")
