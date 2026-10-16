(in-package :sb-manual)

(defsection @efficiency (:title "Efficiency"
                         :concepts ("efficicency"))
  (@tail-call section)
  (@local-call section)
  (@block-compilation section)
  (@inline-expansion section)
  (@slot-access section)
  (@stack-allocation section)
  (@modular-arithmetic section)
  (@recognized-idioms section)
  (@global-and-always-bound-variables section)
  (@miscellaneous-efficiency-issues section))

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
  a number of other advantages that apply in many common situations."
  ;; FIXME: See @LET-OPTIMIZATION} for a discussion of the
  ;; optimizations done on let calls.
  )

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
  extra arguments, so there is no need for you to \"optimize\"
  `LOCALFUN` by explicitly passing in lexically visible values."
  ;; FIXME: Closures may also be subject to @LET-OPTIMIZATION.
  "> _Note_: indirect value cells are currently always allocated on the
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

  Block compilation also allows for more unboxed and non-descriptor
  arguments and @RETURN-VALUES in non-trivial programs."
  ;; FIXME: Link to @NUMBER-LOCAL-CALL.
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
  tests and two calls to a loop with one test and no calls."
  ;; FIXME: See @SOURCE-OPTIMIZATION for more discussion of
  ;; source-level optimization.
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

(defsection @modular-arithmetic (:title "Modular Arithmetic"
                                 :concepts ("modular arithmetic"
                                            ("arithmetic," "modular")
                                            ("arithmetic," "hardware")))
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
  "FIXME: The remaining material in the CMUCL manual about getting good
  performance from the compiler should be reviewed, and added to this
  section. In the meantime, the original CMUCL manual is still 95+%
  correct for the SBCL's compiler. See the sections

  - Advanced Compiler Use and Efficiency Hints
  - Advanced Compiler Introduction
  - More About Types in Python
  - Type Inference
  - Source Optimization
  - Object Representation
  - Numbers
  - General Efficiency Hints
  - Efficiency Notes

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
