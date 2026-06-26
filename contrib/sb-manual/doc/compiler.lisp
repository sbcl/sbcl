(in-package :sb-manual)

(defsection @compiler (:title "Compiler")
  "This chapter will discuss most compiler issues other than efficiency,
  including compiler error messages, the SBCL compiler's unusual
  approach to type safety in the presence of type declarations, the
  effects of various compiler optimization policies, and the way that
  inlining and open coding may cause optimized code to differ from a
  naive translation. Efficiency issues are sufficiently varied and
  separate that they have their own chapter, @EFFICIENCY."
  (@diagnostic-messages section)
  (@handling-of-types section)
  (@compiler-policy section)
  (@compiler-errors section)
  (@open-coding-and-inline-expansion section)
  (@interpreter section)
  (@advanced-compiler-use-and-efficiency-hints section))

(defsection @diagnostic-messages (:title "Diagnostic Messages"
                                  :concepts (("compiler" "messsage")
                                             ("messsage," "compiler")))
  (@controlling-verbosity section)
  (@diagnostic-severity section)
  (@understanding-compiler-diagnostics section))

(defsection @controlling-verbosity
    (:title "Controlling Verbosity"
     :concepts (("compiler" "messsage" "verbosity")
                ("verbosity" "of compiler messsages")))
  "The compiler can be quite verbose in its diagnostic reporting, rather
  more then some users would prefer -- the amount of noise emitted can
  be controlled, however.

  To control emission of compiler diagnostics (of any severity other
  than ERROR: @DIAGNOSTIC-SEVERITY) use the SB-EXT:MUFFLE-CONDITIONS
  and SB-EXT:UNMUFFLE-CONDITIONS declarations, specifying the type of
  condition that is to be muffled (the muffling is done using an
  associated MUFFLE-WARNING restart).

  Global control:

      ;;; Muffle compiler-notes globally
      (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

  Local control:

      ;;; Muffle compiler-notes based on lexical scope
      (defun foo (x)
        (declare (optimize speed) (fixnum x)
                 (sb-ext:muffle-conditions sb-ext:compiler-note))
        (values (* x 5) ; no compiler note from this
          (locally
            (declare (sb-ext:unmuffle-conditions sb-ext:compiler-note))
            ;; this one gives a compiler note
            (* x -5))))"
  (sb-ext:muffle-conditions declaration)
  (sb-ext:unmuffle-conditions declaration)
  "Various details of _how_ the compiler messages are printed can be
  controlled via the alist SB-EXT:*COMPILER-PRINT-VARIABLE-ALIST*."
  (sb-ext:*compiler-print-variable-alist* variable)
  "For information about muffling warnings signaled outside of the
  compiler, see @CUSTOMIZATION-HOOKS-FOR-USERS.")

;; FIXME: How much control over error messages is in SBCL? How much
;; should be? How much of this documentation should we save or adapt?
;;
;; %%\node Error Message Parameterization,  , Read Errors, Interpreting Error Messages
;; \subsection{Error Message Parameterization}
;; \cpsubindex{error messages}{verbosity}
;; \cpsubindex{verbosity}{of error messages}
;;
;; There is some control over the verbosity of error messages.  See also
;; \varref{undefined-warning-limit}, \code{*efficiency-note-limit*} and
;; \varref{efficiency-note-cost-threshold}.
;;
;; \begin{defvar}{}{enclosing-source-cutoff}
;;
;;   This variable specifies the number of enclosing actual source forms
;;   that are printed in full, rather than in the abbreviated processing
;;   path format.  Increasing the value from its default of \code{1}
;;   allows you to see more of the guts of the macroexpanded source,
;;   which is useful when debugging macros.
;; \end{defvar}
;;
;; \begin{defmac}{extensions:}{define-source-context}{%
;;     \args{\var{name} \var{lambda-list} \mstar{form}}}
;;
;;   This macro defines how to extract an abbreviated source context from
;;   the \var{name}d form when it appears in the compiler input.
;;   \var{lambda-list} is a \code{defmacro} style lambda-list used to
;;   parse the arguments.  The \var{body} should return a list of
;;   subforms that can be printed on about one line.  There are
;;   predefined methods for \code{defstruct}, \code{defmethod}, etc.  If
;;   no method is defined, then the first two subforms are returned.
;;   Note that this facility implicitly determines the string name
;;   associated with anonymous functions.
;; \end{defmac}

(defsection @diagnostic-severity
    (:title "Diagnostic Severity"
     :concepts (("compiler" "message" "severity")
                ("severity" "of compiler message")))
  "There are four levels of compiler diagnostic severity:

  - error
  - warning
  - style warning
  - note

  The first three levels correspond to condition classes which are
  defined in the ANSI standard for Common Lisp and which have special
  significance to the COMPILE and COMPILE-FILE functions. These levels
  of compiler error severity occur when the compiler handles
  conditions of these classes.

  The fourth level of compiler error severity, _note_, corresponds to
  the SB-EXT:COMPILER-NOTE, and is used for problems which are too
  mild for the standard condition classes, typically hints about how
  efficiency might be improved. The SB-EXT:CODE-DELETION-NOTE, a
  subtype of SB-EXT:COMPILER-NOTE, is signalled when the compiler
  deletes user-supplied code after proving that the code in question
  is unreachable.

  Future work for SBCL includes expanding this hierarchy of types to
  allow more fine-grained control over emission of diagnostic
  messages."
  (sb-ext:compiler-note condition)
  (sb-ext:code-deletion-note condition))

(defsection @understanding-compiler-diagnostics
    (:title "Understanding Compiler Diagnostics")
  "The messages emitted by the compiler contain a lot of detail in a
  terse format, so they may be confusing at first. The messages will be
  illustrated using this example program:

      (defmacro zoq (x)
        `(roq (ploq (+ ,x 3))))

      (defun foo (y)
        (declare (symbol y))
        (zoq y))

  The main problem with this program is that it is trying to add `3`
  to a symbol. Note also that the functions `ROQ` and `PLOQ` aren't
  defined anywhere."
  (@parts-of-a-compiler-diagnostic section)
  (@original-and-actual-source section)
  (@processing-paths section))

(defsection @parts-of-a-compiler-diagnostic
    (:title "Parts of a Compiler Diagnostic")
  "When processing this program, the compiler will produce this warning:

      ; file: /tmp/foo.lisp
      ; in: DEFUN FOO
      ;     (ZOQ Y)
      ; --> ROQ PLOQ
      ; ==>
      ;   (+ Y 3)
      ;
      ; caught WARNING:
      ;   Asserted type NUMBER conflicts with derived type (VALUES SYMBOL &OPTIONAL).

  In this example we see each of the six possible parts of a compiler
  diagnostic:

  - `file: /tmp/foo.lisp` is the name of the file that the compiler
    read the relevant code from. The file name is displayed because it
    may not be immediately obvious when there is an error during
    compilation of a large system, especially when
    WITH-COMPILATION-UNIT is used to delay undefined warnings.

  - `in: DEFUN FOO` is the definition top level form responsible for
    the diagnostic. It is obtained by taking the first two elements of
    the enclosing form whose first element is a symbol beginning with
    `DEF`. If there is no such enclosing `DEF` form, then the
    outermost form is used. If there are multiple `DEF` forms, then
    they are all printed from the outside in, separated by `=>`s. In
    this example, the problem was in the DEFUN for `FOO`.

  - `(ZOQ Y)` is the _@ORIGINAL-SOURCE_ form responsible for the
    diagnostic. Original source means that the form directly appeared
    in the original input to the compiler, i.e. in the lambda passed
    to COMPILE or in the top level form read from the source file. In
    this example, the expansion of the `ZOQ` macro was responsible for
    the message.

  - `--> ROQ PLOQ` This is the _@PROCESSING-PATH_ that the compiler
    used to produce the code that caused the message to be emitted.
    The processing path is a representation of the evaluated forms
    enclosing the @ACTUAL-SOURCE that the compiler encountered when
    processing the original source. The path is the first element of
    each form, or the form itself if the form is not a list. These
    forms result from the expansion of macros or source-to-source
    transformation done by the compiler. In this example, the
    enclosing evaluated forms are the calls to `ROQ` and `PLOQ`. These
    calls resulted from the expansion of the `ZOQ` macro.

  - `==> (+ Y 3)` is the _actual source_ responsible for the
    diagnostic. If the actual source appears in the explanation, then
    we print the next enclosing evaluated form, instead of printing
    the actual source twice. (This is the form that would otherwise
    have been the last form of the processing path.) In this example,
    the problem is with the evaluation of the reference to the
    variable `Y`.

  - `caught WARNING: Asserted type NUMBER conflicts with derived type
    (VALUES SYMBOL &OPTIONAL).` is the _explanation_ of the problem.
    In this example, the problem is that, while the call to `+`
    requires that its arguments are all of type NUMBER, the compiler
    has derived that Y will evaluate to a SYMBOL. Note that
    `(VALUES SYMBOL &OPTIONAL)` expresses that `Y` evaluates to
    precisely one value.

  Note that each part of the message is distinctively marked:

  - `file:` and `in:` mark the file and definition, respectively.

  - The original source is an indented form with no prefix.

  - Each line of the processing path is prefixed with `-->`.

  - The actual source form is indented like the original source, but
    is marked by a preceding `==>` line. (FIXME: no it isn't.)

  - The explanation is prefixed with the diagnostic severity, which
    can be `caught ERROR:`, `caught WARNING:`, `caught
    STYLE-WARNING:`, or `note:`.

  Each part of the message is more specific than the preceding one. If
  consecutive messages are for nearby locations, then the front part
  of the messages would be the same. In this case, the compiler omits
  as much of the second message as in common with the first. For
  example:

      ; file: /tmp/foo.lisp
      ; in: DEFUN FOO
      ;     (ZOQ Y)
      ; --> ROQ
      ; ==>
      ;   (PLOQ (+ Y 3))
      ;
      ; caught STYLE-WARNING:
      ;   undefined function: PLOQ

      ; ==>
      ;   (ROQ (PLOQ (+ Y 3)))
      ;
      ; caught STYLE-WARNING:
      ;   undefined function: ROQ

  In this example, the file, definition and original source are
  identical for the two messages, so the compiler omits them in the
  second message. If consecutive messages are entirely identical, then
  the compiler prints only the first message, followed by: `[Last
  message occurs <repeats> times]` where `<repeats>` is the number of
  times the message was given.

  If the source was not from a file, then no file line is printed. If
  the actual source is the same as the original source, then the
  processing path and actual source will be omitted. If no forms
  intervene between the original source and the actual source, then
  the processing path will also be omitted.")

(defsection @original-and-actual-source (:title "Original and Actual Source"
                                         :concepts (@original-source
                                                    @actual-source))
  "The _original source_ displayed will almost always be a list. If
  the actual source for an message is a symbol, the original source will
  be the immediately enclosing evaluated list form. So even if the
  offending symbol does appear in the original source, the compiler will
  print the enclosing list and then print the symbol as the actual
  source (as though the symbol were introduced by a macro.)

  When the _actual source_ is displayed (and is not a symbol), it will
  always be code that resulted from the expansion of a macro or a
  source-to-source compiler optimization. This is code that did not
  appear in the original source program; it was introduced by the
  compiler.

  Keep in mind that when the compiler displays a source form in an
  diagnostic message, it always displays the most specific (innermost)
  responsible form. For example, compiling this function

      (defun bar (x)
        (let (a)
          (declare (fixnum a))
          (setq a (foo x))
          a))

  gives this error message

      ; file: /tmp/foo.lisp
      ; in: DEFUN BAR
      ;     (LET (A)
      ;     (DECLARE (FIXNUM A))
      ;     (SETQ A (FOO X))
      ;     A)
      ;
      ; caught WARNING:
      ;   Asserted type FIXNUM conflicts with derived type (VALUES NULL &OPTIONAL).

  This message is not saying that there is a problem somewhere in this
  LET -- it is saying that there is a problem with the LET itself. In
  this example, the problem is that `A`'s NIL initial value is not a
  FIXNUM.")

(defsection @processing-paths (:title "Processing Paths"
                               :concepts (@processing-path))
  "The processing path is mainly useful for debugging macros, so if you
  don't write macros, you can probably ignore it. Consider this example:

      (defun foo (n)
        (dotimes (i n *undefined*)))

  Compiling results in this error message:

      ; in: DEFUN FOO
      ;     (DOTIMES (I N *UNDEFINED*))
      ; --> DO BLOCK LET TAGBODY RETURN-FROM
      ; ==>
      ;   (PROGN *UNDEFINED*)
      ;
      ; caught WARNING:
      ;   undefined variable: *UNDEFINED*

  Note that DO appears in the processing path. This is because
  DOTIMES expands into:

      (do ((i 0 (1+ i)) (#:g1 n))
          ((>= i #:g1) *undefined*)
        (declare (type unsigned-byte i)))

  The rest of the processing path results from the @MACROEXPANSION of
  DO: ~SOURCE-TRANSFORM

      (block nil
        (let ((i 0) (#:g1 n))
          (declare (type unsigned-byte i))
          (tagbody (go #:g3)
            #:g2    (psetq i (1+ i))
            #:g3    (unless (>= i #:g1) (go #:g2))
            (return-from nil (progn *undefined*)))))

  In this example, the compiler descended into the BLOCK, LET, TAGBODY
  and RETURN-FROM to reach the PROGN printed as the actual source.
  This is a place where the \"actual source appears in explanation\"
  rule was applied. The innermost actual source form was the symbol
  _undefined_ itself, but that also appeared in the explanation, so
  the compiler backed out one level.")

(defsection @handling-of-types (:title "Handling of Types")
  "One of the most important features of the SBCL compiler (similar to
  the original CMUCL compiler, also known as _Python_) is its fairly
  sophisticated understanding of the Common Lisp type system and its
  conservative approach to the implementation of type declarations.

  These two features reward the use of type declarations throughout
  development, even when high performance is not a concern. Also, as
  discussed in the chapter on performance (see @EFFICIENCY), the use
  of appropriate type declarations can be very important for
  performance as well.

  The SBCL compiler also has a greater knowledge of the Common Lisp
  type system than other compilers. Support is incomplete only for
  types involving the SATISFIES type specifier."
  (@declarations-as-assertions section)
  (@precise-type-checking section)
  (@getting-existing-programs-to-run section)
  (@implementation-limitations section))

;; FIXME: See also sections \ref{advanced-type-stuff} and
;; \ref{type-inference}, once we snarf them from the CMU CL manual.
;;
;; Also see my paper on improving Baker, when I get round to it.
;;
;; Whose paper?

(defsection @declarations-as-assertions (:title "Declarations as Assertions")
  "The SBCL compiler treats type declarations differently from most other
  Lisp compilers. Under default compilation policy the compiler doesn't
  blindly believe type declarations, but considers them assertions about
  the program that should be checked: all type declarations that have
  not been proven to always hold are asserted at runtime.

  _Remaining bugs in the compiler's handling of types unfortunately
  provide some exceptions to this rule, see
  @IMPLEMENTATION-LIMITATIONS._

  CLOS slot types form a notable exception. Types declared using the
  :TYPE slot option in DEFCLASS are asserted if and only if the class
  was defined in _safe code_ ~SAFETY and the slot access location is
  in _safe code_ as well. This laxness does not pose any internal
  consistency issues, as the CLOS slot types are not available for the
  type inferencer, nor do CLOS slot types provide any efficiency
  benefits.

  There are three type checking policies available in SBCL, selectable
  via OPTIMIZE declarations."
  ;; FIXME: This should be properly integrated with general policy
  ;; stuff, once that gets cleaned up.
  "- __Full Type Checks__

      All declarations are considered assertions to be checked at
      runtime, and all type checks are precise. The default
      compilation policy provides full type checks.

      Used when `(OR (>= SAFETY 2) (>= SAFETY SPEED 1))`.

  - __Weak Type Checks__

      Declared types may be simplified into faster to check
      supertypes: for example, `(OR (INTEGER -17 -7) (INTEGER 7 17))`
      is simplified into `(INTEGER -17 17)`.

      > __Warning__: It is relatively easy to corrupt the heap when
      > weak type checks are used if the program contains type-errors.

      Used when `(AND (< SAFETY 2) (< SAFETY SPEED))`.

  - __No Type Checks__

      All declarations are believed without assertions. Also disables
      argument count and array bounds checking.

      > __Warning__: Any type errors in code where type checks are not
      > performed are liable to corrupt the heap.

      Used when `(= SAFETY 0)`.")

(defsection @precise-type-checking (:title "Precise Type Checking"
                                    :concepts (("type checking," "precise")
                                               ("precise" "type checking")))
  "Precise checking means that the check is done as though TYPEP
  had been called with the exact type specifier that appeared in the
  declaration.

  If a variable is declared to be `(INTEGER 3 17)`, then its value
  must always be an integer between `3` and `17`. If multiple type
  declarations apply to a single variable, then all the declarations
  must be correct; it is as though all the types were intersected
  producing a single AND type specifier.

  To gain maximum benefit from the compiler's type checking, you
  should always declare the types of function arguments and structure
  slots as precisely as possible. This often involves the use of OR,
  MEMBER, and other list-style type specifiers.")

(defsection @getting-existing-programs-to-run
    (:title "Getting Existing Programs to Run"
     :concepts (("existing programs," "getting them to run")
                ("types," "portability")
                ("compatibility" "with other Lisps")))
  "Since SBCL's compiler does much more comprehensive type checking than
  most Lisp compilers, SBCL may detect type errors in programs that have
  been debugged using other compilers. These errors are mostly incorrect
  declarations, although compile-time type errors can find actual bugs
  if parts of the program have never been tested.

  Some incorrect declarations can only be detected by run-time type
  checking. It is very important to initially compile a program with
  full type checks (high @SAFETY optimization) and then test this safe
  version. After the checking version has been tested, then you can
  consider weakening or eliminating type checks. _This applies even to
  previously debugged programs_ because the SBCL compiler does much
  more type inference than other Common Lisp compilers, so an
  incorrect declaration can do more damage.

  The most common problem is with variables whose constant initial
  value doesn't match the type declaration. Incorrect constant initial
  values will always be flagged by a compile-time type error, and they
  are simple to fix once located. Consider this code fragment:

      (prog (foo)
        (declare (fixnum foo))
        (setq foo ...)
        ...)

  Here `FOO` is given an initial value of NIL but is declared to be a
  FIXNUM. Even if it is never read, the initial value of a variable
  must match the declared type. There are two ways to fix this
  problem. Change the declaration

      (prog (foo)
        (declare (type (or fixnum null) foo))
        (setq foo ...)
        ...)

  or change the initial value

      (prog ((foo 0))
        (declare (fixnum foo))
        (setq foo ...)
        ...)

  It is generally preferable to change to a legal initial value rather
  than to weaken the declaration, but sometimes it is simpler to
  weaken the declaration than to try to make an initial value of the
  appropriate type.

  Another declaration problem occasionally encountered is incorrect
  declarations on DEFMACRO arguments. This can happen when a function
  is converted into a macro. Consider this macro:

      (defmacro my-1+ (x)
        (declare (fixnum x))
        `(the fixnum (1+ ,x)))

  Although legal and well-defined Common Lisp code, this meaning of
  this definition is almost certainly not what the writer intended.
  For example, this call is illegal:

      (my-1+ (+ 4 5))

  This call is illegal because the argument to the macro is `(+ 4 5)`,
  which is a LIST, not a FIXNUM. Because of macro semantics, it is
  hardly ever useful to declare the types of macro arguments. If you
  really want to assert something about the type of the result of
  evaluating a macro argument, then put a THE in the expansion:

      (defmacro my-1+ (x)
        `(the fixnum (1+ (the fixnum ,x))))


  In this case, it would be stylistically preferable to change this
  macro back to a function and declare it inline."
  ;; FIXME: <xref>inline-expansion, once we crib the relevant text
  ;; from the CMU CL manual.
  "Some more subtle problems are caused by incorrect declarations that
  can't be detected at compile time. Consider this code:

      (do ((pos 0 (position #\a string :start (1+ pos))))
        ((null pos))
        (declare (fixnum pos))
        ...)

  Although `POS` is almost always a FIXNUM, it is NIL at the end of
  the loop. If this example is compiled with full type checks (the
  default), then running it will signal a type error at the end of the
  loop. If compiled without type checks, the program will go into an
  infinite loop (or perhaps POSITION will complain because `(1+ NIL)`
  isn't a sensible start.) Why? Because if you compile without type
  checks, the compiler just quietly believes the type declaration.
  Since the compiler believes that `POS` is always a FIXNUM, it
  believes that `POS` is never NIL, so `(NULL POS)` is never true, and
  the loop exit test is optimized away. Such errors are sometimes
  flagged by unreachable code notes, but it is still important to
  initially compile and test any system with full type checks, even if
  the system works fine when compiled using other compilers.

  In this case, the fix is to weaken the type declaration to `(OR
  FIXNUM NULL)`. (Actually, this declaration is unnecessary in SBCL,
  since it already knows that POSITION returns a non-negative FIXNUM
  or NIL.)

  Note that there is usually little performance penalty for weakening
  a declaration in this way. Any numeric operations in the body can
  still assume that the variable is a FIXNUM, since NIL is not a legal
  numeric argument. Another possible fix would be to say:

      (do ((pos 0 (position #\a string :start (1+ pos))))
          ((null pos))
        (let ((pos pos))
          (declare (fixnum pos))
          ...))

  This would be preferable in some circumstances, since it would allow
  a non-standard representation to be used for the local `POS`
  variable in the loop body."
  ;; FIXME: <xref>ND-variables, once we crib the text from the CMU CL
  ;; manual.
  )

(defsection @implementation-limitations (:title "Implementation Limitations")
  "If an FTYPE is placed after the function definition the function won't
  perform any type checks, and the calls to the function will blindly
  trust the declared types.
  (OPTIMIZE (DEBUG 3)) will not trust any FTYPE declarations.")

(defsection @compiler-policy (:title "Compiler Policy")
  "Compiler policy is controlled by the OPTIMIZE declaration,
  supporting all ANSI optimization qualities (DEBUG, safety, space,
  and speed). (A deprecated extension SB-EXT:INHIBIT-WARNINGS is still
  supported but liable to go away at any time.)

  For effects of various optimization qualities on type-safety and
  debuggability see @DECLARATIONS-AS-ASSERTIONS and
  @DEBUGGER-POLICY-CONTROL.

  Ordinarily, when the speed quality is high, the compiler emits notes
  to notify the programmer about its inability to apply various
  optimizations. For selective muffling of these notes, see
  @CONTROLLING-VERBOSITY.

  The value of space mostly influences the compiler's decision whether
  to inline operations, which tend to increase the size of programs.
  Use the value `0` with caution, since it can cause the compiler to
  inline operations so indiscriminately that the net effect is to slow
  the program by causing cache misses or even swapping."
  (sb-ext:describe-compiler-policy function)
  (sb-ext:restrict-compiler-policy function)
  (with-compilation-unit macro))

;; FIXME: old CMU CL compiler policy, should perhaps be adapted for
;; SBCL. (Unfortunately, the CMU CL docs are out of sync with the CMU
;; CL code, so adapting this requires not only reformatting the
;; documentation, but rooting out code rot.)
;;
;; <sect2 id=\")compiler-policy\"><title>Compiler Policy</1000
;;   INDEX {policy}{compiler}
;;   INDEX compiler policy
;;
;; <para>The policy is what tells the compiler <emphasis>how</emphasis> to
;; compile a program. This is logically (and often textually) distinct
;; from the program itself. Broad control of policy is provided by the
;; <parameter>optimize</parameter> declaration; other declarations and variables
;; control more specific aspects of compilation.
;;
;; \begin{comment}
;; * The Optimize Declaration::
;; * The Optimize-Interface Declaration::
;; \end{comment}
;;
;; %%\node The Optimize Declaration, The Optimize-Interface Declaration, Compiler Policy, Compiler Policy
;; \subsection{The Optimize Declaration}
;; \label{optimize-declaration}
;; \cindex{optimize declaration}
;; \cpsubindex{declarations}{\code{optimize}}
;;
;; The \code{optimize} declaration recognizes six different
;; \var{qualities}.  The qualities are conceptually independent aspects
;; of program performance.  In reality, increasing one quality tends to
;; have adverse effects on other qualities.  The compiler compares the
;; relative values of qualities when it needs to make a trade-off; i.e.,
;; if \code{speed} is greater than \code{safety}, then improve speed at
;; the cost of safety.
;;
;; The default for all qualities (except \code{debug}) is \code{1}.
;; Whenever qualities are equal, ties are broken according to a broad
;; idea of what a good default environment is supposed to be.  Generally
;; this downplays \code{speed}, \code{compile-speed} and \code{space} in
;; favor of \code{safety} and \code{debug}.  Novice and casual users
;; should stick to the default policy.  Advanced users often want to
;; improve speed and memory usage at the cost of safety and
;; debuggability.
;;
;; If the value for a quality is \code{0} or \code{3}, then it may have a
;; special interpretation.  A value of \code{0} means ``totally
;; unimportant'', and a \code{3} means ``ultimately important.''  These
;; extreme optimization values enable ``heroic'' compilation strategies
;; that are not always desirable and sometimes self-defeating.
;; Specifying more than one quality as \code{3} is not desirable, since
;; it doesn't tell the compiler which quality is most important.
;;
;;
;; These are the optimization qualities:
;; \begin{Lentry}
;;
;; \item[\code{speed}] \cindex{speed optimization quality}How fast the
;;   program should is run.  \code{speed 3} enables some optimizations
;;   that hurt debuggability.
;;
;; \item[\code{compilation-speed}] \cindex{compilation-speed optimization
;;     quality}How fast the compiler should run.  Note that increasing
;;   this above \code{safety} weakens type checking.
;;
;; \item[\code{space}] \cindex{space optimization quality}How much space
;;   the compiled code should take up.  Inline expansion is mostly
;;   inhibited when \code{space} is greater than \code{speed}.  A value
;;   of \code{0} enables indiscriminate inline expansion.  Wide use of a
;;   \code{0} value is not recommended, as it may waste so much space
;;   that run time is slowed.  \xlref{inline-expansion} for a discussion
;;   of inline expansion.
;;
;; \item[\code{debug}] \cindex{debug optimization quality}How debuggable
;;   the program should be.  The quality is treated differently from the
;;   other qualities: each value indicates a particular level of debugger
;;   information; it is not compared with the other qualities.
;;   \xlref{debugger-policy} for more details.
;;
;; \item[\code{safety}] \cindex{safety optimization quality}How much
;;   error checking should be done.  If \code{speed}, \code{space} or
;;   \code{compilation-speed} is more important than \code{safety}, then
;;   type checking is weakened (\pxlref{weakened-type-checks}).  If
;;   \code{safety} if \code{0}, then no run time error checking is done.
;;   In addition to suppressing type checks, \code{0} also suppresses
;;   argument count checking, unbound-symbol checking and array bounds
;;   checks.
;;   ... and checking of tag existence in RETURN-FROM and GO.
;;
;; \item[\code{extensions:inhibit-warnings}] \cindex{inhibit-warnings
;;     optimization quality}This is a CMU extension that determines how
;;   little (or how much) diagnostic output should be printed during
;;   compilation.  This quality is compared to other qualities to
;;   determine whether to print style notes and warnings concerning those
;;   qualities.  If \code{speed} is greater than \code{inhibit-warnings},
;;   then notes about how to improve speed will be printed, etc.  The
;;   default value is \code{1}, so raising the value for any standard
;;   quality above its default enables notes for that quality.  If
;;   \code{inhibit-warnings} is \code{3}, then all notes and most
;;   non-serious warnings are inhibited.  This is useful with
;;   \code{declare} to suppress warnings about unavoidable problems.
;; \end{Lentry}
;;
;; %%\node The Optimize-Interface Declaration,  , The Optimize Declaration, Compiler Policy
;; \subsection{The Optimize-Interface Declaration}
;; \label{optimize-interface-declaration}
;; \cindex{optimize-interface declaration}
;; \cpsubindex{declarations}{\code{optimize-interface}}
;;
;; The \code{extensions:optimize-interface} declaration is identical in
;; syntax to the \code{optimize} declaration, but it specifies the policy
;; used during compilation of code the compiler automatically generates
;; to check the number and type of arguments supplied to a function.  It
;; is useful to specify this policy separately, since even thoroughly
;; debugged functions are vulnerable to being passed the wrong arguments.
;; The \code{optimize-interface} declaration can specify that arguments
;; should be checked even when the general \code{optimize} policy is
;; unsafe.
;;
;; Note that this argument checking is the checking of user-supplied
;; arguments to any functions defined within the scope of the
;; declaration, \code{not} the checking of arguments to \llisp{}
;; primitives that appear in those definitions.
;;
;; The idea behind this declaration is that it allows the definition of
;; functions that appear fully safe to other callers, but that do no
;; internal error checking.  Of course, it is possible that arguments may
;; be invalid in ways other than having incorrect type.  Functions
;; compiled unsafely must still protect themselves against things like
;; user-supplied array indices that are out of bounds and improper lists.
;; See also the \kwd{context-declarations} option to
;; \macref{with-compilation-unit}.
;;
;; (end of section on compiler policy)

(defsection @compiler-errors (:title "Compiler Errors")
  (@type-errors-at-compile-time section)
  (@errors-during-macroexpansion section)
  (@read-errors section))

(defsection @type-errors-at-compile-time
    (:title "Type Errors at Compile Time"
     :concepts (("compile-time" "type error")
                ("type error," "compile-time")))
  "If the compiler can prove at compile time that some portion of the
  program cannot be executed without a type error, then it will give a
  warning at compile time.

  It is possible that the offending code would never actually be
  executed at run-time due to some higher level consistency constraint
  unknown to the compiler, so a type warning doesn't always indicate an
  incorrect program.

  For example, consider this code fragment:

      (defun raz (foo)
        (let ((x (case foo
                    (:this 13)
                    (:that 9)
                    (:the-other 42))))
          (declare (fixnum x))
          (foo x)))

  Compilation produces this warning:

      ; in: DEFUN RAZ
      ;     (CASE FOO (:THIS 13) (:THAT 9) (:THE-OTHER 42))
      ; --> LET COND IF COND IF COND IF
      ; ==>
      ;   (COND)
      ;
      ; caught WARNING:
      ;   This is not a FIXNUM:
      ;   NIL

  In this case, the warning means that if `FOO` isn't any of `:THIS`,
  `:THAT` or `:THE-OTHER`, then `x` will be initialized to NIL, which
  the FIXNUM declaration makes illegal. The warning will go away if
  ECASE is used instead of CASE, or if `:THE-OTHER` is changed to T.

  This sort of spurious type warning happens moderately often in the
  expansion of complex macros and in inline functions. In such cases,
  there may be dead code that is impossible to correctly execute. The
  compiler can't always prove this code is dead (could never be
  executed), so it compiles the erroneous code (which will always signal
  an error if it is executed) and gives a warning.")

(defsection @errors-during-macroexpansion
    (:title "Errors During Macroexpansion"
     :concepts (("macroexpansion," "errors during")))
  "The compiler handles errors that happen during macroexpansion, turning
  them into compiler errors. If you want to debug the error (to debug
  a macro), you can set *BREAK-ON-SIGNALS* to ERROR. For example, this
  definition:

      (defun foo (e l)
        (do ((current l (cdr current))
             ((atom current) nil))
            (when (eq (car current) e) (return current))))

  gives this error:

      ; in: DEFUN FOO
      ;     (DO ((CURRENT L (CDR CURRENT))
      ;        ((ATOM CURRENT) NIL))
      ;       (WHEN (EQ (CAR CURRENT) E) (RETURN CURRENT)))
      ;
      ; caught ERROR:
      ;   (in macroexpansion of (DO # #))
      ;   (hint: For more precise location, try *BREAK-ON-SIGNALS*.)
      ;   DO step variable is not a symbol: (ATOM CURRENT)")

(defsection @read-errors (:title "Read Errors"
                          :concepts (("compiler" "read error")
                                     ("read error," "compiler")))
  "SBCL's compiler does not attempt to recover from read errors when
  reading a source file, but instead just reports the offending
  character position and gives up on the entire source file.")

(defsection @open-coding-and-inline-expansion
    (:title "Open Coding and Inline Expansion"
     :concepts ("open-coding"
                ("inline" "expansion")
                ("static" "functions")))
  "Since Common Lisp forbids the redefinition of standard functions, the
  compiler can have special knowledge of these standard functions
  embedded in it. This special knowledge is used in various ways (open
  coding, inline expansion, source transformation), but the implications
  to the user are basically the same:

  - Attempts to redefine standard functions may be frustrated, since
    the function may never be called. Although it is technically
    illegal to redefine standard functions, users sometimes want to
    implicitly redefine these functions when they are debugging using
    the TRACE macro. Special-casing of standard functions can be
    inhibited using the NOTINLINE declaration, but even then some
    phases of analysis such as type inferencing are applied by the
    compiler.

  - The compiler can have multiple alternate implementations of
    standard functions that implement different trade-offs of speed,
    space and safety. This selection is based on the @COMPILER-POLICY.

  When a function call is _open coded_, inline code whose effect is
  equivalent to the function call is substituted for that function
  call. When a function call is _closed coded_, it is usually left as
  is, although it might be turned into a call to a different function
  with different arguments. As an example, if NTHCDR were to be open
  coded, then

      (nthcdr 4 foobar)

  might turn into

      (cdr (cdr (cdr (cdr foobar))))

  or even

      (do ((i 0 (1+ i))
        (list foobar (cdr foobar)))
        ((= i 4) list))

  If NTH is closed coded, then

      (nth x l)

  might stay the same, or turn into something like

      (car (nthcdr x l))

  In general, open coding sacrifices space for speed, but some functions
  (such as CAR) are so simple that they are always open-coded. Even
  when not open-coded, a call to a standard function may be
  transformed into a different function call (as in the last example)
  or compiled as _static call_. Static function call uses a more
  efficient calling convention that forbids redefinition.")

(defsection @interpreter (:title "Interpreter"
                          :concepts ("interpreter"))
  "By default SBCL implements EVAL by calling the native code
  compiler.

  SBCL also includes an interpreter for use in special cases where
  using the compiler is undesirable, for example due to compilation
  overhead. Unlike in some other Lisp implementations, in SBCL
  interpreted code is not safer or more debuggable than compiled code."
  (sb-ext:*evaluator-mode* variable))

(defsection @advanced-compiler-use-and-efficiency-hints
    (:title "Advanced Compiler Use and Efficiency Hints")
  "For more advanced usages of the compiler, please see the chapter of the
  same name in the CMUCL manual. Many aspects of the compiler have stayed
  exactly the same, and there is a much more detailed explanation of the
  compiler's behavior and how to maximally optimize code in their
  manual. In particular, while SBCL no longer supports byte-code
  compilation, it does support CMUCL's block compilation facility allowing
  whole program optimization and increased use of the local call
  convention.

  Unlike CMUCL, SBCL is able to open-code forward-referenced type
  tests while block compiling. This helps for mutually referential
  DEFSTRUCTs in particular.")
