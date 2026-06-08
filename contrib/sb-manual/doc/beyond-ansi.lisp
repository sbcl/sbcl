(in-package :sb-manual)

(defsection @beyond-the-ansi-standard (:title "Beyond the ANSI Standard")
  "SBCL is derived from CMUCL, which implements many extensions to the
  ANSI standard. SBCL doesn't support as many extensions as CMUCL, but
  it still has quite a few. See @CONTRIBUTED-MODULES."
  (@reader-extensions section)
  (@package-local-nicknames section)
  (@package-variance section)
  (@garbage-collection section)
  (@generic-function-dispatch section)
  (@extended-slot-access section)
  (@metaobject-protocol section)
  (@extensible-sequences section)
  (@support-for-unix section)
  (@unicode-support section)
  (@customization-hooks-for-users section)
  (@tools-to-help-developers section)
  (@resolution-of-name-conflicts section)
  (@hash-table-extensions section)
  (@random-number-generation section)
  (@timeouts-and-deadlines section)
  (@miscellaneous-extensions section)
  (@stale-extensions section)
  (@efficiency-hacks section))

(defsection @reader-extensions (:title "Reader Extensions")
  (@extended-package-prefix-syntax section)
  (@symbol-name-normalization section)
  (@decimal-syntax-for-rationals section))

(defsection @extended-package-prefix-syntax
    (:title "Extended Package Prefix Syntax")
  "SBCL supports extended package prefix syntax, which allows specifying
  an alternate package instead of *PACKAGE* for the reader to use as
  the default package for interning symbols:

      <package-name>::<form-with-interning-into-package>

  Example:

      'foo::(bar quux zot) == '(foo::bar foo::quux foo::zot)

  *PACKAGE* is not rebound during the course of reading a form with
  extended package prefix syntax; if `FOO::BAR` would cause a
  read-time package lock violation, so does `FOO::(BAR)`.")

(defsection @symbol-name-normalization (:title "Symbol Name Normalization")
  "SBCL also extends the reader to normalize all symbols to _Normalization
  Form KC_ in builds with Unicode enabled. Whether symbols are
  normalized is controlled by"
  (sb-ext:readtable-normalization function)
  "Symbols created by INTERN and similar functions are not affected by
  this setting. If SB-EXT:READTABLE-NORMALIZATION is T, symbols that
  are not normalized are escaped during printing.")

(defsection @decimal-syntax-for-rationals
    (:title "Decimal Syntax for Rationals")
  "SBCL supports a decimal syntax for rationals, modelled after the
  standard syntax for floating-point numbers. If a number with
  floating-point syntax has an exponent marker of `r` or `R`
  (rather than one of the standard exponent markers), it is read as
  the rational with the exact value of the decimal number expressed as
  a float.

  In addition, setting or binding the value of
  *READ-DEFAULT-FLOAT-FORMAT* to RATIONAL around a call to READ or
  READ-FROM-STRING has the effect that floating-point numbers without
  exponent markers are read as rational numbers, as if there had been
  an explicit `r` or `R` marker.

  Floating point numbers of all types are printed with an exponent
  marker while the value of *READ-DEFAULT-FLOAT-FORMAT* is RATIONAL;
  however, rational numbers are printed in their standard syntax,
  irrespective of the value of *READ-DEFAULT-FLOAT-FORMAT*.")

(defsection @package-local-nicknames (:title "Package-Local Nicknames")
  "SBCL allows giving packages local nicknames: they allow short and
  easy-to-use names to be used without fear of name conflict associated
  with normal nicknames.

  A local nickname is valid only when inside the package for which it
  has been specified. Different packages can use same local nickname
  for different global names, or different local nickname for same
  global name.

  The symbol :PACKAGE-LOCAL-NICKNAMES in *FEATURES* denotes the
  support for this feature.

  DEFPACKAGE options are extended to include

      :local-nicknames (<local-nickname> <actual-package-name>)*

  with the semantics of adding the package package-local nicknames
  `<local-nickname>`s for the corresponding `<actual-package-name>`s.

  Example:

      (defpackage :bar (:intern \"X\"))
      (defpackage :foo (:intern \"X\"))
      (defpackage :quux (:use :cl) (:local-nicknames (:bar :foo) (:foo :bar)))
      (find-symbol \"X\" :foo) ; => FOO::X
      (find-symbol \"X\" :bar) ; => BAR::X
      (let ((*package* (find-package :quux)))
        (find-symbol \"X\" :foo))               ; => BAR::X
      (let ((*package* (find-package :quux)))
        (find-symbol \"X\" :bar))               ; => FOO::X"
  (sb-ext:package-local-nicknames function)
  (sb-ext:package-locally-nicknamed-by-list function)
  (sb-ext:add-package-local-nickname function)
  (sb-ext:remove-package-local-nickname function))

(defsection @package-variance (:title "Package Variance")
  "DEFPACKAGE CLHS specifies that _if the new definition is at
  variance with the current state of that package, the consequences
  are undefined_. SBCL by default signals a full warning and retains
  as much of the package state as possible. This can be adjusted with
  the following variable."
  (sb-ext:*on-package-variance* variable))

(defsection @garbage-collection (:title "Garbage Collection")
  "SBCL provides additional garbage collection functionality not
  specified by ANSI."
  (sb-ext:gc function)
  (sb-ext:*after-gc-hooks* variable)
  (@finalization section)
  (@weak-pointers section)
  (@introspection-and-tuning section)
  (@tracing-live-objects-back-to-roots section))

(defsection @finalization (:title "Finalization")
  "Finalization allows code to be executed after an object has been
  garbage collected. This is useful for example for releasing foreign
  memory associated with a Lisp object."
  (sb-ext:finalize function)
  (sb-ext:cancel-finalization function))

(defsection @weak-pointers (:title "Weak Pointers")
  "Weak pointers allow references to objects to be maintained without
  keeping them from being garbage collected: useful for building caches
  among other things.

  Hash tables can also have weak keys and values. See
  @HASH-TABLE-EXTENSIONS."
  (sb-ext:make-weak-pointer function)
  (sb-ext:weak-pointer-value function))

(defsection @introspection-and-tuning (:title "Introspection and Tuning")
  (sb-ext:*gc-run-time* (variable 0))
  (sb-ext:*gc-real-time* (variable 0))
  (sb-ext:bytes-consed-between-gcs function)
  (sb-ext:dynamic-space-size function)
  (sb-ext:get-bytes-consed function)
  (sb-ext:gc-logfile function)
  (sb-ext:generation-average-age function)
  (sb-ext:generation-bytes-allocated function)
  (sb-ext:generation-bytes-consed-between-gcs function)
  (sb-ext:generation-minimum-age-before-gc function)
  (sb-ext:generation-number-of-gcs-before-promotion function)
  (sb-ext:generation-number-of-gcs function))

(defsection @tracing-live-objects-back-to-roots
    (:title "Tracing Live Objects Back to Roots")
  "This feature is intended to help expert users diagnose rare low-level
  issues and should not be needed during normal usage. On top of that,
  the interface and implementation are experimental and may change at
  any time without further notice.

  It is sometimes important to understand why a given object is
  retained in the Lisp image instead of being garbage collected. To
  help with this problem, SBCL provides a mechanism that searches
  through the different memory spaces, builds a path of references
  from a root to the object in question and finally reports this
  paths:"
  (sb-ext:search-roots function)
  "An example of using this could look like this:

      * (defvar *my-string* (list 1 2 \"my string\"))
      *MY-STRING*

       * (sb-ext:search-roots (sb-ext:make-weak-pointer (third *my-string*)))
       -> ((SIMPLE-VECTOR 3)) #x10004E9EAF[2] -> (SYMBOL) #x5044100F[1] -> (CONS) #x100181FAE7[1] -> (CONS) #x100181FAF7[1] -> (CONS) #x100181FB07[0] -> #x100181F9AF

  The single line of output on *STANDARD-OUTPUT* shows the path from a
  root to `\"my string\"`: the path starts with SBCL's internal
  package system data structures followed by the symbol
  (`CL-USER:*MY-STRING*`) followed the three cons cells of the list.

  The `:PRINT :VERBOSE` argument produces similar behavior but
  describes the path elements in more detail:

      * (sb-ext:search-roots (sb-ext:make-weak-pointer (third *my-string*))
                             :print :verbose)
      Path to \"my string\":
       6       10004E9EAF [   2] a (simple-vector 3)
       0         5044100F [   1] COMMON-LISP-USER::*MY-STRING*
       0       100181FAE7 [   1] a cons
       0       100181FAF7 [   1] a cons
       0       100181FB07 [   0] a cons

  The `:PRINT NIL` argument is a bit different:

      * (sb-ext:search-roots (sb-ext:make-weak-pointer (third *my-string*))
                             :print nil)
      ((\"my string\" :STATIC (#(*MY-STRING* 0 0) . 2) (*MY-STRING* . 1)
        ((1 2 \"my string\") . 1) ((2 \"my string\") . 1) ((\"my string\") . 0)))


  There is no output on *STANDARD-OUTPUT*, and the return value is a
  single path for the target object `\"my string\"`. As before, the
  path shows the symbol and the three cons cells.")

(defsection @generic-function-dispatch (:title "Generic Function Dispatch")
  "If a generic function with standard or short method combination is
  called, and the set of applicable methods does not include any
  primary methods, then the generic function SB-PCL:NO-PRIMARY-METHOD
  will be invoked with the arguments being the invoked generic
  function and its arguments, similar to the standard function
  NO-APPLICABLE-METHOD. As with NO-APPLICABLE-METHOD, the default
  method on SB-PCL:NO-PRIMARY-METHOD signals an error; programmers may
  define methods on it.")

(defsection @extended-slot-access (:title "Extended Slot Access")
  "The slot access functions SLOT-VALUE, `(SETF SLOT-VALUE)`,
  SLOT-BOUNDP and SLOT-MAKUNBOUND are defined to function as expected
  on conditions (of metaclass SB-PCL::CONDITION-CLASS) and, with some
  limitations, on structures (of metaclass STRUCTURE-CLASS).

  For structures:

  - The name of a slot for the purposes of the slot access functions
    is the symbol used as the slot-name in the slot-description in the
    DEFSTRUCT form;

  - SLOT-VALUE and SLOT-BOUNDP function as expected, including (for
    SLOT-VALUE) calling and respecting the return value of
    SLOT-UNBOUND if the slot is unbound;

  - `(SETF SLOT-VALUE)` functions as expected, including performing
    type checks to verify that the new value is of an appropriate type
    for the slot;

  - SLOT-MAKUNBOUND makes the slot unbound only when the slot
    corresponds to an &AUX argument with no default in a
    by-order-of-arguments (BOA) constructor. In all other cases
    calling SLOT-MAKUNBOUND on a structure signals an error.

  - If any of the slot access functions is called with a structure
    instance which does not have a slot of the given name,
    SLOT-MISSING is called and the return value of the effective
    method, if any, is respected.")

(defsection @metaobject-protocol (:title "Metaobject Protocol")
  (@amop-compatibility-of-metaobject-protocol section)
  (@metaobject-protocol-extensions section))

(defsection @amop-compatibility-of-metaobject-protocol
    (:title "AMOP Compatibility of Metaobject Protocol")
  "SBCL supports a metaobject protocol which is intended to be compatible
  with AMOP; present exceptions to this (as distinct from current bugs)
  are:

  - SB-MOP:COMPUTE-EFFECTIVE-METHOD only returns one value, not two.
    There is no record of what the second return value was meant to
    indicate, and apparently no clients for it.

  - The direct superclasses of SB-MOP:FUNCALLABLE-STANDARD-OBJECT are
    (FUNCTION STANDARD-OBJECT) instead of the correct (STANDARD-OBJECT
    FUNCTION).

      This is to ensure that the STANDARD-OBJECT class is the last of
      the standardized classes before class T appearing in the
      precedence list of GENERIC-FUNCTION and
      STANDARD-GENERIC-FUNCTION, as required by CLHS `1.4.4.5`.

  - The arguments :DECLARE and :DECLARATIONS are both accepted by
    ENSURE-GENERIC-FUNCTION, with the leftmost argument defining the
    declarations to be stored and returned by
    SB-MOP:GENERIC-FUNCTION-DECLARATIONS.

      Where AMOP specifies :DECLARATIONS as the keyword argument to
      ENSURE-GENERIC-FUNCTION, the Common Lisp standard specifies
     :DECLARE. Portable code should use :DECLARE.

  - Although SBCL obeys the requirement in AMOP that
    SB-MOP:VALIDATE-SUPERCLASS should treat STANDARD-CLASS and
    SB-MOP:FUNCALLABLE-STANDARD-CLASS as compatible metaclasses, we
    impose an additional requirement at class finalization time: a
    class of metaclass SB-MOP:FUNCALLABLE-STANDARD-CLASS must have
    FUNCTION in its superclasses, and a class of metaclass
    STANDARD-CLASS must not.

      After a class has been finalized, it is associated with a class
      prototype which is accessible by a standard MOP function
      SB-MOP:CLASS-PROTOTYPE. The user can then ask whether this
      object is a FUNCTION or not in several different ways: whether
      it is a function according to TYPEP; whether its CLASS-OF is
      SUBTYPEP FUNCTION, or whether FUNCTION appears in the
      superclasses of the class. The additional consistency
      requirement comes from the desire to make all of these answers
      the same.

      The following class definitions are bad, and will lead to errors
      either immediately or if an instance is created:

          (defclass bad-object (funcallable-standard-object)
            ()
            (:metaclass standard-class))
          (defclass bad-funcallable-object (standard-object)
            ()
            (:metaclass funcallable-standard-class))

      The following definition is acceptable:

          (defclass mixin ()
            ((slot :initarg slot)))
          (defclass funcallable-object (funcallable-standard-object mixin)
            ()
            (:metaclass funcallable-standard-class))

      and leads to a class whose instances are funcallable and have one slot.

      Note that this requirement also applies to the class
      SB-MOP:FUNCALLABLE-STANDARD-OBJECT, which has metaclass
      SB-MOP:FUNCALLABLE-STANDARD-CLASS rather than STANDARD-CLASS as
      AMOP specifies.

  - The requirement that _no portable class may inherit, by virtue of
    being a direct or indirect subclass of a specified class, any slot
    for which the name is a symbol accessible in the
    `COMMON-LISP-USER` package or exported by any package defined in
    the ANSI Common Lisp standard_. is interpreted to mean that the
    standardized classes themselves should not have slots named by
    external symbols of public packages.

      The rationale behind the restriction is likely to be similar to
      the ANSI Common Lisp restriction on defining functions,
      variables and types named by symbols in the Common Lisp package:
      preventing two independent pieces of software from colliding
      with each other.

  - Specializations of the `NEW-VALUE` argument to (SETF
    SB-MOP:SLOT-VALUE-USING-CLASS) are not allowed: all user-defined
    methods must have a specializer of the class T.

      This prohibition is motivated by a separation of layers: the
      SB-MOP:SLOT-VALUE-USING-CLASS family of functions is intended
      for use in implementing different and new slot allocation
      strategies, rather than in performing application-level
      dispatching. Additionally, with this requirement, there is a
      one-to-one mapping between metaclass, class and
      slot-definition-class tuples and effective methods of (SETF
      SB-MOP:SLOT-VALUE-USING-CLASS), which permits optimization
      of (SETF SB-MOP:SLOT-VALUE-USING-CLASS)'s discriminating
      function in the same manner as for SB-MOP:SLOT-VALUE-USING-CLASS
      and SB-MOP:SLOT-BOUNDP-USING-CLASS.

      Note that application code may specialize on the `NEW-VALUE`
      argument of slot accessors.

  - The class named by the `NAME` argument to SB-MOP:ENSURE-CLASS, if any, is
    only redefined if it is the proper name of that class; otherwise,
    a new class is created.

      This is consistent with the description SB-MOP:ENSURE-CLASS in
      AMOP as the functional version of DEFCLASS, which has this
      behaviour; however, it is not consistent with the weaker
      requirement in AMOP, which states that any class found by
      FIND-CLASS, no matter what its [CLASS-NAME][function], is
      redefined.

  - An error is not signaled in the case of the :NAME initialization
    argument for SB-MOP:SLOT-DEFINITION objects being a constant, when
    the slot definition is of type SB-PCL::STRUCTURE-SLOT-DEFINITION
    (i.e. it is associated with a class of type STRUCTURE-CLASS).

      This allows code which uses constant names for structure slots
      to continue working as specified in ANSI, while enforcing the
      constraint for all other types of slot.

  - The class T is not an instance of the BUILT-IN-CLASS metaclass.

      AMOP specifies, in the _Inheritance Structure of Metaobject
      Classes_ section, that the class T should be an instance of
      BUILT-IN-CLASS. However, it also specifies that
      SB-MOP:VALIDATE-SUPERCLASS should return true (indicating that a
      direct superclass relationship is permissible) if the second
      argument is the class T. Also, ANSI specifies that classes with
      metaclass BUILT-IN-CLASS may not be subclassed using DEFCLASS,
      and also that the class T is the universal superclass,
      inconsistent with it being a BUILT-IN-CLASS.

  - Uses of CHANGE-CLASS and redefinitions of classes with
    DEFCLASS (or the functional interfaces SB-MOP:ENSURE-CLASS or
    SB-MOP:ENSURE-CLASS-USING-CLASS) must ensure that for each slot
    with allocation :INSTANCE or :CLASS, the set of applicable methods
    on the SB-MOP:SLOT-VALUE-USING-CLASS family of generic functions
    is the same before and after the change.

      This is required for correct operation of the protocol to update
      instances for the new or redefined class, and can be seen as
      part of the contract of the :INSTANCE or :CLASS allocations.

  - Metaobject protocol users may wish to override
    SB-MOP:COMPUTE-DISCRIMINATING-FUNCTION for their own generic
    function classes. Overriding implementations of
    SB-MOP:COMPUTE-DISCRIMINATING-FUNCTION must, in order to
    participate in the NO-APPLICABLE-METHOD and
    SB-PCL:NO-PRIMARY-METHOD protocols, perform appropriate checks on
    the return value of COMPUTE-APPLICABLE-METHODS before processing
    the effective method; the standard effective method contains
    error-invoking forms, but those forms have no access to the
    generic function invocation's arguments.")

(defsection @metaobject-protocol-extensions
    (:title "Metaobject Protocol Extensions")
  "In addition, SBCL supports extensions to the Metaobject protocol from
  AMOP; at present, they are:

  - Compile-time support for generating specializer metaobjects from
    specializer names in DEFMETHOD forms is provided by the
    SB-PCL:MAKE-METHOD-SPECIALIZERS-FORM function, which returns a
    form which, when evaluated in the lexical environment of the
    DEFMETHOD, returns a list of specializer metaobjects. This
    operator suffers from similar restrictions to those affecting
    SB-MOP:MAKE-METHOD-LAMBDA, namely that the generic function must
    be defined when the DEFMETHOD form is expanded, so that the
    correct method of SB-PCL:MAKE-METHOD-SPECIALIZERS-FORM is invoked.
    The system-provided method on SB-PCL:MAKE-METHOD-SPECIALIZERS-FORM
    generates a call to FIND-CLASS for each symbol specializer name,
    and a call to SB-MOP:INTERN-EQL-SPECIALIZER for each
    `(EQL <x>)` specializer name.

  - Run-time support for converting between specializer names and
    specializer metaobjects, mostly for the purposes of FIND-METHOD,
    is provided by SB-PCL:PARSE-SPECIALIZER-USING-CLASS and
    SB-PCL:UNPARSE-SPECIALIZER-USING-CLASS, which dispatch on their
    first argument, the generic function associated with a method with
    the given specializer. The system-provided methods on those
    methods convert between classes and proper names and between lists
    of the form `(EQL <x>)` and interned eql specializer objects.

  - Distinguishing unbound instance allocated slots from bound ones
    when using SB-MOP:STANDARD-INSTANCE-ACCESS and
    SB-MOP:FUNCALLABLE-STANDARD-INSTANCE-ACCESS is possible by
    comparison to the symbol-macro SB-PCL:+SLOT-UNBOUND+.")

(defsection @extensible-sequences (:title "Extensible Sequences")
  "ANSI Common Lisp has a class SEQUENCE with subclasses LIST and
  VECTOR, on which the sequence functions like FIND, SUBSEQ, etc.
  operate. As an extension to the ANSI specification, SBCL allows
  additional subclasses of SEQUENCE to be defined.

  > A motivation, rationale and additional examples for the design of
  > this extension can be found in the paper _Rhodes,
  > Christophe (2007): User-extensible sequences in Common Lisp_
  > available for download at
  > <http://www.doc.gold.ac.uk/~mas01cr/papers/ilc2007/sequences-20070301.pdf>.

  Users of this extension just make instances of SEQUENCE subclasses
  and transparently operate on them using sequence functions:

      (coerce (subseq (make-instance 'my-sequence) 5 10) 'list)

  From this perspective, no distinction between builtin and user-defined
  SEQUENCE subclasses should be necessary.

  Providers of the extension, that is of user-defined SEQUENCE
  subclasses, have to adhere to a _sequence protocol_ which consists
  of a set of generic functions in the SEQUENCE package.

  A minimal SEQUENCE subclass has to specify STANDARD-OBJECT and
  SEQUENCE as its superclasses and has to be the specializer of the
  SEQUENCE parameter of methods on at least the following generic
  functions:"
  (sb-sequence:length generic-function)
  (sb-sequence:elt generic-function)
  (sb-sequence:elt setf-generic-function)
  (sb-sequence:adjust-sequence generic-function)
  (sb-sequence:make-sequence-like generic-function)
  "`MAKE-SEQUENCE-LIKE` is needed for functions returning
  freshly-allocated sequences such as SUBSEQ or COPY-SEQ.
  `ADJUST-SEQUENCE` is needed for functions which destructively modify
  their arguments such as DELETE. In fact, all other sequence
  functions can be implemented in terms of the above functions and
  actually are, if no additional methods are defined. However, relying
  on these generic implementations, in particular not implementing the
  @EXSEQ-ITERATOR-PROTOCOL can incur a high performance penalty.

  When the sequence protocol is only partially implemented for a given
  SEQUENCE subclass, an attempt to apply one of the missing operations
  to instances of that class signals the following condition:"
  (sb-sequence:protocol-unimplemented condition)
  "In addition to the mandatory functions above, methods on the sequence
  functions listed below can be defined.

  There are some noteworthy irregularities:

  - The function SB-SEQUENCE:EMPTYP does not have a counterpart in the
    `CL` package. It is intended to be used instead of
    SB-SEQUENCE:LENGTH when working with lazy or infinite sequences.

  - SB-SEQUENCE:DOSEQUENCE does not have a direct counterpart either.
    It is like DOLIST in spirit but traverses generic sequences.

  - The functions MAP, CONCATENATE and MERGE receive a type designator
    specifying the type of the constructed sequence as their first
    argument. However, the corresponding generic functions
    SB-SEQUENCE:MAP, SB-SEQUENCE:CONCATENATE and SB-SEQUENCE:MERGE
    receive a prototype instance of the requested SEQUENCE subclass
    instead.

  - CL:MAP-INTO has no generic sequence counterpart, as its lambda
    list does not provide reasonable specialization opportunities, but
    it supports extensible sequences directly."
  (sb-sequence:emptyp generic-function)
  (sb-sequence:dosequence macro)
  "The remaining list parallels the _Sequence Dictionary_, see
  [17.3][clhs] in the ANSI spec."
  (sb-sequence:copy-seq generic-function)
  (sb-sequence:fill generic-function)
  (sb-sequence:subseq generic-function)
  (sb-sequence:map function)
  (sb-sequence:reduce generic-function)
  (sb-sequence:search generic-function)
  (sb-sequence:mismatch generic-function)
  (sb-sequence:replace generic-function)
  (sb-sequence:concatenate function)
  (sb-sequence:merge function)
  "Counting:"
  (sb-sequence:count generic-function)
  (sb-sequence:count-if generic-function)
  (sb-sequence:count-if-not generic-function)
  "Reversing:"
  (sb-sequence:reverse generic-function)
  (sb-sequence:nreverse generic-function)
  "Sorting:"
  (sb-sequence:sort generic-function)
  (sb-sequence:stable-sort generic-function)
  "Finding an element:"
  (sb-sequence:find generic-function)
  (sb-sequence:find-if generic-function)
  (sb-sequence:find-if-not generic-function)
  "Finding a position:"
  (sb-sequence:position generic-function)
  (sb-sequence:position-if generic-function)
  (sb-sequence:position-if-not generic-function)
  "Substituting elements:"
  (sb-sequence:substitute generic-function)
  (sb-sequence:substitute-if generic-function)
  (sb-sequence:substitute-if-not generic-function)
  (sb-sequence:nsubstitute generic-function)
  (sb-sequence:nsubstitute-if generic-function)
  (sb-sequence:nsubstitute-if-not generic-function)
  "Removing elements:"
  (sb-sequence:remove generic-function)
  (sb-sequence:remove-if generic-function)
  (sb-sequence:remove-if-not generic-function)
  (sb-sequence:delete generic-function)
  (sb-sequence:delete-if generic-function)
  (sb-sequence:delete-if-not generic-function)
  "Removing duplicates:"
  (sb-sequence:remove-duplicates generic-function)
  (sb-sequence:delete-duplicates generic-function)
  (@exseq-iterator-protocol section)
  (@exseq-simple-iterator-protocol section))

(defsection @exseq-iterator-protocol (:title "Iterator Protocol")
  "The general iterator protocol allows subsequently accessing some or
  all elements of a sequence in forward or reverse direction. Users
  first call SB-SEQUENCE:MAKE-SEQUENCE-ITERATOR to create an iteration
  state and receive functions to query and mutate it. These functions
  allow, among other things, moving to, retrieving or modifying
  elements of the sequence. The iteration state consists of a state
  object, a limit object, a from-end indicator and six functions to
  query or mutate this state.

  An iterator is created by calling:"
  (sb-sequence:make-sequence-iterator function)
  "The following convenience macros simplify traversing sequences using
  iterators:"
  (sb-sequence:with-sequence-iterator macro)
  (sb-sequence:with-sequence-iterator-functions macro))

(defsection @exseq-simple-iterator-protocol (:title "Simple Iterator Protocol")
  "For cases in which the full flexibility and performance of the general
  sequence iterator protocol is not required, there is a simplified
  sequence iterator protocol consisting of a few generic functions which
  can be specialized for iterator classes:"
  (sb-sequence:iterator-step generic-function)
  (sb-sequence:iterator-endp generic-function)
  (sb-sequence:iterator-element generic-function)
  (sb-sequence:iterator-element setf-generic-function)
  (sb-sequence:iterator-index generic-function)
  (sb-sequence:iterator-copy generic-function)
  "Iterator objects implementing the above simple iteration protocol are
  created by calling the following generic function:"
  (sb-sequence:make-simple-sequence-iterator generic-function))

(defsection @support-for-unix (:title "Support For Unix")
  (sb-ext:*posix-argv* (variable "<varies>"))
  (sb-ext:posix-getenv function)
  (sb-ext:posix-environ function)
  (@running-external-programs section))

(defsection @running-external-programs (:title "Running external programs")
  "External programs can be run with SB-EXT:RUN-PROGRAM.

  > _Note_: In SBCL versions prior to 1.0.13, SB-EXT:RUN-PROGRAM
  > searched for executables in a manner somewhat incompatible with
  > other languages. As of this version, SBCL uses the system library
  > routine `execvp(3)`, and no longer contains the function
  > `FIND-EXECUTABLE-IN-SEARCH-PATH`, which implemented the old
  > search. Users who need this function may find it in
  > `run-program.lisp` versions 1.67 and earlier in SBCL's CVS
  > repository here
  > <http://sbcl.cvs.sourceforge.net/sbcl/sbcl/src/code/run-program.lisp?view=log>.
  > However, we caution such users that this search routine finds
  > executables that system library routines do not."
  (sb-ext:run-program function)
  "When SB-EXT:RUN-PROGRAM is called with :WAIT NIL, an process object
  is returned. The following functions are available for use with
  processes:"
  (sb-ext:process-p function)
  (sb-ext:process-input function)
  (sb-ext:process-output function)
  (sb-ext:process-error function)
  (sb-ext:process-alive-p function)
  (sb-ext:process-status function)
  (sb-ext:process-wait function)
  (sb-ext:process-exit-code function)
  (sb-ext:process-core-dumped function)
  (sb-ext:process-close function)
  (sb-ext:process-kill function))

(defsection @unicode-support (:title "Unicode Support")
  "SBCL provides support for working with Unicode text and querying the
  standard Unicode database for information about individual codepoints.
  Unicode-related functions are located in the `SB-UNICODE` package.

  SBCL also extends ANSI character literal syntax to support Unicode
  codepoints. You can either specify a character by its Unicode name,
  with spaces replaced by underscores if a unique name exists or by
  giving its hexadecimal codepoint preceded by a `\\U`, an optional
  `\\+`, and an arbitrary number of leading zeros. You may also input
  the character directly into your source code if it can be encoded in
  your file. If a character had an assigned name in Unicode 1.0 that
  was distinct from its current name, you may also use that name (with
  spaces replaced by underscores) to specify the character, unless the
  name is already associated with a codepoint in the latest Unicode
  standard (such as `\\BELL`).

  > _Note_: Please note that the codepoint `U+1F5CF` (Page) introduced
  > in Unicode 7.0 is named `UNICODE_PAGE`, since the name _Page_ is
  > required to be assigned to form-feed (`U+0C`) by the ANSI
  > standard.

  For example, you can specify the codepoint `U+00E1` ( _Latin Small
  Letter A With Acute_) as

  - `#\\LATIN_SMALL_LETTER_A_WITH_ACUTE`
  - `#\\LATIN_SMALL_LETTER_A_ACUTE`
  - `#\\á` (assuming a Unicode source file)
  - `#\\U00E1`
  - `#\\UE1`
  - `#\\U+00E1`"
  (@unicode-property-access section)
  (@string-operations section)
  (@breaking-strings section))

(defsection @unicode-property-access (:title "Unicode property access")
  "The following functions can be used to find information about a
  Unicode codepoint."
  (sb-unicode:general-category function)
  (sb-unicode:bidi-class function)
  (sb-unicode:combining-class function)
  (sb-unicode:decimal-value function)
  (sb-unicode:digit-value function)
  (sb-unicode:numeric-value function)
  (sb-unicode:mirrored-p function)
  (sb-unicode:bidi-mirroring-glyph function)
  (sb-unicode:age function)
  (sb-unicode:hangul-syllable-type function)
  (sb-unicode:east-asian-width function)
  (sb-unicode:script function)
  (sb-unicode:char-block function)
  (sb-unicode:unicode-1-name function)
  (sb-unicode:proplist-p function)
  (sb-unicode:uppercase-p function)
  (sb-unicode:lowercase-p function)
  (sb-unicode:cased-p function)
  (sb-unicode:case-ignorable-p function)
  (sb-unicode:alphabetic-p function)
  (sb-unicode:ideographic-p function)
  (sb-unicode:math-p function)
  (sb-unicode:whitespace-p function)
  (sb-unicode:soft-dotted-p function)
  (sb-unicode:hex-digit-p function)
  (sb-unicode:default-ignorable-p function)
  (sb-unicode:grapheme-break-class function)
  (sb-unicode:word-break-class function)
  (sb-unicode:sentence-break-class function)
  (sb-unicode:line-break-class function))

(defsection @string-operations (:title "String operations")
  "SBCL can normalize strings using:"
  (sb-unicode:normalize-string function)
  (sb-unicode:normalized-p function)
  "SBCL implements the full range of Unicode case operations with the
  functions"
  (sb-unicode:uppercase function)
  (sb-unicode:lowercase function)
  (sb-unicode:titlecase function)
  (sb-unicode:casefold function)
  "It also extends standard Common Lisp case functions such as
  STRING-UPCASE and STRING-DOWNCASE to support a subset of Unicode's
  casing behavior. Specifically, a character is BOTH-CASE-P if its
  case mapping in Unicode is one-to-one and invertable.

  The `SB-UNICODE` package also provides functions for
  collating/sorting strings according to the Unicode Collation
  Algorithm."
  (sb-unicode:unicode< function)
  (sb-unicode:unicode= function)
  (sb-unicode:unicode-equal function)
  (sb-unicode:unicode<= function)
  (sb-unicode:unicode> function)
  (sb-unicode:unicode>= function)
  "The following functions are provided for detecting visually
  confusable strings:"
  (sb-unicode:confusable-p function))

(defsection @breaking-strings (:title "Breaking strings")
  "The `SB-UNICODE` package includes several functions for breaking a
  Unicode string into useful parts."
  (sb-unicode:graphemes function)
  (sb-unicode:words function)
  (sb-unicode:sentences function)
  (sb-unicode:lines function))

(defsection @customization-hooks-for-users
    (:title "Customization Hooks for Users")
  "The toplevel repl prompt may be customized, and the function
  that reads user input may be replaced completely. See the :TOPLEVEL
  argument of SB-EXT:SAVE-LISP-AND-DIE.

  The behaviour of REQUIRE when called with only one argument is
  implementation-defined. In SBCL, REQUIRE behaves in the following
  way:"
  (require function)
  (sb-ext:*module-provider-functions* variable)
  "Although SBCL does not provide a resident editor, the ED
  function can be customized to hook into user-provided editing
  mechanisms as follows:"
  (ed function)
  (sb-ext:*ed-functions* variable)
  "Conditions of type WARNING and STYLE-WARNING are sometimes signaled at
  runtime, especially during execution of Common Lisp defining forms
  such as DEFUN, DEFMETHOD, etc. To muffle these warnings at runtime,
  SBCL provides a variable SB-EXT:*MUFFLED-WARNINGS*:"
  (sb-ext:*muffled-warnings* variable))

(defsection @tools-to-help-developers (:title "Tools To Help Developers")
  "SBCL provides a profiler and other extensions to the TRACE
  facility.

  The debugger supports a number of options. Its documentation is
  accessed by typing `\\help` at the debugger prompt. See @DEBUGGER.

  Documentation for the command `\\inspect` is accessed by typing
  `\\help` at the `\\inspect` prompt.")

(defsection @resolution-of-name-conflicts
    (:title "Resolution of Name Conflicts")
  "CLHS `11.1.1.2.5` requires that name conflicts in packages be
  resolvable in favour of any of the conflicting symbols. In the
  interactive debugger, this is achieved by prompting for the symbol
  in whose favour the conflict should be resolved; for programmatic
  use, the SB-EXT:RESOLVE-CONFLICT restart should be invoked
  with one argument, which should be a member of the list returned by
  the condition accessor SB-EXT:NAME-CONFLICT-SYMBOLS.")

(defsection @hash-table-extensions (:title "Hash Table Extensions")
  "Hash table extensions supported by SBCL are all controlled by keyword
  arguments to MAKE-HASH-TABLE."
  (make-hash-table function)
  (sb-ext:define-hash-table-test macro)
  (sb-ext:with-locked-hash-table macro)
  (sb-ext:hash-table-synchronized-p function)
  (sb-ext:hash-table-weakness function))

(defsection @random-number-generation (:title "Random Number Generation")
  "The initial value of *RANDOM-STATE* is the same each time SBCL
  is started. This makes it possible for user code to obtain
  repeatable pseudo random numbers using only standard-provided
  functionality. See SB-EXT:SEED-RANDOM-STATE below for an SBCL
  extension that allows to seed the random number generator from given
  data for an additional possibility to achieve this. Non-repeatable
  random numbers can always be obtained using (MAKE-RANDOM-STATE T).

  The sequence of numbers produced by repeated calls to RANDOM
  starting with the same random state and using the same sequence of
  `LIMIT` arguments is guaranteed to be reproducible only in the same
  version of SBCL on the same platform, using the same code under the
  same evaluator mode and compiler optimization qualities. Just two
  examples of differences that may occur otherwise: calls to RANDOM
  can be compiled differently depending on how much is known about the
  `LIMIT` argument at compile time, yielding different results even if
  called with the same argument at run time, and the results can
  differ depending on the machine's word size, for example for limits
  that are fixnums under 64-bit word size but bignums under 32-bit
  word size."
  (sb-ext:seed-random-state function)
  "Some notes on random floats: The standard doesn't prescribe a specific
  method of generating random floats. The following paragraph
  describes SBCL's current implementation and should be taken as
  purely informational, that is, user code should not depend on any of
  its specific properties. The method used has been chosen because it
  is common, conceptually simple and fast.

  To generate random floats, SBCL evaluates code that has an equivalent
  effect as

      (* limit
         (float (/ (random (expt 2 23)) (expt 2 23)) 1.0f0))

  (for SINGLE-FLOATs) and correspondingly (with `52` and `1.0d0`
  instead of `23` and `1.0f0`) for DOUBLE-FLOATs. Note especially that
  this means that zero is a possible return value occurring with
  probability `(EXPT 2 -23)` and `(EXPT 2 -52)`, respectively. Also
  note that there exist twice as many equidistant floats between 0 and
  1 as are generated. For example, the largest number that
  `(RANDOM 1.0F0)` ever returns is `(FLOAT (/ (1- (EXPT 2 23)) (EXPT 2
  23)) 1.0F0)` while `(FLOAT (/ (1- (EXPT 2 24)) (EXPT 2 24)) 1.0F0)`
  is the largest SINGLE-FLOAT less than 1. This is a side effect of
  the fact that the implementation uses the fastest possible
  conversion from bits to floats.

  SBCL currently uses the Mersenne Twister as its random number
  generator, specifically the 32-bit version under both 32- and 64-bit
  word size. The seeding algorithm has been improved several times by
  the authors of the Mersenne Twister; SBCL uses the third version
  (from 2002), which is still the most recent as of June 2012. The
  implementation has been tested to provide output identical to the
  recommended \\C implementation.

  While the Mersenne Twister generates random numbers of much better
  statistical quality than other widely used generators, it uses only
  linear operations modulo 2 and thus fails some statistical
  tests.

  (See chapter 7 _Testing widely used RNGs_ in _TestU01: A C Library
  for Empirical Testing of Random Number Generators_ by Pierre
  L'Ecuyer and Richard Simard, ACM Transactions on Mathematical
  Software, Vol. 33, article 22, 2007.)

  For example, the distribution of ranks of (sufficiently large)
  random binary matrices is much distorted compared to the
  theoretically expected one when the matrices are generated by the
  Mersenne Twister. Thus, applications that are sensitive to this
  aspect should use a different type of generator.")

(defsection @timeouts-and-deadlines (:title "Timeouts and Deadlines")
  "SBCL supports three different ways of restricting the execution time
  available to individual operations or parts of computations:

  - _Timeout Parameters_: Some operations such as thread
    synchronization primitives accept a :TIMEOUT parameter. See
    @TIMEOUT-PARAMETERS.

  - _Synchronous Timeouts (Deadlines)_: Certain operations that may
    suspend execution for extended periods of time such as CL:SLEEP,
    thread synchronization primitives, IO and waiting for external
    processes respect deadlines established for a part of a
    computation. See @SYNCHRONOUS-TIMEOUTS.

  - _Asynchronous Timeouts_: Asynchronous timeouts can interrupt most
    computations at (almost) any point. Thus, this kind of timeouts is
    the most versatile but it is also somewhat unsafe. See
    @ASYNCHRONOUS-TIMEOUTS."
  (@timeout-parameters section)
  (@synchronous-timeouts section)
  (@asynchronous-timeouts section)
  (@operations-supporting-timeouts-and-deadlines section))

(defsection @timeout-parameters (:title "Timeout Parameters")
  "Certain operations accept :TIMEOUT keyword arguments. These only
  affect the specific operation and must be specified at each call
  site by passing a :TIMEOUT keyword argument and a corresponding
  timeout value to the respective operation. Expiration of the timeout
  before the operation completes results in either a normal return
  with a return value indicating the timeout or in the signaling of a
  specialized condition such as SB-THREAD:JOIN-THREAD-ERROR.

  Example:

      (defun join-thread-within-5-seconds (thread)
        (multiple-value-bind (value result)
            (sb-thread:join-thread thread :default nil :timeout 5)
          (when (eq result :timeout)
            (error \"Could not join ~A within 5 seconds\" thread))
          value))

  The above code attempts to join the specified thread for up to five
  seconds, returning its value in case of success. If the thread is
  still running after the five seconds have elapsed,
  SB-THREAD:JOIN-THREAD indicates the timeout in its second return
  value. If a :DEFAULT value was not provided, SB-THREAD:JOIN-THREAD
  would signal a SB-THREAD:JOIN-THREAD-ERROR instead.

  To wait for an arbitrary condition, optionally with a timeout, the
  SB-EXT:WAIT-FOR macro can be used:"
  (sb-ext:wait-for macro)
  ;; SB-SYS:MAKE-FD-STREAM also takes a :TIMEOUT} argument resulting
  ;; in @code{sb-sys:io-timeout}, but that seems too niche to document
  ;; here.
  )

(defsection @synchronous-timeouts (:title "Synchronous Timeouts")
  "Deadlines, in contrast to timeout parameters, are established for a
  dynamic scope using the SB-SYS:WITH-DEADLINE macro and indirectly
  affect operations within that scope. In case of nested uses, the
  effective deadline is the one that expires first unless an inner use
  explicitly overrides outer deadlines."
  (sb-sys:with-deadline macro)
  "Expiration of deadlines set up this way only has an effect when it
  happens before or during the execution of a deadline-aware operation
  (@OPERATIONS-SUPPORTING-TIMEOUTS-AND-DEADLINES). In this case, a
  SB-SYS:DEADLINE-TIMEOUT is signaled. A handler for this condition
  type may use the SB-SYS:DEFER-DEADLINE or SB-SYS:CANCEL-DEADLINE
  restarts to defer or cancel the deadline respectively and resume
  execution of the interrupted operation."
  (sb-sys:deadline-timeout condition)
  (sb-sys:defer-deadline function)
  (sb-sys:cancel-deadline function)
  "When a thread is executing the debugger, signaling of
  SB-SYS:DEADLINE-TIMEOUT conditions for that thread is deferred until
  it exits the debugger.

  Example:

      (defun read-input ()
        (list (read-line) (read-line)))

      (defun do-it ()
        (sb-sys:with-deadline (:seconds 5))
          (read-input)
          (sleep 2)
          (sb-ext:run-program \"my-program\"))

  The above code establishes a deadline of five seconds within which
  the body of the `DO-IT` function should execute. All calls of
  deadline-aware functions in the dynamic scope, in this case two
  READ-LINE calls, a SLEEP call and a SB-EXT:RUN-PROGRAM call, are
  affected by the deadline. If, for example, the first READ-LINE call
  completes in one second and the second READ-LINE call completes in
  three seconds, a SB-SYS:DEADLINE-TIMEOUT condition will be signaled
  after the SLEEP call has been executing for one second.")

(defsection @asynchronous-timeouts (:title "Asynchronous Timeouts")
  "Asynchronous timeouts are established for a dynamic scope using the
  SB-EXT:WITH-TIMEOUT macro:"
  (sb-ext:with-timeout macro)
  "Expiration of the timeout will cause the operation being executed at
  that moment to be interrupted by an asynchronously signaled
  SB-EXT:TIMEOUT condition, (almost) irregardless of the operation
  and its context."
  (sb-ext:timeout condition))

(defsection @operations-supporting-timeouts-and-deadlines
    (:title "Operations Supporting Timeouts and Deadlines")
  ;; FIXME: make this conditional on texinfo output
  #+nil
  "@multitable @columnfractions .5 .25 .25
  @headitem Operation                                            @tab Timeout Parameter @tab Affected by Deadlines
  -     @code{cl:sleep}                                      @tab -                 @tab since SBCL 1.4.3
  -     @code{cl:read-line}, etc.                            @tab no                @tab yes
  -     @ref{Macro sb-ext wait-for,,@code{wait-for}}@:  @tab yes               @tab yes
  -     @ref{Function sb-ext process-wait,,@code{process-wait}}@:                   @tab no                @tab yes
  -     @ref{Function sb-thread grab-mutex,,@code{grab-mutex}}@:                  @tab yes               @tab yes
  -     @ref{Function sb-thread condition-wait,,@code{condition-wait}}@:              @tab yes               @tab yes
  -     @ref{Function sb-thread wait-on-semaphore,,@code{wait-on-semaphore}}@:           @tab yes               @tab yes
  -     @ref{Function sb-thread join-thread,,@code{join-thread}}@:                 @tab yes               @tab yes
  -     @ref{Function sb-concurrency receive-message,,@code{receive-message}}@:        @tab yes               @tab yes?
  -     @ref{Function sb-concurrency wait-on-gate,,@code{wait-on-gate}}@:           @tab yes               @tab yes?
  -     @ref{Macro sb-concurrency frlock-write,,@code{frlock-write}}@:              @tab yes               @tab yes?
  -     @ref{Function sb-concurrency grab-frlock-write-lock,,@code{grab-frlock-write-lock}}@: @tab yes               @tab yes?
  @end multitable"
  "```
  | Operation              | Timeout parameter | Affected by deadlines |
  |------------------------+-------------------+-----------------------|
  | cl:sleep               | -                 | since SBCL 1.4.3      |
  | cl:read-line, etc.     | no                | yes                   |
  | wait-for               | yes               | yes                   |
  | process-wait           | no                | yes                   |
  | grab-mutex             | yes               | yes                   |
  | condition-wait         | yes               | yes                   |
  | wait-on-semaphore      | yes               | yes                   |
  | join-thread            | yes               | yes                   |
  | receive-message        | yes               | yes?                  |
  | wait-on-gate           | yes               | yes?                  |
  | frlock-write           | yes               | yes?                  |
  | grab-frlock-write-lock | yes               | yes?                  |
  ```")

(defsection @miscellaneous-extensions (:title "Miscellaneous Extensions")
  (sb-ext:array-storage-vector function)
  (sb-ext:delete-directory function)
  (sb-ext:get-time-of-day function)
  (sb-ext:assert-version->= function)
  (sb-ext:unencapsulated-function function)
  (documentation generic-function))

(defsection @stale-extensions (:title "Stale Extensions")
  "SBCL has inherited from CMUCL various hooks to allow the user to
  tweak and monitor the garbage collection process. These are somewhat
  stale code, and their interface might need to be cleaned up. If you
  have urgent need of them, look at the code in `src/code/gc.lisp` and
  bring it up on the developers' mailing list.

  SBCL has various hooks inherited from CMUCL, like
  SB-EXT:FLOAT-DENORMALIZED-P, to allow a program to take advantage of
  IEEE floating point arithmetic properties which aren't conveniently
  or efficiently expressible using the ANSI standard. These look good,
  and their interface looks good, but IEEE support is slightly broken
  due to a stupid decision to remove some support for infinities
  (because it wasn't in the ANSI spec and it didn't occur to me that
  it was in the IEEE spec). If you need this stuff, take a look at the
  code and bring it up on the developers' mailing list.")

(defsection @efficiency-hacks (:title "Efficiency Hacks")
  "The SB-EXT:PURIFY function (available when `#+cheneygc`) causes
  SBCL first to collect all garbage, then to mark all uncollected
  objects as permanent, never again attempting to collect them as
  garbage. This can cause a large increase in efficiency when using a
  primitive garbage collector, or a more moderate increase in
  efficiency when using a more sophisticated garbage collector which
  is well suited to the program's memory usage pattern. It also allows
  permanent code to be frozen at fixed addresses, a precondition for
  using copy-on-write to share code between multiple Lisp processes.
  This is less important with modern generational garbage collectors,
  but not all SBCL platforms use such a garbage collector.

  The SB-EXT:TRULY-THE special form declares the type of the result of
  the operations, producing its argument; the declaration is not
  checked. In short: don't use it."
  ;; FIXME: It's not a macro.
  #+nil
  (sb-ext:truly-the macro)
  "The SB-EXT:FREEZE-TYPE declaration declares that a type will never
  change, which can make type testing (e.g. with TYPEP) more efficient
  for structure types.")
