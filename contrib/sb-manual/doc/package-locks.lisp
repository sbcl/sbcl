(in-package :sb-manual)

(defsection @package-locks (:title "Package Locks")
  "None of the following sections apply to SBCL built without package
  locking support.

  The interface described here is experimental: incompatible changes
  in future SBCL releases are possible, even expected: the concept of
  _implementation packages_ and the associated operators may be
  renamed; more operations (such as naming restarts or catch tags) may
  be added to the list of operations violating package locks."
  (@package-lock-concepts section)
  (@package-lock-dictionary section))

(defsection @package-lock-concepts (:title "Package Lock Concepts")
  "Package locks protect against unintentional modifications of a package:
  they provide similar protection to user packages as is mandated to
  `COMMON-LISP` package by the ANSI specification. They are not, and
  should not be used as, a security measure.

  Newly created packages are by default unlocked (see the :LOCK option
  to DEFPACKAGE).

  The package `COMMON-LISP` and SBCL internal implementation packages
  are locked by default, including `SB-EXT`.

  It may be beneficial to lock `COMMON-LISP-USER` as well, to ensure
  that various libraries don't pollute it without asking, but this is
  not currently done by default."
  (@implementation-packages section)
  (@package-lock-violations section)
  (@package-locks-in-compiled-code section)
  (@operations-violating-package-locks section))

(defsection @implementation-packages (:title "Implementation Packages")
  "Each package has a list of associated implementation packages. A
  locked package, and the symbols whose home package it is, can be
  modified without violating package locks only when *PACKAGE* is
  bound to one of the implementation packages of the locked package.

  Unless explicitly altered by DEFPACKAGE,
  SB-EXT:ADD-IMPLEMENTATION-PACKAGE, or
  SB-EXT:REMOVE-IMPLEMENTATION-PACKAGE each package is its own
  (only) implementation package.")

(defsection @package-lock-violations (:title "Package Lock Violations")
  (@lexical-bindings-and-declarations section)
  (@other-operations section))

(defsection @lexical-bindings-and-declarations
    (:title "Lexical Bindings and Declarations")
  "Lexical bindings or declarations that violate package locks cause a
  compile-time warning, and a runtime PROGRAM-ERROR when the form that
  violates package locks would be executed.

  A complete listing of operators affect by this is: LET, LET*, FLET,
  LABELS, MACROLET, and SYMBOL-MACROLET, DECLARE.

  Package locks affecting both lexical bindings and declarations can
  be disabled locally with the SB-EXT:DISABLE-PACKAGE-LOCKS
  declaration, and re-enabled with the SB-EXT:ENABLE-PACKAGE-LOCKS
  declaration.

  Example:

      (in-package :locked)

      (defun foo () ...)

      (defmacro with-foo (&body body)
        `(locally (declare (disable-package-locks locked:foo))
           (flet ((foo () ...))
             (declare (enable-package-locks locked:foo)) ; re-enable for body
             ,@body)))")

(defsection @other-operations (:title "Other Operations")
  "If an non-lexical operation violates a package lock, a continuable
  error that is of a subtype of SB-EXT:PACKAGE-LOCK-VIOLATION
  (subtype of PACKAGE-ERROR) is signalled when the operation is
  attempted.

  Additional restarts may be established for continuable package lock
  violations for interactive use.

  The actual type of the error depends on circumstances that caused
  the violation: operations on packages signal errors of type
  SB-EXT:PACKAGE-LOCKED-ERROR, and operations on symbols signal errors
  of type SB-EXT:SYMBOL-PACKAGE-LOCKED-ERROR.")

(defsection @package-locks-in-compiled-code
    (:title "Package Locks in Compiled Code")
  "If file-compiled code contains interned symbols, then loading that
  code into an image without the said symbols will not cause a package
  lock violation, even if the packages in question are locked.

  With the exception of interned symbols, behaviour is unspecified if
  package locks affecting compiled code are not the same during
  loading of the code or execution.

  Specifically, code compiled with packages unlocked may or may not
  fail to signal package-lock-violations even if the packages are
  locked at runtime, and code compiled with packages locked may or may
  not signal spurious package-lock-violations at runtime even if the
  packages are unlocked.

  In practice all this means that package-locks have a negligible
  performance penalty in compiled code as long as they are not
  violated.")

(defsection @operations-violating-package-locks
    (:title "Operations Violating Package Locks")
  (@operations-on-packages section)
  (@operations-on-symbols section))

(defsection @operations-on-packages (:title "Operations on Packages")
  "The following actions cause a package lock violation if the package
  operated on is locked, and *PACKAGE* is not an implementation
  package of that package, and the action would cause a change in the
  state of the package (so e.g. exporting already external symbols is
  never a violation). Package lock violations caused by these
  operations signal errors of type SB-EXT:PACKAGE-LOCKED-ERROR.

  - Shadowing a symbol in a package.

  - Importing a symbol to a package.

  - Uninterning a symbol from a package.

  - Exporting a symbol from a package.

  - Unexporting a symbol from a package.

  - Changing the packages used by a package.

  - Renaming a package.

  - Deleting a package.

  - Adding a new package local nickname to a package.

  - Removing an existing package local nickname to a package.")

(defsection @operations-on-symbols (:title "Operations on Symbols")
  "Following actions cause a package lock violation if the home package
  of the symbol operated on is locked, and *PACKAGE* is not an
  implementation package of that package. Package lock violations
  caused by these action signal errors of type
  SB-EXT:SYMBOL-PACKAGE-LOCKED-ERROR.

  These actions cause only one package lock violation per lexically
  apparent violated package.

  Example:


      ;;; Packages FOO and BAR are locked.
      ;;;
      ;;; Two lexically apparent violated packages: exactly two
      ;;; package-locked-errors will be signalled.

      (defclass foo:point ()
        ((x :accessor bar:x)
         (y :accessor bar:y)))

  - Binding or altering its value lexically or dynamically, or
    establishing it as a symbol-macro.

      Exceptions:

      - If the symbol is not defined as a constant, global
        symbol-macro or a global dynamic variable, it may be lexically
        bound or established as a local symbol macro.

      - If the symbol is defined as a global dynamic variable, it may
        be assigned or bound.

  - Defining, undefining, or binding it, or its setf name as a
    function.

      Exceptions:

      - If the symbol is not defined as a function, macro, or special
        operator it and its setf name may be lexically bound as a
        function.

  - Defining, undefining, or binding it as a macro or compiler macro.

      Exceptions:

      - If the symbol is not defined as a function, macro, or special
        operator it may be lexically bound as a macro.

  - Defining it as a type specifier or structure.

  - Defining it as a declaration with a declaration proclamation.

  - Declaring or proclaiming it special.

  - Declaring or proclaiming its type or ftype.

      Exceptions:

      - If the symbol may be lexically bound, the type of that binding
        may be declared.

      - If the symbol may be lexically bound as a function, the ftype
        of that binding may be declared.

  - Defining a setf expander for it.

  - Defining it as a method combination type.

  - Using it as the CLASS-NAME argument to (SETF FIND-CLASS).

  - Defining it as a hash table test using SB-EXT:DEFINE-HASH-TABLE-TEST.")

(defsection @package-lock-dictionary (:title "Package Lock Dictionary")
  "- [__declaration__] SB-EXT:DISABLE-PACKAGE-LOCKS

      Syntax: `(SB-EXT:DISABLE-PACKAGE-LOCKS &REST SYMBOLS)`

      Disables package locks affecting the named symbols during
      compilation in the lexical scope of the declaration. Disabling
      locks on symbols whose home package is unlocked, or disabling an
      already disabled lock, has no effect.

  - [__declaration__] SB-EXT:ENABLE-PACKAGE-LOCKS

      Syntax: `(SB-EXT:ENABLE-PACKAGE-LOCKS &REST SYMBOLS)`

      Re-enables package locks affecting the named symbols during
      compilation in the lexical scope of the declaration. Enabling
      locks that were not first disabled with
      SB-EXT:DISABLE-PACKAGE-LOCKS declaration, or enabling locks that
      are already enabled has no effect."

  (sb-ext:package-lock-violation condition)
  (sb-ext:package-locked-error condition)
  (sb-ext:symbol-package-locked-error condition)
  (sb-ext:package-locked-error-symbol function)
  (sb-ext:package-locked-p function)
  (sb-ext:lock-package function)
  (sb-ext:unlock-package function)
  (sb-ext:package-implemented-by-list function)
  (sb-ext:package-implements-list function)
  (sb-ext:add-implementation-package function)
  (sb-ext:remove-implementation-package function)
  (sb-ext:without-package-locks macro)
  (sb-ext:with-unlocked-packages macro)

  "The DEFPACKAGE options are extended to include the following:

  - :LOCK `<boolean>` (defaults to NIL)

      If the argument to :LOCK is T, the package is locked, else it is
      unlocked. Existing package are also affected.

  - :IMPLEMENT `<package-designator>*`

      The package is added as an implementation package to the
      packages named. If :IMPLEMENT is not provided, it defaults to
      the package itself.

  Example:

      (defpackage \"FOO\" (:export \"BAR\") (:lock t) (:implement))
      (defpackage \"FOO-INT\" (:use \"FOO\") (:implement \"FOO\" \"FOO-INT\"))

      ;;; is equivalent to

      (defpackage \"FOO\") (:export \"BAR\"))
      (lock-package \"FOO\")
      (remove-implementation-package \"FOO\" \"FOO\")

      (defpackage \"FOO-INT\" (:use \"BAR\"))
      (add-implementation-package \"FOO-INT\" \"FOO\")")
