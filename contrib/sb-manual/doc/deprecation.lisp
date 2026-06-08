(in-package :sb-manual)

(defsection @deprecation (:title "Deprecation")
  "In order to support evolution of interfaces in SBCL as well as in user
  code, SBCL allows declaring functions, variables and types as
  deprecated. Users of deprecated things are notified by means of
  warnings while the deprecated thing in question is still available.

  This chapter documents the interfaces for being notified when using
  deprecated thing and declaring things as deprecated, the deprecation
  process used for SBCL interfaces, and lists legacy interfaces in
  various stages of deprecation.

  _Deprecation_ in this context should not be confused with those
  things the ANSI Common Lisp standard calls _deprecated_: the
  entirety of ANSI CL is supported by SBCL, and none of those
  interfaces are subject to censure."
  (@why-deprecate? section)
  (@the-deprecation-pipeline section)
  (@deprecation-conditions section)
  (@introspecting-deprecation-information section)
  (@deprecation-declaration section)
  (@deprecation-examples section)
  (@deprecated-interfaces-in-sbcl section))

(defsection @why-deprecate? (:title "Why Deprecate?")
  "While generally speaking we try to keep SBCL changes as backwards
  compatible as feasible, there are situations when existing interfaces
  are deprecated:

  - __Broken Interfaces__

      Sometimes it turns out that an interface is sufficiently
      misdesigned that fixing it would be worse than deprecating it
      and replacing it with another.

      This is typically the case when fixing the interface would
      change its semantics in ways that could break user code subtly:
      in such cases we may end up considering the obvious breakage
      caused by deprecation to be preferable.

      Another example are functions or macros whose current signature
      makes them hard or impossible to extend in the future: backwards
      compatible extensions would either make the interface
      intolerably hairy, or are sometimes outright impossible.

  - __Internal Interfaces__

      SBCL has several internal interfaces that were never meant to be
      used in user code -- or at least never meant to be used in user
      code unwilling to track changes to SBCL internals.

      Ideally, we'd like to be free to refactor our own internals as
      we please, without even going through the hassle of deprecating
      things. Sometimes, however, it turns out that our internal
      interfaces have several external users who aren't using them
      advisedly, but due to misunderstandings regarding their status
      or stability.

      Consider a deprecated internal interface a reminder for SBCL
      maintainers not to delete the thing just yet, even though it is
      seems unused -- because it has external users.

      When internal interfaces are deprecated we try our best to
      provide supported alternatives.

  - __Aesthetics & Ease of Maintenance__

      Sometimes an interface isn't broken or internal but just
      inconsistent somehow.

      This mostly happens only with historical interfaces inherited
      from CMUCL which often haven't been officially supported in SBCL
      before, or with new extensions to SBCL that haven't been around
      for very long in the first place.

      The alternative would be to keep the suboptimal version around
      forever, possibly alongside an improved version. Sometimes we
      may do just that, but because every line of code comes with a
      maintenance cost, sometimes we opt to deprecate the suboptimal
      version instead: SBCL doesn't have infinite developer resources.

      We also believe that sometimes cleaning out legacy interfaces
      helps keep the whole system more comprehensible to users, and
      makes introspective tools such as APROPOS more useful.")

(defsection @the-deprecation-pipeline (:title "The Deprecation Pipeline")
  "SBCL uses a _deprecation pipeline_ with multiplestages: as
  time time goes by, deprecated things move from earlier stages of
  deprecation to later stages before finally being removed. The
  intention is making users aware of necessary changes early but
  allowing a migration to new interfaces at a reasonable pace.

  Deprecation proceeds in three stages, each lasting approximately a
  year. In some cases it might move slower or faster, but one year per
  stage is what we aim at in general. During each stage warnings (and
  errors) of increasing severity are signaled, which note that the
  interface is deprecated, and point users towards any replacements
  when applicable.

  - __Early Deprecation__

      During early deprecation the interface is kept in working
      condition. However, when a thing in this deprecation stage is
      used, an SB-EXT:EARLY-DEPRECATION-WARNING, which is a
      STYLE-WARNING, is signaled at compile-time.

      The internals may change at this stage: typically because the
      interface is re-implemented on top of its successor. While we
      try to keep things as backwards-compatible as feasible (taking
      maintenance costs into account), sometimes semantics change
      slightly.

      For example, when the spinlock API was deprecated, spinlock
      objects ceased to exist, and the whole spinlock API became a
      synonym for the mutex API -- so code using the spinlock API
      continued working but silently switched to mutexes instead.
      However, if someone relied on

          (typep lock 'spinlock)

      returning NIL for a mutexes, trouble could ensue.

  - __Late Deprecation__

      During late deprecation the interface remains as it was during
      early deprecation, but the compile-time warning is upgraded:
      when a thing in this deprecation stage is used, a
      SB-EXT:LATE-DEPRECATION-WARNING, which is a full WARNING, is
      signaled at compile-time.

  - __Final Deprecation__

      During final deprecation the symbols still exist. However, when
      a thing in this deprecation stage is used, a
      SB-EXT:FINAL-DEPRECATION-WARNING, which is a full WARNING, is
      signaled at compile-time and an ERROR is signaled at run-time.

  - __After Final Deprecation__

      The interface is deleted entirely.")

(defsection @deprecation-conditions (:title "Deprecation Conditions")
  "SB-EXT:DEPRECATION-CONDITION is the superclass of all
  deprecation-related warning and error conditions. All common slots and
  readers are defined in this condition class."
  (sb-ext:deprecation-condition condition)
  (sb-ext:early-deprecation-warning condition)
  (sb-ext:late-deprecation-warning condition)
  (sb-ext:final-deprecation-warning condition)
  (sb-ext:deprecation-error condition))

(defsection @introspecting-deprecation-information
    (:title "Introspecting Deprecation Information")
  "The deprecation status of functions and variables can be inspected
  using the SB-CLTL2:FUNCTION-INFORMATION and
  SB-CLTL2:VARIABLE-INFORMATION functions provided by the `SB-CLTL2`
  contributed module.")

(defsection @deprecation-declaration (:title "Deprecation Declaration")
  "The SB-EXT:DEPRECATED declaration can be used to declare objects
  in various namespaces as deprecated.

  > _Note_: See the `namespace` CLHS glossary entry in the glossary of
  > the Common Lisp Hyperspec.)"
  (sb-ext:deprecated declaration))

(defsection @deprecation-examples (:title "Deprecation Examples")
  "Marking functions as deprecated:

      (defun foo ())
      (defun bar ())
      (declaim (deprecated :early (\"my-system\" \"1.2.3\")
                           (function foo :replacement bar)))

      ;; Remember: do not define the actual function or variable in case of
      ;; :final deprecation:
      (declaim (deprecated :final (\"my-system\" \"1.2.3\")
                           (function fez :replacement whoop)))

  Attempting to use the deprecated functions:

      (defun baz ()
        (foo))
      | STYLE-WARNING: The function CL-USER::FOO has been deprecated...
      => BAZ
      (baz)
      => NIL ; no error

      (defun danger ()
        (fez))
      | WARNING: The function CL-USER::FEZ has been deprecated...
      => DANGER
      (danger)
      |- ERROR: The function CL-USER::FEZ has been deprecated...")


(defsection @deprecated-interfaces-in-sbcl
    (:title "Deprecated Interfaces in SBCL")
  "This sections lists legacy interfaces in various stages of deprecation."
  (@list-of-deprecated-interfaces section)
  (@historical-interfaces section))

(defsection @list-of-deprecated-interfaces
    (:title "List of Deprecated Interfaces")
  (@early-deprecation section)
  (@late-deprecation section)
  (@final-deprecation section))

(defsection @early-deprecation (:title "Early Deprecation")
  "- `SOCKINT::WIN32-*`

      Deprecated in favor of the corresponding prefix-less functions
      (e.g. `SOCKINT::BIND` replaces `SOCKINT::WIN32-BIND`) as of
      1.2.10 in March 2015. Expected to move into late deprecation in
      August 2015.

  - SB-UNIX:UNIX-EXIT

      Deprecated as of 1.0.56.55 in May 2012. Expected to move into
      late deprecation in May 2013.

      When the SBCL process termination was refactored,
      SB-UNIX:UNIX-EXIT ceased to be used internally. Since `SB-UNIX`
      is an internal package not intended for user code to use, and
      since we're slowly in the process of refactoring things to be
      less Unix-oriented, SB-UNIX:UNIX-EXIT was initially deleted as
      it was no longer used. Unfortunately it became apparent that it
      was used by several external users, so it was re-instated in
      deprecated form.

      While the cost of keeping SB-UNIX:UNIX-EXIT indefinitely is
      trivial, the ability to refactor our internals is important, so
      its deprecation was taken as an opportunity to highlight that
      `SB-UNIX` is an internal package and `SB-POSIX` should be used
      by user-programs instead -- or alternatively calling the foreign
      function directly if the desired interface doesn't for some
      reason exist in `SB-POSIX`.

      __Remedy__

      For code needing to work with legacy SBCLs, use e.g.
      `SYSTEM-EXIT`. In modern SBCLs, simply call either SB-POSIX:EXIT
      or SB-EXT:EXIT with appropriate arguments.

  - `SB-C::MERGE-TAIL-CALLS` compiler policy

      Deprecated as of 1.0.53.74 in November 2011. Expected to move
      into late deprecation in November 2012.

      This compiler policy was never functional: SBCL has always
      merged tail calls when it could, regardless of this policy
      setting. (It was also never officially supported, but several
      code-bases have historically used it.)

      __Remedy__

      Simply remove the policy declarations. They were never necessary: SBCL
      always merged tail-calls when possible. To disable tail merging,
      structure the code to avoid the tail position instead.

  - The Spinlock API

      Deprecated as of 1.0.53.11 in August 2011. Expected to move into
      late deprecation in August 2012.

      Spinlocks were an internal interface but had a number of
      external users and were hence deprecated instead of being simply
      deleted.

      Affected symbols: SB-THREAD::SPINLOCK, SB-THREAD::MAKE-SPINLOCK,
      SB-THREAD::WITH-SPINLOCK, SB-THREAD::WITH-RECURSIVE-SPINLOCK,
      SB-THREAD::GET-SPINLOCK, SB-THREAD::RELEASE-SPINLOCK,
      SB-THREAD::SPINLOCK-VALUE, and SB-THREAD::SPINLOCK-NAME.

      __Remedy__

      Use the mutex API instead, or implement spinlocks suiting your
      needs on top of SB-EXT:COMPARE-AND-SWAP, SB-EXT:SPIN-LOOP-HINT,
      etc.

  - `SOCKINT::HANDLE->FD`, `SOCKINT::FD->HANDLE`

      Internally deprecated in 2012. Declared deprecated as of 1.2.10
      in March 2015. Expected to move into final deprecation in August
      2015.")

(defsection @late-deprecation (:title "Late Deprecation")
  "- SB-THREAD:JOIN-THREAD-ERROR-THREAD and
    SB-THREAD:INTERRUPT-THREAD-ERROR-THREAD

      Deprecated in favor of SB-THREAD:THREAD-ERROR-THREAD as of
      1.0.29.17 in June 2009. Expected to move into final deprecation
      in June 2012.

      __Remedy__

      For code that needs to support legacy SBCLs, use e.g.:

          (defun get-thread-error-thread (condition)
            #+#.(cl:if (cl:find-symbol \"THREAD-ERROR-THREAD\" :sb-thread)
                       '(and) '(or))
            (sb-thread:thread-error-thread condition)
            #-#.(cl:if (cl:find-symbol \"THREAD-ERROR-THREAD\" :sb-thread)
                       '(and) '(or))
            (etypecase condition
             (sb-thread:join-thread-error
              (sb-thread:join-thread-error-thread condition))
             (sb-thread:interrupt-thread-error
              (sb-thread:interrupt-thread-error-thread condition))))

  - SB-INTROSPECT:FUNCTION-ARGLIST

      Deprecated in favor of SB-INTROSPECT:FUNCTION-LAMBDA-LIST as of
      1.0.24.5 in January 2009. Expected to move into final
      deprecation in January 2012.

      Renamed for consistency and aesthetics. Functions have
      lambda-lists, not arglists.

      __Remedy__

          For code that needs to support legacy SBCLs, use e.g.:

          (defun get-function-lambda-list (function)
            #+#.(cl:if (cl:find-symbol \"FUNCTION-LAMBDA-LIST\" :sb-introspect)
                       '(and) '(or))
            (sb-introspect:function-lambda-list function)
            #-#.(cl:if (cl:find-symbol \"FUNCTION-LAMBDA-LIST\" :sb-introspect)
                       '(and) '(or))
            (sb-introspect:function-arglist function))

  - Stack Allocation Policies

      Deprecated in favor of SB-EXT:*STACK-ALLOCATE-DYNAMIC-EXTENT* as
      of 1.0.19.7 in August 2008, and are expected to be removed in
      August 2012.

      Affected symbols: `SB-C::STACK-ALLOCATE-DYNAMIC-EXTENT`,
      `SB-C::STACK-ALLOCATE-VECTOR`, and
      `SB-C::STACK-ALLOCATE-VALUE-CELLS`.

      These compiler policies were never officially supported, and
      turned out the be a flawed design.

      __Remedy__

      For code that needs stack-allocation in legacy SBCLs,
      conditionalize using:

          #-#.(cl:if (cl:find-symbol \"*STACK-ALLOCATE-DYNAMIC-EXTENT*\" :sb-ext)
                     '(and) '(or))
          (declare (optimize sb-c::stack-allocate-dynamic-extent))

      However, unless stack allocation is essential, we recommend
      simply removing these declarations. Refer to documentation on
      `SB-EXT:*STACK-ALLOCATE-DYNAMIC*` for details on stack
      allocation control in modern SBCLs.

  - `SB-SYS:OUTPUT-RAW-BYTES`

      Deprecated as of 1.0.8.16 in June 2007. Expected to move into final
      deprecation in June 2012.

      Internal interface with some external users. Never officially
      supported, deemed unnecessary in presence of WRITE-SEQUENCE and
      bivalent streams.

      __Remedy__

      Use streams with element-type (UNSIGNED-BYTE 8) or
      :DEFAULT -- the latter allowing both binary and character IO --
      in conjunction with WRITE-SEQUENCE.")

(defsection @final-deprecation (:title "Final Deprecation")
  "No interfaces are currently in final deprecation.")

(defsection @historical-interfaces (:title "Historical Interfaces")
  "The following is a partial list of interfaces present in historical
  versions of SBCL, which have since then been deleted.

  - `SB-KERNEL:INSTANCE-LAMBDA`

      Historically needed for CLOS code. Deprecated as of 0.9.3.32 in
      August 2005. Deleted as of 1.0.47.8 in April 2011. Plain LAMBDA
      can be used where SB-KERNEL:INSTANCE-LAMBDA used to be needed.

  - `SB-ALIEN:DEF-ALIEN-ROUTINE`, `SB-ALIEN:DEF-ALIEN-VARIABLE`,
    `SB-ALIEN:DEF-ALIEN-TYPE`

      Inherited from CMUCL, naming convention not consistent with
      preferred SBCL style. Deprecated as of 0.pre7.90 in December
      2001. Deleted as of 1.0.9.17 in September 2007. Replaced by
      SB-ALIEN:DEFINE-ALIEN-ROUTINE, SB-ALIEN:DEFINE-ALIEN-VARIABLE,
      and SB-ALIEN:DEFINE-ALIEN-TYPE.")
