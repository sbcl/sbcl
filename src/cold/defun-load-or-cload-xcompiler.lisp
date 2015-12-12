;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

(defparameter *full-calls-to-warn-about*
  '(;mask-signed-field ;; Too many to fix
    ))

;;; Set of function names whose definition will never be seen in make-host-2,
;;; as they are deferred until warm load.
;;; The table is populated later in this file.
(defparameter *undefined-fun-whitelist* (make-hash-table :test 'equal))

(export '*symbol-values-for-genesis*)
(let ((pathname "output/init-symbol-values.lisp-expr"))
  (defvar *symbol-values-for-genesis*
    (and (probe-file pathname) (read-from-file pathname)))
  (defun save-initial-symbol-values ()
    (with-open-file (f pathname :direction :output :if-exists :supersede)
      (declare (special *symbol-values-for-genesis*)) ; non-toplevel DEFVAR
      (write *symbol-values-for-genesis* :stream f :readably t))))

;;; Either load or compile-then-load the cross-compiler into the
;;; cross-compilation host Common Lisp.
(defun load-or-cload-xcompiler (load-or-cload-stem)

  (declare (type function load-or-cload-stem))

  ;; The running-in-the-host-Lisp Python cross-compiler defines its
  ;; own versions of a number of functions which should not overwrite
  ;; host-Lisp functions. Instead we put them in a special package.
  ;;
  ;; The common theme of the functions, macros, constants, and so
  ;; forth in this package is that they run in the host and affect the
  ;; compilation of the target.
  (let ((package-name "SB-XC"))
    (make-package package-name :use nil :nicknames nil)
    (dolist (name '(;; the constants (except for T and NIL which have
                    ;; a specially hacked correspondence between
                    ;; cross-compilation host Lisp and target Lisp)
                    "ARRAY-DIMENSION-LIMIT"
                    "ARRAY-RANK-LIMIT"
                    "ARRAY-TOTAL-SIZE-LIMIT"
                    "BOOLE-1"
                    "BOOLE-2"
                    "BOOLE-AND"
                    "BOOLE-ANDC1"
                    "BOOLE-ANDC2"
                    "BOOLE-C1"
                    "BOOLE-C2"
                    "BOOLE-CLR"
                    "BOOLE-EQV"
                    "BOOLE-IOR"
                    "BOOLE-NAND"
                    "BOOLE-NOR"
                    "BOOLE-ORC1"
                    "BOOLE-ORC2"
                    "BOOLE-SET"
                    "BOOLE-XOR"
                    "CALL-ARGUMENTS-LIMIT"
                    "CHAR-CODE-LIMIT"
                    "DEFMETHOD"
                    "DOUBLE-FLOAT-EPSILON"
                    "DOUBLE-FLOAT-NEGATIVE-EPSILON"
                    "INTERNAL-TIME-UNITS-PER-SECOND"
                    "LAMBDA-LIST-KEYWORDS"
                    "LAMBDA-PARAMETERS-LIMIT"
                    "LEAST-NEGATIVE-DOUBLE-FLOAT"
                    "LEAST-NEGATIVE-LONG-FLOAT"
                    "LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT"
                    "LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT"
                    "LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT"
                    "LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT"
                    "LEAST-NEGATIVE-SHORT-FLOAT"
                    "LEAST-NEGATIVE-SINGLE-FLOAT"
                    "LEAST-POSITIVE-DOUBLE-FLOAT"
                    "LEAST-POSITIVE-LONG-FLOAT"
                    "LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT"
                    "LEAST-POSITIVE-NORMALIZED-LONG-FLOAT"
                    "LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT"
                    "LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT"
                    "LEAST-POSITIVE-SHORT-FLOAT"
                    "LEAST-POSITIVE-SINGLE-FLOAT"
                    "LONG-FLOAT-EPSILON"
                    "LONG-FLOAT-NEGATIVE-EPSILON"
                    "MOST-NEGATIVE-DOUBLE-FLOAT"
                    "MOST-NEGATIVE-FIXNUM"
                    "MOST-NEGATIVE-LONG-FLOAT"
                    "MOST-NEGATIVE-SHORT-FLOAT"
                    "MOST-NEGATIVE-SINGLE-FLOAT"
                    "MOST-POSITIVE-DOUBLE-FLOAT"
                    "MOST-POSITIVE-FIXNUM"
                    "MOST-POSITIVE-LONG-FLOAT"
                    "MOST-POSITIVE-SHORT-FLOAT"
                    "MOST-POSITIVE-SINGLE-FLOAT"
                    "MULTIPLE-VALUES-LIMIT"
                    "PI"
                    "SHORT-FLOAT-EPSILON"
                    "SHORT-FLOAT-NEGATIVE-EPSILON"
                    "SINGLE-FLOAT-EPSILON"
                    "SINGLE-FLOAT-NEGATIVE-EPSILON"

                    ;; everything else which needs a separate
                    ;; existence in xc and target
                    "BOOLE"
                    "BOOLE-CLR" "BOOLE-SET" "BOOLE-1" "BOOLE-2"
                    "BOOLE-C1" "BOOLE-C2" "BOOLE-AND" "BOOLE-IOR"
                    "BOOLE-XOR" "BOOLE-EQV" "BOOLE-NAND" "BOOLE-NOR"
                    "BOOLE-ANDC1" "BOOLE-ANDC2" "BOOLE-ORC1" "BOOLE-ORC2"
                    "BUILT-IN-CLASS"
                    "BYTE" "BYTE-POSITION" "BYTE-SIZE"
                    "CHAR-CODE"
                    "CLASS" "CLASS-NAME" "CLASS-OF"
                    "CODE-CHAR"
                    "COMPILE-FILE"
                    "COMPILE-FILE-PATHNAME"
                    "*COMPILE-FILE-PATHNAME*"
                    "*COMPILE-FILE-TRUENAME*"
                    "*COMPILE-PRINT*"
                    "*COMPILE-VERBOSE*"
                    "COMPILER-MACRO-FUNCTION"
                    "CONSTANTP"
                    "DEFCONSTANT"
                    "DEFINE-MODIFY-MACRO"
                    "DEFINE-SETF-EXPANDER"
                    "DEFMACRO" "DEFSETF" "DEFSTRUCT" "DEFTYPE"
                    "DEPOSIT-FIELD" "DPB"
                    "FBOUNDP" "FDEFINITION" "FMAKUNBOUND"
                    "FIND-CLASS"
                    "GENSYM" "*GENSYM-COUNTER*"
                    "GET-SETF-EXPANSION"
                    "LDB" "LDB-TEST"
                    "LISP-IMPLEMENTATION-TYPE" "LISP-IMPLEMENTATION-VERSION"
                    "MACRO-FUNCTION"
                    "MACROEXPAND" "MACROEXPAND-1" "*MACROEXPAND-HOOK*"
                    "MAKE-LOAD-FORM"
                    "MAKE-LOAD-FORM-SAVING-SLOTS"
                    "MASK-FIELD"
                    "PROCLAIM"
                    "SPECIAL-OPERATOR-P"
                    "STANDARD-CLASS"
                    "STRUCTURE-CLASS"
                    "SUBTYPEP"
                    "TYPE-OF" "TYPEP"
                    "UPGRADED-ARRAY-ELEMENT-TYPE"
                    "UPGRADED-COMPLEX-PART-TYPE"
                    "WITH-COMPILATION-UNIT"))
      (export (intern name package-name) package-name)))
  ;; Symbols that can't be entered into the whitelist
  ;; until this function executes.
  (setf (gethash (intern "MAKE-LOAD-FORM" "SB-XC")
                 *undefined-fun-whitelist*) t)
  ;; don't watch:
  (dolist (package (list-all-packages))
    (when (= (mismatch (package-name package) "SB!") 3)
      (shadowing-import
       (mapcar (lambda (name) (find-symbol name "SB-XC"))
               '("BYTE" "BYTE-POSITION" "BYTE-SIZE"
                 "DPB" "LDB" "LDB-TEST"
                 "DEPOSIT-FIELD" "MASK-FIELD"

                 "BOOLE"
                 "BOOLE-CLR" "BOOLE-SET" "BOOLE-1" "BOOLE-2"
                 "BOOLE-C1" "BOOLE-C2" "BOOLE-AND" "BOOLE-IOR"
                 "BOOLE-XOR" "BOOLE-EQV" "BOOLE-NAND" "BOOLE-NOR"
                 "BOOLE-ANDC1" "BOOLE-ANDC2" "BOOLE-ORC1" "BOOLE-ORC2"))
       package)))

  ;; Build a version of Python to run in the host Common Lisp, to be
  ;; used only in cross-compilation.
  ;;
  ;; Note that files which are marked :ASSEM, to cause them to be
  ;; processed with SB!C:ASSEMBLE-FILE when we're running under the
  ;; cross-compiler or the target lisp, are still processed here, just
  ;; with the ordinary Lisp compiler, and this is intentional, in
  ;; order to make the compiler aware of the definitions of assembly
  ;; routines.
  (do-stems-and-flags (stem flags)
    (unless (find :not-host flags)
      (funcall load-or-cload-stem stem flags)
      #!+sb-show (warn-when-cl-snapshot-diff *cl-snapshot*)))

  ;; If the cross-compilation host is SBCL itself, we can use the
  ;; PURIFY extension to freeze everything in place, reducing the
  ;; amount of work done on future GCs. In machines with limited
  ;; memory, this could help, by reducing the amount of memory which
  ;; needs to be juggled in a full GC. And it can hardly hurt, since
  ;; (in the ordinary build procedure anyway) essentially everything
  ;; which is reachable at this point will remain reachable for the
  ;; entire run.
  ;;
  ;; (Except that purifying actually slows down GENCGC). -- JES, 2006-05-30
  #+(and sbcl (not gencgc))
  (sb-ext:purify)

  (values))

;; Keep these in order by package, then symbol.
(dolist (sym
         (append
          ;; CL, EXT, KERNEL
          '(allocate-instance
            compute-applicable-methods
            slot-makunbound
            sb!ext:run-program
            sb!kernel:profile-deinit)
          ;; CLOS implementation
          '(sb!mop:class-finalized-p
            sb!mop:class-prototype
            sb!mop:eql-specializer-object
            sb!mop:finalize-inheritance
            sb!pcl::%force-cache-flushes
            sb!pcl::check-wrapper-validity
            sb!pcl::class-has-a-forward-referenced-superclass-p
            sb!pcl::class-wrapper
            sb!pcl::compute-gf-ftype
            sb!pcl::definition-source
            sb!pcl:ensure-class-finalized
            sb!pcl::get-instance-hash-code)
          ;; CLOS-based packages
          '(sb!gray:stream-clear-input
            sb!gray:stream-clear-output
            sb!gray:stream-file-position
            sb!gray:stream-finish-output
            sb!gray:stream-force-output
            sb!gray:stream-fresh-line
            sb!gray:stream-line-column
            sb!gray:stream-line-length
            sb!gray:stream-listen
            sb!gray:stream-peek-char
            sb!gray:stream-read-byte
            sb!gray:stream-read-char
            sb!gray:stream-read-char-no-hang
            sb!gray:stream-read-line
            sb!gray:stream-read-sequence
            sb!gray:stream-terpri
            sb!gray:stream-unread-char
            sb!gray:stream-write-byte
            sb!gray:stream-write-char
            sb!gray:stream-write-sequence
            sb!gray:stream-write-string
            sb!sequence:concatenate
            sb!sequence:copy-seq
            sb!sequence:count
            sb!sequence:count-if
            sb!sequence:count-if-not
            sb!sequence:delete
            sb!sequence:delete-duplicates
            sb!sequence:delete-if
            sb!sequence:delete-if-not
            (setf sb!sequence:elt)
            sb!sequence:elt
            sb!sequence:emptyp
            sb!sequence:fill
            sb!sequence:find
            sb!sequence:find-if
            sb!sequence:find-if-not
            (setf sb!sequence:iterator-element)
            sb!sequence:iterator-endp
            sb!sequence:iterator-step
            sb!sequence:length
            sb!sequence:make-sequence-iterator
            sb!sequence:make-sequence-like
            sb!sequence:map
            sb!sequence:merge
            sb!sequence:mismatch
            sb!sequence:nreverse
            sb!sequence:nsubstitute
            sb!sequence:nsubstitute-if
            sb!sequence:nsubstitute-if-not
            sb!sequence:position
            sb!sequence:position-if
            sb!sequence:position-if-not
            sb!sequence:reduce
            sb!sequence:remove
            sb!sequence:remove-duplicates
            sb!sequence:remove-if
            sb!sequence:remove-if-not
            sb!sequence:replace
            sb!sequence:reverse
            sb!sequence:search
            sb!sequence:sort
            sb!sequence:stable-sort
            sb!sequence:subseq
            sb!sequence:substitute
            sb!sequence:substitute-if
            sb!sequence:substitute-if-not)
          ;; Fast interpreter
          #!+sb-fasteval
          '(sb!interpreter:%fun-type
            sb!interpreter:env-policy
            sb!interpreter:eval-in-environment
            sb!interpreter:find-lexical-fun
            sb!interpreter:find-lexical-var
            sb!interpreter::flush-everything
            sb!interpreter::fun-lexically-notinline-p
            sb!interpreter:lexenv-from-env
            sb!interpreter:list-locals
            sb!interpreter:prepare-for-compile
            sb!interpreter::reconstruct-syntactic-closure-env)
          ;; Other
          '(sb!debug::find-interrupted-name-and-frame
            sb!impl::encapsulate-generic-function
            sb!impl::encapsulated-generic-function-p
            sb!impl::get-processes-status-changes
            sb!impl::step-form
            sb!impl::step-values
            sb!impl::unencapsulate-generic-function)))
  (setf (gethash sym *undefined-fun-whitelist*) t))
