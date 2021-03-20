;;;; cross-compile-time-only replacements for miscellaneous unportable
;;;; stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Forward declarations

(defun make-system-hash-table (&rest args)
  (let ((args (copy-list args)))
    (remf args :weakness)
    (remf args :synchronized)
    (remf args :finalizer)
    (let ((hash-fun (getf args :hash-function)))
      (when hash-fun
        (assert (eq (getf args :test) 'eq))
        (remf args :hash-function)))
    (apply 'make-hash-table args)))

;;; In correct code, TRULY-THE has only a performance impact and can
;;; be safely degraded to ordinary THE.
(defmacro truly-the (type expr)
  `(the ,type ,expr))

(defmacro the* ((type &rest args) expr)
  (declare (ignore args))
  `(the ,type ,expr))

(defmacro named-lambda (name args &body body)
  (declare (ignore name))
  `#'(lambda ,args ,@body))

(defmacro define-thread-local (&rest rest) `(defvar ,@rest))

(defmacro defglobal (name value &rest doc)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,name
       (if (boundp ',name)
           (symbol-value ',name)
           ,value)
       ,@doc)))

(defmacro define-load-time-global (&rest args) `(defvar ,@args))

;;; Necessary only to placate the host compiler in %COMPILER-DEFGLOBAL.
(defun set-symbol-global-value (sym val)
  (error "Can't set symbol-global-value: ~S ~S" sym val))

;;; The GENESIS function works with fasl code which would, in the
;;; target SBCL, work on ANSI-STREAMs (streams which aren't extended
;;; Gray streams). In ANSI Common Lisp, an ANSI-STREAM is just a
;;; CL:STREAM.
(deftype ansi-stream () 'stream)

;;; In the target SBCL, the INSTANCE type refers to a base
;;; implementation for compound types with lowtag
;;; INSTANCE-POINTER-LOWTAG. There's no way to express exactly that
;;; concept portably, but we know that anything derived from STRUCTURE!OBJECT
;;; is equivalent to the target INSTANCE type. Also, because we use host packages
;;; as proxies for target packages, those too must satisfy our INSTANCEP
;;; - even if not a subtype of (OR STANDARD-OBJECT STRUCTURE-OBJECT).
;;; Nothing else satisfies this definition of INSTANCEP.
;;; As a guarantee that our set of host object types is exhaustive, we add one
;;; more constraint when self-hosted: host instances of unknown type cause failure.
;;; Some objects manipulated by the cross-compiler like the INTERVAL struct
;;; - which is not a STRUCTURE!OBJECT - should never be seen as literals in code.
;;; We assert that by way of the guard function.
#+host-quirks-sbcl
(defun unsatisfiable-instancep (x)
  (when (and (host-sb-kernel:%instancep x)
             (not (target-num-p x)))
    (bug "%INSTANCEP test on ~S" x)))
(deftype instance ()
  '(or structure!object package
    #+host-quirks-sbcl (and host-sb-kernel:instance ; optimizes out a call when false
                            (satisfies unsatisfiable-instancep))))
(defun %instancep (x)
  (typep x 'instance))

(deftype funcallable-instance ()
  (error "not clear how to represent FUNCALLABLE-INSTANCE type"))
(defun funcallable-instance-p (x)
  (error "Called FUNCALLABLE-INSTANCE-P ~s" x))

(defun simple-fun-p (x)
  (if (symbolp x) nil (error "Called SIMPLE-FUN-P on ~S" x)))
(defun closurep (x)
  (if (symbolp x) nil (error "Called CLOSUREP on ~S" x)))
(defun unbound-marker-p (x)
  (if (symbolp x) nil (error "Called UNBOUND-MARKER-P on ~S" x)))
(defun vector-with-fill-pointer-p (x)
  (if (symbolp x) nil (error "Called VECTOR-WITH-FILL-POINTER-P on ~S" x)))

(defparameter sb-vm::*backend-cross-foldable-predicates* nil)

;; The definition of TYPE-SPECIFIER for the target appears in the file
;; 'deftypes-for-target' - it allows CLASSes and CLASOIDs as specifiers.
;; Instances are never used as specifiers when building SBCL,
;; handily avoiding a problem in figuring out an order in which to
;; define the types CLASS, CLASSOID, and TYPE-SPECIFIER.
(deftype type-specifier () '(or list symbol))

;;; This seems to be the portable Common Lisp type test which
;;; corresponds to the effect of the target SBCL implementation test...
(defun array-header-p (x)
  (and (typep x 'array)
       (or (not (typep x 'simple-array))
           (/= (array-rank x) 1))))

(defvar sb-xc:*gensym-counter* 0)

(defun sb-xc:gensym (&optional (thing "G"))
  (declare (type string thing))
  (let ((n sb-xc:*gensym-counter*))
    (prog1
        (make-symbol (concatenate 'string thing (write-to-string n :base 10 :radix nil :pretty nil)))
      (incf sb-xc:*gensym-counter*))))

;;; These functions are needed for constant-folding.
(defun simple-array-nil-p (object)
  (when (typep object 'array)
    (assert (not (eq (array-element-type object) nil))))
  nil)

(defun data-vector-ref-with-offset (array index offset)
  (svref array (+ index offset)))

(defun data-vector-ref (array index)
  (svref array index))

(defun %negate (number)
  (sb-xc:- number))

(defun %single-float (number)
  (coerce number 'single-float))

(defun %double-float (number)
  (coerce number 'double-float))

(defun %ldb (size posn integer)
  (ldb (byte size posn) integer))

(defun %dpb (newbyte size posn integer)
  (dpb newbyte (byte size posn) integer))

(defun %with-array-data (array start end)
  (assert (typep array '(simple-array * (*))))
  (values array start end 0))

(defun %with-array-data/fp (array start end)
  (assert (typep array '(simple-array * (*))))
  (values array start end 0))

(defun make-value-cell (value)
  (declare (ignore value))
  (error "cross-compiler can not make value cells"))

;;; package-related stubs for the cross-compiler

(defun find-undeleted-package-or-lose (string)
  (or (find-package string)
      (error "Cross-compiler bug: no package named ~S" string)))

(defmacro without-package-locks (&body body)
  `(progn ,@body))

(defmacro with-single-package-locked-error ((&optional kind thing &rest format)
                                            &body body)
  (declare (ignore kind format))
  `(let ((.dummy. ,thing))
     (declare (ignore .dummy.))
     ,@body))

(defun program-assert-symbol-home-package-unlocked (context symbol control)
  (declare (ignore context control))
  symbol)

(defun assert-symbol-home-package-unlocked (name &optional format-control
                                            &rest format-arguments)
  (declare (ignore format-control format-arguments))
  name)

(declaim (declaration enable-package-locks disable-package-locks))

;; Nonstandard accessor for when you know you have a valid package in hand.
;; This avoids double lookup in *PACKAGE-NAMES* in a few places.
;; But portably we have to just fallback to PACKAGE-NAME.
(defun package-%name (x) (package-name x))

;;; This definition collapses SB-XC back into COMMON-LISP.
;;; Use CL:SYMBOL-PACKAGE if that's not the behavior you want.
;;; Notice that to determine whether a package is really supposed to be CL,
;;; we look for the symbol in the restricted lisp package, not the real
;;; host CL package. This works around situations where the host has *more*
;;; symbols exported from CL than should be.
(defun sb-xc:symbol-package (symbol)
  (let ((p (cl:symbol-package symbol))
        (name (string symbol)))
    (if (and p
             (or (eq (find-symbol name "XC-STRICT-CL") symbol)
                 (eq (find-symbol name "SB-XC") symbol)
                 ;; OK if the name of a symbol in the host CL package
                 ;; is found in XC-STRICT-CL, even if the symbols
                 ;; differ.
                 (and (find-symbol name "XC-STRICT-CL")
                      (eq (find-symbol name "CL") symbol))))
        *cl-package*
        p)))

;;; printing structures

(defun default-structure-print (structure stream depth)
  (declare (ignore depth))
  (write structure :stream stream :circle t))

(in-package "SB-KERNEL")

;;; These functions are required to emulate SBCL kernel functions
;;; in a vanilla ANSI Common Lisp cross-compilation host.
;;; The emulation doesn't need to be efficient, since it's needed
;;; only for object dumping.

;; The set of structure types that we access by slot position at cross-compile
;; time is fairly small:
;;   - DEFINITION-SOURCE-LOCATION
;;   - DEFSTRUCT-DESCRIPTION, DEFSTRUCT-SLOT-DESCRIPTION
;;   - DEBUG-SOURCE, COMPILED-DEBUG-INFO, COMPILED-DEBUG-FUN-{something}
;;   - HEAP-ALIEN-INFO and ALIEN-{something}-TYPE
;;   - COMMA
(defun classoid-layout (x)
  (declare (notinline classoid-wrapper))
  (classoid-wrapper x))
(defun layout-friend (x) x)
(defun wrapper-friend (x) x)
(defmacro wrapper-%info (x) `(layout-info ,x))
(defun %instance-layout (instance)
  (classoid-layout (find-classoid (type-of instance))))
(defun %instance-length (instance)
  (declare (notinline layout-length))
  ;; In the target, it is theoretically possible to have %INSTANCE-LENGTH
  ;; exceeed layout length, but in the cross-compiler they're the same.
  (layout-length (%instance-layout instance)))
(defun %raw-instance-ref/word (instance index)
  (declare (ignore instance index))
  (error "No such thing as raw structure access on the host"))

(defun %find-position (item seq from-end start end key test)
  (let ((position (position item seq :from-end from-end
                            :start start :end end :key key :test test)))
    (values (if position (elt seq position) nil) position)))

(defun sb-impl::split-seconds-for-sleep (&rest args)
  (declare (ignore args))
  (error "Can't call SPLIT-SECONDS-FOR-SLEEP"))

;;; Needed for constant-folding
(defun system-area-pointer-p (x) x nil) ; nothing is a SAP
(defmacro sap-ref-word (sap offset)
  `(#+64-bit sap-ref-64 #-64-bit sap-ref-32 ,sap ,offset))

(defun logically-readonlyize (x) x)

;;; Mainly for the fasl loader
(defun %fun-name (f) (nth-value 2 (function-lambda-expression f)))

;;;; Variables which have meaning only to the cross-compiler, defined here
;;;; in lieu of #+sb-xc-host elsewere which messes up toplevel form numbers.
(in-package "SB-C")

;;; For macro lambdas that are processed by the host
(declaim (declaration top-level-form))

;;; The opposite of *undefined-fun-allowlist* - if certain full calls
;;; are seen, it is probably the result of a missed transform and/or
;;; misconfiguration.
(defparameter *full-calls-to-warn-about*
  '(;mask-signed-field ;; Too many to fix
    ))

;;; Used by OPEN-FASL-OUTPUT
(defun string-to-octets (string &key external-format)
  (assert (eq external-format :utf-8))
  (let* ((n (length string))
         (a (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n a)
      (let ((code (sb-xc:char-code (char string i))))
        (unless (<= 0 code 127)
          (setf code (sb-xc:char-code #\?)))
        (setf (aref a i) code)))))

;;;; Stubs for host
(defun sb-c:compile-in-lexenv (lambda &rest rest)
  (declare (ignore rest))
  (compile nil (if (eq (car lambda) 'named-lambda)
                   `(lambda ,@(cddr lambda))
                   lambda)))

(defun sb-impl::%defun (name lambda &optional inline-expansion)
  (declare (ignore inline-expansion))
  (proclaim `(ftype function ,name))
  (setf (fdefinition name) (eval lambda)))

(defun %svset (vector index val) ; stemming from toplevel (SETF SVREF)
  (setf (aref vector index) val))
(defun %puthash (key table val) ; stemming from toplevel (SETF GETHASH)
  (setf (gethash key table) val))

;;; The compiler calls this with forms in EVAL-WHEN (:COMPILE-TOPLEVEL) situations.
;;; Since we've already performed macroexpansion using our macros, we can either
;;; implement target-compatible functions for all things into which we might expand,
;;; or we can un-macro-expand the form.  This does a little of both,
;;; mainly for the sake of showing that it's quite easily done.
;;; Truth be told I'd have preferred to use the anti-expansion technique consistently,
;;; however occasionally we see things like (LET ((V FROB)) (%SVSET *THING* X V))
;;; which means that the host is going to do the LET and then call %SVSET.
(defun eval-tlf (form index &optional lexenv)
  (declare (ignore index lexenv))
  (flet ((matchp (template form &aux results)
           (if (named-let recurse ((form form) (template template))
                 (typecase template
                   (null (null form))
                   ((eql ?) (push form results) t) ; match and store anything
                   ((eql :ignore) t) ; match anything and disregard
                   ((cons (eql :or))
                    (some (lambda (template)
                            (recurse form template))
                          (cdr template)))
                   (cons (and (consp form)
                              (and (recurse (car form) (car template))
                                   (recurse (cdr form) (cdr template)))))
                   (t (eql template form)))) ; match template exactly
               (nreverse results)
               (error "Pattern match failure: ~S~% ~S~%" template form))))
    ;; Note that in all cases below, the package lock on CL prevents
    ;; accidental appearance of a CL symbol as a thing being defined.
    (named-let recurse ((form form))
      (case (car form)
       (progn (mapc #'recurse (cdr form))) ; compiler doesn't care about return value
       (t
        (eval
         (case (car form)
           (sb-impl::%defglobal
            (destructuring-bind (symbol value)
                (matchp '((quote ?) (if (%boundp :ignore) :ignore ?)) (cdr form))
              `(defvar ,symbol ,value)))
           (sb-impl::%defparameter
            (destructuring-bind (symbol value)
                (matchp '((quote ?) ? :ignore) (cdr form))
              `(defparameter ,symbol ,value)))
           (sb-impl::%defvar
            (destructuring-bind (symbol value) ; always occurs with a value
                (matchp '((quote ?) (source-location) (:or (unless (%boundp :ignore) ?) ?))
                        (cdr form))
              `(defvar ,symbol ,value)))
           (sb-c::%defconstant
            ;; There is genuinely ambiguity here - does :COMPILE-TOPLEVEL situation for
            ;; DEFCONSTANT mean that we want the host compiler to know the constant?
            ;; It must, because the standard specifies that defconstant need not be within
            ;; EVAL-WHEN for the compiler to know it. But because of how our defconstant
            ;; expands - calling %defconstant inside of an eval-when listing all 3
            ;; situations - we can't discern whether this is our defconstant doing its
            ;; normal thing, versus inside an explicity written eval-when with intent
            ;; to convey the constant to the host - perhaps because of a DEFUN also inside
            ;; an eval-when where we need the host to reference the constant.
            ;; Therefore every constant has to be made known to the host under the
            ;; assumption that it needs it, AND the cross-compiler under the assumption
            ;; that this is our normal :compile-toplevel handling.
            (destructuring-bind (symbol value) (matchp '((quote ?) ? . :ignore) (cdr form))
              `(progn (defconstant ,symbol ,value)
                      (sb-c::%defconstant ',symbol ,symbol nil))))
           (defconstant ; we see this macro as well. The host expansion will not do,
            ;; because it calls our %defconstant which does not assign the symbol a value.
            ;; It might be possible to change that now that we don't use CL: symbols.
            (destructuring-bind (symbol value) (cdr form)
              `(progn (defconstant ,symbol ,value)
                      (sb-c::%defconstant ',symbol ,symbol nil))))
           (t
            form))))))))

(defmacro sb-format:tokens (string) string)

;;; For a use in EARLY-TYPE
(defmacro with-system-mutex ((lock) &body body)
  (declare (ignore lock))
  `(progn ,@body))

;;; Used by our lockfree memoization functions (define-hash-cache)
(defmacro sb-thread:barrier ((kind) &body body)
  (declare (ignore kind))
  `(progn ,@body))

;;; For (eval-when (:compile-toplevel) (/show)) forms that reach the host's EVAL.
(defmacro %primitive (name arg)
  (ecase name
    (print `(format t "~A~%" ,arg))))

(defmacro %with-output-to-string ((var) &body body)
  ;; Let's suppose that the host lisp knows what it's doing to efficiently
  ;; compile the standard macro. Don't try to outdo it.
  `(with-output-to-string (,var) ,@body))
