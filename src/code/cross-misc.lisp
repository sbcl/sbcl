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
  (setf (symbol-value sym) val))

(defun %defun (name lambda &optional inline-expansion)
  (declare (ignore inline-expansion))
  (cl:proclaim `(ftype function ,name))
  (setf (fdefinition name) lambda))

(defun %defglobal (name value source-location &optional (doc nil docp))
  (declare (ignore source-location doc docp))
  (cl:proclaim `(special ,name))
  (setf (symbol-value name) value))

(defun %defparameter (var val source-location &optional (doc nil docp))
  (declare (ignore source-location doc docp))
  (cl:proclaim `(special ,var))
  (setf (symbol-value var) val))

(defun %defvar (var source-location &optional (val nil valp) (doc nil docp))
  (declare (ignore source-location doc docp))
  (cl:proclaim `(special ,var))
  (when (and valp (not (boundp var)))
    (setf (symbol-value var) val)))

(defun %boundp (symbol) (boundp symbol))

;;; The GENESIS function works with fasl code which would, in the
;;; target SBCL, work on ANSI-STREAMs (streams which aren't extended
;;; Gray streams). In ANSI Common Lisp, an ANSI-STREAM is just a
;;; CL:STREAM.
(deftype ansi-stream () 'stream)

;;; In the target SBCL, the INSTANCE type refers to a base
;;; implementation for compound types with lowtag
;;; INSTANCE-POINTER-LOWTAG. There's no way to express exactly that
;;; concept portably, but we can at least declare here what kind of
;;; host objects get dumped into something that when loaded would be a
;;; target INSTANCE.
(deftype instance ()
  '(or (and structure-object (not target-num))
       package))
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

;;; We maintain a separate GENSYM counter since the host is allowed to
;;; mutate its counter however it wishes.
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
(declaim (declaration sb-c::tlab))

;;; The XC-STRICT-CL and SB-XC packages are called COMMON-LISP in the
;;; target image.
(defun sb-xc:package-name (package)
  (if (or (eq package #.(find-package "SB-XC"))
          (eq package #.(find-package "XC-STRICT-CL")))
      "COMMON-LISP"
      (cl:package-name package)))

;; Nonstandard accessor for when you know you have a valid package in hand.
;; This avoids double lookup in *PACKAGE-NAMES* in a few places.
;; But portably we have to just fallback to PACKAGE-NAME.
(defun package-%name (x) (sb-xc:package-name x))

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
             (let ((xc-strict-symbol (find-symbol name #.(find-package "XC-STRICT-CL"))))
               (and xc-strict-symbol
                    (or (eq xc-strict-symbol symbol)
                        (eq symbol (find-symbol name #.(find-package "SB-XC")))
                        (eq symbol (find-symbol name #.(find-package "COMMON-LISP")))))))
        *cl-package*
        p)))

(defun possibly-base-stringize (s) (coerce (the string s) 'simple-base-string))
(defun possibly-base-stringize-to-heap (s) (coerce (the string s) 'simple-base-string))

;;; printing structures

(defun default-structure-print (structure stream depth)
  (declare (ignore depth))
  (write structure :stream stream :circle t))

(defun unreachable ()
  (bug "Unreachable reached"))

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
(defun %instance-layout (instance)
  (declare (notinline classoid-layout))
  (classoid-layout (find-classoid (type-of instance))))
(defun %instance-length (instance)
  (declare (notinline layout-length))
  ;; In the target, it is theoretically possible to have %INSTANCE-LENGTH
  ;; exceeed layout length, but in the cross-compiler they're the same.
  (layout-length (%instance-layout instance)))

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

;;; Needed for assembler.
(defstruct (asm-sap-wrapper (:constructor vector-sap (vector)))
  (vector (missing-arg) :type (simple-array (unsigned-byte 8) (*))))
(defmacro with-pinned-objects (list &body body)
  (declare (ignore list))
  `(progn ,@body))
;;; The assembler does not USE-PACKAGE sb-sys, which works to our advantage
;;; in that we can define SAP-REF-16 and -32 as macros in the ASM package
;;; which avoids conflict with genesis. Genesis has its own SAP emulations in SB-SYS
;;; so all the definitions of FIXUP-CODE-OBJECT using the native accessors
;;; can transparently operate on genesis's model of target code blobs.
(defsetf sb-assem::sap-ref-16 sb-assem::asm-set-sap-ref-16)
(defsetf sb-assem::sap-ref-32 sb-assem::asm-set-sap-ref-32)
(defun sb-assem::asm-set-sap-ref-16 (sap index val)
  (declare (type asm-sap-wrapper sap))
  (multiple-value-bind (b0 b1)
    #+little-endian (values (ldb (byte 8 0) val) (ldb (byte 8 8) val))
    #+big-endian    (values (ldb (byte 8 8) val) (ldb (byte 8 0) val))
    (let ((octets (asm-sap-wrapper-vector sap)))
      (setf (aref octets (+ index 0)) b0
            (aref octets (+ index 1)) b1)))
  val)
(defun sb-assem::asm-set-sap-ref-32 (sap index val)
  (declare (type asm-sap-wrapper sap))
  (multiple-value-bind (b0 b1 b2 b3)
      #+little-endian (values (ldb (byte 8  0) val) (ldb (byte 8  8) val)
                              (ldb (byte 8 16) val) (ldb (byte 8 24) val))
      #+big-endian    (values (ldb (byte 8 24) val) (ldb (byte 8 16) val)
                              (ldb (byte 8  8) val) (ldb (byte 8  0) val))
    (let ((octets (asm-sap-wrapper-vector sap)))
      (setf (aref octets (+ index 0)) b0
            (aref octets (+ index 1)) b1
            (aref octets (+ index 2)) b2
            (aref octets (+ index 3)) b3)))
  val)

(defun logically-readonlyize (x) x)

;;; Mainly for the fasl loader
(defun %fun-name (f) (nth-value 2 (function-lambda-expression f)))

(defun %svset (vector index val) ; stemming from toplevel (SETF SVREF)
  (setf (aref vector index) val))
(defun %puthash (key table val) ; stemming from toplevel (SETF GETHASH)
  (setf (gethash key table) val))

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
      (let ((code (char-code (char string i))))
        (unless (<= 0 code 127)
          (setf code (char-code #\?)))
        (setf (aref a i) code)))))

;;;; Stubs for host
(defun sb-c:compile-in-lexenv (lambda &rest rest)
  (declare (ignore rest))
  (compile nil (if (eq (car lambda) 'named-lambda)
                   `(lambda ,@(cddr lambda))
                   lambda)))

;;; The compiler calls this with forms in EVAL-WHEN (:COMPILE-TOPLEVEL) situations.
(defun eval-tlf (form index &optional lexenv)
  (declare (ignore index lexenv))
  (eval form))

(defmacro sb-format:tokens (string) string)

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

(defun source-location ())

;;; %SYMBOL-INFO is a primitive object accessor defined in 'objdef.lisp'
;;; But in the host Lisp, there is no such thing. Instead, SYMBOL-%INFO
;;; is kept as a property on the host symbol.
(declaim (inline symbol-%info))
(defun symbol-%info (symbol) (get symbol :sb-xc-globaldb-info))
(defun symbol-dbinfo (symbol) (symbol-%info symbol))

(defun sys-copy-struct (x) (copy-structure x))
(defun ensure-heap-list (x) (copy-list x))

(defun range< (l x h) (< l x h))
(defun range<= (l x h) (<= l x h))
(defun range<<= (l x h) (and (< l x) (<= x h)))
(defun range<=< (l x h) (and (<= l x) (< x h)))

