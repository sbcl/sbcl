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

(defun %hash-table-alist (hash-table &aux result)
  (maphash (lambda (key value) (push (cons key value) result))
           hash-table)
  result)

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

(defun %set-symbol-global-value (sym val) (setf (symbol-value sym) val))

;;; Necessary only to placate the host compiler in %COMPILER-DEFGLOBAL.
(defun set-symbol-global-value (sym val)
  (error "Unexpected (~S ~S ~S)" 'set-symbol-global-value sym val))

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

;;; While cross-compiling, we do not use *GENSYM-COUNTER* to make part
;;; of the new symbol's name.  This is for two reasons: firstly,
;;; genesis can coalesce uninterned symbols of similar names, which
;;; leads to significant space savings in the resulting core;
;;; secondly, it is very hard to maintain a consistent state of
;;; *GENSYM-COUNTER*, even if we attempt to maintain our own: the
;;; presence of (EVAL-WHEN (:COMPILE-TOPLEVEL ...) ...) in expansions
;;; of e.g. SB-XC:DEFMACRO, SB-XC:DEFINE-COMPILER-MACRO and so on can
;;; lead to macro-functions of target macros on the host, which then
;;; will GENSYM varying number of times depending on whether they are
;;; minimally-compiled or not: the only way to guarantee identical
;;; symbol names at the same point in the compilation is not to use
;;; *GENSYM-COUNTER* at all.
;;;
;;; Not using *GENSYM-COUNTER* would not be an issue under normal
;;; circumstances, modulo the demands of the language spec: the only
;;; thing that matters for the typical use of GENSYMs in code is the
;;; identity of the symbol, not its name.  However, the coalescing of
;;; uninterned symbols by genesis introduces a potential pitfall: if a
;;; macro-writing-macro generates symbols at first use that the
;;; newly-defined macro will use when *it* expands, the coalescing in
;;; genesis would lead to that inner macro no longer having the
;;; correct expansion.  (Or equivalents, such as macros expanded
;;; inside definitions of functions declaimed INLINE).  In the event
;;; of truly weird behaviour of some macro leading the intrepid
;;; maintainer to this comment: try turning off uninterned symbol
;;; coalescing in genesis by redefining GET-UNINTERNED-SYMBOL; if
;;; things work after that, find the offending use of GENSYM and make
;;; sure that everything has a distinct symbol name.  (This is not an
;;; issue for user code, which is compiled with a normal
;;; implementation of GENSYM).
(defun sb-xc:gensym (&optional (thing "G"))
  (declare (type string thing))
  (make-symbol thing))

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

(defun %ldb (size posn integer)
  (ldb (byte size posn) integer))

(defun %dpb (newbyte size posn integer)
  (dpb newbyte (byte size posn) integer))

(defun %with-array-data (array start end)
  (assert (typep array '(simple-array * (*))))
  (values array start end 0))

;; We could probably just implement this by creating a displaced array
;; if need be.
(defmacro with-array-data (((data-var array &key offset-var)
                            (start-var &optional (svalue 0))
                            (end-var &optional (evalue nil))
                            &key force-inline check-fill-pointer
                                 array-header-p)
                           &body forms
                           &environment env)
  (declare (ignore data-var array offset-var)
           (ignore start-var svalue)
           (ignore end-var evalue)
           (ignore force-inline check-fill-pointer array-header-p)
           (ignore forms env))
  `(error "WITH-ARRAY-DATA not implemented on the host."))

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

(defmacro with-single-package-locked-error ((&optional kind thing &rest format)
                                            &body body)
  (declare (ignore kind format))
  `(progn
     ,thing
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

(define-symbol-macro *gc-epoch* 0)
(deftype weak-vector () nil)
(defun weak-vector-p (x) (declare (ignore x)) nil)
(defun sb-thread:make-mutex (&key name) (list :mock-mutex name))
(deftype sb-thread:mutex () '(cons (eql :mock-mutex)))

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

(defun allocate-weak-vector (n) (make-array (the integer n)))

#+weak-vector-readbarrier
(progn (deftype weak-vector () nil) ; nothing is a weak-vector
       (defun sb-int:weak-vector-ref (v i)
         (error "Called WEAK-VECTOR-REF on ~S ~S" v i))
       (defun (setf sb-int:weak-vector-ref) (new v i)
         (error "Called (SETF WEAK-VECTOR-REF) on ~S ~S ~S" new v i))
       (defun sb-int:weak-vector-len (v)
         (error "Called WEAK-VECTOR-LEN on ~S" v)))

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
(defun check-range<= (l x h)
  (and (typep x 'sb-xc:fixnum)
       (<= l x h)))
(defvar *unbound-marker* (make-symbol "UNBOUND-MARKER"))

(defun make-unbound-marker ()
  *unbound-marker*)

(defun unbound-marker-p (x)
  (eq x *unbound-marker*))
