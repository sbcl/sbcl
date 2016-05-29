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

(in-package "SB!IMPL")

;;; Forward declarations

;;; In correct code, TRULY-THE has only a performance impact and can
;;; be safely degraded to ordinary THE.
(defmacro truly-the (type expr)
  `(the ,type ,expr))

(defmacro named-lambda (name args &body body)
  (declare (ignore name))
  `#'(lambda ,args ,@body))

;;; Interrupt control isn't an issue in the cross-compiler: we don't
;;; use address-dependent (and thus GC-dependent) hashes, and we only
;;; have a single thread of control.
(defmacro without-interrupts (&rest forms)
  `(macrolet ((allow-with-interrupts (&body body)
                `(progn ,@body))
              (with-local-interrupts (&body body)
                `(progn ,@body)))
     ,@forms))

(defmacro with-locked-hash-table ((table) &body body)
  (declare (ignore table))
  `(progn ,@body))

(defmacro with-locked-system-table ((table) &body body)
  (declare (ignore table))
  `(progn ,@body))

(defmacro defglobal (name value &rest doc)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,name
       (if (boundp ',name)
           (symbol-value ',name)
           ,value)
       ,@doc)))

;;; The GENESIS function works with fasl code which would, in the
;;; target SBCL, work on ANSI-STREAMs (streams which aren't extended
;;; Gray streams). In ANSI Common Lisp, an ANSI-STREAM is just a
;;; CL:STREAM.
(deftype ansi-stream () 'stream)

(deftype instance ()
  '(or condition structure-object standard-object))
(deftype funcallable-instance ()
  (error "not clear how to represent FUNCALLABLE-INSTANCE type"))

;; The definition of TYPE-SPECIFIER for the target appears in the file
;; 'deftypes-for-target' - it allows CLASSes and CLASOIDs as specifiers.
;; Instances are never used as specifiers when building SBCL,
;; handily avoiding a problem in figuring out an order in which to
;; define the types CLASS, CLASSOID, and TYPE-SPECIFIER.
(deftype type-specifier () '(or list symbol))

;;; In the target SBCL, the INSTANCE type refers to a base
;;; implementation for compound types with lowtag
;;; INSTANCE-POINTER-LOWTAG. There's no way to express exactly that
;;; concept portably, but we can get essentially the same effect by
;;; testing for any of the standard types which would, in the target
;;; SBCL, be derived from INSTANCE:
(defun %instancep (x)
  (typep x '(or condition structure-object standard-object)))

;;; There aren't any FUNCALLABLE-INSTANCEs in the cross-compilation
;;; host Common Lisp.
(defun funcallable-instance-p (x)
  (if (typep x 'generic-function)
    ;; In the target SBCL, FUNCALLABLE-INSTANCEs are used to implement
    ;; generic functions, so any case which tests for this might in
    ;; fact be trying to test for generic functions. My (WHN 19990313)
    ;; expectation is that this case won't arise in the
    ;; cross-compiler, but if it does, it deserves a little thought,
    ;; rather than reflexively returning NIL.
    (error "not clear how to handle GENERIC-FUNCTION")
    nil))

;;; This seems to be the portable Common Lisp type test which
;;; corresponds to the effect of the target SBCL implementation test...
(defun array-header-p (x)
  (and (typep x 'array)
       (or (not (typep x 'simple-array))
           (/= (array-rank x) 1))))

(defvar sb!xc:*gensym-counter* 0)

(defun sb!xc:gensym (&optional (thing "G"))
  (declare (type string thing))
  (let ((n sb!xc:*gensym-counter*))
    (prog1
        (make-symbol (concatenate 'string thing (write-to-string n :base 10 :radix nil :pretty nil)))
      (incf sb!xc:*gensym-counter*))))

;;; These functions are needed for constant-folding.
(defun simple-array-nil-p (object)
  (when (typep object 'array)
    (assert (not (eq (array-element-type object) nil))))
  nil)

(defun %negate (number)
  (- number))

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

(defun signed-byte-32-p (number)
  (typep number '(signed-byte 32)))

;; This has an obvious portable implementation
;; as (typep number 'ratio), but apparently we
;; expect never to need it.
(defun ratiop (number)
  (declare (ignore number))
  (error "Should not call RATIOP"))

(defun make-value-cell (value)
  (declare (ignore value))
  (error "cross-compiler can not make value cells"))

;;; package locking nops for the cross-compiler

(defmacro without-package-locks (&body body)
  `(progn ,@body))

(defmacro with-single-package-locked-error ((&optional kind thing &rest format)
                                            &body body)
  ;; FIXME: perhaps this should touch THING to make it used?
  (declare (ignore kind thing format))
  `(progn ,@body))

(defun program-assert-symbol-home-package-unlocked (context symbol control)
  (declare (ignore context control))
  symbol)

(defun assert-package-unlocked (package &optional format-control
                                &rest format-arguments)
  (declare (ignore format-control format-arguments))
  package)

(defun assert-symbol-home-package-unlocked (name &optional format-control
                                            &rest format-arguments)
  (declare (ignore format-control format-arguments))
  name)

(declaim (declaration enable-package-locks disable-package-locks))

;; Nonstandard accessor for when you know you have a valid package in hand.
;; This avoids double lookup in *PACKAGE-NAMES* in a few places.
;; But portably we have to just fallback to PACKAGE-NAME.
(defun package-%name (x) (package-name x))

;;; printing structures

(defun default-structure-print (structure stream depth)
  (declare (ignore depth))
  (write structure :stream stream :circle t))

(in-package "SB!KERNEL")
(defun %find-position (item seq from-end start end key test)
  (let ((position (position item seq :from-end from-end
                            :start start :end end :key key :test test)))
    (values (if position (elt seq position) nil) position)))

(defun sb!impl::split-seconds-for-sleep (&rest args)
  (declare (ignore args))
  (error "Can't call SPLIT-SECONDS-FOR-SLEEP"))

;;; Avoid an unknown type reference from globaldb.
(deftype fdefn () '(satisfies fdefn-p))
