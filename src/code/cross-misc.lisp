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

;;; In correct code, TRULY-THE has only a performance impact and can
;;; be safely degraded to ordinary THE.
(defmacro truly-the (type expr)
  `(the ,type ,expr))

;;; MAYBE-INLINE and FREEZE-TYPE declarations can be safely ignored
;;; (possibly at some cost in efficiency).
(declaim (declaration freeze-type maybe-inline))

;;; INHIBIT-WARNINGS declarations can be safely ignored (although we
;;; may then have to wade through some irrelevant warnings).
(declaim (declaration inhibit-warnings))

;;; Interrupt control isn't an issue in the cross-compiler: we don't
;;; use address-dependent (and thus GC-dependent) hashes, and we only
;;; have a single thread of control.
(defmacro without-interrupts (&rest forms)
  `(progn ,@forms))

;;; When we're running as a cross-compiler in an arbitrary host ANSI
;;; Lisp, we shouldn't be doing anything which is sensitive to GC.
;;; KLUDGE: I (WHN 19990131) think the proper long-term solution would
;;; be to remove any operations from cross-compiler source files
;;; (putting them in target-only source files) if they refer to these
;;; hooks. This is a short-term hack.
(defvar *before-gc-hooks* nil)
(defvar *after-gc-hooks* nil)

;;; The GENESIS function works with fasl code which would, in the
;;; target SBCL, work on ANSI-STREAMs (streams which aren't extended
;;; Gray streams). In ANSI Common Lisp, an ANSI-STREAM is just a
;;; CL:STREAM.
(deftype ansi-stream () 'stream)

;;; In the target SBCL, the INSTANCE type refers to a base
;;; implementation for compound types. There's no way to express
;;; exactly that concept portably, but we can get essentially the same
;;; effect by testing for any of the standard types which would, in
;;; the target SBCL, be derived from INSTANCE:
(deftype sb!kernel:instance ()
  '(or condition standard-object structure-object))

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
(defun sb!kernel:array-header-p (x)
  (and (typep x 'array)
       (or (not (typep x 'simple-array))
	   (/= (array-rank x) 1))))

;;; GENESIS needs these at cross-compile time. The target
;;; implementation of these is reasonably efficient by virtue of its
;;; ability to peek into the internals of the package implementation;
;;; this reimplementation is portable but slow.
(defun package-internal-symbol-count (package)
  (let ((result 0))
    (declare (type fixnum result))
    (do-symbols (i package)
      ;; KLUDGE: The ANSI Common Lisp specification warns that
      ;; DO-SYMBOLS may execute its body more than once for symbols
      ;; that are inherited from multiple packages, and we currently
      ;; make no attempt to correct for that here. (The current uses
      ;; of this function at cross-compile time don't really care if
      ;; the count is a little too high.) -- WHN 19990826
      (multiple-value-bind (symbol status)
	  (find-symbol (symbol-name i) package)
	(declare (ignore symbol))
	(when (member status '(:internal :inherited))
	  (incf result))))
    result))
(defun package-external-symbol-count (package)
  (let ((result 0))
    (declare (type fixnum result))
    (do-external-symbols (i package)
      (declare (ignorable i))
      (incf result))
    result))

;;; In the target Lisp, INTERN* is the primitive and INTERN is
;;; implemented in terms of it. This increases efficiency by letting
;;; us reuse a fixed-size buffer; the alternative would be
;;; particularly painful because we don't implement DYNAMIC-EXTENT. In
;;; the host Lisp, this is only used at cold load time, and we don't
;;; care as much about efficiency, so it's fine to treat the host
;;; Lisp's INTERN as primitive and implement INTERN* in terms of it.
(defun intern* (nameoid length package)
  (intern (replace (make-string length) nameoid :end2 length) package))

;;; In the target Lisp this is implemented by reading a fixed slot in
;;; the symbol. In portable ANSI Common Lisp the same criteria can be
;;; met (more slowly, and with the extra property of repeatability
;;; between runs) by just calling SXHASH.
(defun symbol-hash (symbol)
  (declare (type symbol symbol))
  (sxhash symbol))

;;; These functions are needed for constant-folding.
(defun sb!kernel:simple-array-nil-p (object)
  (when (typep object 'array)
    (assert (not (eq (array-element-type object) nil))))
  nil)

(defun sb!kernel:%negate (number)
  (- number))

(defun sb!kernel:%single-float (number)
  (coerce number 'single-float))

(defun sb!kernel:%double-float (number)
  (coerce number 'double-float))

(defun sb!kernel:%ldb (size posn integer)
  (ldb (byte size posn) integer))

(defun sb!kernel:%dpb (newbyte size posn integer)
  (dpb newbyte (byte size posn) integer))

(defun sb!kernel:%with-array-data (array start end)
  (assert (typep array '(simple-array * (*))))
  (values array start end 0))

#!-alpha
(defun sb!vm::ash-left-constant-mod32 (integer amount)
  (ldb (byte 32 0) (ash integer amount)))
#!+alpha
(defun sb!vm::ash-left-constant-mod64 (integer amount)
  (ldb (byte 64 0) (ash integer amount)))
