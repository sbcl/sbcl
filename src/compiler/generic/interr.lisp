;;;; This file defines all of the internal errors. How they are
;;;; handled is defined in .../code/interr.lisp. How they are signaled
;;;; depends on the machine.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(defun error-number-or-lose (name)
  (or (position name sb!c:*backend-internal-errors* :key #'car)
      (error "unknown internal error: ~S" name)))

;;; FIXME: Having each of these error handlers be a full, named function
;;; seems to contribute a noticeable amount of bloat and little value.
;;; Perhaps we could just make a single error-handling function with a
;;; big CASE statement inside it? Or at least implement the error handling
;;; functions as closures instead of DEFUNs?
(eval-when (:compile-toplevel :execute)
  (def!macro define-internal-errors (&rest errors)
	     (let ((info (mapcar (lambda (x)
				   (if x
				       (cons (symbolicate (first x) "-ERROR")
					     (second x))
				       '(nil . "unused")))
				 errors)))
	       `(progn
		  (setf sb!c:*backend-internal-errors*
			',(coerce info 'vector))
		  nil))))

(define-internal-errors
  (unknown
   "unknown system lossage")
  (object-not-fun
   "Object is not of type FUNCTION.")
  (object-not-list
   "Object is not of type LIST.")
  (object-not-bignum
   "Object is not of type BIGNUM.")
  (object-not-ratio
   "Object is not of type RATIO.")
  (object-not-single-float
   "Object is not of type SINGLE-FLOAT.")
  (object-not-double-float
   "Object is not of type DOUBLE-FLOAT.")
  #!+long-float
  (object-not-long-float
   "Object is not of type LONG-FLOAT.")
  (object-not-simple-string
   "Object is not of type SIMPLE-STRING.")
  (object-not-simple-bit-vector
   "Object is not of type SIMPLE-BIT-VECTOR.")
  (object-not-simple-vector
   "Object is not of type SIMPLE-VECTOR.")
  (object-not-fixnum
   "Object is not of type FIXNUM.")
  (object-not-vector
   "Object is not of type VECTOR.")
  (object-not-string
   "Object is not of type STRING.")
  (object-not-bit-vector
   "Object is not of type BIT-VECTOR.")
  (object-not-array
   "Object is not of type ARRAY.")
  (object-not-number
   "Object is not of type NUMBER.")
  (object-not-rational
   "Object is not of type RATIONAL.")
  (object-not-float
   "Object is not of type FLOAT.")
  (object-not-real
   "Object is not of type REAL.")
  (object-not-integer
   "Object is not of type INTEGER.")
  (object-not-cons
   "Object is not of type CONS.")
  (object-not-symbol
   "Object is not of type SYMBOL.")
  (undefined-symbol
   ;; FIXME: Isn't this used for calls to unbound (SETF FOO) too? If so, revise
   ;; the name.
   "An attempt was made to use an undefined FDEFINITION.")
  (object-not-coerceable-to-fun
   "Object is not coerceable to type FUNCTION.")
  (invalid-arg-count
   "invalid argument count")
  (bogus-arg-to-values-list
   "bogus argument to VALUES-LIST")
  (unbound-symbol
   "An attempt was made to use an undefined SYMBOL-VALUE.")
  ;; FIXME: We shouldn't need these placeholder NIL entries any more
  ;; now that we pass our magic numbers cleanly through sbcl.h.
  nil 
  (object-not-sap
   "Object is not a System Area Pointer (SAP).")
  (invalid-unwind
   "attempt to RETURN-FROM a block that no longer exists")
  (unseen-throw-tag
   "attempt to THROW to a non-existent tag")
  (division-by-zero
   "division by zero")
  (object-not-type
   "Object is of the wrong type.")
  (odd-key-args
   "odd number of &KEY arguments")
  (unknown-key-arg
   "unknown &KEY argument")
  nil
  nil
  (invalid-array-index
   "invalid array index")
  (wrong-number-of-indices
   "wrong number of indices")
  (object-not-simple-array
   "Object is not of type SIMPLE-ARRAY.")
  (object-not-signed-byte-32
   "Object is not of type (SIGNED-BYTE 32).")
  (object-not-unsigned-byte-32
   "Object is not of type (UNSIGNED-BYTE 32).")
  (object-not-simple-array-unsigned-byte-2
   "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 2) (*)).")
  (object-not-simple-array-unsigned-byte-4
   "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 4) (*)).")
  (object-not-simple-array-unsigned-byte-8
   "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).")
  (object-not-simple-array-unsigned-byte-16
   "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*)).")
  (object-not-simple-array-unsigned-byte-32
   "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*)).")
  (object-not-simple-array-signed-byte-8
   "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 8) (*)).")
  (object-not-simple-array-signed-byte-16
   "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 16) (*)).")
  (object-not-simple-array-signed-byte-30
   "Object is not of type (SIMPLE-ARRAY FIXNUM (*)).")
  (object-not-simple-array-signed-byte-32
   "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)).")
  (object-not-simple-array-single-float
   "Object is not of type (SIMPLE-ARRAY SINGLE-FLOAT (*)).")
  (object-not-simple-array-double-float
   "Object is not of type (SIMPLE-ARRAY DOUBLE-FLOAT (*)).")
  #!+long-float
  (object-not-simple-array-long-float
   "Object is not of type (SIMPLE-ARRAY LONG-FLOAT (*)).")
  (object-not-simple-array-complex-single-float
   "Object is not of type (SIMPLE-ARRAY (COMPLEX SINGLE-FLOAT) (*)).")
  (object-not-simple-array-complex-double-float
   "Object is not of type (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)).")
  #!+long-float
  (object-not-simple-array-complex-long-float
   "Object is not of type (SIMPLE-ARRAY (COMPLEX LONG-FLOAT) (*)).")
  (object-not-complex
   "Object is not of type COMPLEX.")
  (object-not-complex-rational
   "Object is not of type (COMPLEX RATIONAL).")
  (object-not-complex-float
   "Object is not of type (COMPLEX FLOAT).")
  (object-not-complex-single-float
   "Object is not of type (COMPLEX SINGLE-FLOAT).")
  (object-not-complex-double-float
   "Object is not of type (COMPLEX DOUBLE-FLOAT).")
  #!+long-float
  (object-not-complex-long-float
   "Object is not of type (COMPLEX LONG-FLOAT).")
  (object-not-weak-pointer
   "Object is not a WEAK-POINTER.")
  (object-not-instance
   "Object is not a INSTANCE.")
  (object-not-base-char
   "Object is not of type BASE-CHAR.")
  (nil-fun-returned
   "A function with declared result type NIL returned.")
  (layout-invalid
   "Object layout is invalid. (indicates obsolete instance)")
  (object-not-complex-vector
   "Object is not a complex (non-SIMPLE-ARRAY) vector."))
