;;;; predicate functions (EQUAL and friends, and type predicates)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; miscellaneous non-primitive predicates

#!-sb-fluid (declaim (inline streamp))
(defun streamp (stream)
  (typep stream 'stream))

;;; Is X a (VECTOR T)?
(defun vector-t-p (x)
  (or (simple-vector-p x)
      (and (complex-vector-p x)
	   (simple-vector-p (%array-data-vector x)))))

;;;; primitive predicates. These must be supported directly by the
;;;; compiler.

(defun not (object)
  #!+sb-doc
  "Return T if X is NIL, otherwise return NIL."
  (not object))

;;; All the primitive type predicates share a parallel form..
(macrolet
    ((frob ()
       `(progn
	  ,@(mapcar (lambda (pred)
		      (let* ((name (symbol-name pred))
			     (stem (string-right-trim name "P-"))
			     (article (if (find (schar name 0) "AEIOU")
					"an"
					"a")))
			`(defun ,pred (object)
			   ,(format nil
				    "Return T if OBJECT is ~A ~A, ~
				     and NIL otherwise."
				    article
				    stem)
			   (,pred object))))
		    '(array-header-p
		      arrayp
		      atom
		      base-char-p
		      bignump
		      bit-vector-p
		      characterp
		      code-component-p
		      consp
		      compiled-function-p
		      complexp
		      complex-double-float-p
		      complex-float-p
		      #!+long-float complex-long-float-p
		      complex-rational-p
		      complex-single-float-p
		      ;; (COMPLEX-VECTOR-P is not included here since
		      ;; it's awkward to express the type it tests for
		      ;; in the Common Lisp type system, and since
		      ;; it's only used in the implementation of a few
		      ;; specialized things.)
		      double-float-p
		      fdefn-p
		      fixnump
		      floatp
		      functionp
		      integerp
		      listp
		      long-float-p
		      lra-p
		      null
		      numberp
		      rationalp
		      ratiop
		      realp
		      short-float-p
		      sb!kernel:simple-array-p
		      simple-bit-vector-p
		      simple-string-p
		      simple-vector-p
		      single-float-p
		      stringp
		      %instancep
		      symbolp
		      system-area-pointer-p
		      weak-pointer-p
		      vectorp
		      unsigned-byte-32-p
		      signed-byte-32-p
		      simple-array-unsigned-byte-2-p
		      simple-array-unsigned-byte-4-p
		      simple-array-unsigned-byte-8-p
		      simple-array-unsigned-byte-16-p
		      simple-array-unsigned-byte-32-p
		      simple-array-signed-byte-8-p
		      simple-array-signed-byte-16-p
		      simple-array-signed-byte-30-p
		      simple-array-signed-byte-32-p
		      simple-array-single-float-p
		      simple-array-double-float-p
		      #!+long-float simple-array-long-float-p
		      simple-array-complex-single-float-p
		      simple-array-complex-double-float-p
		      #!+long-float simple-array-complex-long-float-p
		      )))))
  (frob))

;;; Return the specifier for the type of object. This is not simply
;;; (TYPE-SPECIFIER (CTYPE-OF OBJECT)) because CTYPE-OF has different
;;; goals than TYPE-OF. In particular, speed is more important than
;;; precision, and it is not permitted to return member types.
(defun type-of (object)
  #!+sb-doc
  "Return the type of OBJECT."
  (if (typep object '(or function array complex))
    (type-specifier (ctype-of object))
    (let* ((class (layout-class (layout-of object)))
	   (name (class-name class)))
      (if (typep object 'instance)
      (case name
	(sb!alien-internals:alien-value
	 `(sb!alien:alien
	   ,(sb!alien-internals:unparse-alien-type
	     (sb!alien-internals:alien-value-type object))))
	(t
	 (class-proper-name class)))
      name))))

;;; FIXME: This belongs somewhere else, perhaps in code/array.lisp.
(defun upgraded-array-element-type (spec)
  #!+sb-doc
  "Return the element type that will actually be used to implement an array
   with the specifier :ELEMENT-TYPE Spec."
  (type-specifier
   (array-type-specialized-element-type
    (specifier-type `(array ,spec)))))

;;;; equality predicates

;;; This is real simple, 'cause the compiler takes care of it.
(defun eq (obj1 obj2)
  #!+sb-doc
  "Return T if OBJ1 and OBJ2 are the same object, otherwise NIL."
  (eq obj1 obj2))

(defun equal (x y)
  #!+sb-doc
  "Returns T if X and Y are EQL or if they are structured components
  whose elements are EQUAL. Strings and bit-vectors are EQUAL if they
  are the same length and have identical components. Other arrays must be
  EQ to be EQUAL."
  (cond ((eql x y) t)
	((consp x)
	 (and (consp y)
	      (equal (car x) (car y))
	      (equal (cdr x) (cdr y))))
	((stringp x)
	 (and (stringp y) (string= x y)))
	((pathnamep x)
	 (and (pathnamep y) (pathname= x y)))
	((bit-vector-p x)
	 (and (bit-vector-p y)
	      (= (the fixnum (length x))
		 (the fixnum (length y)))
	      (do ((i 0 (1+ i))
		   (length (length x)))
		  ((= i length) t)
		(declare (fixnum i))
		(or (= (the fixnum (bit x i))
		       (the fixnum (bit y i)))
		    (return nil)))))
	(t nil)))

;;; EQUALP comparison of HASH-TABLE values
(defun hash-table-equalp (x y)
  (declare (type hash-table x y))
  (or (eq x y)
      (and (hash-table-p y)
	   (eql (hash-table-count x) (hash-table-count y))
	   (eql (hash-table-test x) (hash-table-test y))
	   (block comparison-of-entries
	     (maphash (lambda (key x-value)
			(multiple-value-bind (y-value y-value-p)
			    (gethash key y)
			  (unless (and y-value-p (equalp x-value y-value))
			    (return-from comparison-of-entries nil))))
		      x)
	     t))))

(defun equalp (x y)
  #+nil ; KLUDGE: If doc string, should be accurate: Talk about structures
  ; and HASH-TABLEs.
  "Just like EQUAL, but more liberal in several respects.
  Numbers may be of different types, as long as the values are identical
  after coercion. Characters may differ in alphabetic case. Vectors and
  arrays must have identical dimensions and EQUALP elements, but may differ
  in their type restriction."
  (cond ((eq x y) t)
	((characterp x) (and (characterp y) (char-equal x y)))
	((numberp x) (and (numberp y) (= x y)))
	((consp x)
	 (and (consp y)
	      (equalp (car x) (car y))
	      (equalp (cdr x) (cdr y))))
	((pathnamep x)
	 (and (pathnamep y) (pathname= x y)))
	((hash-table-p x)
	 (and (hash-table-p y)
	      (hash-table-equalp x y)))
	((typep x 'instance)
	 (let* ((layout-x (%instance-layout x))
		(len (layout-length layout-x)))
	   (and (typep y 'instance)
		(eq layout-x (%instance-layout y))
		(structure-class-p (layout-class layout-x))
		(do ((i 1 (1+ i)))
		    ((= i len) t)
		  (declare (fixnum i))
		  (let ((x-el (%instance-ref x i))
			(y-el (%instance-ref y i)))
		    (unless (or (eq x-el y-el)
				(equalp x-el y-el))
		      (return nil)))))))
	((vectorp x)
	 (let ((length (length x)))
	   (and (vectorp y)
		(= length (length y))
		(dotimes (i length t)
		  (let ((x-el (aref x i))
			(y-el (aref y i)))
		    (unless (or (eq x-el y-el)
				(equalp x-el y-el))
		      (return nil)))))))
	((arrayp x)
	 (and (arrayp y)
	      (= (array-rank x) (array-rank y))
	      (dotimes (axis (array-rank x) t)
		(unless (= (array-dimension x axis)
			   (array-dimension y axis))
		  (return nil)))
	      (dotimes (index (array-total-size x) t)
		(let ((x-el (row-major-aref x index))
		      (y-el (row-major-aref y index)))
		  (unless (or (eq x-el y-el)
			      (equalp x-el y-el))
		    (return nil))))))
	(t nil)))
#!+sb-test
(let ((test-cases '((0.0 -0.0 t)
		    (0.0 1.0 nil)
		    (#c(1 0) #c(1.0 0) t)
		    (#c(1.1 0) #c(11/10 0) nil) ; due to roundoff error
		    ("Hello" "hello" t)
		    ("Hello" #(#\h #\E #\l #\l #\o) t)
		    ("Hello" "goodbye" nil))))
  (dolist (test-case test-cases)
    (destructuring-bind (x y expected-result) test-case
      (let* ((result (equalp x y))
	     (bresult (if result 1 0))
	     (expected-bresult (if expected-result 1 0)))
	(unless (= bresult expected-bresult)
	  (error "failed test (EQUALP ~S ~S)" x y))))))
