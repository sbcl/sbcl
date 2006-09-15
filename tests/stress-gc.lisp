;;;; a stress test for the garbage collector

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;;; TO DO:
;;;;   * Add conses:
;;;;     ** Make REPR-CONS.
;;;;     ** Some generations should be lists, not vectors.
;;;;   * Make it so that ASSIGN-GENERATION on an existing generation
;;;;     only overwrites some of the elements (randomly), not all.
;;;;   * Review the GC code to look for other stuff I should test.

(in-package :cl-user)

(declaim (optimize (safety 3) (speed 2)))

;;; a table of functions REPR-FOO which bear a vague correspondence
;;; to the types of memory representations used by SBCL (with each
;;; typically trying to exercise that type of representation)
(defvar *reprs*)
(declaim (type simple-vector *reprs*))

(defun random-element (seq)
  (elt seq (random (length seq))))

(defun repr (i)
  (declare (type fixnum i))
  (let ((result (svref *reprs* (mod i (length *reprs*)))))
    #+nil (/show "REPRESENT" i result)
    result))

(defun stress-gc (n-passes &optional (size 3000))
  (format t "~&beginning STRESS-GC N-PASSES=~W SIZE=~W~%" n-passes size)
  (let ((generations (make-array (isqrt size) :initial-element nil))
        ;; We allocate on the order of MOST-POSITIVE-FIXNUM things
        ;; before doing a full GC.
        (max-passes-to-full-gc (floor most-positive-fixnum size))
        (remaining-passes-to-full-gc 0))
    (dotimes (j-pass n-passes)
      #+nil (/show j-pass)
      (if (plusp remaining-passes-to-full-gc)
          (decf remaining-passes-to-full-gc)
          (progn
            #+nil (/show "doing GC :FULL T")
            (gc :full t)
            (setf remaining-passes-to-full-gc (random max-passes-to-full-gc))))
      (let* (;; (The (ISQRT (RANDOM (EXPT .. 2))) distribution here is
             ;; intended to give a distribution of lifetimes of memory
             ;; usage, with low-indexed generations tending to live
             ;; for a long time.)
             (i-generation (isqrt (random (expt (length generations) 2))))
             (generation-i (aref generations i-generation)))
        #+nil (/show i-generation generation-i)
        (when generation-i
          (assert-generation i-generation generation-i))
        (when (or (null generation-i)
                  (plusp (random 3)))
          #+nil (/show "allocating or reallocating" i-generation)
          (setf generation-i
                (make-array (random (1+ size)))))
        (assign-generation i-generation generation-i)
        (when (plusp (random 3))
          (assert-generation i-generation generation-i))
        (setf (aref generations i-generation)
              generation-i))))
  (format t "~&done with STRESS-GC N-PASSES=~W SIZE=~W~%" n-passes size))

(defvar *expected*)
(defvar *got*)
(defun assert-generation (index-of-generation generation)
  (dotimes (index-within-generation (length generation))
    #+nil (/show "assert-generation" index-of-generation index-within-generation)
    (let ((element-of-generation (aref generation index-within-generation))
          (repr (repr (+ index-within-generation index-of-generation))))
      (unless (funcall repr index-within-generation element-of-generation)
        ;; KLUDGE: We bind these to special variables for the
        ;; convenience of the debugger, which ca. SBCL 0.6.6 is too
        ;; wimpy to inspect lexical variables.
        (let ((*expected* (funcall repr index-within-generation))
              (*got* element-of-generation))
          (error "bad element #~W in generation #~D:~%  expected ~S~%  from ~S,~%  got ~S"
                 index-within-generation
                 index-of-generation
                 *expected*
                 repr
                 *got*))))))

(defun assign-generation (index-of-generation generation)
  (dotimes (index-within-generation (length generation))
    #+nil (/show "assert-generation" index-of-generation index-within-generation)
    (setf (aref generation index-within-generation)
          (funcall (repr (+ index-within-generation index-of-generation))
                   index-within-generation))))

(defun repr-fixnum (index &optional (value nil value-p))
  (let ((fixnum (the fixnum (+ index 101))))
    (if value-p
        (eql fixnum value)
        fixnum)))

(defun repr-function (index &optional (value nil value-p))
  (let ((fixnum (mod (+ index 2) 3)))
    (if value-p
        (eql fixnum (funcall value))
        (ecase fixnum
          (0 #'repr-fixnum-zero)
          (1 #'repr-fixnum-one)
          (2 #'repr-fixnum-two)))))
(defun repr-fixnum-zero () 0)
(defun repr-fixnum-one () 1)
(defun repr-fixnum-two () 2)

(defstruct repr-instance slot)
(defun repr-instance (index &optional (value nil value-p))
  (let ((fixnum (mod (* index 3) 4)))
    (if value-p
        (and (typep value 'repr-instance)
             (eql (repr-instance-slot value) fixnum))
        (make-repr-instance :slot fixnum))))

(defun repr-eql-hash-table (index &optional (value nil value-p))
  (let ((first-fixnum (mod (* index 31) 9))
        (n-fixnums 5))
    (if value-p
        (and (hash-table-p value)
             (= (hash-table-count value) n-fixnums)
             (dotimes (i n-fixnums t)
               (unless (= (gethash (+ i first-fixnum) value) i)
                 (return nil)))
             #|
             (repr-bignum index (gethash 'bignum value))
             (repr-ratio index (gethash 'ratio value))
             |#)
        (let ((hash-table (make-hash-table :test 'eql)))
          (dotimes (i n-fixnums)
            (setf (gethash (+ first-fixnum i) hash-table) i))
          #|
          (setf (gethash 'bignum hash-table) (repr-bignum index)
                (gethash 'ratio hash-table) (repr-ratio index))
          |#
          hash-table))))

(defun repr-weak-key-hash-table (index &optional (value nil value-p))
  (let ((first (+ most-positive-fixnum (mod (* index 31) 9)))
        (n 5))
    (if value-p
        (and (hash-table-p value)
             (<= (hash-table-count value) n)
             (dotimes (i n t)
               (let ((x (gethash (+ i first) value)))
                 (unless (or (null x) (= x i))
                   (return nil)))))
        (let ((hash-table (make-hash-table
                           :weakness :key
                           :test (random-element '(eq eql equal equalp)))))
          (dotimes (i n)
            (setf (gethash (+ first i) hash-table) i))
          hash-table))))

(defun repr-bignum (index &optional (value nil value-p))
  (let ((bignum (+ index 10000300020)))
    (if value-p
        (eql value bignum)
        bignum)))

(defun repr-ratio (index &optional (value nil value-p))
  (let ((ratio (/ index (1+ index))))
    (if value-p
        (eql value ratio)
        ratio)))

(defun repr-single-float (index &optional (value nil value-p))
  (let ((single-float (* 0.25 (float index) (1+ (float index)))))
    (if value-p
        (eql value single-float)
        single-float)))

(defun repr-double-float (index &optional (value nil value-p))
  (let ((double-float (+ 0.25d0 (1- index) (1+ (float index)))))
    (if value-p
        (eql value double-float)
        double-float)))

(defun repr-simple-string (index &optional (value nil value-p))
  (let ((length (mod index 14)))
    (if value-p
        (and (stringp value)
             (typep value 'simple-array)
             (= (length value) length))
        (make-string length))))

(defun repr-simple-vector (index &optional (value nil value-p))
  (let ((length (mod (1+ index) 16)))
    (if value-p
        (and (simple-vector-p value)
             (= (array-dimension value 0) length))
        (make-array length))))

(defun repr-complex-vector (index &optional (value nil value-p))
  (let* ((size (mod (* 5 index) 13))
         (length (floor size 3)))
    (if value-p
        (and (vectorp value)
             (not (typep value 'simple-array))
             (= (array-dimension value 0) size)
             (= (length value) length))
        (make-array size :fill-pointer length))))

(defun repr-symbol (index &optional (value nil value-p))
  (let* ((symbols #(zero one two three four))
         (symbol (aref symbols (mod index (length symbols)))))
    (if value-p
        (eq value symbol)
        symbol)))

(defun repr-base-char (index &optional (value nil value-p))
  (let* ((base-chars #(#\z #\o #\t #\t #\f #\f #\s #\s #\e))
         (base-char (aref base-chars (mod index (length base-chars)))))
    (if value-p
        (eql value base-char)
        base-char)))

(setf *reprs*
      (vector #'repr-fixnum
              #'repr-function
              #'repr-instance
              #'repr-eql-hash-table
              #'repr-weak-key-hash-table
#|
              #'repr-equal-hash-table
              #'repr-equalp-hash-table
|#
              #'repr-bignum
              #'repr-ratio
              #'repr-single-float
              #'repr-double-float
#|
              #'repr-complex-single-float
              #'repr-complex-double-float
              #'repr-simple-array
|#
              #'repr-simple-string
#|
              #'repr-simple-bit-vector
|#
              #'repr-simple-vector
#|
              #'repr-simple-array-u2
              #'repr-simple-array-u4
              #'repr-simple-array-u8
              #'repr-simple-array-u16
              #'repr-simple-array-u32
              #'repr-simple-array-single-float
              #'repr-simple-array-double-float
              #'repr-complex-string
              #'repr-complex-bit-vector
|#
              #'repr-complex-vector
#|
              #'repr-complex-array
              ;; TO DO: #'repr-funcallable-instance
|#
              #'repr-symbol
              #'repr-base-char
              ;; TO DO: #'repr-sap
              ;; TO DO? #'repr-unbound-marker
              ;; TO DO? #'repr-weak-pointer
              ;; TO DO? #'repr-instance-header
              ;; TO DO? #'repr-fdefn
              ))
