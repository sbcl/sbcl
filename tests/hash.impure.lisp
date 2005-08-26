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

(in-package :cl-user)

(defstruct foo)
(defstruct bar x y)

;;; SXHASH and PSXHASH should distribute hash values well over the
;;; space of possible values, so that collisions between the hash
;;; values of unequal objects should be very uncommon. (Except of
;;; course the hash values must collide when the objects are EQUAL or
;;; EQUALP respectively!)
(locally
  ;; In order to better test not-EQ-but-EQUAL and not-EQ-but-EQUALP,
  ;; we'd like to suppress some optimizations.
  (declare (notinline complex float coerce + - expt))
  (flet ((make-sxhash-subtests ()
           (list (cons 0 1)
                 (list 0 1)
                 (cons 1 0)
                 (cons (cons 1 0) (cons 0 0))
                 (cons (list 1 0) (list 0 0))
                 (list (cons 1 0) (list 0 0))
                 (list (cons 0 1) (list 0 0))
                 (list (cons 0 0) (cons 1 0))
                 (list (cons 0 0) (cons 0 1))

                 44     (float 44)     (coerce 44 'double-float)
                 -44    (float -44)    (coerce -44 'double-float)
                 0      (float 0)      (coerce 0 'double-float)
                 -0     (- (float 0))  (- (coerce 0 'double-float))
                 -121   (float -121)   (coerce -121 'double-float)
                 3/4    (float 3/4)    (coerce 3/4 'double-float)
                 -3/4   (float -3/4)   (coerce -3/4 'double-float)
                 45     (float 45)     (coerce 45 'double-float)
                 441/10 (float 441/10) (coerce (float 441/10) 'double-float)

                 (expt 2 33) (expt 2.0 33) (expt 2.0d0 33)
                 (- (expt 1/2 50)) (- (expt 0.5 50)) (- (expt 0.5d0 50))
                 (+ (expt 1/2 50)) (+ (expt 0.5 50)) (+ (expt 0.5d0 50))

                 (complex 1.0 2.0) (complex 1.0d0 2.0)
                 (complex 1.5 -3/2) (complex 1.5 -1.5d0)

                 #\x #\X #\*

                 (copy-seq "foo") (copy-seq "foobar") (copy-seq "foobarbaz")

                 (copy-seq #*)
                 (copy-seq #*0) (copy-seq #*1)
                 (copy-seq #*00) (copy-seq #*10)
                 (copy-seq #*01) (copy-seq #*11)
                 (copy-seq #*10010) (copy-seq #*100101) (bit-not #*01101)
                 (make-array 6 :fill-pointer 6
                             :element-type 'bit :initial-contents #*100101)

                 #'allocate-instance #'no-applicable-method))
         (make-psxhash-extra-subtests ()
           (list (copy-seq "")
                 (copy-seq #*)
                 (copy-seq #())
                 (copy-seq ())
                 (copy-seq '(()))
                 (copy-seq #(()))
                 (copy-seq '(#()))
                 (make-array 3 :fill-pointer 0)
                 (make-array 7 :fill-pointer 0 :element-type 'bit)
                 (make-array 8 :fill-pointer 0 :element-type 'character)
                 (vector (cons 1 0) (cons 0 0))
                 (vector (cons 0 1) (cons 0 0))
                 (vector (cons 0 0) (cons 1 0))
                 (vector (cons 0 0) (cons 0 1))
                 (vector (cons 1 0) (cons 0 0))
                 (vector (cons 0 1) (cons 0 0))
                 (vector (list 0 0) (cons 1 0))
                 (vector (list 0 0) (list 0 1))
                 (vector (vector 1 0) (list 0 0))
                 (vector (vector 0 1) (list 0 0))
                 (vector (vector 0 0) (list 1 0))
                 (vector (vector 0 0) (list 0 1))
                 (vector #*00 #*10)
                 (vector (vector 0 0) (list 0 1.0d0))
                 (vector (vector -0.0d0 0) (list 1.0 0))
                 (vector 1 0 1 0)
                 (vector 0 0 0)
                 (copy-seq #*1010)
                 (copy-seq #*000)
                 (replace (make-array 101
                                      :element-type 'bit
                                      :fill-pointer 4)
                          #*1010)
                 (replace (make-array 14
                                      :element-type '(unsigned-byte 8)
                                      :fill-pointer 3)
                          #*000)
                 (replace (make-array 14
                                      :element-type t
                                      :fill-pointer 3)
                          #*000)
                 (copy-seq "abc")
                 (copy-seq "ABC")
                 (copy-seq "aBc")
                 (copy-seq "abcc")
                 (copy-seq "1001")
                 'abc
                 (vector #\a #\b #\c)
                 (vector 'a 'b 'c)
                 (vector "A" 'b 'c)
                 (replace (make-array 14
                                      :element-type 'character
                                      :fill-pointer 3)
                          "aBc")
                 (replace (make-array 11
                                      :element-type 'character
                                      :fill-pointer 4)
                          "1001")
                 (replace (make-array 12
                                      :element-type 'bit
                                      :fill-pointer 4)
                          #*1001)
                 (replace (make-array 13
                                      :element-type t
                                      :fill-pointer 4)
                          "1001")
                 (replace (make-array 13
                                      :element-type t
                                      :fill-pointer 4)
                          #*1001)
                 ;; FIXME: What about multi-dimensional arrays, hmm?

                 (make-hash-table)
                 (make-hash-table :test 'equal)

                 (make-foo)
                 (make-bar)
                 (make-bar :x (list 1))
                 (make-bar :y (list 1))))
         (t->boolean (x) (if x t nil)))
    (let* (;; Note:
           ;;   * The APPEND noise here is to help more strenuously test
           ;;     not-EQ-but-EQUAL and not-EQ-but-EQUALP cases.
           ;;   * It seems not to be worth the hassle testing SXHASH on
           ;;     values whose structure isn't understood by EQUAL, since
           ;;     we get too many false positives "SXHASHes are equal even
           ;;     though values aren't EQUAL, what a crummy hash function!"
           ;;     FIXME: Or am I misunderstanding the intent of the
           ;;     the SXHASH specification? Perhaps SXHASH is supposed to
           ;;     descend into the structure of objects even when EQUAL
           ;;     doesn't, in order to avoid hashing together things which
           ;;     are guaranteed not to be EQUAL? The definition of SXHASH
           ;;     seems to leave this completely unspecified: should
           ;;     "well-distributed" depend on substructure that EQUAL
           ;;     ignores? For our internal hash tables, the stricter
           ;;     descend-into-the-structure behavior might improve
           ;;     performance even though it's not specified by ANSI. But
           ;;     is it reasonable for users to expect it? Hmm..
           (sxhash-tests (append (make-sxhash-subtests)
                                 (make-sxhash-subtests)))
           (psxhash-tests (append sxhash-tests
                                  (make-psxhash-extra-subtests)
                                  (make-psxhash-extra-subtests))))
      ;; Check that SXHASH compiler transforms give the same results
      ;; as the out-of-line version of SXHASH.
      (let* ((fundef `(lambda ()
                        (list ,@(mapcar (lambda (value)
                                          `(sxhash ',value))
                                        sxhash-tests))))
             (fun (compile nil fundef)))
        (assert (equal (funcall fun)
                       (mapcar #'sxhash sxhash-tests))))
      ;; Note: The tests for SXHASH-equality iff EQUAL and
      ;; PSXHASH-equality iff EQUALP could fail because of an unlucky
      ;; random collision. That's not very likely (since there are
      ;; (EXPT 2 29) possible hash values and only on the order of 100
      ;; test cases, so even with the birthday paradox a collision has
      ;; probability only (/ (EXPT 100 2) (EXPT 2 29)), but it's
      ;; probably worth checking if you are getting a mystifying error
      ;; from this test. (SXHASH values and PSXHASH values don't
      ;; change from run to run, so the random chance of bogus failure
      ;; happens once every time the code is changed in such a way
      ;; that the SXHASH distribution changes, not once every time the
      ;; tests are run.)
      (dolist (i sxhash-tests)
        (declare (notinline funcall))
        (unless (typep (funcall #'sxhash i) '(and fixnum unsigned-byte))
          (error "bad SXHASH behavior for ~S" i))
        (dolist (j sxhash-tests)
          (unless (or (eq (t->boolean (equal i j))
                          (t->boolean (= (sxhash i) (sxhash j))))
                      (and (typep i 'number)
                           (typep j 'number)
                           (= i j)
                           (subtypep (type-of i) (type-of j))
                           (subtypep (type-of j) (type-of i))))
            ;; (If you get a surprising failure here, maybe you were
            ;; just very unlucky; see the notes above.)
            (error "bad SXHASH behavior for ~S ~S" i j))))
      (dolist (i psxhash-tests)
        (unless (typep (sb-int:psxhash i) '(and fixnum unsigned-byte))
          (error "bad PSXHASH behavior for ~S" i))
        (dolist (j psxhash-tests)
          (unless (eq (t->boolean (equalp i j))
                      (t->boolean (= (sb-int:psxhash i) (sb-int:psxhash j))))
            ;; (If you get a surprising failure here, maybe you were
            ;; just very unlucky; see the notes above.)
            (error "bad PSXHASH behavior for ~S ~S" i j))))
      )))

;;; As of sbcl-0.6.12.10, writing hash tables readably should work.
;;; This isn't required by the ANSI standard, but it should be, since
;;; it's well-defined useful behavior which ANSI prohibits the users
;;; from implementing themselves. (ANSI says the users can't define
;;; their own their own PRINT-OBJECT (HASH-TABLE T) methods, and they
;;; can't even wiggle out of it by subclassing HASH-TABLE or STREAM.)
(let ((original-ht (make-hash-table :test 'equal :size 111))
      (original-keys '(1 10 11 400030002 -100000000)))
  (dolist (key original-keys)
    (setf (gethash key original-ht)
          (expt key 4)))
  (let* ((written-ht (with-output-to-string (s)
                       (write original-ht :stream s :readably t)))
         (read-ht (with-input-from-string (s written-ht)
                    (read s))))
    (assert (= (hash-table-count read-ht)
               (hash-table-count original-ht)
               (length original-keys)))
    (assert (eql (hash-table-test original-ht) (hash-table-test read-ht)))
    (assert (eql (hash-table-size original-ht) (hash-table-size read-ht)))
    (dolist (key original-keys)
      (assert (eql (gethash key read-ht)
                   (gethash key original-ht))))))

;;; NIL is both SYMBOL and LIST
(dolist (fun '(sxhash sb-impl::psxhash))
  (assert (= (eval `(,fun nil))
             (funcall fun nil)
             (funcall (compile nil `(lambda (x)
                                      (declare (symbol x))
                                      (,fun x)))
                      nil)
             (funcall (compile nil `(lambda (x)
                                      (declare (list x))
                                      (,fun x)))
                      nil)
             (funcall (compile nil `(lambda (x)
                                      (declare (null x))
                                      (,fun x)))
                      nil))))

;;; success
