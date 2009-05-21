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

(use-package :test-util)
(use-package :assertoid)

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

;;; This test works reliably on non-conservative platforms and
;;; somewhat reliably on conservative platforms with threads.
#+(or (not (or x86 x86-64)) sb-thread)
(progn

(defparameter *ht* nil)

(defvar *cons-here*)

(declaim (notinline args))
(defun take (&rest args)
  (declare (ignore args)))

(defmacro alloc (&body body)
  "Execute BODY and try to reduce the chance of leaking a conservative root."
  #-sb-thread
  `(multiple-value-prog1
       (progn ,@body)
     (loop repeat 20000 do (setq *cons-here* (cons nil nil)))
     ;; KLUDGE: Clean the argument passing regs.
     (apply #'take (loop repeat 36 collect #'cons)))
  #+sb-thread
  (let ((values (gensym))
        (sem (gensym)))
    `(let ((,sem (sb-thread::make-semaphore))
           ,values)
       (sb-thread:make-thread (lambda ()
                                (setq ,values
                                      (multiple-value-list (progn ,@body)))
                                (sb-thread::signal-semaphore ,sem)))
       (sb-thread::wait-on-semaphore ,sem)
       (values-list ,values))))

(with-test (:name (:hash-table :weakness :eql :numbers))
  (flet ((random-number ()
           (random 1000)))
    (loop for weakness in '(nil :key :value :key-and-value :key-or-value) do
          (let* ((ht (make-hash-table :weakness weakness))
                 (n (alloc (loop repeat 1000
                                 count (let ((key (random-number)))
                                         (if (gethash key ht)
                                             (setf (gethash key ht)
                                                   (random-number))))))))
            (gc :full t)
            (gc :full t)
            (assert (= n (hash-table-count ht)))))))

(defun add-removable-stuff (ht &key (n 100) (size 10))
  (flet ((unique-object ()
           (make-array size :fill-pointer 0)))
    (loop for i below n do
          (multiple-value-bind (key value)
              (ecase (hash-table-weakness ht)
                ((:key) (values (unique-object) i))
                ((:value) (values i (unique-object)))
                ((:key-and-value)
                 (if (zerop (random 2))
                     (values (unique-object) i)
                     (values i (unique-object))))
                ((:key-or-value)
                 (values (unique-object) (unique-object))))
            (setf (gethash key ht) value)))
    (values)))

(defun print-ht (ht &optional (stream t))
  (format stream "Weakness: ~S~%" (sb-impl::hash-table-weakness ht))
  (format stream "Table: ~S~%" (sb-impl::hash-table-table ht))
  (format stream "Next: ~S~%" (sb-impl::hash-table-next-vector ht))
  (format stream "Index: ~S~%" (sb-impl::hash-table-index-vector ht))
  (format stream "Hash: ~S~%" (sb-impl::hash-table-hash-vector ht))
  (force-output stream))

(with-test (:name (:hash-table :weakness :removal))
  (loop for test in '(eq eql equal equalp) do
        (format t "test: ~A~%" test)
        (loop for weakness in '(:key :value :key-and-value :key-or-value)
              do
              (format t "weakness: ~A~%" weakness)
              (let ((ht (make-hash-table :test 'equal :weakness weakness)))
                (alloc (add-removable-stuff ht :n 117 :size 1))
                (loop for i upfrom 0
                      do (format t "~A. count: ~A~%" i (hash-table-count ht))
                      (force-output)
                      until (zerop (hash-table-count ht))
                      do
                      (when (= i 10)
                        (print-ht ht)
                        #-(or x86 x86-64)
                        (assert nil)
                        ;; With conservative gc the test may not be
                        ;; bullet-proof so it's not an outright
                        ;; failure but a warning.
                        #+(or x86 x86-64)
                        (progn
                          (warn "Weak hash removal test failed for weakness ~A"
                                weakness)
                          (return)))
                      (gc :full t))))))

(with-test (:name (:hash-table :weakness :string-interning))
  (let ((ht (make-hash-table :test 'equal :weakness :key))
        (s "a"))
    (setf (gethash s ht) s)
    (assert (eq (gethash s ht) s))
    (assert (eq (gethash (copy-seq s) ht) s))))

;;; see if hash_vector is not written when there is none ...
(with-test (:name (:hash-table :weakness :eq))
  (loop repeat 10 do
        (let ((index (random 2000)))
          (let ((first (+ most-positive-fixnum (mod (* index 31) 9)))
                (n 50000))
            (let ((hash-table (make-hash-table :weakness :key :test 'eq)))
              (dotimes (i n)
                (setf (gethash (+ first i) hash-table) i))
              hash-table)))))

;; used to crash in gc
(with-test (:name (:hash-table :weakness :keep))
  (loop repeat 2 do
        (let ((h1 (make-hash-table :weakness :key :test #'equal))
              (keep ()))
          (loop for i from 0 to 1000
                for key = i
                for value = (make-array 10000 :fill-pointer 0)
                do
                (push value keep)
                (setf (gethash key h1) value))
          (sb-ext:gc :full t))))

)

;;; DEFINE-HASH-TABLE-TEST

(defstruct custom-hash-key name)
(defun custom-hash-test (x y)
  (equal (custom-hash-key-name x)
         (custom-hash-key-name y)))
(defun custom-hash-hash (x)
  (sxhash (custom-hash-key-name x)))
(define-hash-table-test custom-hash-test custom-hash-hash)
(with-test (:name define-hash-table-test.1)
  (let ((table (make-hash-table :test 'custom-hash-test)))
    (setf (gethash (make-custom-hash-key :name "foo") table) :foo)
    (setf (gethash (make-custom-hash-key :name "bar") table) :bar)
    (assert (eq :foo (gethash (make-custom-hash-key :name "foo") table)))
    (assert (eq :bar (gethash (make-custom-hash-key :name "bar") table)))
    (assert (eq 'custom-hash-test (hash-table-test table))))
  (let ((table (make-hash-table :test #'custom-hash-test)))
    (setf (gethash (make-custom-hash-key :name "foo") table) :foo)
    (setf (gethash (make-custom-hash-key :name "bar") table) :bar)
    (assert (eq :foo (gethash (make-custom-hash-key :name "foo") table)))
    (assert (eq :bar (gethash (make-custom-hash-key :name "bar") table)))
    (assert (eq 'custom-hash-test (hash-table-test table)))))


(defun head-eql (x y)
  (every #'eql (subseq x 0 3) (subseq y 0 3)))
(define-hash-table-test head-eql
    (lambda (x)
      (logand most-positive-fixnum
              (reduce #'+ (map 'list #'sxhash (subseq x 0 3))))))
(with-test (:name define-hash-table-test.2)
  (let ((table (make-hash-table :test 'head-eql)))
    (setf (gethash #(1 2 3 4) table) :|123|)
    (setf (gethash '(2 3 4 7) table) :|234|)
    (setf (gethash "foobar" table) :foo)
    (assert (eq :|123| (gethash '(1 2 3 ! 6) table)))
    (assert (eq :|234| (gethash #(2 3 4 0 2 1 a) table)))
    (assert (eq :foo (gethash '(#\f #\o #\o 1 2 3) table)))
    (assert (eq 'head-eql (hash-table-test table))))
  (let ((table (make-hash-table :test #'head-eql)))
    (setf (gethash #(1 2 3 4) table) :|123|)
    (setf (gethash '(2 3 4 7) table) :|234|)
    (setf (gethash "foobar" table) :foo)
    (assert (eq :|123| (gethash '(1 2 3 ! 6) table)))
    (assert (eq :|234| (gethash #(2 3 4 0 2 1 a) table)))
    (assert (eq :foo (gethash '(#\f #\o #\o 1 2 3) table)))
    (assert (eq 'head-eql (hash-table-test table)))))

(with-test (:name make-hash-table/hash-fun)
  (let ((table (make-hash-table
                :test #'=
                :hash-function (lambda (x)
                                 (sxhash (coerce (abs x) 'double-float))))))
    (incf (gethash 1 table 0))
    (incf (gethash 1.0f0 table))
    (incf (gethash 1.0d0 table))
    (incf (gethash (complex 1.0f0 0.0f0) table))
    (incf (gethash (complex 1.0d0 0.0d0) table))
    (assert (= 5 (gethash 1 table)))
    (assert (eq '= (hash-table-test table)))))

;;; success
