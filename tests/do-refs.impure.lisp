;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package sb-vm)

(test-util:with-test (:name :safe-layoutless-instance)
  (assert (not (sb-vm::references-p (sb-kernel:%make-instance 5) '(foo)))))

(defun collect-slot-values (obj &aux result)
  (flet ((slots (x)
           (push x result)))
    (do-referenced-object (obj slots))
    (nreverse result)))

(defun walk-slots-test (obj expect)
  (assert (equal (collect-slot-values obj) expect)))
(defun walk-slots-test* (obj test)
  (assert (funcall test (collect-slot-values obj))))

(defstruct foo (z 0 :type sb-ext:word) (x 'x) (y 'y))

(test-util:with-test (:name :walk-slots-trivial ; lists and vectors
                            :fails-on :interpreter)
  (walk-slots-test '(a . b) '(a b))
  (walk-slots-test #(a b c) '(a b c))
  (walk-slots-test #(a b c d) '(a b c d))
  (walk-slots-test (make-foo) `(,(find-layout 'foo) x y))
  )

(test-util:with-test (:name :walk-slots-numbers
                      :fails-on :interpreter)
  (let ((c #c(45d0 33d0)))
    (walk-slots-test c nil))
  (let ((r 22/7))
    (walk-slots-test r '(22 7))))

(test-util:with-test (:name :walk-slots-fancy-array)
  (let* ((inner (make-array 10 :element-type 'character))
         (a (make-array 10 :element-type 'character :displaced-to inner)))
    (walk-slots-test a (list inner t nil))))

(test-util:with-test (:name :walk-slots-symbol
                      :fails-on :interpreter)
  (let* ((name "ZOT")
         (s (make-symbol name))
         (info '((bork 42))))
    (import s "CL-USER")
    (set s 'hi)
    (setf (symbol-plist s) (car info) info (sb-kernel:symbol-%info s))
    ;; ASSUMPTION: slot ordering
    (walk-slots-test s `(hi ,info ,name ,(find-package "CL-USER")))))

(test-util:with-test (:name :walk-slots-closure)
  (let ((c (funcall (compile nil '(lambda (a b c)
                                   (lambda (x) (+ x (incf a) (incf b) (incf c)))))
                    8 9 10)))
    (walk-slots-test* c
                      (lambda (x)
                        (and (= (length x) 4)
                             (functionp (first x))
                             (not (find sb-vm:value-cell-widetag (cdr x)
                                        :key 'widetag-of :test #'/=)))))))

(test-util:with-test (:name :walk-slots-fdefn)
  (let* ((closure (funcall (compile nil '(lambda (x)  (lambda () x))) t))
         (fname `(cas ,(gensym))))
    (setf (fdefinition fname) closure)
    (walk-slots-test*
     (sb-int:find-fdefn fname)
     (lambda (slots)
       (and (= (length slots) 2)
            (equal (first slots) fname)
            (closurep (second slots)))))))

(defclass mystdinst ()
  ((a :initform 1) (b :initform 2)
   (c :initform 3) (d :initform 4) (e :initform 5)))

(test-util:with-test (:name :walk-slots-standard-instance
                            :fails-on :interpreter)
  (let ((o (make-instance 'mystdinst)))
    (walk-slots-test* o
                      (lambda (slots)
                        (destructuring-bind (layout clos-slots) slots
                          (and (eq layout (%instance-layout o))
                               (eq clos-slots (sb-pcl::std-instance-slots o))))))))

(define-condition cfoo (simple-condition) ((a :initarg :a) (b :initarg :b) (c :initform 'c)))
(test-util:with-test (:name :walk-slots-condition-instance
                            :fails-on :interpreter)
  (let ((instance (make-condition 'cfoo :a 'ay :b 'bee :format-arguments "wat")))
    (walk-slots-test instance
                     `(,(find-layout 'cfoo) ((c . c) (format-control . nil))
                       :a  ay :b bee :format-arguments "wat"))))

(defun make-random-funinstance (&rest values)
  (let* ((ctor (apply #'sb-pcl::%make-ctor values))
         (layout (sb-kernel:%fun-layout ctor)))
    ;; If the number of payload words is even, then there's a padding word
    ;; because adding the header makes the unaligned total an odd number.
    ;; Fill that padding word with something - it should not be visible.
    ;; Whether GC should trace the word is a different question,
    ;; on whose correct answer I waver back and forth.
    (when (evenp (sb-kernel:get-closure-length ctor)) ; payload length
      (let ((max (reduce #'max (sb-kernel:dd-slots (sb-kernel:layout-dd layout))
                         :key 'sb-kernel:dsd-index)))
        (setf (sb-kernel:%funcallable-instance-info ctor (1+ max))
              (elt sb-vm:+static-symbols+ 0))))
    ;; stuff in a random function as the implementation
    (setf (sb-kernel:%funcallable-instance-fun ctor) #'error)
    ctor))
(compile 'make-random-funinstance)

(test-util:with-test (:name :walk-slots-pcl-ctor)
  (let* ((slot-vals '("A" "B" "C" "D" "E" "F"))
         (f (apply #'make-random-funinstance slot-vals)))
    (walk-slots-test f `(,(find-layout 'sb-pcl::ctor) ,#'error ,@slot-vals))))

#+sb-fasteval
(test-util:with-test (:name :walk-slots-interpreted-fun)
  (let ((f (let ((sb-ext:*evaluator-mode* :interpret))
             (eval '(lambda (x y z))))))
    (funcall f 1 2 3) ; compute the digested slots
    (walk-slots-test* f
                      (lambda (slots)
                        (destructuring-bind (type fin-fun a b c d) slots
                          (declare (ignore a b c))
                          (and (typep type 'layout)
                               (typep fin-fun 'closure)
                               (typep d '(and integer (not (eql 0))))))))))

(test-util:with-test (:name :deep-sizer)
  (multiple-value-bind (tot-bytes n-kids)
      (test-util:deep-size #(a b c d (e f) #*0101))
    ;; 8 words for the vector
    ;; 4 words for 2 conses
    ;; 4 words for a bit-vector: header/length/bits/padding
    (assert (= tot-bytes (* 16 n-word-bytes)))
    ;; 2 conses and 1 bit-vector
    (assert (= n-kids 3))))

(defvar *some-symbol* 'a)
(test-util:with-test (:name :symbol-refs
                      :fails-on :interpreter)
  (sb-int:collect ((results))
    (let ((*some-symbol* 'b))
      (do-referenced-object ('*some-symbol* results)))
    (assert (eq (first (results)) #+sb-thread 'a
                                  #-sb-thread 'b))))
