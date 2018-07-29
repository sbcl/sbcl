;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package sb-vm)

(defun collect-slot-values (obj)
  (collect ((slots))
    (do-referenced-object (obj slots))
    (slots)))

(defun walk-slots-test (obj expect)
  (assert (equal (collect-slot-values obj) expect)))
(defun walk-slots-test* (obj test)
  (assert (funcall test (collect-slot-values obj))))

(defstruct foo (z 0 :type sb-ext:word) (x 'x) (y 'y))

(test-util:with-test (:name :walk-slots-trivial) ; lists and vectors
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
    (setf (symbol-info s) info)
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
  (walk-slots-test*
   (sb-kernel::find-fdefn 'constantly-t)
   (lambda (slots)
     #+immobile-code
     (and (= (length slots) 3)
          (symbolp (first slots))
          (closurep (second slots))
          (code-component-p (third slots)))
     #-immobile-code
     (and (= (length slots) 2)
          (symbolp (first slots))
          (closurep (second slots))))))

(defclass mystdinst ()
  ((a :initform 1) (b :initform 2)
   (c :initform 3) (d :initform 4) (e :initform 5)))

(test-util:with-test (:name :walk-slots-standard-instance)
  (let ((o (make-instance 'mystdinst)))
    (walk-slots-test* o
                      (lambda (slots)
                        (if (= (length slots) 3)
                            (destructuring-bind (layout vect hash) slots
                              (and (eq layout (%instance-layout o))
                                   (equalp vect #(1 2 3 4 5))
                                   (integerp hash)))
                            ;; hash code in the upper half of the vector header word
                            (destructuring-bind (layout vect) slots
                              (and (eq layout (%instance-layout o))
                                   (equalp vect #(1 2 3 4 5)))))))))


(define-condition cfoo (simple-condition) ((a :initarg :a) (b :initarg :b) (c :initform 'c)))
(test-util:with-test (:name :walk-slots-condition-instance)
  (let ((instance (make-condition 'cfoo :a 'ay :b 'bee :format-arguments "wat")))
    (walk-slots-test instance
                     `(,(find-layout 'cfoo)
                       (c c format-control nil)
                       ,(sxhash instance)
                       :a  ay :b bee :format-arguments "wat"))))

(test-util:with-test (:name :walk-slots-pcl-ctor
                      :fails-on :interpreter)
  (let* ((slot-vals '("A" "B" "C" "D" "E" "F"))
         (f (apply (compile nil '(lambda (&rest args)
                                  (let ((ctor (apply #'sb-pcl::%make-ctor args)))
                                    (setf (%funcallable-instance-function ctor) #'error)
                                    ctor)))
                   slot-vals)))
    (walk-slots-test f `(,(find-layout 'sb-pcl::ctor) ,#'error ,@slot-vals))))

#+sb-fasteval
(test-util:with-test (:name :walk-slots-interpreted-fun
                      :fails-on :interpreter)
  (let ((f (let ((sb-ext:*evaluator-mode* :interpret))
             (eval '(lambda (x y z))))))
    (funcall f 1 2 3) ; compute the digested slots
    (walk-slots-test* f
                      (lambda (slots)
                        (destructuring-bind (layout fin-fun a b c d) slots
                          (declare (ignore a b c))
                          (and (typep layout 'layout)
                               (typep fin-fun 'closure)
                               (typep d '(and integer (not (eql 0))))))))))

;;; Compute size of OBJ including descendants.
;;; LEAFP specifies what object types to treat as not reaching
;;; any other object. You pretty much have to treat symbols
;;; as leaves, otherwise you reach a package and then the result
;;; just explodes to beyond the point of being useful.
;;; (It works, but might reach the entire heap)
;;; To turn this into an actual thing, we'd want to reduce the consing.
(defun deep-size (obj &optional (leafp (lambda (x)
                                         (typep x '(or symbol layout fdefn
                                                       classoid)))))
  (let ((worklist (list obj))
        (seen (make-hash-table :test 'eq))
        (tot-bytes 0))
    (setf (gethash obj seen) t)
    (flet ((visit (thing)
             (when (is-lisp-pointer (get-lisp-obj-address thing))
               (unless (or (funcall leafp thing)
                           (gethash thing seen))
                 (push thing worklist)
                 (setf (gethash thing seen) t)))))
      (loop
        (unless worklist (return))
        (let ((x (pop worklist)))
          (incf tot-bytes (primitive-object-size x))
          (do-referenced-object (x visit)))))
    ;; Secondary values is number of visited objects not incl. original one.
    (values tot-bytes
            (1- (hash-table-count seen))
            seen)))

(test-util:with-test (:name :deep-sizer)
  (multiple-value-bind (tot-bytes n-kids)
      (deep-size #(a b c d (e f) #*0101))
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
