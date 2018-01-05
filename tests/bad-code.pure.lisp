(in-package :cl-user)

(with-test (:name (make-array :bad-initial-contents))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (make-array '(1) :initial-contents 'foo))
               :allow-warnings t))))

(with-test (:name (make-string-output-stream :bad-element-type))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-string-output-stream :element-type '((x))))
               :allow-warnings t))))

(with-test (:name (coerce :bad-type-specifier))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (coerce (list 2) 1))
               :allow-warnings t))))

(with-test (:name :zombie-entry-point-reference)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (labels ((%f ())) (%f #'%f)))
               :allow-warnings t))))
(with-test (:name :ir1-optimize-combination-dead-node)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (flet ((%f2 (x) x))
                    (list (%f2 1)
                          (multiple-value-call #'%f2 (values)))))
               :allow-warnings t))))

(with-test (:name (:bogus-block &key))
  (assert
   (nth-value 1
              (checked-compile `(lambda (&key (x (block 1 10))) x)
                               :allow-failure t))))

(with-test (:name :type-error-reporting)
  (assert
   (nth-value 1
              (checked-compile `(lambda ()
                                  (lambda ()
                                    (let ((v3 0))
                                      (cdr (1- (block b5 (if nil v3 0)))))))
                               :allow-warnings t))))

(with-test (:name :dx-on-deleted-nodes)
  (assert
   (nth-value 1
              (checked-compile `(lambda ()
                                  (restart-bind ((1 3))))
                               :allow-warnings t))))

(with-test (:name :transform-call-dfo-consistency)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (flet ((%f (&optional x) x))
                    (%f)
                    ;; Two of the %f calls are erroneous, with an extra argument
                    (flet ((%f6 (&key (k (%f (%f -1 (%f -2 -3))))) 0))
                      5)))
               :allow-warnings t))))

(with-test (:name :&aux-check-variable-names)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (&aux (nil 10))
                  nil)
               :allow-failure t))))

(with-test (:name :mv-call-too-many-values)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (a)
                  (flet ((%f1 (x) x))
                    (apply #'%f1 a 2 (list 0))))
               :allow-warnings t))))

(with-test (:name :mv-call-too-many-values)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array (list 'x)))
               :allow-warnings t))))

(with-test (:name (map :values-type))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (map '* #'+ #(1) #(2)))
               :allow-warnings t))))


(with-test (:name :bad-type-specifier)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array 10 :element-type '((x))))
               :allow-warnings t))))

(with-test (:name (make-array :bad-dimensions))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(x)))
               :allow-warnings t)))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(-10)))
               :allow-warnings t))))

(with-test (:name (make-array :initial-contents :bad-macro))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(10) :initial-contents (do)))
               :allow-failure t))))

(with-test (:name :&rest-ref-bad-n)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (&rest a) (lambda () (nth nil a)))
               :allow-warnings t))))

(with-test (:name :bad-type-specifier-handling)
  (multiple-value-bind (fun failure warnings)
      (checked-compile
       `(lambda (v) (typep v '(unsigned-byte 8 x (error ~s v))))
       :allow-warnings t)
    (declare (ignore fun))
    (assert failure)
    (mapcar #'princ-to-string warnings)))
