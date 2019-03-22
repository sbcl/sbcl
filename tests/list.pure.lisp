;;;; tests related to lists

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

(enable-test-parallelism)

;;; Since *another* BUTLAST problem was reported (anonymously!) on the
;;; SourceForge summary page magical bugs web interface 2001-09-01, it
;;; looks as though it's past time to start accumulating regression
;;; tests for these.
(with-test (:name (butlast nbutlast :structure-sharing))
  (dolist (testcase
            '((:args ((1 2 3 4 5))   :result (1 2 3 4))
              (:args ((1 2 3 4 5) 6) :result nil)
              (:args (nil)           :result nil)
              (:args ((1 2 3) 0)     :result (1 2 3))
              (:args ((1 2 3) 1)     :result (1 2))
              (:args ((1 2 3))       :result (1 2))
              (:args ((1 2 3) 2)     :result (1))
              (:args ((1 2 3) 3)     :result nil)
              (:args ((1 2 3) 4)     :result nil)
              (:args ((1 2 3 . 4) 0) :result (1 2 3 . 4))
              (:args ((1 2 3 . 4) 1) :result (1 2))
              (:args ((1 2 3 . 4))   :result (1 2))
              (:args ((1 2 3 . 4) 2) :result (1))
              (:args ((1 2 3 . 4) 3) :result nil)
              (:args ((1 2 3 . 4) 4) :result nil)))
    (destructuring-bind (&key args result) testcase
      (destructuring-bind (list &rest rest) args
        ;; Test with BUTLAST.
        (let ((actual-result (apply #'butlast args)))
          (when (and (consp list) (eq actual-result list))
            (error "not a copy in BUTLAST for ~S" args))
          (unless (equal actual-result result)
            (error "failed BUTLAST for ~S" args)))
        ;; Test with NBUTLAST.
        (let* ((copied-list (copy-list list))
               (actual-result (apply #'nbutlast copied-list rest)))
          (unless (equal actual-result result)
            (error "failed NBUTLAST for ~S" args)))))))

(with-test (:name (butlast type-error))
  (assert-error (apply #'butlast (list t)) type-error))

;;; reported by Paul Dietz on cmucl-imp: LDIFF does not check type of
;;; its first argument
(with-test (:name (ldiff type-error))
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile '(lambda () (ldiff 1 2))
                       :allow-failure t :allow-warnings t)
    (assert failure-p)
    (assert (= 1 (length warnings)))
    (assert (typep (first warnings) 'sb-int:type-warning))
    (assert-error (funcall fun) type-error)))

;;; evaluation order in PUSH, PUSHNEW
(with-test (:name (push :evaluation-order.1))
  (let ((a (map 'vector #'list '(a b c)))
        (i 0))
    (pushnew (incf i) (aref a (incf i)))
    (assert (equalp a #((a) (b) (1 c))))))

(with-test (:name (push pushnew :evaluation-order.2))
  (symbol-macrolet ((s (aref a (incf i))))
    (let ((a (map 'vector #'list '(a b c)))
          (i 0))
      (push t s)
      (assert (equalp a #((a) (t b) (c))))
      (pushnew 1 s)
      (assert (equalp a #((a) (t b) (1 c))))
      (setq i 0)
      (assert (eql (pop s) 't))
      (assert (equalp a #((a) (b) (1 c)))))))

;;; Type checking in NCONC
(with-test (:name (nconc :improper-list))
 (let ((tests '((((1 . 2)) (1 . 2))
                (((1 . 2) (3 . 4)) (1 3 . 4))
                (((1 . 2) 3) (1 . 3))
                ((3) 3))))
   (loop for (args result) in tests
      do (assert (equal (apply 'nconc (copy-tree args)) result))
      do (let* ((exp `(nconc ,@ (mapcar (lambda (arg)
                                          `(copy-tree ',arg))
                                        args))))
           (checked-compile-and-assert ()
               `(lambda () ,exp)
             (() result))))))

(with-test (:name (nconc :improper-list type-error))
  (let ((tests '(((3 (1 . 2)) 3)
                 (((1 . 2) 3 (4 . 5)) 3))))
    (macrolet ((check-error (form failed-arg)
                 `(multiple-value-bind (.result. .error.)
                      (ignore-errors ,form)
                    (assert (null .result.))
                    (assert (typep .error. 'type-error))
                    (assert (eq (type-error-expected-type .error.) 'list))
                    (assert (equal (type-error-datum .error.) ,failed-arg)))))
      (loop for (args fail) in tests
         do (check-error (apply #'nconc (copy-tree args)) fail)
         do (let* ((exp `(nconc ,@(mapcar (lambda (arg)
                                            `(copy-tree ',arg))
                                          args)))
                   (fun (checked-compile `(lambda () ,exp))))
              (check-error (funcall fun) fail))))))

(with-test (:name (append nreverse nreverse nreconc copy-alist type-error))
  (dolist (test '((append 1 2)
                  (append (1 2) nil (3 . 4) nil)
                  (append nil (1 2) nil (3 . 4) nil)
                  (reverse (1 2 . 3))
                  (nreverse (1 2 . 3))
                  (nreconc (1 2 . 3) (4 5))
                  (copy-alist ((1 . 2) (3 . 4) . 5))))
    (assert-error (apply (first test) (copy-tree (rest test)))
                  type-error)))

;;; Bug reported by Paul Dietz: NSET-EXCLUSIVE-OR should not return
;;; extra elements, even when given "sets" contain duplications
(with-test (:name (nset-exclusive-or :duplicates))
  (assert (equal (remove-duplicates (sort (nset-exclusive-or (list 1 2 1 3)
                                                             (list 4 1 3 3))
                                          #'<))
                 '(2 4))))

;;; Bug reported by Adam Warner: valid list index designator is not
;;; necessarily a fixnum
(with-test (:name (nth bignum))
  (let ((s (read-from-string "(a . #1=(b c . #1#))")))
    (assert (eq (nth (* 1440 most-positive-fixnum) s) 'c))
    (setf (nth (* 1440 most-positive-fixnum) s) 14)
    (assert (eq (nth (* 1440 most-positive-fixnum) s) 14)))

  (let ((s (copy-list '(1 2 3))))
    (assert (eq s (last s (* 1440 most-positive-fixnum))))
    (assert (null (butlast s (* 1440 most-positive-fixnum))))
    (assert (null (nbutlast s (* 1440 most-positive-fixnum))))))

(assert (eq :atom (last (list* 1 2 3 :atom) (eval 0))))
(assert (eq :atom (last (list* 1 2 3 :atom) 0)))

;;; enforce lists in symbol-plist
(with-test (:name symbol-plist)
 (let ((s (gensym))
       (l (list 1 3 4)))
   (assert (not (symbol-plist s)))
   (assert (eq l (setf (symbol-plist s) l)))
   (assert-error (setf (symbol-plist s) (car l)) type-error)))

;;; member

(with-test (:name member)
  (macrolet ((test  (expected form)
               `(progn
                  (assert (equal ,expected (let ((numbers '(1 2)))
                                             (funcall fun ,@(cdr form)))))
                  (assert (equal ,expected (funcall (lambda ()
                                                      (declare (optimize speed))
                                                      (let ((numbers '(1 2)))
                                                        ,form)))))
                  (assert (equal ,expected (funcall (lambda ()
                                                      (declare (optimize space))
                                                      (let ((numbers '(1 2)))
                                                        ,form))))))))
    (let ((x-numbers '(1 2))
          (fun (car (list 'member))))
      (test x-numbers (member 1 numbers))
      (test x-numbers (member 1 numbers :key 'identity))
      (test x-numbers (member 1 numbers :key #'identity))
      (test (cdr x-numbers) (member 2 numbers))
      (test nil (member 1.0 numbers ))

      (test x-numbers (member 1.0 numbers :test #'=))
      (test x-numbers (member 1.0 numbers :test #'= :key nil))
      (test (cdr x-numbers) (member 2.0 numbers :test '=))
      (test nil (member 0 numbers :test '=))

      (test x-numbers (member 0 numbers :test-not #'>))
      (test (cdr x-numbers) (member 1 numbers :test-not 'eql))
      (test nil (member 0 numbers :test-not '<))

      (test x-numbers (member -1 numbers :key #'-))
      (test (cdr x-numbers) (member -2 numbers :key '-))
      (test nil (member -1.0 numbers :key #'-))

      (test x-numbers (member -1.0 numbers :key #'- :test '=))
      (test (cdr x-numbers) (member -2.0 numbers :key #'- :test '=))
      (test nil (member -1.0 numbers :key #'- :test 'eql)))))

(flet ((test (function needle haystack args expected)
         (checked-compile-and-assert ()
             `(lambda ()
                (let ((function (car (list ',function)))
                      (list ',haystack))
                  (funcall function ,needle list ,@args)))
           (() expected))
         (checked-compile-and-assert ()
             `(lambda ()
                (let ((list ',haystack))
                  (,function ,needle list ,@args)))
           (() expected))))

  (with-test (:name assoc :serial t)
    (let ((numbers '((1 a) (2 b)))
          (tricky '(nil (a . b) nil (nil . c) (c . d))))
      (test 'assoc 1    numbers '()                    '(1 a))
      (test 'assoc 2    numbers '()                    '(2 b))
      (test 'assoc 1    numbers '(:key 'identity)      '(1 a))
      (test 'assoc 2    numbers '(:key #'identity)     '(2 b))
      (test 'assoc 1.0  numbers '()                    nil)

      (test 'assoc 1.0  numbers '(:test #'=)           '(1 a))
      (test 'assoc 1.0  numbers '(:test #'= :key nil)  '(1 a))
      (test 'assoc 2.0  numbers '(:test '=)            '(2 b))
      (test 'assoc 0    numbers '(:test '=)            nil)

      (test 'assoc 0    numbers '(:test-not #'>)       '(1 a))
      (test 'assoc 1    numbers '(:test-not 'eql)      '(2 b))
      (test 'assoc 0    numbers '(:test-not '<)        nil)

      (test 'assoc -1   numbers '(:key #'-)            '(1 a))
      (test 'assoc -2   numbers '(:key '-)             '(2 b))
      (test 'assoc -1.0 numbers '(:key #'-)            nil)

      (test 'assoc -1.0 numbers '(:key #'- :test '=)   '(1 a))
      (test 'assoc -2.0 numbers '(:key #'- :test '=)   '(2 b))
      (test 'assoc -1.0 numbers '(:key #'- :test 'eql) nil)

      ;; Bug reported by Paul Dietz: ASSOC should ignore NIL elements
      ;; in a alist
      (test 'assoc nil tricky  '(:test #'eq)      '(nil . c))))

  (with-test (:name rassoc :serial t)
    (let ((numbers '((a . 1) (b . 2)))
          (tricky '(nil (b . a) nil (c . nil) (d . c))))
      (test 'rassoc 1    numbers '()                    '(a . 1))
      (test 'rassoc 2    numbers '()                    '(b . 2))
      (test 'rassoc 1    numbers '(:key 'identity)      '(a . 1))
      (test 'rassoc 2    numbers '(:key #'identity)     '(b . 2))
      (test 'rassoc 1.0  numbers '()                    nil)

      (test 'rassoc 1.0  numbers '(:test #'=)           '(a . 1))
      (test 'rassoc 1.0  numbers '(:test #'= :key nil)  '(a . 1))
      (test 'rassoc 2.0  numbers '(:test '=)            '(b . 2))
      (test 'rassoc 0    numbers '(:test '=)            nil)

      (test 'rassoc 0    numbers '(:test-not #'>)       '(a . 1))
      (test 'rassoc 1    numbers '(:test-not 'eql)      '(b . 2))
      (test 'rassoc 0    numbers '(:test-not '<)        nil)

      (test 'rassoc -1   numbers '(:key #'-)            '(a . 1))
      (test 'rassoc -2   numbers '(:key '-)             '(b . 2))
      (test 'rassoc -1.0 numbers '(:key #'-)            nil)

      (test 'rassoc -1.0 numbers '(:key #'- :test '=)   '(a . 1))
      (test 'rassoc -2.0 numbers '(:key #'- :test '=)   '(b . 2))
      (test 'rassoc -1.0 numbers '(:key #'- :test 'eql) nil)

      (test 'rassoc nil  tricky  '(:test #'eq)          '(c . nil)))))

;;;; member-if & assoc-if & rassoc-if
(with-test (:name (member-if assoc-if rassoc-if) :slow t)
  (macrolet ((test (value form)
               `(let ((* ,value))
                  (assert (eval ,form))
                  (checked-compile-and-assert (:optimize :safe)
                      '(lambda () ,form)
                    (() t)))))
    (test 'evenp
          (equal '(2 3 4) (member-if * (list 1 2 3 4))))
    (test 'evenp
          (equal '(2 3 4) (locally (declare (optimize speed))
                            (member-if * '(1 2 3 4)))))
    (test 'evenp
          (equal '(3 4) (member-if * (list 1 2 3 4) :key (lambda (x) (if (= 3 x) 2 1)))))
    (test 'evenp
          (equal '(2 :two) (assoc-if * (list (list 1 :one) (list 3 :three) (list 2 :two) (list 4 :four)))))
    (test 'evenp
          (equal '(3 :three) (assoc-if * (list (list 1 :one) (list 3 :three) (list 2 :two) (list 4 :four))
                                       :key (lambda (x) (if (= 3 x) 2 1)))))
    (test 'evenp
          (equal '(:two . 2) (rassoc-if * (list '(:one . 1) '(:three . 3) '(:two . 2) '(:four . 4)))))
    (test (list 1 2 3 4)
          (equal '(2 3 4) (member-if 'evenp *)))
    (test (list (cons 1 'a) (cons 2 'b) (cons 3 'c))
          (equal (cons 2 'b) (assoc-if 'evenp *)))))

;;;; member-if-not & assoc-if-not
(with-test (:name (member-if-not assoc-if-not) :slow t)
  (macrolet ((test (value form)
               `(let ((* ,value))
                  (assert (eval ,form))
                  (checked-compile-and-assert ()
                      '(lambda () ,form)
                    (() t)))))
    (test 'oddp
          (equal '(2 3 4) (member-if-not * (list 1 2 3 4))))
    (test 'oddp
          (equal '(2 3 4) (locally (declare (optimize speed))
                            (member-if-not * '(1 2 3 4)))))
    (test 'oddp
          (equal '(3 4) (member-if-not * (list 1 2 3 4) :key (lambda (x) (if (= 3 x) 2 1)))))
    (test 'oddp
          (equal '(2 :two) (assoc-if-not * (list (list 1 :one) (list 3 :three) (list 2 :two) (list 4 :four)))))
    (test 'oddp
          (equal '(3 :three) (assoc-if-not * (list (list 1 :one) (list 3 :three) (list 2 :two) (list 4 :four))
                                           :key (lambda (x) (if (= 3 x) 2 1)))))
    (test (list 1 2 3 4)
          (equal '(2 3 4) (member-if-not 'oddp *)))
    (test (list (cons 1 'a) (cons 2 'b) (cons 3 'c))
          (equal (cons 2 'b) (assoc-if-not 'oddp *)))))

;;; bug reported by Dan Corkill: *PRINT-CASE* affected the compiler transforms
;;; for ASSOC & MEMBER
(with-test (:name (assoc member *print-case*))
  (let ((*print-case* :downcase))
    (checked-compile-and-assert ()
        `(lambda (i l) (assoc i l))
      ((:b '((:a . 1) (:b . 2))) '(:b . 2)))
    (checked-compile-and-assert ()
        `(lambda (i l) (member i l))
      ((3 '(1 2 3 4 5)) '(3 4 5)))))

;;; bad bounding index pair to SUBSEQ on a list
(with-test (:name (subseq sb-kernel:bounding-indices-bad-error))
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile `(lambda ()
                          (let ((list (list 0 1 2 3 4 5)))
                            (subseq list 4 2)))
                       :allow-warnings t)
    (assert failure-p)
    (assert (= (length warnings) 1))
    (assert-error (funcall fun)  sb-kernel:bounding-indices-bad-error)))

;;; ADJOIN must apply key to item as well
(with-test (:name (adjoin :key))
  (checked-compile-and-assert ()
      `(lambda (x y)
         (adjoin x y :key #'car :test #'string=))
    (((list 'b) (list '(:b))) '((:b))))

  #+(or sb-eval sb-fasteval)
  (assert (equal '((:b))
                 (let ((sb-ext:*evaluator-mode* :interpret))
                   (eval '(adjoin (list 'b) (list '(:b))
                                  :key #'car :test #'string=))))))

;;; constant list argument to ADJOIN
(with-test (:name (adjoin :constant :list-argument))
  (flet ((test (form args expected)
           (let ((fun (checked-compile form)))
             (assert (equal expected (apply fun args))))))
    (test `(lambda (elt)
             (declare (optimize speed))
             (adjoin elt '(:x :y)))
          '(:x) '(:x :y))
    (test `(lambda (elt)
             (declare (optimize speed))
             (adjoin elt '(:y)))
          '(:x) '(:x :y))
    (test `(lambda () (adjoin 'a nil)) '() '(a))))

(with-test (:name union)
  (macrolet ((test (expected list-1 list-2 &rest args)
               `(progn
                  (assert (equal ,expected (funcall #'union ,list-1 ,list-2 ,@args)))
                  (assert (equal ,expected (funcall #'nunion
                                                    (copy-list ,list-1)
                                                    (copy-list ,list-2)
                                                    ,@args))))))
    (test nil nil nil)
    (test '(42) nil '(42))
    (test '(42) '(42) nil)
    (test '(42) '(42) '(42))
    (test '((42) (42)) '((42)) '((42)))
    (test '((42) (42)) '((42)) '((42)) :test-not #'equal)
    (test '((42)) '((42)) '((42)) :test #'equal)
    (test '((42)) '((42)) '((42)) :key #'car)
    (test '((42)) '((42)) '((42)) :key #'car :test-not #'<)))

;;; FIND on lists should not call key outside the specified
;;; subsequence.
(with-test (:name (find :start :end))
  (assert (not (find :a '(0 (:c) 1) :start 1 :end 2 :key #'car))))

(with-test (:name (adjoin :folding))
  (flet ((%f () (adjoin 'x '(a b))))
    (assert (not (eq (%f) (%f))))))

(with-test (:name (butlast :dotted))
  (assert (null (butlast '(1 2 . 3) 4)))
  (assert (null (nbutlast (list* 1 2 3) 4))))

(with-test (:name :tree-equal)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (tree-equal a '(a (b c) (3/4 (d))) :test #'eql))
    (('(a (b c) (3/4 (d)))) t)
    (('(a (b c) (3/4 (d) e))) nil)))
