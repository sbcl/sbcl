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

(in-package :cl-user)

;;; Since *another* BUTLAST problem was reported (anonymously!) on the
;;; SourceForge summary page magical bugs web interface 2001-09-01, it
;;; looks as though it's past time to start accumulating regression
;;; tests for these.
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
	  (error "failed NBUTLAST for ~S" args))))))

(multiple-value-bind (result error)
    (ignore-errors (apply #'butlast (list t)))
  (assert (null result))
  (assert (typep error 'type-error)))

;;; reported by Paul Dietz on cmucl-imp: LDIFF does not check type of
;;; its first argument
(assert (not (ignore-errors (ldiff 1 2))))

;;; evaluation order in PUSH, PUSHNEW
(let ((a (map 'vector #'list '(a b c))))
  (let ((i 0))
    (pushnew (incf i) (aref a (incf i)))
    (assert (equalp a #((a) (b) (1 c))))))

(symbol-macrolet ((s (aref a (incf i))))
    (let ((a (map 'vector #'list '(a b c))))
      (let ((i 0))
        (push t s)
        (assert (equalp a #((a) (t b) (c))))
        (pushnew 1 s)
        (assert (equalp a #((a) (t b) (1 c))))
        (setq i 0)
        (assert (eql (pop s) 't))
        (assert (equalp a #((a) (b) (1 c)))))))

;;; Type checking in NCONC
(let ((tests '((((1 . 2)) (1 . 2))
               (((1 . 2) (3 . 4)) (1 3 . 4))
               (((1 . 2) 3) (1 . 3))
               ((3) 3))))
  (loop for (args result) in tests
     do (assert (equal (apply 'nconc (copy-tree args)) result))
     do (let ((exp `(nconc ,@ (mapcar (lambda (arg)
                                        `',(copy-tree arg))
                                      args))))
          (assert (equal (funcall (compile nil `(lambda () ,exp))) result)))))

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
       do (let ((exp `(nconc ,@ (mapcar (lambda (arg)
                                          `',(copy-tree arg))
                                        args))))
            (check-error (funcall (compile nil `(lambda () ,exp))) fail)))))

(dolist (test '((append 1 2)
                (append (1 2) nil (3 . 4) nil)
                (append nil (1 2) nil (3 . 4) nil)
                (reverse (1 2 . 3))
                (nreverse (1 2 . 3))
                (nreconc (1 2 . 3) (4 5))
		(copy-alist ((1 . 2) (3 . 4) . 5))))
  (assert (raises-error? (apply (first test) (copy-tree (rest test)))
			 type-error)))

;;; Bug reported by Paul Dietz: NSET-EXCLUSIVE-OR should not return
;;; extra elements, even when given "sets" contain duplications
(assert (equal (remove-duplicates (sort (nset-exclusive-or (list 1 2 1 3)
                                                           (list 4 1 3 3))
                                        #'<))
               '(2 4)))
