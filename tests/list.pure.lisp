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
	   (:args (t)             :result nil)
	   (:args (foosymbol 0)   :result foosymbol)
	   (:args (foosymbol)     :result nil)
	   (:args (foosymbol 1)   :result nil)
	   (:args (foosymbol 2)   :result nil)
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
