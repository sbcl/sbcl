;;; CLOS interrupt safety tests

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

(defpackage "CLOS-INTERRUPT-TEST"
  (:use "COMMON-LISP" "SB-EXT"))

(in-package "CLOS-INTERRUPT-TEST")

;;;;; Interrupting applicable method computation and calling the same
;;;;; GF that was being computed in the interrupt handler must not show
;;;;; up as metacircle.

;;; KLUDGE: We just want a way to ensure our interrupt happens at a
;;; bad place.
;;;
;;; FIXME: While an invasive hook like this is probably ok for testing
;;; purposes, it would also be good to have a proper interrupt-stress
;;; test for CLOS.
(defmacro define-wrapper (name &key before after)
  (let ((real (intern (format nil "*REAL-~A*" name)))
        (our (intern (format nil "OUR-~A" name))))
    `(progn
       (defvar ,real #',name)
       (defun ,our (&rest args)
         ,@before
         (multiple-value-prog1
             (apply ,real args)
           ,@after))
       (without-package-locks
         (setf (fdefinition ',name) #',our)))))

(defgeneric compute-test (x y))

(defvar *interrupting* nil)

(defun interrupt ()
  (unless *interrupting*
    (let ((self sb-thread:*current-thread*)
          (*interrupting* t))
      ;; Test both interrupting yourself and using another thread
      ;; for to interrupting.
      #+sb-thread
      (progn
        (write-line "/interrupt-other")
        (sb-thread:join-thread (sb-thread:make-thread
                                (lambda ()
                                  (sb-thread:interrupt-thread
                                   self
                                   (lambda ()
                                     (compute-test 1 2)))))))
      (write-line "/interrupt-self")
      (sb-thread:interrupt-thread self (lambda () (compute-test 1 2))))))

(defvar *interrupted-gfs* nil)

(define-wrapper sb-pcl::compute-applicable-methods-using-types
    :before ((when (and (eq (car args) #'compute-test)
                        ;; Check that we are at "bad place"
                        (assoc (car args) sb-pcl::*cache-miss-values-stack*))
               (interrupt)
               (pushnew (car args) *interrupted-gfs*))))

(defmethod compute-test (x y)
  t)
(defmethod compute-test ((x fixnum) (y fixnum))
  'fixnum)
(defmethod compute-test ((x symbol) (y symbol))
  'symbol)

(test-util:with-test (:name :compute-test
                      :skipped-on (not :sb-thread)
                      :broken-on :win32)
  (compute-test 1 2)

  ;; Check that we actually interrupted something.
  (assert (equal (list #'compute-test) *interrupted-gfs*)))
