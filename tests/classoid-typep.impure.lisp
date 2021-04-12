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

#-sb-thread (sb-ext:exit :code 104)

;;; Using a fresh package as a namespace for classes is a quick-and-dirty
;;; way to create a class hierarchy without anonymous classes.
;;; Not to mention, I think anonymous classes have a whole
;;; bunch of other problems.
(defun make-test-object ()
  (let ((*package* (make-package "TEST" :use '("CL"))))
    ;; The following magic incantation will leave *THING* holding
    ;; an instance of FOO whose LAYOUT-INVALID slot is T.
    ;; There may be other ways that this happens,
    ;; but exactly how it happens is irrelevant.
    (proclaim (list 'special (intern "*THING*")))
    (eval
     (read-from-string
      "(progn
(defclass foo (bar) ())
(defclass bar () ())
(setq *thing* (make-instance 'foo))
(defclass bar () (a))
;; Don't take out this WRITE-TO-STRING (or the one below)!
;; While it makes the test run faster, it renders it useless as a test.
(write-to-string *thing*)
(defclass baz () ())
(defclass bar (baz) (a))
(write-to-string *thing*)
(values *thing* (sb-kernel:%instance-wrapper *thing*)))"))))

;;; Given the PCL state set up by the above function,
;;; execute CLASSOID-TYPEP simultaneously in two threads.
(defun concurrent-classoid-typep (obj)
  (let* ((sem (sb-thread:make-semaphore))
         (obj-layout (sb-kernel:%instance-wrapper obj))
         (classoid (sb-kernel:wrapper-classoid obj-layout))
         (fun (lambda ()
                (sb-thread:wait-on-semaphore sem)
                (handler-case (sb-kernel:classoid-typep obj-layout classoid obj)
                  (error () :fail))))
         (a (sb-thread:make-thread fun))
         (b (sb-thread:make-thread fun)))
    (sb-thread:signal-semaphore sem 2)
    (let ((result1 (sb-thread:join-thread a))
          (result2 (sb-thread:join-thread b)))
      (values result1 result2))))

(defun run-classoid-typep-test (n-iterations &aux (failures 0))
  (when (find-package "TEST")
    (delete-package "TEST"))
  (loop for iteration below n-iterations
        do (let ((object (make-test-object)))
             (delete-package "TEST")
             (multiple-value-bind (a b)
                 (concurrent-classoid-typep object)
               (when (or (eq a :fail) (eq b :fail))
                 (incf failures)
                 (format t "Iteration ~d - ~S and ~S~%"
                         iteration a b)))))
  failures)

;;; This test is really slow. (Almost all its time is spent in the compiler
;;; compiling an EMF for CHANGE-CLASS)
;;; Prior to the bugfix, it would fail about .1% of the time (x86-64/linux, ymmv)
;;; so in 10000 iterations we expect 10 failures, though I've seen as few as 1
;;; and as many as 15. 5000 iterations whould produce at least 1 failure
;;; with fairly high confidence.
;;; In a real application, a failure occured in an end-to-end test (that
;;; merely involved TYPEP not as the thing under test) as often as 10% of the
;;; time due just to worse luck in terms of thread concurrency.
(with-test (:name :concurrent-classoid-typep)
  (assert (zerop (run-classoid-typep-test 1000))))
