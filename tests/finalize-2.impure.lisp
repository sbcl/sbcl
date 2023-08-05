#-sb-thread (invoke-restart 'run-tests::skip-file)

(setq *evaluator-mode* :compile)
;; example from lp#2029306
(defvar *x*)

(defun clear-registers (&optional a b c d e f g h i j k l m n o p q)
  (values q p o n m l k j i h g f e d c b a))

(defun test ()
  (setf *x* (make-hash-table))
  (clear-registers)
  (sb-ext:finalize *x* (lambda () (error "final")) :dont-save t)
  (clear-registers)
  (sb-sys:scrub-control-stack)
  (assert (sb-ext:cancel-finalization *x*)))

(defglobal *runflag* t)
(defvar *thr1*
  (sb-thread:make-thread
   (lambda ()
     (loop while *runflag* do (gc :full t) (sleep 0.01)))
   :name "gc"))

(defvar *thr2*
  (sb-thread:make-thread
   (lambda ()
     (let ((*x* nil)) (loop while *runflag* do (test))))
   :name "test"))

(test-util:with-test (:name :finalizer-insertion-race)
  (sleep 8) ; arbitrary time to let the test run
  (setq *runflag* nil)
  (mapc 'sb-thread:join-thread (list *thr1* *thr2*)))
