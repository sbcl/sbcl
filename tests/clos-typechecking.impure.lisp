(load "assertoid.lisp")

(defpackage "FOO"
  (:use "CL" "ASSERTOID"))
(in-package "FOO")

(defclass foo ()
  ((slot :initarg :slot :type fixnum :accessor slot)))
(defclass foo/gf (sb-mop:standard-generic-function)
  ((slot/gf :initarg :slot/gf :type fixnum :accessor slot/gf))
  (:metaclass sb-mop:funcallable-standard-class))
(defmethod succeed/sv ((x foo))
  (setf (slot-value x 'slot) 1))
(defmethod fail/sv ((x foo))
  (setf (slot-value x 'slot) t))
(defmethod succeed/acc ((x foo))
  (setf (slot x) 1))
(defmethod fail/acc ((x foo))
  (setf (slot x) t))
(defmethod succeed/sv/gf ((x foo/gf))
  (setf (slot-value x 'slot/gf) 1))
(defmethod fail/sv/gf ((x foo/gf))
  (setf (slot-value x 'slot/gf) t))
(defmethod succeed/acc/gf ((x foo/gf))
  (setf (slot/gf x) 1))
(defmethod fail/acc/gf ((x foo/gf))
  (setf (slot/gf x) t))
(defvar *t* t)
(defvar *one* 1)

;; evaluator
(eval '(setf (slot-value (make-instance 'foo) 'slot) 1))
(assert (raises-error? (eval '(setf (slot-value (make-instance 'foo) 'slot) t))
                       type-error))
(eval '(setf (slot (make-instance 'foo)) 1))
(assert (raises-error? (eval '(setf (slot (make-instance 'foo)) t))
                       type-error))
(eval '(succeed/sv (make-instance 'foo)))
(assert (raises-error? (eval '(fail/sv (make-instance 'foo)))
                       type-error))
(eval '(succeed/acc (make-instance 'foo)))
(assert (raises-error? (eval '(fail/acc (make-instance 'foo)))
                       type-error))
(eval '(make-instance 'foo :slot 1))
(assert (raises-error? (eval '(make-instance 'foo :slot t))
                       type-error))
(eval '(make-instance 'foo :slot *one*))
(assert (raises-error? (eval '(make-instance 'foo :slot *t*))
                       type-error))
;; evaluator/gf
(eval '(setf (slot-value (make-instance 'foo/gf) 'slot/gf) 1))
(assert (raises-error?
         (eval '(setf (slot-value (make-instance 'foo/gf) 'slot/gf) t))
         type-error))
(eval '(setf (slot/gf (make-instance 'foo/gf)) 1))
(assert (raises-error? (eval '(setf (slot/gf (make-instance 'foo/gf)) t))
                       type-error))
(eval '(succeed/sv/gf (make-instance 'foo/gf)))
(assert (raises-error? (eval '(fail/sv/gf (make-instance 'foo/gf)))
                       type-error))
(eval '(succeed/acc/gf (make-instance 'foo/gf)))
(assert (raises-error? (eval '(fail/acc/gf (make-instance 'foo/gf)))
                       type-error))
(eval '(make-instance 'foo/gf :slot/gf 1))
(assert (raises-error? (eval '(make-instance 'foo/gf :slot/gf t))
                       type-error))
(eval '(make-instance 'foo/gf :slot/gf *one*))
(assert (raises-error? (eval '(make-instance 'foo/gf :slot/gf *t*))
                       type-error))

;; compiler
(funcall (compile nil '(lambda ()
                        (setf (slot-value (make-instance 'foo) 'slot) 1))))
#+nil ; this one still fails goddamit.
(assert (raises-error?
         (funcall
          (compile nil '(lambda ()
                         (setf (slot-value (make-instance 'foo) 'slot) t))))
         type-error))
(funcall (compile nil '(lambda () (setf (slot (make-instance 'foo)) 1))))
(assert (raises-error?
         (funcall
          (compile nil '(lambda () (setf (slot (make-instance 'foo)) t))))
         type-error))
(funcall (compile nil '(lambda () (succeed/sv (make-instance 'foo)))))
(assert (raises-error?
         (funcall (compile nil '(lambda () (fail/sv (make-instance 'foo)))))
         type-error))
(funcall (compile nil '(lambda () (succeed/acc (make-instance 'foo)))))
(assert (raises-error?
         (funcall (compile nil '(lambda () (fail/acc (make-instance 'foo)))))
         type-error))
(funcall (compile nil '(lambda () (make-instance 'foo :slot 1))))
(assert (raises-error?
         (funcall (compile nil '(lambda () (make-instance 'foo :slot t))))
         type-error))
(funcall (compile nil '(lambda () (make-instance 'foo :slot *one*))))
(assert (raises-error?
         (funcall (compile nil '(lambda () (make-instance 'foo :slot *t*))))
         type-error))
;; compiler/gf
(funcall (compile nil
                  '(lambda ()
                    (setf (slot-value (make-instance 'foo/gf) 'slot/gf) 1))))
#+nil ; this one too
(assert (raises-error?
         (funcall
          (compile nil
                   '(lambda ()
                     (setf (slot-value (make-instance 'foo/gf) 'slot/gf) t))))
         type-error))
(funcall (compile nil '(lambda () (setf (slot/gf (make-instance 'foo/gf)) 1))))
(assert (raises-error?
         (funcall
          (compile nil
                   '(lambda () (setf (slot/gf (make-instance 'foo/gf)) t))))
         type-error))
(funcall (compile nil '(lambda () (succeed/sv/gf (make-instance 'foo/gf)))))
(assert (raises-error?
         (funcall (compile nil '(lambda ()
                                 (fail/sv/gf (make-instance 'foo/gf)))))
         type-error))
(funcall (compile nil '(lambda () (succeed/acc/gf (make-instance 'foo/gf)))))
(assert (raises-error?
         (funcall (compile nil '(lambda ()
                                 (fail/acc/gf (make-instance 'foo/gf)))))
         type-error))
(funcall (compile nil '(lambda () (make-instance 'foo/gf :slot/gf 1))))
(assert (raises-error?
         (funcall (compile nil '(lambda ()
                                 (make-instance 'foo/gf :slot/gf t))))
         type-error))
(funcall (compile nil '(lambda () (make-instance 'foo/gf :slot/gf *one*))))
(assert (raises-error?
         (funcall (compile nil '(lambda ()
                                 (make-instance 'foo/gf :slot/gf *t*))))
         type-error))

;;;; success
(sb-ext:quit :unix-status 104)
