;;;; This file is for macroexpander tests which have side effects

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

;;; From Matthew Swank on cll 2005-10-06

(defmacro defglobal* (name &optional value)
  (let ((internal (gensym)))
    `(progn
       (defparameter ,internal ,value)
       (define-symbol-macro ,name ,internal))))

(defglobal* glob)

(assert (= (let ((glob 4)) glob)))
(assert (null glob))
(assert (equal (let ((glob nil)) (setf glob (cons 'foo glob)) glob) '(foo)))
(assert (null glob))
(assert (equal (let ((glob nil)) (push 'foo glob) glob) '(foo)))
(assert (null glob))



;;; CLHS 3.1.2.1.1 specifies that symbol macro expansion must also
;;; go through *MACROEXPAND-HOOK*. (2007-09-22, -TCR.)

(define-symbol-macro .foo. 'foobar)

(let* ((expanded-p nil)
      (*macroexpand-hook* #'(lambda (fn form env)
                              (when (eq form '.foo.)
                                (setq expanded-p t))
                              (funcall fn form env))))
  (multiple-value-bind (expansion flag) (macroexpand '.foo.)
    (assert (equal expansion '(quote foobar)))
    (assert flag)
    (assert expanded-p)))

(let ((sb-ext::*evaluator-mode* :interpret))
  (let* ((expanded-p nil)
         (*macroexpand-hook* #'(lambda (fn form env)
                                 (when (eq form '.foo.)
                                   (setq expanded-p t))
                                 (funcall fn form env))))
    (eval '.foo.)
    (assert expanded-p)))

(let* ((expanded-p nil)
       (*macroexpand-hook* #'(lambda (fn form env)
                               (when (eq form '/foo/)
                                 (setq expanded-p t))
                               (funcall fn form env))))
  (compile nil '(lambda ()
                 (symbol-macrolet ((/foo/ 'foobar))
                   (macrolet ((expand (symbol &environment env)
                                (macroexpand symbol env)))
                     (expand /foo/)))))
  (assert expanded-p))
