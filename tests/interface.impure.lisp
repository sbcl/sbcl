;;;; tests for problems in the interface presented to the user/programmer

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

(load "assertoid.lisp")
(use-package "ASSERTOID")

(defun (setf foo) (x)
  "(setf foo) documentation"
  x)

(assert (string= (documentation '(setf foo) 'function)
                 "(setf foo) documentation"))
(assert (string= (documentation #'(setf foo) 'function)
                 "(setf foo) documentation"))

(assert (string= (documentation '(setf foo) 'function)
                 "(setf foo) documentation"))
(assert (string= (documentation #'(setf foo) 'function)
                 "(setf foo) documentation"))

;;; DISASSEMBLE shouldn't fail on closures or unpurified functions
(defun disassemble-fun (x) x)
(disassemble 'disassemble-fun)

(let ((x 1)) (defun disassemble-closure (y) (if y (setq x y) x)))
(disassemble 'disassemble-closure)

#+sb-eval
(progn
  ;; Nor should it fail on interpreted functions
  (let ((sb-ext:*evaluator-mode* :interpret))
    (eval `(defun disassemble-eval (x) x))
    (disassemble 'disassemble-eval))

  ;; disassemble-eval should still be an interpreted function.
  ;; clhs disassemble: "(If that function is an interpreted function,
  ;; it is first compiled but the result of this implicit compilation
  ;; is not installed.)"
  (assert (sb-eval:interpreted-function-p #'disassemble-eval)))

;;; support for DESCRIBE tests
(defstruct to-be-described a b)
(defclass forward-describe-class (forward-describe-ref) (a))

;;; DESCRIBE should run without signalling an error.
(describe (make-to-be-described))
(describe 12)
(describe "a string")
(describe 'symbolism)
(describe (find-package :cl))
(describe '(a list))
(describe #(a vector))

;;; The DESCRIBE-OBJECT methods for built-in CL stuff should do
;;; FRESH-LINE and TERPRI neatly.
(dolist (i (list (make-to-be-described :a 14) 12 "a string"
                 #0a0 #(1 2 3) #2a((1 2) (3 4)) 'sym :keyword
                 (find-package :keyword) (list 1 2 3)
                 nil (cons 1 2) (make-hash-table)
                 (let ((h (make-hash-table)))
                   (setf (gethash 10 h) 100
                         (gethash 11 h) 121)
                   h)
                 (make-condition 'simple-error)
                 (make-condition 'simple-error :format-control "fc")
                 #'car #'make-to-be-described (lambda (x) (+ x 11))
                 (constantly 'foo) #'(setf to-be-described-a)
                 #'describe-object (find-class 'to-be-described)
                 (find-class 'forward-describe-class)
                 (find-class 'forward-describe-ref) (find-class 'cons)))
  (let ((s (with-output-to-string (s)
             (write-char #\x s)
             (describe i s))))
    (unless (and (char= #\x (char s 0))
                 ;; one leading #\NEWLINE from FRESH-LINE or the like, no more
                 (char= #\newline (char s 1))
                 (char/= #\newline (char s 2))
                 ;; one trailing #\NEWLINE from TERPRI or the like, no more
                 (let ((n (length s)))
                   (and (char= #\newline (char s (- n 1)))
                        (char/= #\newline (char s (- n 2))))))
      (error "misbehavior in DESCRIBE of ~S" i))))


;;; Tests of documentation on types and classes
(defclass foo ()
  ()
  (:documentation "FOO"))
(defstruct bar "BAR")
(define-condition baz ()
  ()
  (:documentation "BAZ"))
(deftype quux ()
  "QUUX"
  't)
(defstruct (frob (:type vector)) "FROB")
(macrolet
    ((do-class (name expected &optional structurep)
       `(progn
         (assert (string= (documentation ',name 'type) ,expected))
         (assert (string= (documentation (find-class ',name) 'type) ,expected))
         (assert (string= (documentation (find-class ',name) 't) ,expected))
         ,@(when structurep
            `((assert (string= (documentation ',name 'structure) ,expected))))
         (let ((new1 (symbol-name (gensym "NEW1")))
               (new2 (symbol-name (gensym "NEW2")))
               (new3 (symbol-name (gensym "NEW3")))
               (new4 (symbol-name (gensym "NEW4"))))
           (declare (ignorable new4))
           (setf (documentation ',name 'type) new1)
           (assert (string= (documentation (find-class ',name) 'type) new1))
           (setf (documentation (find-class ',name) 'type) new2)
           (assert (string= (documentation (find-class ',name) 't) new2))
           (setf (documentation (find-class ',name) 't) new3)
           (assert (string= (documentation ',name 'type) new3))
           ,@(when structurep
              `((assert (string= (documentation ',name 'structure) new3))
                (setf (documentation ',name 'structure) new4)
                (assert (string= (documentation ',name 'structure) new4))))))))
  (do-class foo "FOO")
  (do-class bar "BAR" t)
  (do-class baz "BAZ"))

(assert (string= (documentation 'quux 'type) "QUUX"))
(setf (documentation 'quux 'type) "NEW4")
(assert (string= (documentation 'quux 'type) "NEW4"))

(assert (string= (documentation 'frob 'structure) "FROB"))
(setf (documentation 'frob 'structure) "NEW5")
(assert (string= (documentation 'frob 'structure) "NEW5"))

;;;; success
