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


;;;; success
