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
(load "test-util.lisp")
(use-package "ASSERTOID")
(use-package "TEST-UTIL")

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

;; nor should it fail on generic functions or other funcallable instances
(defgeneric disassemble-generic (x))
(disassemble 'disassemble-generic)
(let ((fin (sb-mop:make-instance 'sb-mop:funcallable-standard-object)))
  (disassemble fin))

;;; while we're at it, much the same applies to
;;; FUNCTION-LAMBDA-EXPRESSION:
(defun fle-fun (x) x)
(function-lambda-expression #'fle-fun)

(let ((x 1)) (defun fle-closure (y) (if y (setq x y) x)))
(function-lambda-expression #'fle-closure)

#+sb-eval
(progn
  ;; Nor should it fail on interpreted functions
  (let ((sb-ext:*evaluator-mode* :interpret))
    (eval `(defun fle-eval (x) x))
    (function-lambda-expression #'fle-eval))

  ;; fle-eval should still be an interpreted function.
  (assert (sb-eval:interpreted-function-p #'fle-eval)))

;; nor should it fail on generic functions or other funcallable instances
(defgeneric fle-generic (x))
(function-lambda-expression #'fle-generic)
(let ((fin (sb-mop:make-instance 'sb-mop:funcallable-standard-object)))
  (function-lambda-expression fin))

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
    (macrolet ((check (form)
                 `(or ,form
                      (error "misbehavior in DESCRIBE of ~S:~%   ~S" i ',form))))
      (check (char= #\x (char s 0)))
      ;; one leading #\NEWLINE from FRESH-LINE or the like, no more
      (check (char= #\newline (char s 1)))
      (check (char/= #\newline (char s 2)))
      ;; one trailing #\NEWLINE from TERPRI or the like, no more
      (let ((n (length s)))
        (check (char= #\newline (char s (- n 1))))
        (check (char/= #\newline (char s (- n 2))))))))


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

(define-compiler-macro cmacro (x)
  "compiler macro"
  x)

(define-compiler-macro (setf cmacro) (y x)
  "setf compiler macro"
  y)

(with-test (:name (documentation compiler-macro))
  (unless (equal "compiler macro"
                 (documentation 'cmacro 'compiler-macro))
    (error "got ~S for cmacro"
           (documentation 'cmacro 'compiler-macro)))
  (unless (equal "setf compiler macro"
                 (documentation '(setf cmacro) 'compiler-macro))
    (error "got ~S for setf macro" (documentation '(setf cmacro) 'compiler-macro))))

(with-test (:name (documentation lambda))
  (let ((f (lambda () "aos the zos" t))
        (g (sb-int:named-lambda fii () "zoot the fruit" t)))
    (dolist (doc-type '(t function))
      (assert (string= (documentation f doc-type) "aos the zos"))
      (assert (string= (documentation g doc-type) "zoot the fruit")))
    (setf (documentation f t) "fire")
    (assert (string= (documentation f t) "fire"))
    (assert (string= (documentation g t) "zoot the fruit"))))

(with-test (:name (documentation flet))
  (assert
   (string= (documentation
             (flet ((quux (x)
                      "this is FLET quux"
                      (/ x 2)))
               #'quux)
             t)
            "this is FLET quux")))

(with-test (:name (documentation labels))
  (assert
   (string= (documentation
             (labels ((rec (x)
                        "this is LABELS rec"
                        (if (plusp x)
                            (* x (rec (1- x)))
                            1)))
               #'rec)
             t)
            "this is LABELS rec")))

(let ((x 1))
  (defun docfoo (y)
    "bar"
    (incf x y)))

(with-test (:name (documentation closure))
  (assert (string= (documentation 'docfoo 'function) "bar"))
  (assert (string= (documentation #'docfoo t) "bar"))
  (assert (string= (setf (documentation 'docfoo 'function) "baz") "baz"))
  (assert (string= (documentation 'docfoo 'function) "baz"))
  (assert (string= (documentation #'docfoo t) "bar"))
  (assert (string= (setf (documentation #'docfoo t) "zot") "zot"))
  (assert (string= (documentation #'docfoo t) "zot"))
  (assert (string= (documentation 'docfoo 'function) "baz"))
  (assert (not (setf (documentation 'docfoo 'function) nil)))
  (assert (string= (documentation 'docfoo 'function) "zot")))

#+sb-doc
(with-test (:name (documentation built-in-macro))
  (assert (documentation 'trace 'function)))

#+sb-doc
(with-test (:name (documentation built-in-function))
  (assert (documentation 'cons 'function)))

(with-test (:name :describe-generic-function-with-assumed-type)
  ;; Signalled an error at one point
  (flet ((zoo () (gogo)))
    (defmethod gogo () nil)
    (describe 'gogo)))

(defmacro bug-643958-test ()
  "foo"
  :ding!)

(with-test (:name :bug-643958)
  (assert (equal "foo" (documentation 'bug-643958-test 'function)))
  (setf (documentation 'bug-643958-test 'function) "bar")
  (assert (equal "bar" (documentation 'bug-643958-test 'function))))

;;;; success
