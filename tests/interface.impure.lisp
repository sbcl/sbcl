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

(defmacro silently (&rest things)
  `(let ((*standard-output* (make-broadcast-stream))) ,@things))

;; Interpreted closure is a problem for COMPILE
(with-test (:name :disassemble :skipped-on :interpreter)
;;; DISASSEMBLE shouldn't fail on closures or unpurified functions
  (defun disassemble-fun (x) x)
  (silently (disassemble 'disassemble-fun)))

(with-test (:name :disassemble-closure :skipped-on :interpreter)
  (let ((x 1)) (defun disassemble-closure (y) (if y (setq x y) x)))
  (silently (disassemble 'disassemble-closure)))

#+sb-eval
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'sb-eval:interpreted-function-p))
#+sb-fasteval
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'sb-interpreter:interpreted-function-p))

#+(or sb-eval sb-fasteval)
(with-test (:name :disassemble-interpreted)
    ;; Nor should it fail on interpreted functions
    (let ((sb-ext:*evaluator-mode* :interpret))
      (eval `(defun disassemble-eval (x) x))
      (silently (disassemble 'disassemble-eval)))

    ;; disassemble-eval should still be an interpreted function.
    ;; clhs disassemble: "(If that function is an interpreted function,
    ;; it is first compiled but the result of this implicit compilation
    ;; is not installed.)"
    (assert (interpreted-function-p (symbol-function 'disassemble-eval))))

(with-test (:name :disassemble-generic)
  ;; nor should it fail on generic functions or other funcallable instances
  (defgeneric disassemble-generic (x))
  (silently (disassemble 'disassemble-generic))
  (let ((fin (make-instance 'sb-mop:funcallable-standard-object)))
    (silently (disassemble fin))))

;;; while we're at it, much the same applies to
;;; FUNCTION-LAMBDA-EXPRESSION:
(defun fle-fun (x) x)

(let ((x 1)) (defun fle-closure (y) (if y (setq x y) x)))

(with-test (:name :function-lambda-expression)
  (flet ((fle-name (x)
           (nth-value 2 (function-lambda-expression x))))
    (assert (eql (fle-name #'fle-fun) 'fle-fun))
    (assert (eql (fle-name #'fle-closure) 'fle-closure))
    (assert (eql (fle-name #'disassemble-generic) 'disassemble-generic))
    (function-lambda-expression
     (make-instance 'sb-mop:funcallable-standard-object))
    (function-lambda-expression
     (make-instance 'generic-function))
    (function-lambda-expression
     (make-instance 'standard-generic-function))
    #+(or sb-eval sb-fasteval)
    (progn
      (let ((sb-ext:*evaluator-mode* :interpret))
        (eval `(defun fle-eval (x) x))
        (assert (eql (fle-name (symbol-function 'fle-eval)) 'fle-eval)))

      ;; fle-eval should still be an interpreted function.
      (assert (interpreted-function-p (symbol-function 'fle-eval))))))


;;; support for DESCRIBE tests
(defstruct to-be-described a b)
(defclass forward-describe-class (forward-describe-ref) (a))
(let ((sb-ext:*evaluator-mode* :compile))
  (eval `(let (x) (defun closure-to-describe () (incf x)))))

(with-test (:name :describe-empty-gf)
  (silently (describe (make-instance 'generic-function)))
  (silently (describe (make-instance 'standard-generic-function))))

;;; DESCRIBE should run without signalling an error.
(with-test (:name (describe :no-error))
 (silently
  (describe (make-to-be-described))
  (describe 12)
  (describe "a string")
  (describe 'symbolism)
  (describe (find-package :cl))
  (describe '(a list))
  (describe #(a vector))
;; bug 824974
  (describe 'closure-to-describe)))

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

(defun assert-documentation (thing doc-type expected)
  ;; This helper function makes ASSERT errors print THING, DOC-TYPE,
  ;; the return value of DOCUMENTATION and EXPECTED.
  (flet ((assert-documentation-helper (thing doc-type documentation expected)
           (declare (ignore thing doc-type))
           (equal documentation expected)))
    (assert (assert-documentation-helper
             thing doc-type (documentation thing doc-type) expected))))

(defpackage #:documentation.package
  (:documentation "PACKAGE"))

(with-test (:name (documentation package))
  (assert-documentation (find-package '#:documentation.package) t "PACKAGE")
  (setf (documentation (find-package '#:documentation.package) t) "PACKAGE2")
  (assert-documentation (find-package '#:documentation.package) t "PACKAGE2"))

(defclass foo ()
  ()
  (:documentation "FOO"))

(defclass documentation.funcallable-instance ()
  ()
  (:metaclass sb-mop:funcallable-standard-class)
  (:documentation "FEZ"))

(defstruct bar "BAR")

(define-condition baz ()
  ()
  (:documentation "BAZ"))

(macrolet
    ((do-class (name expected &optional structurep)
       `(progn
          (assert-documentation ',name 'type ,expected)
          (assert-documentation (find-class ',name) 'type ,expected)
          (assert-documentation (find-class ',name) 't ,expected)
          ,@(when structurep
              `((assert-documentation ',name 'structure ,expected)))

          (let ((new1 (symbol-name (gensym "NEW1")))
                (new2 (symbol-name (gensym "NEW2")))
                (new3 (symbol-name (gensym "NEW3")))
                (new4 (symbol-name (gensym "NEW4"))))
            (declare (ignorable new4))
            (setf (documentation ',name 'type) new1)
            (assert-documentation (find-class ',name) 'type new1)
            (setf (documentation (find-class ',name) 'type) new2)
            (assert-documentation (find-class ',name) 't new2)
            (setf (documentation (find-class ',name) 't) new3)
            (assert-documentation ',name 'type new3)
            ,@(when structurep
                `((assert-documentation ',name 'structure new3)
                  (setf (documentation ',name 'structure) new4)
                  (assert-documentation ',name 'structure new4)))))))

  (with-test (:name (documentation class standard-class))
    (do-class foo "FOO"))

  (with-test (:name (documentation class sb-mop:funcallable-standard-class))
    (do-class documentation.funcallable-instance "FEZ"))

  (with-test (:name (documentation struct 1))
    (do-class bar "BAR" t))

  (with-test (:name (documentation condition))
    (do-class baz "BAZ")))

(defstruct (frob (:type vector)) "FROB")

(with-test (:name (documentation struct 2))
  (assert-documentation 'frob 'structure "FROB")
  (setf (documentation 'frob 'structure) "NEW5")
  (assert-documentation 'frob 'structure "NEW5"))

(deftype quux ()
  "QUUX"
  't)

(with-test (:name (documentation type))
  (assert-documentation 'quux 'type "QUUX")
  (setf (documentation 'quux 'type) "NEW4")
  (assert-documentation 'quux 'type "NEW4"))

(define-compiler-macro cmacro (x)
  "compiler macro"
  x)

(define-compiler-macro (setf cmacro) (y x)
  "setf compiler macro"
  (declare (ignore x))
  y)

(with-test (:name (documentation compiler-macro))
  (assert-documentation 'cmacro 'compiler-macro "compiler macro")
  (assert-documentation '(setf cmacro) 'compiler-macro "setf compiler macro"))

(defun (setf documentation.setf) (x)
  "(setf foo) documentation"
  x)

(with-test (:name (documentation function setf))
  (flet ((expect (documentation)
           (assert-documentation
            '(setf documentation.setf) 'function documentation)
           (assert-documentation
            #'(setf documentation.setf) 'function documentation)
           (assert-documentation
            #'(setf documentation.setf) t documentation)))
    (expect "(setf foo) documentation")
    ;; The original test checked this twice. No idea why.
    (expect "(setf foo) documentation")

    ;; Modification
    (setf (documentation '(setf documentation.setf) 'function)
          "(setf bar) documentation")
    (expect "(setf bar) documentation")

    (setf (documentation #'(setf documentation.setf) 'function)
          "(setf baz) documentation")
    (expect "(setf baz) documentation")

    (setf (documentation #'(setf documentation.setf) t)
          "(setf fez) documentation")
    (expect "(setf fez) documentation")))

(with-test (:name (documentation lambda))
  (let ((f (lambda () "aos the zos" t))
        (g (sb-int:named-lambda fii () "zoot the fruit" t)))
    (dolist (doc-type '(t function))
      (assert-documentation f doc-type "aos the zos")
      (assert-documentation g doc-type "zoot the fruit"))
    (setf (documentation f t) "fire")
    (assert-documentation f t "fire")
    (assert-documentation g t "zoot the fruit")))

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

(with-test (:name (documentation :closure))
  (assert-documentation 'docfoo 'function "bar")
  (assert (string= (setf (documentation 'docfoo 'function) "baz") "baz"))
  (assert-documentation 'docfoo 'function "baz")
  (assert-documentation #'docfoo t "baz")
  (assert (string= (setf (documentation #'docfoo t) "zot") "zot"))
  (assert-documentation #'docfoo t "zot")
  (assert-documentation 'docfoo 'function "zot")
  (assert (not (setf (documentation 'docfoo 'function) nil)))
  (assert-documentation 'docfoo 'function nil))

(with-test (:name (documentation :built-in-macro) :skipped-on '(not :sb-doc))
  (assert (documentation 'trace 'function)))

(with-test (:name (documentation :built-in-function) :skipped-on '(not :sb-doc))
  (assert (documentation 'cons 'function)))

(defvar documentation.variable nil
  "foo variable documentation")

(with-test (:name (documentation variable))
  (assert-documentation 'documentation.variable 'variable
                        "foo variable documentation")
  (setf (documentation 'documentation.variable 'variable)
        "baz variable documentation")
  (assert-documentation 'documentation.variable 'variable
                        "baz variable documentation"))

(with-test (:name (documentation :mismatch-for-function))
  (defun test ()
    "X"
    nil)
  (setf (symbol-function 'test2) #'test)
  (setf (documentation 'test 'function) "Y")
  (assert (equal (documentation #'test t)
                 (documentation 'test 'function)))
  (setf (documentation 'test2 'function) "Z")
  (assert (not
           (equal (documentation 'test 'function)
                  (documentation 'test2 'function)))))

(with-test (:name (documentation setf :on nil))
  (assert
   (handler-case
       (assert (equal (setf (documentation nil 'function) "foo") "foo"))
     (style-warning () t)
     (:no-error (x)
       (declare (ignore x))
       nil))))

(with-test (:name :describe-generic-function-with-assumed-type)
  ;; Signalled an error at one point
  (flet ((zoo () (gogo)))
    (defmethod gogo () nil)
    (silently (describe 'gogo))))

(defmacro bug-643958-test ()
  "foo"
  :ding!)

(with-test (:name :bug-643958)
  (assert (equal "foo" (documentation 'bug-643958-test 'function)))
  (setf (documentation 'bug-643958-test 'function) "bar")
  (assert (equal "bar" (documentation 'bug-643958-test 'function))))

(defclass cannot-print-this ()
  ())
(defmethod print-object ((oops cannot-print-this) stream)
  (error "No go!"))
(with-test (:name :describe-suppresses-print-errors)
  (handler-bind ((error #'continue))
    (with-output-to-string (s)
      (describe (make-instance 'cannot-print-this) s))))
(with-test (:name :backtrace-suppresses-print-errors)
  (handler-bind ((error #'continue))
    (with-output-to-string (s)
      (labels ((foo (n x)
                 (when (plusp n)
                   (foo (1- n) x))
                 (when (zerop n)
                   (sb-debug:print-backtrace :count 100 :stream s))))
        (foo 100 (make-instance 'cannot-print-this))))))
(with-test (:name :backtrace-and-circles)
  (handler-bind ((error #'continue))
    (with-output-to-string (s)
      (labels ((foo (n x)
                 (when (plusp n)
                   (foo (1- n) x))
                 (when (zerop n)
                   (sb-debug:print-backtrace :count 100 :stream s))))
        (foo 100 (let ((list (list t)))
                   (nconc list list)))))))

(with-test (:name :endianness-in-features)
  (assert
   (or (member :big-endian *features*)
       (member :little-endian *features*))))

(with-test (:name (trace generic-function))
  (defgeneric traced-gf (x))
  (defmethod traced-gf (x) (1+ x))
  (assert (= (traced-gf 3) 4))
  (trace traced-gf)
  (let ((output (with-output-to-string (*trace-output*)
                  (assert (= (traced-gf 3) 4)))))
    (assert (> (length output) 0)))
  (assert (typep #'traced-gf 'standard-generic-function))
  (untrace traced-gf)
  (let ((output (with-output-to-string (*trace-output*)
                  (assert (= (traced-gf 3) 4)))))
    (assert (= (length output) 0))))

(with-test (:name (apropos :inherited :bug-1364413))
  (let* ((package (make-package "BUGGALO" :use nil))
         (symbol (intern "BUGGALO" package)))
    (export (list symbol) package)
    (let ((inherits (make-package "BUGGALO-INHERITS" :use (list package))))
      (assert (= (length (apropos-list "BUGGALO" package)) 1))
      (assert (= (length (apropos-list "BUGGALO" inherits)) 1))
      (delete-package inherits))
    (delete-package package)))

(with-test (:name (apropos :inherited :external-only :bug-1364413))
  (let* ((package (make-package "BUGGALO" :use nil))
         (symbol (intern "BUGGALO" package)))
    (export (list symbol) package)
    (let ((inherits (make-package "BUGGALO-INHERITS" :use (list package))))
      (assert (= (length (apropos-list "BUGGALO" package t)) 1))
      (assert (= (length (apropos-list "BUGGALO" inherits t)) 0))
      (delete-package inherits))
    (delete-package package)))

(with-test (:name (apropos :once-only))
  (assert (= (length (apropos-list "UPDATE-INSTANCE-FOR-REDEFINED-CLASS")) 1)))

(defgeneric gf-arglist-1 (x &key y))
(defmethod gf-arglist-1 (x &key (y nil) (z nil z-p))
  (list x y z z-p))

(defgeneric gf-arglist-2 (x &key y))
(defmethod gf-arglist-2 ((x integer) &key (y nil) ((z f) nil z-p)) (list x y f z-p))
(defmethod gf-arglist-2 ((x string) &key (y nil) ((z w) nil z-p)) (list x y w z-p))

(defgeneric gf-arglist-3 (x &key ((:y y))))

(defgeneric gf-arglist-4 (x &key ((:y z))))

(defgeneric gf-arglist-5 (x &key y))
(defmethod gf-arglist-5 ((x integer) &key z &allow-other-keys) (list x z))

(with-test (:name (:generic-function-pretty-arglist 1))
  (assert (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-1)
                 '(x &key y z))))
(with-test (:name (:generic-function-pretty-arglist 2))
  (assert (or (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-2)
                     '(x &key y ((z w))))
              (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-2)
                     '(x &key y ((z f)))))))
(with-test (:name (:generic-function-pretty-arglist 3))
  (assert (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-3)
                 '(x &key y))))
(with-test (:name (:generic-function-pretty-arglist 4))
  (assert (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-4)
                 '(x &key ((:y z))))))
(with-test (:name (:generic-function-pretty-arglist 5))
  (assert (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-5)
                 '(x &key y z &allow-other-keys))))
;;;; success
