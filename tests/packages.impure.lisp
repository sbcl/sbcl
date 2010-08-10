;;;; miscellaneous tests of package-related stuff

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

(make-package "FOO")
(defvar *foo* (find-package (coerce "FOO" 'base-string)))
(rename-package "FOO" (make-array 0 :element-type nil))
(assert (eq *foo* (find-package "")))
(assert (delete-package ""))

(make-package "BAR")
(defvar *baz* (rename-package "BAR" "BAZ"))
(assert (eq *baz* (find-package "BAZ")))
(assert (delete-package *baz*))

(handler-case
    (export :foo)
  (package-error (c) (princ c))
  (:no-error (&rest args) (error "(EXPORT :FOO) returned ~S" args)))

(make-package "FOO")
(assert (shadow #\a :foo))

(defpackage :PACKAGE-DESIGNATOR-1 (:use #.(find-package :cl)))

(defpackage :PACKAGE-DESIGNATOR-2
  (:import-from #.(find-package :cl) "+"))

(defpackage "EXAMPLE-INDIRECT"
  (:import-from "CL" "+"))

(defpackage "EXAMPLE-PACKAGE"
  (:shadow "CAR")
  (:shadowing-import-from "CL" "CAAR")
  (:use)
  (:import-from "CL" "CDR")
  (:import-from "EXAMPLE-INDIRECT" "+")
  (:export "CAR" "CDR" "EXAMPLE"))

(flet ((check-symbol (name expected-status expected-home-name)
         (multiple-value-bind (symbol status)
             (find-symbol name "EXAMPLE-PACKAGE")
           (let ((home (symbol-package symbol))
                 (expected-home (find-package expected-home-name)))
             (assert (eql home expected-home))
             (assert (eql status expected-status))))))
  (check-symbol "CAR" :external "EXAMPLE-PACKAGE")
  (check-symbol "CDR" :external "CL")
  (check-symbol "EXAMPLE" :external "EXAMPLE-PACKAGE")
  (check-symbol "CAAR" :internal "CL")
  (check-symbol "+" :internal "CL")
  (check-symbol "CDDR" nil "CL"))

(defpackage "TEST-ORIGINAL" (:nicknames "A-NICKNAME"))

(assert (raises-error? (defpackage "A-NICKNAME")))

(assert (eql (find-package "A-NICKNAME")
             (find-package "TEST-ORIGINAL")))

;;;; Utilities
(defun sym (package name)
 (let ((package (or (find-package package) package)))
   (multiple-value-bind (symbol status)
       (find-symbol name package)
     (assert status
             (package name symbol status)
             "No symbol with name ~A in ~S." name package symbol status)
   (values symbol status))))

(defmacro with-name-conflict-resolution ((symbol &key restarted)
                                         form &body body)
  "Resolves potential name conflict condition arising from FORM.

The conflict is resolved in favour of SYMBOL, a form which must
evaluate to a symbol.

If RESTARTED is a symbol, it is bound for the BODY forms and set to T
if a restart was invoked."
  (check-type restarted symbol "a binding name")
  (let ((%symbol (copy-symbol 'symbol)))
    `(let (,@(when restarted `((,restarted)))
           (,%symbol ,symbol))
       (handler-bind
           ((sb-ext:name-conflict
             (lambda (condition)
               ,@(when restarted `((setf ,restarted t)))
               (assert (member ,%symbol (sb-ext:name-conflict-symbols condition)))
               (invoke-restart 'sb-ext:resolve-conflict ,%symbol))))
         ,form)
       ,@body)))

(defmacro with-packages (specs &body forms)
  (let ((names (mapcar #'car specs)))
    `(unwind-protect
          (progn
            (delete-packages ',names)
            ,@(mapcar (lambda (spec)
                        `(defpackage ,@spec))
                      specs)
            ,@forms)
       (delete-packages ',names))))

(defun delete-packages (names)
  (dolist (p names)
    (ignore-errors (delete-package p))))


;;;; Tests
;;; USE-PACKAGE
(with-test (:name use-package.1)
  (with-packages (("FOO" (:export "SYM"))
                  ("BAR" (:export "SYM"))
                  ("BAZ" (:use)))
    (with-name-conflict-resolution ((sym "BAR" "SYM") :restarted restartedp)
        (use-package '("FOO" "BAR") "BAZ")
      (is restartedp)
      (is (eq (sym "BAR" "SYM")
              (sym "BAZ" "SYM"))))))

(with-test (:name use-package.2)
  (with-packages (("FOO" (:export "SYM"))
                  ("BAZ" (:use) (:intern "SYM")))
    (with-name-conflict-resolution ((sym "FOO" "SYM") :restarted restartedp)
        (use-package "FOO" "BAZ")
      (is restartedp)
      (is (eq (sym "FOO" "SYM")
              (sym "BAZ" "SYM"))))))

(with-test (:name use-package.2a)
  (with-packages (("FOO" (:export "SYM"))
                  ("BAZ" (:use) (:intern "SYM")))
    (with-name-conflict-resolution ((sym "BAZ" "SYM") :restarted restartedp)
        (use-package "FOO" "BAZ")
      (is restartedp)
      (is (equal (list (sym "BAZ" "SYM") :internal)
                 (multiple-value-list (sym "BAZ" "SYM")))))))

(with-test (:name use-package-conflict-set :fails-on :sbcl)
  (with-packages (("FOO" (:export "SYM"))
                  ("QUX" (:export "SYM"))
                  ("BAR" (:intern "SYM"))
                  ("BAZ" (:use) (:import-from "BAR" "SYM")))
    (let ((conflict-set))
      (block nil
        (handler-bind
            ((sb-ext:name-conflict
              (lambda (condition)
                (setf conflict-set (copy-list
                                    (sb-ext:name-conflict-symbols condition)))
                (return))))
          (use-package '("FOO" "QUX") "BAZ")))
      (setf conflict-set
            (sort conflict-set #'string<
                  :key (lambda (symbol)
                         (package-name (symbol-package symbol)))))
      (is (equal (list (sym "BAR" "SYM") (sym "FOO" "SYM") (sym "QUX" "SYM"))
                 conflict-set)))))

;;; EXPORT
(with-test (:name export.1)
  (with-packages (("FOO" (:intern "SYM"))
                  ("BAR" (:export "SYM"))
                  ("BAZ" (:use "FOO" "BAR")))
    (with-name-conflict-resolution ((sym "FOO" "SYM") :restarted restartedp)
        (export (sym "FOO" "SYM") "FOO")
      (is restartedp)
      (is (eq (sym "FOO" "SYM")
              (sym "BAZ" "SYM"))))))

(with-test (:name export.1a)
  (with-packages (("FOO" (:intern "SYM"))
                  ("BAR" (:export "SYM"))
                  ("BAZ" (:use "FOO" "BAR")))
    (with-name-conflict-resolution ((sym "BAR" "SYM") :restarted restartedp)
        (export (sym "FOO" "SYM") "FOO")
      (is restartedp)
      (is (eq (sym "BAR" "SYM")
              (sym "BAZ" "SYM"))))))

(with-test (:name export.ensure-exported)
  (with-packages (("FOO" (:intern "SYM"))
                  ("BAR" (:export "SYM"))
                  ("BAZ" (:use "FOO" "BAR") (:IMPORT-FROM "BAR" "SYM")))
    (with-name-conflict-resolution ((sym "FOO" "SYM") :restarted restartedp)
        (export (sym "FOO" "SYM") "FOO")
      (is restartedp)
      (is (equal (list (sym "FOO" "SYM") :external)
                 (multiple-value-list (sym "FOO" "SYM"))))
      (is (eq (sym "FOO" "SYM")
              (sym "BAZ" "SYM"))))))

(with-test (:name export.3.intern)
  (with-packages (("FOO" (:intern "SYM"))
                  ("BAZ" (:use "FOO") (:intern "SYM")))
    (with-name-conflict-resolution ((sym "FOO" "SYM") :restarted restartedp)
        (export (sym "FOO" "SYM") "FOO")
      (is restartedp)
      (is (eq (sym "FOO" "SYM")
              (sym "BAZ" "SYM"))))))

(with-test (:name export.3a.intern)
  (with-packages (("FOO" (:intern "SYM"))
                  ("BAZ" (:use "FOO") (:intern "SYM")))
    (with-name-conflict-resolution ((sym "BAZ" "SYM") :restarted restartedp)
        (export (sym "FOO" "SYM") "FOO")
      (is restartedp)
      (is (equal (list (sym "BAZ" "SYM") :internal)
                 (multiple-value-list (sym "BAZ" "SYM")))))))

;;; IMPORT
(with-test (:name import-nil.1)
  (with-packages (("FOO" (:use) (:intern "NIL"))
                  ("BAZ" (:use) (:intern "NIL")))
    (with-name-conflict-resolution ((sym "FOO" "NIL") :restarted restartedp)
        (import (list (sym "FOO" "NIL")) "BAZ")
      (is restartedp)
      (is (eq (sym "FOO" "NIL")
              (sym "BAZ" "NIL"))))))

(with-test (:name import-nil.2)
  (with-packages (("BAZ" (:use) (:intern "NIL")))
    (with-name-conflict-resolution ('CL:NIL :restarted restartedp)
        (import '(CL:NIL) "BAZ")
      (is restartedp)
      (is (eq 'CL:NIL
              (sym "BAZ" "NIL"))))))

(with-test (:name import-single-conflict :fails-on :sbcl)
  (with-packages (("FOO" (:export "NIL"))
                  ("BAR" (:export "NIL"))
                  ("BAZ" (:use)))
    (let ((conflict-sets '()))
      (handler-bind
          ((sb-ext:name-conflict
            (lambda (condition)
              (push (copy-list (sb-ext:name-conflict-symbols condition))
                    conflict-sets)
              (invoke-restart 'sb-ext:resolve-conflict 'CL:NIL))))
        (import (list 'CL:NIL (sym "FOO" "NIL") (sym "BAR" "NIL")) "BAZ"))
      (is (eql 1 (length conflict-sets)))
      (is (eql 3 (length (first conflict-sets)))))))

;;; Make sure that resolving a name-conflict in IMPORT doesn't leave
;;; multiple symbols of the same name in the package (this particular
;;; scenario found in 1.0.38.9, but clearly a longstanding issue).
(with-test (:name import-conflict-resolution)
  (with-packages (("FOO" (:export "NIL"))
                  ("BAR" (:use)))
    (with-name-conflict-resolution ((sym "FOO" "NIL"))
      (import (list 'CL:NIL (sym "FOO" "NIL")) "BAR"))
    (do-symbols (sym "BAR")
      (assert (eq sym (sym "FOO" "NIL"))))))

;;; UNINTERN
(with-test (:name unintern.1)
  (with-packages (("FOO" (:export "SYM"))
                  ("BAR" (:export "SYM"))
                  ("BAZ" (:use "FOO" "BAR") (:shadow "SYM")))
    (with-name-conflict-resolution ((sym "FOO" "SYM") :restarted restartedp)
        (unintern (sym "BAZ" "SYM") "BAZ")
      (is restartedp)
      (is (eq (sym "FOO" "SYM")
              (sym "BAZ" "SYM"))))))

;;; WITH-PACKAGE-ITERATOR error signalling had problems
(with-test (:name with-package-itarator.error)
  (assert (eq :good
              (handler-case
                  (progn
                    (eval '(with-package-iterator (sym :cl-user :foo)
                            (sym)))
                    :bad)
                ((and simple-condition program-error) (c)
                  (assert (equal (list :foo) (simple-condition-format-arguments c)))
                  :good)))))

;;; MAKE-PACKAGE error in another thread blocking FIND-PACKAGE & FIND-SYMBOL
#+sb-thread
(with-test (:name :bug-511072)
  (let* ((p (make-package :bug-511072))
         (sem (sb-thread:make-semaphore))
         (t2 (sb-thread:make-thread (lambda ()
                                      (handler-bind ((error (lambda (c)
                                                              (sb-thread:signal-semaphore sem)
                                                              (signal c))))
                                        (make-package :bug-511072))))))
    (sb-thread:wait-on-semaphore sem)
    (assert (eq 'cons (read-from-string "CL:CONS")))))
