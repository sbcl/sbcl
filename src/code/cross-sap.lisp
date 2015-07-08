;;;; support and placeholders for System Area Pointers (SAPs) in the host
;;;; Common Lisp at cross-compile time

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!SYS")

;;; SYSTEM-AREA-POINTER is not a primitive type in ANSI Common Lisp,
;;; so we need a compound type to represent it in the host Common Lisp
;;; at cross-compile time:
(defstruct (system-area-pointer (:constructor int-sap (int))
                                (:conc-name "SAP-"))
  ;; the integer representation of the address
  (int nil :type unsigned-byte :read-only t))

;;; cross-compilation-host analogues of target-CMU CL primitive SAP operations
#.`(progn
     ,@(mapcar (lambda (info)
                 (destructuring-bind (sap-fun int-fun) info
                   `(defun ,sap-fun (x y)
                      (,int-fun (sap-int x) (sap-int y)))))
               '((sap< <) (sap<= <=) (sap= =) (sap>= >=) (sap> >) (sap- -))))

;;; dummies, defined so that we can declare they never return and
;;; thereby eliminate a thundering herd of optimization notes along
;;; the lines of "can't optimize this expression because we don't know
;;; the return type of SAP-REF-8"
(defun sap-ref-stub (name)
  (error "~S doesn't make sense on cross-compilation host." name))
#.`(progn
     ,@(mapcan (lambda (name)
                 `((declaim (ftype (function (system-area-pointer fixnum) nil)
                                   ,name))
                   (defun ,name (sap offset)
                     (declare (ignore sap offset))
                     (sap-ref-stub ',name))
                   ,@(let ((setter-stub (gensym "SETTER-STUB-")))
                       `((defun ,setter-stub (foo sap offset)
                           (declare (ignore foo sap offset))
                           (sap-ref-stub '(setf ,name)))
                         (defsetf ,name ,setter-stub)))))
               '(sap-ref-8
                 sap-ref-16
                 sap-ref-32
                 sap-ref-64
                 sap-ref-sap
                 sap-ref-word
                 sap-ref-single
                 sap-ref-double
                 signed-sap-ref-8
                 signed-sap-ref-16
                 signed-sap-ref-32
                 signed-sap-ref-64
                 signed-sap-ref-word)))
