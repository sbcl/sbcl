;;;; This file is for compiler tests which have side effects (e.g.
;;;; executing DEFUN) but which don't need any special side-effecting
;;;; environmental stuff (e.g. DECLAIM of particular optimization
;;;; settings). Similar tests which *do* expect special settings may
;;;; be in files compiler-1.impure.lisp, compiler-2.impure.lisp, etc.

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

(cl:in-package :cl-user)

;;; In sbcl-0.6.10, Douglas Brebner reported that (SETF EXTERN-ALIEN)
;;; was messed up so badly that trying to execute expressions like
;;; this signalled an error.
(setf (sb-alien:extern-alien "current_control_stack_pointer" sb-alien:unsigned)
      (sb-alien:extern-alien "current_control_stack_pointer" sb-alien:unsigned))

;;; bug 133, fixed in 0.7.0.5: Somewhere in 0.pre7.*, C void returns
;;; were broken ("unable to use values types here") when
;;; auto-PROCLAIM-of-return-value was added to DEFINE-ALIEN-ROUTINE.
(sb-alien:define-alien-routine ("free" free) void (ptr (* t) :in))

;;; Types of alien functions were being incorrectly DECLAIMED when
;;; docstrings were included in the definition until sbcl-0.7.6.15.
(sb-alien:define-alien-routine ("getenv" ftype-correctness) c-string
  "docstring"
  (name c-string))

(multiple-value-bind (function warningsp failurep)
    (compile nil '(lambda () (ftype-correctness)))
  (assert warningsp))

(multiple-value-bind (function warningsp failurep)
    (compile nil '(lambda () (ftype-correctness "FOO")))
  (assert (not warningsp)))

(multiple-value-bind (function warningsp failurep)
    (compile nil '(lambda () (ftype-correctness "FOO" "BAR")))
  (assert warningsp))

;;; This used to break due to too eager auxiliary type twiddling in
;;; parse-alien-record-type.
(defparameter *maybe* nil)
(defun with-alien-test-for-struct-plus-funcall ()
  (with-alien ((x (struct bar (x unsigned) (y unsigned)))
               ;; bogus definition, but we just need the symbol
               (f (function int (* (struct bar))) :extern "printf"))
    (when *maybe*
      (alien-funcall f (addr x)))))

;;; Mutually referent structures
(define-alien-type struct.1 (struct struct.1 (x (* (struct struct.2))) (y int)))
(define-alien-type struct.2 (struct struct.2 (x (* (struct struct.1))) (y int)))
(let ((s1 (make-alien struct.1))
      (s2 (make-alien struct.2)))
  (setf (slot s1 'x) s2
        (slot s2 'x) s1
        (slot (slot s1 'x) 'y) 1
        (slot (slot s2 'x) 'y) 2)
  (assert (= 1 (slot (slot s1 'x) 'y)))
  (assert (= 2 (slot (slot s2 'x) 'y))))

;;; "Alien bug" on sbcl-devel 2004-10-11 by Thomas F. Burdick caused
;;; by recursive struct definition.
(let ((fname "alien-bug-2004-10-11.tmp.lisp"))
  (unwind-protect
       (progn
         (with-open-file (f fname :direction :output)
           (mapc (lambda (form) (print form f))
                 '((defpackage :alien-bug
                     (:use :cl :sb-alien))
                   (in-package :alien-bug)
                   (define-alien-type objc-class
                       (struct objc-class
                        (protocols
                         (* (struct protocol-list
                                    (list (array (* (struct objc-class))))))))))))
           (load fname)
           (load fname)
           (load (compile-file fname))
           (load (compile-file fname)))
    (delete-file (compile-file-pathname fname))
    (delete-file fname)))

;;; enumerations with only one enum resulted in division-by-zero
;;; reported on sbcl-help 2004-11-16 by John Morrison
(define-alien-type enum.1 (enum nil (:val0 0)))

(define-alien-type enum.2 (enum nil (zero 0) (one 1) (two 2) (three 3)
                                    (four 4) (five 5) (six 6) (seven 7)
                                    (eight 8) (nine 9)))
(with-alien ((integer-array (array int 3)))
  (let ((enum-array (cast integer-array (array enum.2 3))))
    (setf (deref enum-array 0) 'three
          (deref enum-array 1) 'four)
    (setf (deref integer-array 2) (+ (deref integer-array 0)
                                     (deref integer-array 1)))
    (assert (eql (deref enum-array 2) 'seven))))
;; The code that is used for mapping from integers to symbols depends on the
;; `density' of the set of used integers, so test with a sparse set as well.
(define-alien-type enum.3 (enum nil (zero 0) (one 1) (k-one 1001) (k-two 1002)))
(with-alien ((integer-array (array int 3)))
  (let ((enum-array (cast integer-array (array enum.3 3))))
    (setf (deref enum-array 0) 'one
          (deref enum-array 1) 'k-one)
    (setf (deref integer-array 2) (+ (deref integer-array 0)
                                     (deref integer-array 1)))
    (assert (eql (deref enum-array 2) 'k-two))))

;; enums used to allow values to be used only once
;; C enums allow for multiple tags to point to the same value
(define-alien-type enum.4
    (enum nil (:key1 1) (:key2 2) (:keytwo 2)))
(with-alien ((enum-array (array enum.4 3)))
  (setf (deref enum-array 0) :key1)
  (setf (deref enum-array 1) :key2)
  (setf (deref enum-array 2) :keytwo)
  (assert (and (eql (deref enum-array 1) (deref enum-array 2))
               (eql (deref enum-array 1) :key2))))

;;; As reported by Baughn on #lisp, ALIEN-FUNCALL loops forever when
;;; compiled with (DEBUG 3).
(sb-kernel::values-specifier-type-cache-clear)
(proclaim '(optimize (debug 3)))
(let ((f (compile nil '(lambda (v)
                        (sb-alien:alien-funcall (sb-alien:extern-alien "getenv"
                                                 (function (c-string) c-string))
                         v)))))
  (assert (typep (funcall f "HOME") '(or string null))))


;;; CLH: Test for non-standard alignment in alien structs
;;;
(sb-alien:define-alien-type align-test-struct
    (sb-alien:union align-test-union
                    (s (sb-alien:struct nil
                                        (s1 sb-alien:unsigned-char)
                                        (c1 sb-alien:unsigned-char :alignment 16)
                                        (c2 sb-alien:unsigned-char :alignment 32)
                                        (c3 sb-alien:unsigned-char :alignment 32)
                                        (c4 sb-alien:unsigned-char :alignment 8)))
                    (u (sb-alien:array sb-alien:unsigned-char 16))))

(let ((a1 (sb-alien:make-alien align-test-struct)))
  (declare (type (sb-alien:alien (* align-test-struct)) a1))
  (setf (sb-alien:slot (sb-alien:slot a1 's) 's1) 1)
  (setf (sb-alien:slot (sb-alien:slot a1 's) 'c1) 21)
  (setf (sb-alien:slot (sb-alien:slot a1 's) 'c2) 41)
  (setf (sb-alien:slot (sb-alien:slot a1 's) 'c3) 61)
  (setf (sb-alien:slot (sb-alien:slot a1 's) 'c4) 81)
  (assert (equal '(1 21 41 61 81)
                 (list (sb-alien:deref (sb-alien:slot a1 'u) 0)
                       (sb-alien:deref (sb-alien:slot a1 'u) 2)
                       (sb-alien:deref (sb-alien:slot a1 'u) 4)
                       (sb-alien:deref (sb-alien:slot a1 'u) 8)
                       (sb-alien:deref (sb-alien:slot a1 'u) 9)))))

(handler-bind ((compiler-note (lambda (c)
                                (error "bad note! ~A" c))))
  (funcall (compile nil '(lambda () (sb-alien:make-alien sb-alien:int)))))

;;; Test case for unwinding an alien (Win32) exception frame
;;;
;;; The basic theory here is that failing to honor a win32
;;; exception frame during stack unwinding breaks the chain.
;;; "And if / You don't love me now / You will never love me
;;; again / I can still hear you saying / You would never break
;;; the chain."  If the chain is broken and another exception
;;; occurs (such as an error trap caused by an OBJECT-NOT-TYPE
;;; error), the system will kill our process.  No mercy, no
;;; appeal. So, to check that we have done our job properly, we
;;; need some way to put an exception frame on the stack and then
;;; unwind through it, then trigger another exception.  (FUNCALL
;;; 0) will suffice for the latter, and a simple test shows that
;;; CallWindowProc() establishes a frame and calls a function
;;; passed to it as an argument.
#+win32
(progn
  (load-shared-object "USER32")
  (assert
   (eq :ok
       (handler-case
           (tagbody
              (alien-funcall
               (extern-alien "CallWindowProcW"
                             (function unsigned-int
                                       (* (function int)) unsigned-int
                                       unsigned-int unsigned-int unsigned-int))
               (alien-sap
                (sb-alien::alien-callback (function unsigned-int)
                                          #'(lambda () (go up))))
               0 0 0 0)
            up
              (funcall 0))
         (error ()
           :ok)))))

;;; Unused local alien caused a compiler error
(with-test (:name unused-local-alien)
  (let ((fun `(lambda ()
                (sb-alien:with-alien ((alien1923 (array (sb-alien:unsigned 8) 72)))
                  (values)))))
    (assert (not (funcall (compile nil fun))))))

;;; Non-local exit from WITH-ALIEN caused alien stack to be leaked.
(defvar *sap-int*)
(defun try-to-leak-alien-stack (x)
  (with-alien ((alien (array (sb-alien:unsigned 8) 72)))
    (let ((sap-int (sb-sys:sap-int (alien-sap alien))))
      (if *sap-int*
          (assert (= *sap-int* sap-int))
          (setf *sap-int* sap-int)))
    (when x
      (return-from try-to-leak-alien-stack 'going))
    (never)))
(with-test (:name :nlx-causes-alien-stack-leak)
  (let ((*sap-int* nil))
    (loop repeat 1024
          do (try-to-leak-alien-stack t))))

;;; success
