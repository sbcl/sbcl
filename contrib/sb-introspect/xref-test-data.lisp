;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(defpackage :sb-introspect-test/xref
  (:use "SB-INTROSPECT" "CL" "SB-RT"))

(in-package :sb-introspect-test/xref)

(defvar *a* nil)
(defconstant +z+ 'zzz)

(defun foo () 1)
(defun bar (x) x)

;; Should:
;;   reference *a*
;;   call bar
;;   not call foo
;;   not call xref/2
(defun xref/1 ()
  (flet ((foo ()
           (bar *a*)))
    (flet ((xref/2 ()
             1))
      (foo)
      (xref/2))))

;; Should:
;;   reference *a*, set *a*, bind *a*
;;   call xref/1
;;   not bind b
(defun xref/2 ()
  (setf *a* *a*)
  (let* ((b 1)
         (*a* b))
    (when nil
      (xref/1))))

(let ((x 1))
  ;; Should:
  ;;   call bar
  ;;   not reference *a*
  (defun xref/3 ()
    (bar x))
  ;; Should:
  ;;   not call bar
  ;;   reference *a*
  (defun xref/4 ()
    (setf x *a*)))


(flet ((z ()
         (xref/2)))
  ;; Should:
  ;;   call xref/2
  ;;   not call z
  (defun xref/5 ()
    (z))
  ;; Should:
  ;;   call xref/2
  ;;   not call z
  (defun xref/6 ()
    (z)))

(defun xref/7 ()
  (flet ((a ()
           (xref/6)))
    #'a))

;; call xref/2
(let ((a 1))
  (defvar *b* (or (xref/2) a)))

;; call xref/6
(defvar *c* (xref/6))

;; call xref/2 twice (not three times)
(defun xref/8 ()
  (flet ((a ()
           (xref/2)))
    (a)
    (a)
    (xref/2)))

;; Methods work, even ones with lots of arguments.
(defmethod xref/10 (a b c d e f g h (i fixnum))
  (xref/2))

;; Separate methods are indeed separate
(defmethod xref/11 ((a fixnum))
  (declare (ignore a))
  (xref/2))

(defmethod xref/11 ((a (eql 'z)))
  (declare (ignore a))
  (xref/2))

(defmethod xref/11 ((a float))
  (declare (ignore a))
  (xref/3))

(declaim (inline inline/1))
(defun inline/1 ()
  (xref/3)
  (values +z+ *a*))

(eval-when (:compile-toplevel :load-toplevel)
  (defun xref/12 ()
    (flet ((a ()
             ;; Counts as calling xref/2
             (xref/2)))
      (declare (inline a))
      (a)
      ;; Doesn't count as calling xref/3, or referring to +z+ / *a*
      (inline/1))))

;; last node of block should also be taken into account
(defun xref/13 (x)
  (setf *a* x))

(defun xref/14 ()
  *a*)

(sb-ext:defglobal **global** 31)

(defun xref/15 ()
  **global**)

(defun xref/16 (x)
  (setf **global** x))

;; calling a function in a macro body
(defmacro macro/1 ()
  (when nil
    (xref/12))
  nil)

;; expanding a macro
(defun macro-use/1 ()
  (macro/1))

;; expanding a macro in an flet/labels
(defun macro-use/2 ()
  (flet ((inner-flet ()
           (macro/1)))
    (inner-flet)))

;; expanding a macro in an toplevel flet/labels
(flet ((outer-flet ()
         (macro/1)))
  (defun macro-use/3 ()
    (outer-flet)))

;; expanding a macro in an inlined flet/labels
(defun macro-use/4 ()
  (flet ((inner-flet ()
           (macro/1)))
    (declare (inline inner-flet))
    (inner-flet)))

(declaim (inline inline/2))
(defun inline/2 ()
  (macro/1))

;; Inlining inline/3 doesn't count as macroexpanding macro/1
(defun macro-use/5 ()
  (inline/2))

;;; Code in the macrolet definition bodies is currently not considered
;;; at all for XREF. Maybe it should be, but it's slightly tricky to
;;; implement.
#+nil
(progn
  (defun macrolet/1 ()
    (macrolet ((a ()
                 (inline/2)
               1))
      (a)))
  (defun macrolet/2 ()
    (macrolet ((inner-m ()
                 (macro/1)))
      (inner-m))))

;;; Inlining functions with non-trivial lambda-lists.
(declaim (inline inline/3))
(defun inline/3 (a &optional b &key c d)
  (declare (sb-ext:muffle-conditions sb-kernel:&optional-and-&key-in-lambda-list))
  (list a b c d))
(defun inline/3-user/1 (a)
  (inline/3 a))
(defun inline/3-user/2 (a b)
  (inline/3 a b))
(defun inline/3-user/3 (a b c)
  (inline/3 a b :c c))
(defun inline/3-user/4 (a b c d)
  (inline/3 a b :d d :c c))

(declaim (inline inline/4))
(defun inline/4 (a &rest more)
  (cons a more))
(defun inline/4-user ()
  (inline/4 :a :b :c))

;;; Test references to / from compiler-macros and source-transforms

(define-compiler-macro cmacro (x)
  `(+ ,x 42))
(defstruct struct slot)
(defun source-user (x)
  (cmacro (struct-slot x)))

;;; Test specialization

(defclass a-class () ())
(defclass a-subclass (a-class) ())

(defstruct a-structure)
(defstruct (a-substructure (:include a-structure)))

(defvar *an-instance-of-a-class* (make-instance 'a-class))
(defvar *an-instance-of-a-subclass* (make-instance 'a-subclass))

(defvar *an-instance-of-a-structure* (make-a-structure))
(defvar *an-instance-of-a-substructure* (make-a-substructure))

(defmethod a-gf-1 ((x a-class)))
(defmethod a-gf-1 ((x a-structure)))

(defmethod a-gf-2 ((x (eql *an-instance-of-a-class*))))
(defmethod a-gf-2 ((x (eql *an-instance-of-a-structure*))))

(defmethod a-gf-3 ((x (eql *an-instance-of-a-subclass*))))
(defmethod a-gf-3 ((x (eql *an-instance-of-a-substructure*))))

(defun called-by-traced-fun ())

(defun traced-fun ()
  (called-by-traced-fun))
(trace traced-fun)

#+sb-eval
(progn
  (defun called-by-interpreted-funs ())

  (let ((sb-ext:*evaluator-mode* :interpret))
    (eval '(defun interpreted-fun ()
             (called-by-interpreted-funs))))

  (let ((sb-ext:*evaluator-mode* :interpret))
    (eval '(defun traced-interpreted-fun ()
            (called-by-interpreted-funs))))
  (trace traced-interpreted-fun))
