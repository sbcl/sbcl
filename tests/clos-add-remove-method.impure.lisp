;;;; testing add/remove-method thread thread safety

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

(defpackage "CLOS-ADD/REMOVE-METHOD"
  (:use "COMMON-LISP" "SB-THREAD"))

(in-package "CLOS-ADD/REMOVE-METHOD")

;;; We make a generic function, add a bunch of method for it, and
;;; prepare another bunch of method objects for later addition.
;;;
;;; Then we run several threads in parallel, removing all the old
;;; ones and adding all the new ones -- and finally we verify that
;;; the resulting method set is correct.

(defgeneric foo (x))

(defvar *to-remove-a* nil)
(defvar *to-remove-b* nil)
(defvar *to-remove-c* nil)
(defvar *to-add-d* nil)
(defvar *to-add-e* nil)
(defvar *to-add-f* nil)

(defun name (key n)
  (intern (format nil "FOO-~A-~A" key n)))

(defun names (key)
  (loop for i from 0 upto 128
        collect (name key i)))

(defun to-remove (key)
  (loop for s in (names key)
        collect
        `(progn
           (defclass ,s () ())
           (defmethod foo ((x ,s))
             ',s)
           (push (find-method #'foo nil (list (find-class ',s)) t)
                 ,(intern (format nil "*TO-REMOVE-~A*" key))))))

(defun to-add (key)
  (loop for s in (names key)
        collect
        `(progn
           (defclass ,s () ())
           (push (make-instance
                  'standard-method
                  :qualifiers nil
                  :specializers (list (find-class ',s))
                  :function (lambda (args next)
                              (declare (ignore args next))
                              ',s)
                  :lambda-list '(x))
                 ,(intern (format nil "*TO-ADD-~A*" key))))))

(macrolet ((def ()
             `(progn
                ,@(to-remove 'a)
                ,@(to-remove 'b)
                ,@(to-remove 'c)
                ,@(to-add 'd)
                ,@(to-add 'e)
                ,@(to-add 'f))))
  (def))

(defvar *run* nil)

(defun remove-methods (list)
  (loop until *run* do (sb-thread:thread-yield))
  (dolist (method list)
    (remove-method #'foo method)))

(defun add-methods (list)
  (loop until *run* do (sb-thread:thread-yield))
  (dolist (method list)
    (add-method #'foo method)))

#+sb-thread
(let ((threads (list (make-thread (lambda () (remove-methods *to-remove-a*)))
                     (make-thread (lambda () (remove-methods *to-remove-b*)))
                     (make-thread (lambda () (remove-methods *to-remove-c*)))
                     (make-thread (lambda () (add-methods *to-add-d*)))
                     (make-thread (lambda () (add-methods *to-add-e*)))
                     (make-thread (lambda () (add-methods *to-add-f*))))))
  (setf *run* t)
  (mapcar #'join-thread threads))

#-sb-thread
(progn
  (setf *run* t)
  (remove-methods *to-remove-a*)
  (remove-methods *to-remove-b*)
  (remove-methods *to-remove-c*)
  (add-methods *to-add-d*)
  (add-methods *to-add-e*)
  (add-methods *to-add-f*))

(let ((target (append *to-add-d* *to-add-e* *to-add-f*))
      (real (sb-mop:generic-function-methods #'foo)))
  (assert (subsetp target real))
  (assert (subsetp real target)))
