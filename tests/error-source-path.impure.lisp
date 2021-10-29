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


;;; Utilities

(defmacro assert-condition-source-paths (form &rest source-paths)
  `(assert (equal (checked-compile-condition-source-paths
                   '(lambda () ,form))
                  (mapcar (lambda (path)
                            (append path '(2 0)))
                          '(,@source-paths)))))

(defmacro warning-signalling-macro (&body body)
  (warn "warning from macro")
  `(progn ,@body))

(defmacro error-signalling-macro (&body body)
  (declare (ignore body))
  (error "error from macro"))


;;; Tests

(with-test (:name (:source-path multiple-value-bind))
  (assert-condition-source-paths
   (multiple-value-bind (1 2) (list 1 2))
   (1)))

(with-test (:name (:source-path multiple-value-setq))
  (assert-condition-source-paths
   (multiple-value-setq (1 2) (list 1 2))
   (1)))

(with-test (:name (:source-path cond))
  (assert-condition-source-paths (cond 1) (1))
  (assert-condition-source-paths (cond #()) (1))
  (assert-condition-source-paths (cond "foo") (1))
  (assert-condition-source-paths (cond (t t) 1) (2)))

(with-test (:name (:source-path do))
  (assert-condition-source-paths (do () 1) ())
  (assert-condition-source-paths (do 1 (t)) ()) ; should be improved
  (assert-condition-source-paths (do (1) (t)) (1))
  (assert-condition-source-paths (do (x (1)) (t)) (1 1)))

(with-test (:name (:source-path do*))
  (assert-condition-source-paths (do* () 1) ())
  (assert-condition-source-paths (do* 1 (t)) ()) ; should be improved
  (assert-condition-source-paths (do* (1) (t)) (1))
  (assert-condition-source-paths (do* (x (1)) (t)) (1 1)))

(with-test (:name (:source-path dolist))
  (assert-condition-source-paths (dolist (x (1 . 2))) (1 1)))

(with-test (:name (:source-path restart-bind))
  (assert-condition-source-paths (restart-bind ((continue (lambda ()) 1))) (0 1))
  (assert-condition-source-paths (restart-bind ((nil (lambda ()) 1))) (0 1)))

(with-test (:name (:source-path restart-case))
  (assert-condition-source-paths (restart-case 1 (1)) (2))
  (assert-condition-source-paths (restart-case 1 (continue 1)) (2)))

(with-test (:name (:source-path handler-bind))
  (assert-condition-source-paths (handler-bind (1)) (1))
  (assert-condition-source-paths
   (handler-bind ((error (lambda (c) (declare (ignore c))) 1)))
   (0 1))

  ;; Not sure what's going on with this one.
  #+nil (assert-condition-source-paths
   (handler-bind ((no-such-type #'continue)))
   (0 1)))

(with-test (:name (:source-path handler-case))
  (assert-condition-source-paths (handler-case 1 (error)) (2)))

(with-test (:name (:source-path case))
  (assert-condition-source-paths (case 1 1) (2))
  (assert-condition-source-paths (case 1 (a :a) 1) (3))
  (assert-condition-source-paths (case 1 (a :a) (a :b)) (3))
  (assert-condition-source-paths (case 1 (t :a) (b :b)) (2)))

(with-test (:name (:source-path declare))
  (assert-condition-source-paths (declare (1)) (1))
  (assert-condition-source-paths (declare (type integer) (1)) (2)))

(with-test (:name (:source-path defclass :slot :type :initform))
  (assert-condition-source-paths
   (defclass foo () ((x :type string :initform 1)))
   (0 3))
  (assert-condition-source-paths
   (defclass foo () ((x :type string :initform (+ 1 5))))
   (4 0 3)))

(with-test (:name (:source-path defclass :slot :type :malformed))
  (assert-condition-source-paths
   (defclass foo () ((x :type 1)))
   (0 3))
  (assert-condition-source-paths
   (defclass foo () ((x :type (null cons))))
   (2 0 3))
  (assert-condition-source-paths
   (define-condition foo () ((x :type 1)))
   (0 3))
  (assert-condition-source-paths
   (define-condition foo () ((x :type (null cons))))
   (2 0 3)))

(with-test (:name (:source-path :&key :initform))
  (assert-condition-source-paths
   (defun foo (&key (x 15))
     (declare (float x))
     x)
   (1 2))
  (assert-condition-source-paths
   (defun foo (&key (x /))
     (declare (float x))
     x)
   (1 2))
  (assert-condition-source-paths
   (defun foo (&key (x (print 16)))
     (declare (float x))
     x)
   (1 1 2)))

(with-test (:name (:source-path :defstruct :initform))
  (assert-condition-source-paths
   (locally (declare (optimize (safety 0)))
    (defstruct f
      (x (print t) :type fixnum)))
   (2 2))
  (assert-condition-source-paths
   (locally (declare (optimize (safety 0)))
     (defstruct f
       (x 33 :type cons)))
   (2 2))
  (assert-condition-source-paths
   (locally (declare (optimize (safety 0)))
     (defstruct f
       (x mm)))
   (2 2)))

(with-test (:name (:source-path defgeneric :lambda-list))
  (assert-condition-source-paths
   (defgeneric foo (x x))
   (2)))

(with-test (:name (:source-path defmethod :lambda-list))
  (assert-condition-source-paths
   (defmethod foo (x x))
   (2)))

(defclass deprecated-class () ())
(declaim (deprecated :early "1.0" (type deprecated-class)))
(defgeneric using-deprecated (thing))
(with-test (:name (:source-path defmethod deprecated :specializer))
  (assert-condition-source-paths
   (defmethod using-deprecated ((thing deprecated-class)))
   (0 2)))

(with-test (:name (:source-path defclass deprecated :slot :type))
  (assert-condition-source-paths
   (defclass foo ()
     ((bar :type deprecated-class)))
   (0 3))
  (assert-condition-source-paths
   (define-condition foo ()
     ((bar :type deprecated-class)))
   (0 3)))

(with-test (:name (:source-path defmethod :walk-body))
  (assert-condition-source-paths
   (defmethod using-deprecated ((thing t))
     (progn (warning-signalling-macro)))
   (1 3) (1 3)) ; FIXME duplication is an artifact of DEFMETHOD's implementation
  (assert-condition-source-paths
   (defmethod using-deprecated ((thing t))
     (progn (error-signalling-macro)))
   (1 3)))

;;; In the following two tests, using 1 as the instance avoids
;;; "undefined variable" noise. The strange "slot names" EVEN and ODD
;;; stem from that (and would work with WITH-ACCESSORS).

(with-test (:name (:source-path with-slots))
  ;; instance sub-form
  (assert-condition-source-paths
   (with-slots (even) (the integer 1 2))
   (2))
  ;; slot-entry sub-forms
  (assert-condition-source-paths
   (with-slots (1) 1)
   (1))
  (assert-condition-source-paths
   (with-slots (()) 1)
   (1))
  (assert-condition-source-paths
   (with-slots ((even)) 1)
   (0 1))
  (assert-condition-source-paths
   (with-slots ((even 1)) 1)
   (0 1))
  (assert-condition-source-paths
   (with-slots ((even even) (odd odd 1)) 1)
   (1 1)))

(with-test (:name (:source-path with-accessors))
  ;; instance sub-form
  (assert-condition-source-paths
   (with-accessors ((even evenp)) (the integer 1 2))
   (2))
  ;; slot-entry sub-forms
  (assert-condition-source-paths
   (with-accessors (1) 1)
   (1))
  (assert-condition-source-paths
   (with-accessors (()) 1)
   (1))
  (assert-condition-source-paths
   (with-accessors ((even)) 1)
   (0 1))
  (assert-condition-source-paths
   (with-accessors ((even evenp) (odd oddp 1)) 1)
   (1 1)))

(with-test (:name (:source-path flet :unused))
  (assert-condition-source-paths
   (flet ((f ())))
   (0 1)))

(with-test (:name (:source-path flet :malformed))
  (assert-condition-source-paths
   (flet ((f)))
   (0 1))
  (assert-condition-source-paths
   (flet #())
   ()))

(with-test (:name (:source-path labels :unused))
  (assert-condition-source-paths
   (labels ((f ())))
   (0 1)))

(with-test (:name (:source-path labels :malformed))
  (assert-condition-source-paths
   (labels ((f)))
   (0 1))
  (assert-condition-source-paths
   (labels #())
   ()))

(with-test (:name (:source-path let :malformed))
  (assert-condition-source-paths
   (let ((x 1 2)))
   (0 1))
  (assert-condition-source-paths
   (let #())
   ()))

(with-test (:name (:source-path let* :malformed))
  (assert-condition-source-paths
   (let* ((x 1 2)))
   (0 1))
  (assert-condition-source-paths
   (let* #())
   ()))

(with-test (:name (:source-path typep :invalid-type-specifier))
  (assert-condition-source-paths
   (typep 1 'undefined-type)
   ;; both the style-warning and the note count
   (2) (2)))

(with-test (:name :dead-code-note-after-transforms)
  (assert
   (typep (nth-value 4
                     (checked-compile
                      `(lambda (x)
                         (when nil
                           (funcall x)))))
          '(cons sb-ext:code-deletion-note null))))

(with-test (:name :ignore-deleted-subforms)
  (assert-condition-source-paths
   (lambda (x m)
     (when nil
       (funcall x
                (if m
                    (print 20)
                    (print x)))))
   (2 2)))

(with-test (:name :ignore-deleted-subforms.2)
  (assert-condition-source-paths
   (lambda ()
     (when nil
       (let ((z (print 10)))
         z)))
   (2 2)))

(with-test (:name :ignore-deleted-subforms.3)
  (assert-condition-source-paths
   (lambda (x)
     (when x
       (unless x
         (let ((z (print 10)))
           (if z
               10
               (funcall x))))))
   (2 2 2)))

(with-test (:name :ignore-deleted-subforms.4)
  (assert-condition-source-paths
   (lambda (x)
     (when nil
       (if (print 10)
           10
           x)))
   (2 2)))
