;;;; CLOS tests with no side effects

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

;;; not really a test for observable behaviour, but: make sure that
;;; all generic functions on startup have lambda lists known to the
;;; system, because some functionality (e.g. &key argument checking)
;;; depends on it.  The basic functionality is tested elsewhere, but
;;; this is to investigate the internals for possible inconsistency.
(with-test (:name (:builtin-generic-functions :known :lambda-list))
  (let ((collect '()))
    (sb-pcl::map-all-generic-functions
     (lambda (gf)
       (let ((arg-info (sb-pcl::gf-arg-info gf)))
         (when (eq (sb-pcl::arg-info-lambda-list arg-info)
                   :no-lambda-list)
           (push gf collect)))))
    (assert (null collect))))

;;; Regressing test for invalid slot specification error printing
(with-test (:name (defclass :slot :syntax-error print))
  (multiple-value-bind (value err)
      (ignore-errors (macroexpand '(defclass foo () (frob (frob bar)))))
    (declare (ignore value))
    (assert (typep err 'simple-condition))
    (multiple-value-bind (value format-err)
        (ignore-errors (apply #'format nil
                              (simple-condition-format-control err)
                              (simple-condition-format-arguments err)))
      (declare (ignore value))
      (assert (not format-err)))))

;;; another not (user-)observable behaviour: make sure that
;;; sb-pcl::map-all-classes calls its function on each class once and
;;; exactly once.
(with-test (:name (sb-pcl::map-all-classes :no-duplicates))
  (let ((result '()))
    (sb-pcl::map-all-classes (lambda (c) (push c result)))
    (assert (equal result (remove-duplicates result)))))

;;; this one's user-observable
(with-test (:name (type-of (setf class-name)))
  (assert (typep #'(setf class-name) 'generic-function)))

;;; CLHS 1.4.4.5.  We could test for this by defining methods
;;; (i.e. portably) but it's much easier using the MOP and
;;; MAP-ALL-CLASSES.
(with-test (:name :check-standard-superclasses)
  (flet ((standardized-class-p (c)
           (and (class-name c)
                (eq (symbol-package (class-name c))
                    (find-package :cl)))))
    (let (result)
      (sb-pcl::map-all-classes
       (lambda (c) (when (standardized-class-p c)
                     (let* ((cpl (sb-mop:class-precedence-list c))
                            (std (position (find-class 'standard-object) cpl))
                            (str (position (find-class 'structure-object) cpl))
                            (last (position-if
                                   #'standardized-class-p (butlast cpl)
                                   :from-end t)))
                       (when (and std str)
                         (push `(:and ,c) result))
                       (when (and str (< str last))
                         (push `(:str ,c) result))
                       (when (and std (< std last))
                         (push `(:std ,c) result))))))
      (assert (null result)))))

;; No compiler-notes for non-constant slot-names in default policy.
(with-test (:name (slot-value :no sb-ext:compiler-note))
  (checked-compile '(lambda (x y z)
                     (setf (slot-value x z) (slot-value y z)))
                   :allow-notes nil))

(with-test (:name :slot-table-of-symbol-works)
  (assert (eq :win
              ;; the error that I want is about a missing slot,
              ;; not a missing method, so don't let the compiler turn
              ;; this into (funcall #'(SLOT-ACCESSOR :GLOBAL A READER)...)
              (handler-case (eval '(slot-value 'a 'a))
                (simple-condition (c)
                  (and (search "slot ~S is missing"
                               (simple-condition-format-control c))
                       :win))))))

(with-test (:name :funcallable-instance-sxhash)
  (assert
   (/= (sxhash (make-instance 'sb-mop:funcallable-standard-object))
       (sxhash (make-instance 'sb-mop:funcallable-standard-object))
       42)))

(with-test (:name (typep :literal-class))
  (checked-compile-and-assert ()
      `(lambda (x)
         (typep x #.(find-class 'symbol)))
    (('x) t)))
