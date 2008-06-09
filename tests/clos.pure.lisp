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
(assert (null
         (let (collect)
           (sb-pcl::map-all-generic-functions
            (lambda (gf)
              (let ((arg-info (sb-pcl::gf-arg-info gf)))
                (when (eq (sb-pcl::arg-info-lambda-list arg-info)
                          :no-lambda-list)
                  (push gf collect)))))
           (print (nreverse collect)))))

;;; Regressing test for invalid slot specification error printing
(multiple-value-bind (value err)
    (ignore-errors (macroexpand '(defclass foo () (frob (frob bar)))))
  (declare (ignore value))
  (assert (typep err 'simple-condition))
  (multiple-value-bind (value format-err)
      (ignore-errors (apply #'format nil
                            (simple-condition-format-control err)
                            (simple-condition-format-arguments err)))
    (declare (ignore value))
    (assert (not format-err))))

;;; another not (user-)observable behaviour: make sure that
;;; sb-pcl::map-all-classes calls its function on each class once and
;;; exactly once.
(let (result)
  (sb-pcl::map-all-classes (lambda (c) (push c result)))
  (assert (equal result (remove-duplicates result))))

;;; this one's user-observable
(assert (typep #'(setf class-name) 'generic-function))

;;; CLHS 1.4.4.5.  We could test for this by defining methods
;;; (i.e. portably) but it's much easier using the MOP and
;;; MAP-ALL-CLASSES.
(flet ((standardized-class-p (c)
         (eq (class-name c) (find-symbol (symbol-name (class-name c)) "CL"))))
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
    (assert (null result))))

;; No compiler-notes for non-constant slot-names in default policy.
(handler-case
    (compile nil '(lambda (x y z)
                   (setf (slot-value x z)
                         (slot-value y z))))
  (sb-ext:compiler-note (e)
    (error e)))


