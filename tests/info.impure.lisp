;;;; tests of the INFO/globaldb system
;;;;
;;;; KLUDGE: Unlike most of the system's tests, these are not in the
;;;; problem domain, but in the implementation domain, so modification
;;;; of the system could cause these tests to fail even if the system
;;;; was still a correct implementation of ANSI Common Lisp + SBCL
;;;; extensions. Perhaps such tests should be separate from tests in
;;;; the problem domain. -- WHN 2001-02-11

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

(in-package :cl-user)

(defun foo (a) (list a))
(let ((x 1)) (foo x))

(assert (eq (sb-int:info :function :where-from 'foo)
            :defined))

(defun foo (a b) (list a b))
(let ((x 1)) (foo x 2))

(flet ((foo (a b c)
         (list a b c)))
  (foo 1 2 3))

;;; FIXME: This one is commented out since it doesn't work when
;;; the DEFUN is just LOADed instead of COMPILE-FILEd, and it's
;;; not immediately obvious what's the best way to set up
;;; the COMPILE-FILE test.
#||
(assert
  (equal
   (format nil "~A" (sb-int:info :function :type 'foo))
   "#<FUN-TYPE (FUNCTION (T T) LIST)>"))
||#

(in-package "SB-C")

(test-util:with-test (:name :bug-458015)
  ;; Make sure layouts have sane source-locations
  (dolist (env *info-environment*)
    (do-info (env :class class :type type :name info-name :value value)
      (when (and (symbolp info-name)
                 (eql class :type)
                 (eql type :kind))
        (let* ((classoid (find-classoid info-name nil))
               (layout (and classoid (classoid-layout classoid)))
               (srcloc (and layout (sb-kernel::layout-source-location layout))))
          (when (and layout)
            (assert (or (definition-source-location-p srcloc)
                        (null srcloc)))))))))

(test-util:with-test (:name :set-info-value-type-check)
  (loop for type-info across *info-types*
        when (and type-info (not (eq (type-info-type type-info) 't)))
        do
        (let ((key1 (class-info-name (type-info-class type-info)))
              (key2 (type-info-name type-info))
              (sillyval (make-string-output-stream))) ; nothing should be this
          ;; check the type-checker function
          (let ((f (compile nil
                            `(lambda (x)
                               (declare (notinline (setf info)))
                               (setf (info ,key1 ,key2 'grrr) x)))))
            (assert (typep (nth-value 1 (ignore-errors (funcall f sillyval)))
                           'type-error)))
          ;; Demonstrate that the SETF disallows the illegal value
          ;; even though this lambda attempts to be non-type-safe.
          (let ((f (compile nil `(lambda (x)
                                   (declare (optimize (safety 0)))
                                   (setf (info ,key1 ,key2 'grrr) x)))))
            (assert (typep (nth-value 1 (ignore-errors (funcall f sillyval)))
                           'type-error)))))
  ;; but if I *really* want, a bad value can be installed
  (set-info-value (gensym)
                  (type-info-number (type-info-or-lose :variable :kind))
                  :this-is-no-good))

(test-util:with-test (:name :unrecognize-recognized-declaration)
  (proclaim '(declaration happiness))
  (let ((saved (copy-list *recognized-declarations*)))
    (assert (member 'happiness *recognized-declarations*))
    (proclaim '(declaration happiness))
    (assert (equal *recognized-declarations* saved)) ; not pushed again
    (setf (info :declaration :recognized 'happiness) nil)
    (assert (not (member 'happiness *recognized-declarations*)))))

(test-util:with-test (:name :recognized-decl-not-also-type)
  (deftype pear (x) `(cons ,x ,x))
  (assert (typep (nth-value 1 (ignore-errors (proclaim '(declaration pear))))
                 'declaration-type-conflict-error))
  (proclaim '(declaration nthing))
  (assert (typep (nth-value 1 (ignore-errors (deftype nthing (x) `(not ,x))))
                 'declaration-type-conflict-error)))

(test-util:with-test (:name :info-env-clear)
  (let ((e (make-info-environment :name "Ben")))
    (setf (info :variable :kind 'beefsupreme) :special)
    (let ((*info-environment* (cons e *info-environment*)))
      ;; ordinarily there will not be two volatile info environments
      ;; in the list of environments, but make sure it works ok.
      (assert (eq (info :variable :kind 'beefsupreme) :special))
      (setf (info :variable :kind 'fruitbaskets) :macro
            (info :variable :macro-expansion 'fruitbaskets) 32))
    (let ((ce (compact-info-environment e))) ; compactify E
      ;; Now stick an empty volatile env in front of the compact env.
      ;; This is realistic in that it mimics an image restarted
      ;; from (save-lisp-and-die) built on top of the base core image.
      (let ((*info-environment* (list* (make-info-environment)
                                       ce (cdr *info-environment*))))
        (assert (eq (info :variable :kind 'fruitbaskets) :macro))
        (assert (eq (info :variable :macro-expansion 'fruitbaskets) 32))
        (setf (info :variable :kind 'fruitbaskets) :constant)
        (clear-info :variable :kind 'fruitbaskets)
        (multiple-value-bind (data foundp)
            (info :variable :kind 'fruitbaskets)
          (assert (and (eq data :unknown) (not foundp))))
        (multiple-value-bind (data foundp)
            (info :variable :macro-expansion 'fruitbaskets)
          (assert (and foundp (eql data 32))))
        (clear-info :variable :macro-expansion 'fruitbaskets)
        (multiple-value-bind (data foundp)
            (info :variable :macro-expansion 'fruitbaskets)
          (assert (and (not foundp) (not data))))
        (assert (every #'null (compact-info-env-entries ce)))))))

;;; success
