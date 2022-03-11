;;;; miscellaneous tests of SYMBOL-related stuff

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

;;; Reported by Paul F. Dietz
(with-test (:name (:symbol :non-simple-string-name))
  (let ((sym (make-symbol (make-array '(1) :element-type 'character
                                      :adjustable t :initial-contents "X"))))
    (assert (simple-string-p (symbol-name sym)))
    (print sym (make-broadcast-stream))))

(with-test (:name (gentemp :pprinter))
  (let* ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'string
                         (lambda (stream obj)
                           (declare (ignore obj))
                           (write-string "BAR-" stream)))
    (assert (string= "FOO-" (gentemp "FOO-") :end2 4))))

(with-test (:name (gensym :fixnum-restriction))
  (gensym (1+ most-positive-fixnum)))

;; lp#1439921
;; CLHS states that SYMBOL-FUNCTION of a symbol naming a special operator
;; or macro must return something implementation-defined that might not
;; be a function. In this implementation it is a function, but it is illegal
;; to assign that function into another symbol via (SETF FDEFINITION).
(with-test (:name :setf-fdefinition-no-magic-functions)
  (assert-error (setf (fdefinition 'mysym) (fdefinition 'and)))
  (assert-error (setf (fdefinition 'mysym) (fdefinition 'if)))
  (assert-error (setf (symbol-function 'mysym) (symbol-function 'and)))
  (assert-error (setf (symbol-function 'mysym) (symbol-function 'if))))

(with-test (:name :macro-guard-function-name)
  (do-all-symbols (s)
    (when (macro-function s)
      (let* ((f (symbol-function s))
             (name (sb-kernel:%fun-name f)))
        (if (special-operator-p s)
            (assert (typep name '(cons (eql :special)
                                       (cons symbol null))))
            (assert (typep name '(cons (eql :macro)
                                       (cons symbol null)))))))))

(with-test (:name :fdefinition-no-consing
            :skipped-on :interpreter)
  (ctu:assert-no-consing (fdefinition 'list)))

(with-test (:name :tree-shaker :skipped-on :sb-devel)
  ;; Assert that even without the "!" prefix convention
  ;; these used-only-at-cross-compile-time macros disappear.
  (dolist (s '("DEFINE-FOP"
               "DEFINE-TYPE-CLASS"
               "DEFINE-TYPE-METHOD"
               "DEF-TYPE-TRANSLATOR"
               "DEFINE-TYPE-VOP"
               "DEFINE-PRIMITIVE-OBJECT"))
    (assert (not (apropos-list s)))))

(with-test (:name :progv-no-body)
  (checked-compile-and-assert
   ()
   '(lambda (vars vals)
     (progv vars vals))
   ((nil nil) nil)))
