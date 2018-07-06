;;;; tests for renameable closures

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

(defun makec1 (a) (lambda () "doc" (values a)))
(defun makec2 (a b) (lambda () (values a b)))
(compile 'makec1)
(compile 'makec2)

(with-test (:name :closure-renaming)
  (let ((c1 (makec1 :a))) ; C1 has a padding slot
    (assert (eq (sb-int:set-closure-name c1 t 'foo) c1)) ; T = permit copy
    ;; But it's not copied, because it had a slot available.
    (assert (eq (sb-kernel:%fun-name c1) 'foo))
    (assert (zerop (hash-table-count sb-impl::**closure-extra-values**)))

    (assert (eq (sb-int:set-closure-name c1 nil 'foo2) c1)) ; NIL = don't permit copy
    ;; And again was not copied
    (assert (eq (sb-kernel:%fun-name c1) 'foo2))
    (assert (zerop (hash-table-count sb-impl::**closure-extra-values**)))

  (let* ((c2 (makec2 :a :b)) ; C2 doesn't have a padding slot
         (c2* (sb-int:set-closure-name c2 t 'bar)))
    (assert (not (eq c2 c2*)))
    (assert (eq (sb-kernel:%fun-name c2*) 'bar))
    (assert (zerop (hash-table-count sb-impl::**closure-extra-values**)))

    ;; C2* has a padding slot
    (assert (eq (sb-int:set-closure-name c2* t 'baz) c2*))
    (assert (eq (sb-kernel:%fun-name c2*) 'baz))

    ;; Don't permit copy
    (assert (eq (sb-int:set-closure-name c2 nil 'fred) c2))
    (assert (eq (sb-kernel:%fun-name c2) 'fred))
    (assert (plusp (hash-table-count sb-impl::**closure-extra-values**)))
    )))

(with-test (:name :closure-docstrings)
  (dolist (name `("afun" afun (afun) (afun . "guy") (afun . guy) (afun guy)
                  ,sb-pcl:+slot-unbound+ nil))
    (dolist (doc `("what's up" nil ,sb-pcl:+slot-unbound+))
      (let ((closure (makec1 :a)))
        (sb-impl::set-closure-extra-values
         closure nil (sb-impl::pack-closure-extra-values name doc))
        (multiple-value-bind (stored-name stored-doc)
            (sb-impl::closure-extra-values closure)
          (assert (eq stored-name name))
          (assert (eq stored-doc doc)))
        (assert (string= (documentation closure t)
                         (if (eq doc sb-pcl:+slot-unbound+) "doc" doc)))))))
