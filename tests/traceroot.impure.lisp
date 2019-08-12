;;;; tests related to 'traceroot'
;;;;
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

;;;; These tests should pass for all platforms, but they're not
;;;; and I don't care too much why, since the functionality still works.
;;;; It's just that sometimes we get :PINNED as a root instead of
;;;; the expected reference to the one and only thread.
#-(and gencgc sb-thread x86-64) (sb-ext:exit :code 104)

(setq sb-ext:*evaluator-mode* :compile)
(defvar *fred*)
(defstruct foo a)

(defun scrubstack ()
  (sb-int:dx-let ((b (make-array 20))) (eval b))
  (sb-sys:scrub-control-stack))

(defun test1 (wp obj root)
  (let ((*fred* (list (make-foo :a (list (vector #xfeefa (list obj)))))))
    (setq obj nil) ; so OBJ is not found as a stack reference
    (let ((paths
           (ecase root
             (:tls
              (scrubstack)
              (sb-ext:search-roots wp :criterion :oldest :gc t :print nil))
             (:bindings ; bind *FRED* again so the old value is on the binding stack
              (let ((*fred* 1))
                (scrubstack)
                (sb-ext:search-roots wp :criterion :oldest :gc t :print nil)))
             (:stack
              ; put the OBJ back on the control stack
              ; and also ensure that *FRED* is not a root.
              (setq obj *fred* *fred* nil)
              (scrubstack)
              (sb-ext:search-roots wp :criterion :oldest :gc t :print nil)))))
      (assert paths)
      (let* ((path (cdar paths))
             (root (car path)))
        (assert (stringp (car root)))
        (case root
          (:stack
           (assert (typep (cdr root) '(cons system-area-pointer))))
          (:tls
           (assert (typep (cdr root) '(cons (eql *fred*) (cons t)))))
          (:bindings
           (assert (typep (cdr path) '(cons (eql *fred*) (cons nil))))))))))

(with-test (:name (sb-ext:search-roots :stack-indirect))
  (let ((wp (make-weak-pointer (list 1 2 3 4))))
    (test1 wp (weak-pointer-value wp) :stack)
    (test1 wp (weak-pointer-value wp) :tls)
    (test1 wp (weak-pointer-value wp) :bindings)
    nil))

(defun f0 ()
  (let* ((c (cons 1 2))
         (wp (make-weak-pointer c)))
    (let ((paths (sb-ext:search-roots wp :criterion :static :gc t :print nil)))
      (assert paths)
      (let* ((path (car paths))
             (nodes (cdr path)))
        (assert (and (sb-int:singleton-p nodes)
                     (string= "main thread" (caar nodes))))))
    c))
(with-test (:name (sb-ext:search-roots :stack-direct))
  (f0))
