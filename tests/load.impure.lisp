;;;; miscellaneous side-effectful tests of LOAD

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

;;; Bug reported by Sean Ross: FASL loader set fill pointer to loaded
;;; simple arrays.
(defvar *array*)
(defvar *tmp-filename* "load-test.tmp")

(progn
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *array* #3a(((1 2) (2 1)) ((3 4) (4 3)))) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*array* nil))
             (load tmp-fasl)
             (assert (arrayp *array*))
             (assert (= (array-rank *array*) 3))
             (assert (not (array-has-fill-pointer-p *array*)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))
