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

(defvar *tmp-filename* "load-test.tmp")

;;; Bug reported by Sean Ross: FASL loader set fill pointer to loaded
;;; simple arrays.

(defvar *array*)

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

;;; rudimentary external-format test
(dolist (ef '(:default :ascii :latin-1 :utf-8))
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(defun foo (x) (1+ x)) s))
  (fmakunbound 'foo)
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename* :external-format ef))
           (load tmp-fasl)
           (assert (= (foo 1) 2)))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

;;; As reported by David Tolpin *LOAD-PATHNAME* was not merged.
(progn
  (defvar *saved-load-pathname*)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *saved-load-pathname* *load-pathname*) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (load *tmp-filename*)
           (assert (equal (merge-pathnames *tmp-filename*) *saved-load-pathname*)))
      (delete-file *tmp-filename*))))

(quit :unix-status 104)
