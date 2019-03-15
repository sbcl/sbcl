;;;; This file is for testing WITH-COMPILATION-UNIT (particularily the
;;;; suppression of undefined-foo warnings for forward-references).

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

(defvar *file-a* (scratch-file-name "lisp"))
(defvar *file-b* (scratch-file-name "lisp"))

(defun test-files (reset &optional want-suppress-p)
  (funcall reset)
  (assert (eql (has-error?
                (handler-bind ((warning (lambda (c)
                                          (error "got a warning: ~a" c))))
                  (with-compilation-unit ()
                    (compile-file *file-a*)
                    (compile-file *file-b*))))
               want-suppress-p))

  (funcall reset)
  (assert
   (has-error?
    (handler-bind ((warning (lambda (c)
                              (error "got a warning: ~a" c))))
      (compile-file *file-a*)
      (compile-file *file-b*))))

  (funcall reset)
  (assert (eql (has-error?
                (handler-bind ((warning (lambda (c)
                                          (error "got a warning: ~a" c))))
                  (with-compilation-unit ()
                    (compile-file *file-a*)
                    (load (compile-file-pathname *file-b*)))))
               want-suppress-p))

  (funcall reset)
  (assert
   (has-error?
    (handler-bind ((warning (lambda (c)
                              (error "got a warning: ~a" c))))
      (compile-file *file-a*)
      (load (compile-file-pathname *file-b*))))))

(with-test (:name (:with-compilation-unit :function))
  (with-open-file (stream *file-b* :direction :output
:if-exists :supersede)
    (write '(defun foo () 1) :stream stream))
  (with-open-file (stream *file-a* :direction :output
:if-exists :supersede)
    (write '(defun bar () (foo)) :stream stream))

  (test-files (lambda ()
                (fmakunbound 'foo)
                (fmakunbound 'bar))))

(with-test (:name (:with-compilation-unit :generic-function))
  (with-open-file (stream *file-b* :direction :output
                          :if-exists :supersede)
    (write '(defgeneric foo ()) :stream stream)
    (write '(defmethod foo () 1) :stream stream))
  (with-open-file (stream *file-a* :direction :output
                          :if-exists :supersede)
    (write '(defmethod bar () (foo)) :stream stream))

  (test-files (lambda ()
                (fmakunbound 'foo)
                (fmakunbound 'bar))))

(with-test (:name (:with-compilation-unit :variable))
  (with-open-file (stream *file-b* :direction :output
                          :if-exists :supersede)
    (write `(defvar ,(intern "*A*") nil) :stream stream))
  (with-open-file (stream *file-a* :direction :output
                          :if-exists :supersede)
    (write `(defun bar () ,(intern "*A*")) :stream stream))

  (test-files (lambda ()
                (unintern (find-symbol "*A*"))
                (fmakunbound 'bar))
              ;; Check that undefined variables are warned for, even
              ;; if the variable is defined later in the compilation
              ;; unit.
              t))

(with-test (:name (:with-compilation-unit :type))
  (with-open-file (stream *file-b* :direction :output
                          :if-exists :supersede)
    (write `(deftype ,(intern "A-TYPE") () 'fixnum) :stream stream))
  (with-open-file (stream *file-a* :direction :output
                          :if-exists :supersede)
    (write `(defun bar () (typep 1 ',(intern "A-TYPE"))) :stream stream))

  (test-files (lambda ()
                (unintern (find-symbol "A-TYPE"))
                (fmakunbound 'bar))))

(delete-file *file-a*)
(delete-file *file-b*)
(ignore-errors (delete-file (compile-file-pathname *file-a*)))
(ignore-errors (delete-file (compile-file-pathname *file-b*)))
