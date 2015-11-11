;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INTERPRETER")

(defun list-locals (env)
  (do ((env env (env-parent env)))
      ((null env))
    (when (or (var-env-p env) (lambda-env-p env))
      (with-environment-vars (symbols end) env
        (let ((values (the simple-vector (env-payload env))))
          (dotimes (i (min end (length values)))
            (unless (logbitp i (frame-special-b (env-contour env)))
              (format t "~S  =  ~S~%"
                      (car (svref symbols i))
                      (svref values i)))))))))

(defun flush-macros () (atomic-incf *globaldb-cookie*))

;; This is for SAVE-LISP-AND-DIE. It removes all accumulated stuff
;; that can be recreated in the normal course of evaluation.
(defun flush-everything ()
  (sb-vm::map-allocated-objects
   (lambda (obj type size)
     (declare (ignore type size))
     (when (typep obj 'interpreted-function)
       (let ((proto-fn (interpreted-function-proto-fn obj)))
         (setf (proto-fn-%frame proto-fn) nil
               (proto-fn-cookie proto-fn) nil
               (proto-fn-type proto-fn) nil
               (interpreted-function-frame obj) nil
               (interpreted-function-cookie obj) nil))))
   :dynamic))
