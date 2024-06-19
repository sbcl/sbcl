;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

(defun pcl-compile (expr safety)
  (labels ((strictly-heap-p (x)
             (or (atom x)
                 (and (heap-allocated-p x) (strictly-heap-p (cdr x)))))
           (maybe-copy-expr ()
             ;; The lambda list goes into a code component as-is.
             ;; To avoid creating a heap->arena pointer we might have to copy it.
             #-system-tlabs expr
             #+system-tlabs
             (multiple-value-bind (name rest)
                 (ecase (car expr)
                   (named-lambda (values (cadr expr) (cddr expr)))
                   (lambda (values nil (cdr expr))))
               (destructuring-bind (args . body) rest
                   (if (strictly-heap-p args)
                       expr
                       (let ((args-copy (copy-list args)))
                         (if name
                             `(named-lambda ,name ,args-copy . ,body)
                             `(lambda ,args-copy . ,body))))))))
    (sb-vm:without-arena "pcl-compile"
     (let* ((base-policy sb-c::*policy*)
            (lexenv
             (sb-c::make-almost-null-lexenv
              (ecase safety
                (:safe base-policy)
                (:unsafe (sb-c::process-optimize-decl
                          '((space 1) (compilation-speed 1)
                            (speed 3) (safety 0) (sb-ext:inhibit-warnings 3) (debug 0))
                          base-policy)))
              ;; I suspect that INHIBIT-WARNINGS precludes them from happening
              (list (cons (sb-kernel:find-classoid 'style-warning) 'muffle-warning)
                    (cons (sb-kernel:find-classoid 'compiler-note) 'muffle-warning))
              nil nil nil
              '((:declare sb-c::tlab :system)))))
       (sb-c:compile-in-lexenv (maybe-copy-expr) lexenv nil nil nil nil nil)))))

;;; GET-FUN is the main user interface to this code. It is like
;;; COMPILE, only more efficient. It achieves this efficiency by
;;; reducing the number of times that the compiler needs to be called.
;;; Calls to GET-FUN in which the lambda forms differ only by
;;; constants can use the same piece of compiled code. (For example,
;;; dispatch dfuns and combined method functions can often be shared,
;;; if they differ only by referring to different methods.)
;;;
;;; If GET-FUN is called with a lambda expression only, it will return
;;; a corresponding function. The optional constant-converter argument
;;; can be a function which will be called to convert each constant appearing
;;; in the lambda to whatever value should appear in the function.
;;;
;;; There are three internal functions which operate on the lambda argument
;;; to GET-FUN:
;;;   COMPUTE-TEST converts the lambda into a key to be used for lookup,
;;;   COMPUTE-CODE is used by GET-NEW-FUN-GENERATOR-INTERNAL to
;;;             generate the actual lambda to be compiled, and
;;;   COMPUTE-CONSTANTS is used to generate the argument list that is
;;;             to be passed to the compiled function.
;;;
(defun get-fun (lambda &optional
                 (test-converter #'default-test-converter)
                 (code-converter #'default-code-converter)
                 (constant-converter #'default-constant-converter))
  (values (the function
            (get-fun-generator lambda test-converter code-converter))
          (compute-constants lambda constant-converter)))

(defun default-constantp (form)
  (and (constantp form)
       (not (typep (constant-form-value form) '(or symbol fixnum cons layout)))))

(defun default-test-converter (form)
  (if (default-constantp form)
      '.constant.
      form))

(defun default-code-converter  (form)
  (if (default-constantp form)
      (let ((gensym (gensym))) (values gensym (list gensym)))
      form))

(defun default-constant-converter (form)
  (if (default-constantp form)
      (list (constant-form-value form))
      nil))

(defstruct (fgen (:constructor make-fgen (gensyms generator generator-lambda system))
                 (:copier nil))
  gensyms
  generator
  generator-lambda
  system)
(declaim (freeze-type fgen))

;;; *FGENS* stores all the function generators we have so far. Each
;;; element is a FGEN structure as implemented below. Don't ever touch this
;;; table by hand, use GET-FUN-GENERATOR and ENSURE-FGEN.
;;; We use explicit locking for properly scoped R/M/W operation without
;;; recursion on the mutex. So the table is not specified as :SYNCHRONIZED.
(define-load-time-global *fgens* (make-hash-table :test #'equal))

(defun ensure-fgen (test gensyms generator generator-lambda system)
  (let ((table *fgens*))
    (with-system-mutex ((hash-table-lock table))
      (let ((old (gethash test table)))
        (cond (old
               (setf (fgen-generator old) generator)
               (unless (fgen-system old)
                 (setf (fgen-system old) system)))
              (t
               (unless (eql (sb-vm:thread-current-arena) 0)
                 (setq gensyms (ensure-heap-list gensyms))
                 (sb-vm:without-arena
                     (setq test (copy-tree test)
                           generator-lambda  (copy-tree generator-lambda))))
               (setf (gethash test table)
                     (make-fgen gensyms generator generator-lambda system))))))))


(defun get-fun-generator (lambda test-converter code-converter)
  (let* ((test (compute-test lambda test-converter))
         (table *fgens*)
         (fgen (with-system-mutex ((hash-table-lock table))
                 (gethash test table))))
    (if fgen
        (fgen-generator fgen)
        (get-new-fun-generator lambda test code-converter))))

(defun get-new-fun-generator (lambda test code-converter)
  (multiple-value-bind (code gensyms) (compute-code lambda code-converter)
    (let ((generator-lambda `(lambda ,gensyms
                               (declare (optimize (sb-c:store-source-form 0)
                                                  (sb-c::store-xref-data 0)))
                               (function ,code))))
      (let ((generator (pcl-compile generator-lambda :safe)))
        (ensure-fgen test gensyms generator generator-lambda nil)
        generator))))

(defun compute-test (lambda test-converter)
  (let ((*walk-form-expand-macros-p* t))
    (walk-form lambda
               nil
               (lambda (f c e)
                 (declare (ignore e))
                 (if (neq c :eval)
                     f
                     (let ((converted (funcall test-converter f)))
                       (values converted (neq converted f))))))))

(defun compute-code (lambda code-converter)
  (let ((*walk-form-expand-macros-p* t)
        (gensyms ()))
    (values (walk-form lambda
                       nil
                       (lambda (f c e)
                         (declare (ignore e))
                         (if (neq c :eval)
                             f
                             (multiple-value-bind (converted gens)
                                 (funcall code-converter f)
                               (when gens
                                 (setq gensyms (append gensyms gens)))
                               (values converted (neq converted f))))))
            gensyms)))

(defun compute-constants (lambda constant-converter)
  (let ((*walk-form-expand-macros-p* t)) ; doesn't matter here.
   (collect ((res))
    (walk-form lambda
               nil
               (lambda (f c e)
                 (declare (ignore e))
                 (if (neq c :eval)
                     f
                     (let ((consts (funcall constant-converter f)))
                       (if consts
                           (dolist (x consts (values f t)) (res x))
                           f)))))
    (res))))

(defmacro precompile-function-generators (&optional system)
  (let (collect)
    ;; In single threaded code, only at system build time, and not used after.
    (maphash (lambda (test fgen)
                 (when (or (null (fgen-system fgen))
                           (eq (fgen-system fgen) system))
                   (when system
                     (setf (fgen-system fgen) system))
                   (push `(ensure-fgen
                           ',test
                           ',(fgen-gensyms fgen)
                           (function ,(fgen-generator-lambda fgen))
                           ',(fgen-generator-lambda fgen)
                           ',system)
                         collect)))
               *fgens*)
    `(progn ,@collect)))
