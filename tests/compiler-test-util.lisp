;;;; Utilities for verifying features of compiled code

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

(defpackage :compiler-test-util
  (:nicknames :ctu)
  (:use :cl :sb-c :sb-kernel)
  (:export #:assert-consing
           #:assert-no-consing
           #:compiler-derived-type
           #:find-value-cell-values
           #:find-code-constants
           #:find-named-callees
           #:file-compile))

(cl:in-package :ctu)

(unless (fboundp 'compiler-derived-type)
  (defknown compiler-derived-type (t) (values t t) (movable flushable unsafe))
  (deftransform compiler-derived-type ((x) * * :node node)
    (sb-c::delay-ir1-transform node :optimize)
    `(values ',(type-specifier (sb-c::lvar-type x)) t))
  (defun compiler-derived-type (x)
    (declare (ignore x))
    (values t nil)))

(defun find-value-cell-values (fun)
  (let ((code (fun-code-header (%fun-fun fun))))
    (loop for i from sb-vm::code-constants-offset below (get-header-data code)
          for c = (code-header-ref code i)
          when (= sb-vm::value-cell-header-widetag (widetag-of c))
          collect (sb-vm::value-cell-ref c))))

(defun find-named-callees (fun &key (type t) (name nil namep))
  (let ((code (sb-kernel:fun-code-header (sb-kernel:%fun-fun fun))))
    (loop for i from sb-vm::code-constants-offset below (sb-kernel:get-header-data code)
          for c = (sb-kernel:code-header-ref code i)
          when (and (typep c 'sb-impl::fdefn)
                    (let ((fun (sb-impl::fdefn-fun c)))
                      (and (typep fun type)
                           (or (not namep)
                               (equal name (sb-impl::fdefn-name c))))))
          collect (sb-impl::fdefn-fun c))))

(defun find-code-constants (fun &key (type t))
  (let ((code (sb-kernel:fun-code-header (sb-kernel:%fun-fun fun))))
    (loop for i from sb-vm::code-constants-offset below (sb-kernel:get-header-data code)
          for c = (sb-kernel:code-header-ref code i)
          when (typep c type)
          collect c)))

(defun collect-consing-stats (thunk times)
  (declare (type function thunk))
  (declare (type fixnum times))
  (let ((before (sb-ext:get-bytes-consed)))
    (dotimes (i times)
      (funcall thunk))
    (values before (sb-ext:get-bytes-consed))))

(defun check-consing (yes/no form thunk times)
  (multiple-value-bind (before after)
      (collect-consing-stats thunk times)
    (let ((consed-bytes (- after before)))
      (assert (funcall (if yes/no #'not #'identity)
                       ;; I do not know why we do this comparasion,
                       ;; the original code did, so I let it
                       ;; in. Perhaps to prevent losage on GC
                       ;; fluctuations, or something. --TCR.
                       (< consed-bytes times))
              ()
              "~@<Expected the form ~
                      ~4I~@:_~A ~0I~@:_~
                  ~:[NOT to cons~;to cons~], yet running it for ~
                  ~D times resulted in the allocation of ~
                  ~D bytes~:[ (~,3F per run)~;~].~@:>"
              form yes/no times consed-bytes
              (zerop consed-bytes) (float (/ consed-bytes times))))
    (values before after)))

(defparameter +times+ 10000)

(defmacro assert-no-consing (form &optional (times '+times+))
  `(check-consing nil ',form (lambda () ,form) ,times))

(defmacro assert-consing (form &optional (times '+times+))
  `(check-consing t ',form (lambda () ,form) ,times))

(defun file-compile (toplevel-forms &key load)
  (let* ((lisp (merge-pathnames "file-compile-tmp.lisp"))
         (fasl (compile-file-pathname lisp)))
    (unwind-protect
         (progn
           (with-open-file (f lisp :direction :output)
             (dolist (form toplevel-forms)
               (prin1 form f)))
           (multiple-value-bind (fasl warn fail) (compile-file lisp)
             (when load
               (load fasl))
             (values warn fail)))
      (ignore-errors (delete-file lisp))
      (ignore-errors (delete-file fasl)))))
