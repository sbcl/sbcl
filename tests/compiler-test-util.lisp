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
           #:find-named-callees))

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

(defmacro assert-no-consing (form &optional times)
  `(%assert-no-consing (lambda () ,form) ,times))
(defun %assert-no-consing (thunk &optional times)
  (let ((before (sb-ext:get-bytes-consed))
        (times (or times 10000)))
    (declare (type (integer 1 *) times))
    (dotimes (i times)
      (funcall thunk))
    (assert (< (- (sb-ext:get-bytes-consed) before) times))))

(defmacro assert-consing (form &optional times)
  `(%assert-consing (lambda () ,form) ,times))
(defun %assert-consing (thunk &optional times)
  (let ((before (sb-ext:get-bytes-consed))
        (times (or times 10000)))
    (declare (type (integer 1 *) times))
    (dotimes (i times)
      (funcall thunk))
    (assert (not (< (- (sb-ext:get-bytes-consed) before) times)))))
