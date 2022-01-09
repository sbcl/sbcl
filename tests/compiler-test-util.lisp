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
           #:count-full-calls
           #:find-code-constants
           #:find-named-callees
           #:find-anonymous-callees
           #:file-compile))

(cl:in-package :ctu)

(unless (fboundp 'compiler-derived-type)
  (defknown compiler-derived-type (t) (values t t) (flushable))
  (deftransform compiler-derived-type ((x) * * :node node)
    (sb-c::delay-ir1-transform node :optimize)
    `(values ',(type-specifier (sb-c::lvar-type x)) t))
  (defun compiler-derived-type (x)
    (declare (ignore x))
    (values t nil)))

(defun find-named-callees (fun &key (type t) (name nil namep))
  (let ((code (fun-code-header (%fun-fun fun))))
    (loop for i from sb-vm:code-constants-offset below (code-header-words code)
          for c = (code-header-ref code i)
          when (and (typep c 'sb-impl::fdefn)
                    (let ((fun (sb-impl::fdefn-fun c)))
                      (and (typep fun type)
                           (or (not namep)
                               (equal name (sb-impl::fdefn-name c))))))
          collect (sb-impl::fdefn-fun c))))

(defun find-anonymous-callees (fun &key (type 'function))
  (let ((code (fun-code-header (%fun-fun fun))))
    (loop for i from sb-vm:code-constants-offset below (code-header-words code)
          for fun = (code-header-ref code i)
          when (typep fun type)
          collect fun)))

;;; Return a subset of the code constants for FUN's code but excluding
;;; constants that are present on behalf of %SIMPLE-FUN-foo accessors.
(defun find-code-constants (fun &key (type t))
  (let ((code (fun-code-header (%fun-fun fun))))
    (loop for i from (+ sb-vm:code-constants-offset
                        (* (code-n-entries code) sb-vm:code-slots-per-simple-fun))
          below (code-header-words code)
          for c = (code-header-ref code i)
          for value = (if (= (widetag-of c) sb-vm:value-cell-widetag)
                          (value-cell-ref c)
                          c)
          when (and (not (eql value 0)) ;; alignment zeros
                    (typep value type))
          collect value)))

(defun collect-consing-stats (thunk times)
  (declare (type function thunk))
  (declare (type fixnum times))
  #+(and sb-thread gencgc) (sb-vm::close-thread-alloc-region)
  (setf sb-int:*n-bytes-freed-or-purified* 0)
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

(defun file-compile (toplevel-forms &key load
                                         before-load
                                         block-compile)
  (let* ((lisp (test-util:scratch-file-name "lisp"))
         (fasl (compile-file-pathname lisp))
         (error-stream (make-string-output-stream)))
    (unwind-protect
         (progn
           (with-open-file (f lisp :direction :output)
             (if (stringp toplevel-forms)
                 (write-line toplevel-forms f)
                 (dolist (form toplevel-forms)
                   (prin1 form f))))
           (multiple-value-bind (fasl warn fail)
               (let ((*error-output* error-stream))
                 (compile-file lisp :print nil :verbose nil
                                    :block-compile block-compile))
             (when load
               (when before-load
                 (funcall before-load))
               (let ((*error-output* error-stream))
                 (load fasl :print nil :verbose nil)))
             (values warn fail error-stream)))
      (ignore-errors (delete-file lisp))
      (ignore-errors (delete-file fasl)))))

;; Pretty horrible, but does the job
(defun count-full-calls (name function)
  (let ((code (with-output-to-string (s)
                (let ((*print-right-margin* 120))
                  (sb-disassem:disassemble-code-component function :stream s))))
        (n 0))
    (flet ((asm-line-calls-name-p (line name)
             (dolist (herald '("#<FDEFN" "#<SB-KERNEL:FDEFN" "#<FUNCTION"))
               (let ((pos (search herald line)))
                 (when pos
                   (return (string= (subseq line
                                            (+ pos (length herald) 1)
                                            (1- (length line)))
                                    name)))))))
      (with-input-from-string (s code)
        (loop for line = (read-line s nil nil)
              while line
              when (asm-line-calls-name-p line name) do (incf n)))
      n)))
