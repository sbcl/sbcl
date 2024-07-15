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
  (:import-from #:sb-c #:*compile-component-hook*)
  (:export #:asm-search
           #:assert-consing
           #:assert-no-consing
           #:compiler-derived-type
           #:count-full-calls
           #:find-code-constants
           #:find-named-callees
           #:find-anonymous-callees
           #:file-compile
           #:inspect-ir
           #:ir1-named-calls
           #:ir1-funargs
           #:disassembly-lines))

(cl:in-package :ctu)

(unless (fboundp 'compiler-derived-type)
  (defknown compiler-derived-type (t) (values t t) (flushable))
  (deftransform compiler-derived-type ((x) * * :node node)
    (sb-c::delay-ir1-transform node :ir1-phases)
    `(values ',(type-specifier (sb-c::lvar-type x)) t))
  (defun compiler-derived-type (x)
    (declare (ignore x))
    (values t nil)))

;;; New tests should use INSPECT-IR or ASM-SEARCH rather than FIND-NAMED-CALLEES
;;; unless you are 100% certain that there will be an fdefn of the given name.
;;; (negative assertions may yield falsely passing tests)
(defun asm-search (expect lambda)
  (let* ((code (etypecase lambda
                 (cons (test-util:checked-compile lambda))
                 (function lambda)))
         (disassembly
          (with-output-to-string (s)
            (let ((sb-disassem:*disassem-location-column-width* 0)
                  (*print-pretty* nil))
              (sb-c:dis code s)))))
    (loop for line in (test-util:split-string disassembly #\newline)
          when (and (search expect line)
                    (not (search "; Origin" line)))
          collect line)))

(defun inspect-ir (form fun &rest checked-compile-args)
  (let ((*compile-component-hook* fun))
    (apply #'test-util:checked-compile form checked-compile-args)))

(defun ir1-named-calls (lambda-expression &optional (full t))
  (declare (ignorable lambda-expression full))
  #-sb-devel
  (throw 'test-util::skip-test t)
  #+sb-devel
  (let* ((calls)
         (compiled-fun
           (inspect-ir
            lambda-expression
            (lambda (component)
              (sb-c::do-blocks (block component)
                (sb-c::do-nodes (node nil block)
                  (when (and (sb-c::basic-combination-p node)
                             (if full
                                 (eq (sb-c::basic-combination-info node) :full)
                                 t))
                    (push (sb-c::combination-fun-debug-name node) calls))))))))
    (values calls compiled-fun)))

;;; For any call that passes a global constant funarg - as in (FOO #'EQ) -
;;; return the name of the caller and the names of all such funargs.
(defun ir1-funargs (lambda-expression)
  (declare (ignorable lambda-expression))
  #-sb-devel
  (throw 'test-util::skip-test t)
  #+sb-devel
  (let* ((calls)
         (compiled-fun
           (inspect-ir
            lambda-expression
            (lambda (component)
              (sb-c::do-blocks (block component)
                (sb-c::do-nodes (node nil block)
                  (when (and (sb-c::basic-combination-p node)
                             (eq (sb-c::basic-combination-info node) :full))
                    (let ((filtered
                            (mapcan
                             (lambda (arg &aux (uses (sb-c::lvar-uses arg)))
                               (when (sb-c::ref-p uses)
                                 (let ((leaf (sb-c::ref-leaf uses)))
                                   (when (and (sb-c::global-var-p leaf)
                                              (eq (sb-c::global-var-kind leaf) :global-function))
                                     (list (sb-c::leaf-source-name leaf))))))
                             (sb-c::combination-args node))))
                      (when filtered
                        (push (cons (sb-c::combination-fun-debug-name node) filtered)
                              calls))))))))))
    (values calls compiled-fun)))

(when (member :linkage-space sb-impl:+internal-features+) ; for below
  (pushnew :linkage-space *features*))

(defun find-named-callees (fun &key (name nil namep))
  (let ((code (fun-code-header (%fun-fun fun))))
    #+linkage-space
    (loop for index in (sb-c:unpack-code-fixup-locs (sb-vm::%code-fixups code))
          for this = (sb-vm::linkage-addr->name index :index)
          when (or (not namep) (equal this name))
          collect this)
    #-linkage-space
    (sb-int:binding* (((start count) (sb-kernel:code-header-fdefn-range code)))
      (loop for i from start repeat count
            for c = (code-header-ref code i)
            when (or (not namep) (equal name (sb-kernel:fdefn-name c)))
            collect (sb-kernel:fdefn-name c)))))

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
    (loop for i from sb-vm:code-constants-offset
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
    (let* ((consed-bytes (- after before))
           (bytes-per-iteration (float (/ consed-bytes times))))
      (assert (progn
                (funcall (if yes/no #'not #'identity)
                         ;; If allocation really happened, it can't have been less than one cons cell
                         ;; per iteration (unless the test is nondeterministic - but in that case
                         ;; we can't really use this strategy anyway). So consider it to have consed
                         ;; nothing if the fraction is too small.
                         (< bytes-per-iteration (* 2 sb-vm:n-word-bytes)))
                #+gc-stress t)
              ()
              "~@<Expected the form ~
                      ~4I~@:_~A ~0I~@:_~
                  ~:[NOT to cons~;to cons~], yet running it for ~
                  ~D times resulted in the allocation of ~
                  ~D bytes~:[ (~,3F per run)~;~].~@:>"
              form yes/no times consed-bytes
              (zerop consed-bytes) bytes-per-iteration))
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
           ;; Preserve all referenced callees. This has no effect on semantics
           (sb-int:encapsulate 'sb-int:permanent-fname-p 'test-shim #'sb-int:constantly-nil)
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
      (sb-int:unencapsulate 'sb-int:permanent-fname-p 'test-shim)
      (ignore-errors (delete-file lisp))
      (ignore-errors (delete-file fasl)))))

;;; TODO: this would be better done as LIST-FULL-CALLS so that you could
;;; make an assertion that the list EQUALs something in particular.
;;; Negative assertions (essentially "count = 0") are silently susceptible
;;; to spelling mistakes or a change in how we name nodes.
(defun count-full-calls (function-name lambda-expression)
  (declare (ignorable function-name lambda-expression))
  #-sb-devel
  (throw 'test-util::skip-test t)
  #+sb-devel
  (let ((n 0))
    (inspect-ir
     lambda-expression
     (lambda (component)
       (sb-c::do-blocks (block component)
         (sb-c::do-nodes (node nil block)
           (when (and (sb-c::basic-combination-p node)
                      (eq (sb-c::basic-combination-info node) :full)
                      (equal (sb-c::combination-fun-debug-name node)
                             function-name))
             (incf n))))))
    n))

(defun disassembly-lines (fun)
  ;; FIXME: I don't remember what this override of the hook is for.
  (sb-int:encapsulate 'sb-disassem::add-debugging-hooks 'test
                      (lambda (f &rest args) (declare (ignore f args))))
  (prog1
      (mapcar (lambda (x) (string-left-trim " ;" x))
              (cddr
               (test-util:split-string
                (with-output-to-string (s)
                  (let ((sb-disassem:*disassem-location-column-width* 0)
                        (*print-pretty* nil))
                    (disassemble fun :stream s)))
                #\newline)))
    (sb-int:unencapsulate 'sb-disassem::add-debugging-hooks 'test)))
