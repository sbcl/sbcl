;;; This code is currently essentially the same as code posted by Eric
;;; Marsden to cmucl-imp, to detect stale symbols in a core.
;;;
;;; Known deficiencies:
;;;
;;; * flags CATCH tags as stale;
;;; * flags constants (under certain circumstances) as stale;
;;; * output is not necessarily terribly clear;
;;; * takes a long time (several hours on CSR's 300MHz x86 desktop) to
;;;   run.

(defun print-stale-reference (obj stream)
  (cond ((vectorp obj)
         (format stream "vector (probable package internals)"))
        ((sb-c::compiled-debug-function-p obj)
         (format stream "#<compiled-debug-function ~a>"
                 (sb-c::compiled-debug-function-name obj)))
        (t
         (format stream "~w" obj))))

(defun find-stale-objects ()
  (dolist (space '(:static :dynamic :read-only))
    (sb-vm::map-allocated-objects
     (lambda (obj type size)
       (declare (optimize (safety 0))
                (ignore size))
       (when (eql type sb-vm:symbol-header-widetag)
         (ignore-errors
           (let ((read-only-space-refs (sb-vm::list-referencing-objects :read-only obj))
                 (static-space-refs (sb-vm::list-referencing-objects :static obj))
                 (dynamic-space-refs (sb-vm::list-referencing-objects :dynamic obj)))
             (when (>= 1 (+ (length read-only-space-refs)
                            (length static-space-refs)
                            (length dynamic-space-refs)))
               (format t "Symbol ~a::~a~%"
                       (and (symbol-package obj) (package-name (symbol-package obj)))
                       (symbol-name obj))
               (unless (null read-only-space-refs)
                 (princ "   Reference in read-only space: ")
                 (print-stale-reference (car read-only-space-refs) t)
                 (terpri))
               (unless (null static-space-refs)
                 (princ "   Reference in static space: ")
                 (print-stale-reference (car static-space-refs) t)
                 (terpri))
               (unless (null dynamic-space-refs)
                 (princ "   Reference in dynamic space: ")
                 (print-stale-reference (car dynamic-space-refs) t)
                 (terpri)))))))
     space)))
