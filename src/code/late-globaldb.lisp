;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;; Call FUNCTION once for each Name in globaldb that has information associated
;; with it, passing the function the Name as its only argument.
;;
;; This is in its own file to avoid creating an early dependency on
;; target-package iterators.
(macrolet
    ((def (&rest situations)
       `(eval-when ,situations
          (defun call-with-each-globaldb-name (fun-designator)
            (let ((function (cl:coerce fun-designator 'function)))
              (with-package-iterator (iter (list-all-packages) :internal :external)
                (loop (multiple-value-bind (winp symbol access package) (iter)
                        (declare (ignore access))
                        (if (not winp) (return))
                        ;; Try to process each symbol at most once by associating it with
                        ;; a single package. If a symbol is apparently uninterned,
                        ;; always keep it since we can't know if it has been seen once.
                        (when (or (not (sb-xc:symbol-package symbol))
                                  (eq package (sb-xc:symbol-package symbol)))
                          (dolist (name (info-vector-name-list symbol))
                            (funcall function name))))))
              ,@(unless (equal situations '(:compile-toplevel))
                  `((dovector (obj (car *fdefns*))
                      (when (fdefn-p obj)
                        (funcall function (fdefn-name obj))))
                    (info-maphash (lambda (name data)
                                    (declare (ignore data))
                                    (funcall function name))
                                  *info-environment*))))))))
  ;; In host mode we can not use INFO-MAPHASH because the lockfree hashtable
  ;; is target-only code.  We don't have a deep dependence on this for the host,
  ;; but make-host-2 does iterate over globaldb post-build
  ;; to detect possible inlining failures
  (def :compile-toplevel)
  (def :load-toplevel :execute))
