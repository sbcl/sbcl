;;;; error stuff that needs to wait until warm load

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(eval-when (:compile-toplevel)
  ;; Any globaldb value that references either the type named CLASS or CONDITION-CLASS
  ;; would have been stored as its specifier instead of the parse of the specifier.
  ;; Parse them now because "reasons" which will store unknown types, but is preferable
  ;; to the alternative of warning each time we compile a function (such as WARN)
  ;; that needs to know what the unknown type is, and tries to re-parse.
  ;; Also, there are some unknowns in there already. How on earth did that happen???
  (do-all-symbols (s)
    (let ((info (sb-int:info :function :type s)))
      (when (consp info)
        (let ((parsed (specifier-type info)))
          (setf (sb-int:info :function :type s) parsed)))))
  ;; One good kludge deserves another.
  ;; This is OK only because it's the very first file compiled in warm build.
  (let ((disallowed-undefineds
         (remove-if (lambda (x) (member x '(class sb-pcl::condition-class)))
                    (mapcar #'sb-c::undefined-warning-name
                            sb-c::*undefined-warnings*))))
    (assert (not disallowed-undefineds)))
  (setf sb-c::*undefined-warnings* nil))

;;; Moved from 'cold-error' to this file because of (at least) these reasons:
;;;  - the LOAD-TIME-VALUE forms need to run after 'condition.lisp'
;;;    has created the WARNING and STYLE-WARNING classoids
;;;  - even if this were loaded no earlier than the classoid definitions,
;;;    the entirety of condition handling doesn't work soon enough in cold-init.
(flet ((%warn (datum super default-type &rest arguments)
         (infinite-error-protect
          (let ((condition (apply #'coerce-to-condition datum default-type 'warn
                                  arguments))
                (superclassoid-name (classoid-name super)))
            ;; CONDITION is necessarily an INSTANCE,
            ;; but pedantry requires it be the right subtype of instance.
            (unless (classoid-typep (%instance-wrapper condition)
                                    super condition)
              (error 'simple-type-error
                     :datum datum :expected-type superclassoid-name
                     :format-control "~S does not designate a ~A class"
                     :format-arguments (list datum superclassoid-name)))
            (restart-case (%signal condition)
              (muffle-warning ()
                :report "Skip warning."
                (return-from %warn nil)))
            (format *error-output* "~&~@<~S: ~3i~:_~A~:>~%"
                    superclassoid-name condition)))
         nil))

  ;; We don't warn about redefinition of WARN, apparently (not sure why)
  (defun warn (datum &rest arguments)
  "Warn about a situation by signalling a condition formed by DATUM and
   ARGUMENTS. While the condition is being signaled, a MUFFLE-WARNING restart
   exists that causes WARN to immediately return NIL."
    (declare (explicit-check))
    ;; FIXME: figure out how to get genesis to understand that certain
    ;; classoids (particularly those defined by the language standard)
    ;; never need to go through the classoid-cell indirection,
    ;; as the classoid object itself is essentially permanent.
    (apply #'%warn datum (load-time-value (find-classoid 'warning) t)
           'simple-warning arguments))

  ;; But we would warn about redefinition of STYLE-WARN.
  ;; Do a "gentle" fmakunbound, because real FMAKUNBOUND removes
  ;; sb-c:fun-info thus blowing out the control string transformer.
  (fdefn-makunbound (find-fdefn 'style-warn))

  (defun style-warn (datum &rest arguments)
    (declare (explicit-check))
    (apply #'%warn datum (load-time-value (find-classoid 'style-warning) t)
           'simple-style-warning arguments)))
