;;;; Compile the fundamental system sources (not CLOS, and possibly
;;;; not some other warm-load-only stuff like DESCRIBE) to produce
;;;; object files. Also set *TARGET-OBJECT-FILES* to all of their
;;;; names.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

(defvar *target-object-file-names*)

;;; Evaluate compile-time effects only
(when (make-host-2-parallelism)
  (require :sb-posix))
#+#.(cl:if (cl:find-package "SB-POSIX") '(and) '(or))
(defun parallel-make-host-2 (max-jobs)
  (let ((reversed-target-object-file-names nil)
        (subprocess-count 0)
        (subprocess-list nil))
    (flet ((wait ()
             (multiple-value-bind (pid status) (sb-posix:wait)
               (format t "~&; Subprocess ~D exit status ~D~%"  pid status)
               (setq subprocess-list (delete pid subprocess-list)))
             (decf subprocess-count)))
      (do-stems-and-flags (stem flags)
        (unless (position :not-target flags)
          (when (>= subprocess-count max-jobs)
            (wait))
          (let ((pid (sb-posix:fork)))
            (when (zerop pid)
              (target-compile-stem stem flags)
              ;; FIXME: convey exit code based on COMPILE result.
              (sb-sys:os-exit 0))
            (push pid subprocess-list))
          (incf subprocess-count)
          ;; Cause the compile-time effects from this file
          ;; to appear in subsequently forked children.
          (let ((*compile-for-effect-only* t))
            (target-compile-stem stem flags))
          (unless (find :not-target flags)
            (push (stem-object-path stem flags :target-compile)
                  reversed-target-object-file-names))))
      (loop (if (plusp subprocess-count) (wait) (return)))
      (nreverse reversed-target-object-file-names))))

;;; Actually compile
(setf *target-object-file-names*
      (if (make-host-2-parallelism)
          (parallel-make-host-2 (make-host-2-parallelism))
          (let ((reversed-target-object-file-names nil))
            (do-stems-and-flags (stem flags)
              (unless (position :not-target flags)
                (let ((filename (target-compile-stem stem flags)))
                  (unless (position :not-genesis flags)
                    (push filename reversed-target-object-file-names)))
                #!+sb-show (warn-when-cl-snapshot-diff *cl-snapshot*)))
            (nreverse reversed-target-object-file-names))))
