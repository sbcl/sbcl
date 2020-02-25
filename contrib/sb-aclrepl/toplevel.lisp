(cl:defpackage :sb-aclrepl
  (:use "COMMON-LISP" "SB-EXT")
  (:shadowing-import-from "SB-IMPL" "SCRUB-CONTROL-STACK")
  (:shadowing-import-from "SB-INT" "*REPL-PROMPT-FUN*" "*REPL-READ-FORM-FUN*")
  (:export
   ;; user-level customization of UI
   "*PROMPT*" "*EXIT-ON-EOF*" "*MAX-HISTORY*"
   "*USE-SHORT-PACKAGE-NAME*" "*COMMAND-CHAR*"
   ;; user-level customization of functionality
   "ALIAS"
   ;; internalsish, but the documented way to make a new repl "object"
   ;; such that it inherits the current state of the repl but has its
   ;; own independent state subsequently.
   "MAKE-REPL-FUN"))

(cl:in-package :sb-aclrepl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p *package*) t))

(defvar *noprint* nil
  "boolean: T if don't print prompt and output")
(defvar *break-level* 0
  "current break level")
(defvar *inspect-break* nil
  "boolean: T if break caused by inspect")
(defvar *continuable-break* nil
  "boolean: T if break caused by continuable error")

(defun repl (&key
             (break-level (1+ *break-level*))
             (noprint *noprint*)
             (inspect nil)
             (continuable nil))
  (let ((*noprint* noprint)
        (*break-level* break-level)
        (*inspect-break* inspect)
        (*continuable-break* continuable))
    (loop
     (multiple-value-bind (reason reason-param)
         (catch 'repl-catcher
           (loop
            (unwind-protect
                 (rep-one)
              ;; if we started stepping in the debugger, now is the
              ;; time to stop
              (sb-impl::disable-stepping))))
       (declare (ignore reason-param))
       (cond
         ((and (eq reason :inspect)
               (plusp *break-level*))
          (return-from repl))
         ((and (eq reason :pop)
               (plusp *break-level*))
          (return-from repl)))))))

(defun rep-one ()
  "Read-Eval-Print one form"
  ;; (See comment preceding the definition of SCRUB-CONTROL-STACK.)
  (scrub-control-stack)
  (unless *noprint*
    (funcall *repl-prompt-fun* *standard-output*)
    ;; (Should *REPL-PROMPT-FUN* be responsible for doing its own
    ;; FORCE-OUTPUT? I can't imagine a valid reason for it not to
    ;; be done here, so leaving it up to *REPL-PROMPT-FUN* seems
    ;; odd. But maybe there *is* a valid reason in some
    ;; circumstances? perhaps some deadlock issue when being driven
    ;; by another process or something...)
    (force-output *standard-output*))
  (let* ((form (funcall *repl-read-form-fun*
                        *standard-input*
                        *standard-output*))
         (results (multiple-value-list (sb-impl::interactive-eval form))))
    (unless *noprint*
      (dolist (result results)
        ;; FIXME: Calling fresh-line before a result ensures the result starts
        ;; on a newline, but it usually generates an empty line.
        ;; One solution would be to have the newline's entered on the
        ;; input stream inform the output stream that the column should be
        ;; reset to the beginning of the line.
        (fresh-line *standard-output*)
        (prin1 result *standard-output*)))))
