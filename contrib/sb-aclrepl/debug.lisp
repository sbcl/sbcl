;;;; Debugger for sb-aclrepl
;;;;
;;;; The documentation, which may or may not apply in its entirety at
;;;; any given time, for this functionality is on the ACL website:
;;;;   <http://www.franz.com/support/documentation/6.2/doc/top-level.htm>.

(cl:in-package :sb-aclrepl)

(defun debugger (condition)
  "Enter the debugger."
  (print "Entering debugger")
  (let ((old-hook *debugger-hook*))
    (when old-hook
      (let ((*debugger-hook* nil))
	(funcall old-hook condition old-hook))))

  (format t "~&Error: ~A~%" condition)
  (format t "~&  [Condition type: ~A]~%" (type-of condition))
  (format t "~%")
  (format t "~&Restart actions (select using :continue)~%")
  (let ((restarts (compute-restarts)))
    (dotimes (i (length restarts))
      (format t "~&~2D: ~A~%" i (nth i restarts)))
    (new-break :restarts (cons condition restarts)))
  (sb-impl::toplevel-repl nil))

;(setq sb-debug::*invoke-debugger-fun* #'debugger)
