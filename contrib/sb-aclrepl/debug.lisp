;;;; Debugger for sb-aclrepl
;;;;
;;;; The documentation, which may or may not apply in its entirety at
;;;; any given time, for this functionality is on the ACL website:
;;;;   <http://www.franz.com/support/documentation/6.2/doc/top-level.htm>.

(cl:in-package :sb-aclrepl)

(defun debug-loop ()
  (let* ((sb-debug::*debug-command-level* (1+ sb-debug::*debug-command-level*))
         (sb-debug::*current-frame* (or sb-debug::*stack-top-hint*
                                    (sb-di:top-frame)))
         (sb-debug::*stack-top-hint* nil)
         (continuable (continuable-break-p)))
    (handler-bind ((sb-di:debug-condition
                    (lambda (condition)
                      (princ condition sb-debug::*debug-io*)
                      (throw 'debug-loop-catcher nil))))
      (fresh-line)
      ;;(sb-debug::print-frame-call sb-debug::*current-frame* :verbosity 2)
      (loop ;; only valid to way to exit invoke-debugger is by a restart
       (catch 'debug-loop-catcher
         (handler-bind ((error (lambda (condition)
                                 (when sb-debug::*flush-debug-errors*
                                   (clear-input *debug-io*)
                                   (princ condition)
                                   ;; FIXME: Doing input on *DEBUG-IO*
                                   ;; and output on T seems broken.
                                   (format t
                                           "~&error flushed (because ~
                                             ~S is set)"
                                          'sb-debug::*flush-debug-errors*)
                                   (throw 'debug-loop-catcher nil)))))

           (if (zerop *break-level*) ; restart added by SBCL
               (repl :continuable continuable)
               (let ((level *break-level*))
                 (with-simple-restart
                     (abort "~@<Reduce debugger level (to break level ~W).~@:>"
                            level)
                   (let ((sb-debug::*debug-restarts* (compute-restarts)))
                     (repl :continuable continuable)))))))
       (throw 'repl-catcher (values :debug :exit))
       ))))


(defun continuable-break-p ()
  (when (eq 'continue
            (restart-name (car (compute-restarts))))
    t))

#+ignore
(when (boundp 'sb-debug::*debug-loop-fun*)
  (setq sb-debug::*debug-loop-fun* #'debug-loop))

(defun print-restarts ()
  ;;  (format *output* "~&Restart actions (select using :continue)~%")
  (format *standard-output* "~&Restart actions (select using :continue)~%")
  (let ((restarts (compute-restarts)))
    (dotimes (i (length restarts))
      (format *standard-output* "~&~2D: ~A~%" i (nth i restarts)))))


#+ignore
(defun debugger (condition)
  "Enter the debugger."
  (let ((old-hook *debugger-hook*))
    (when old-hook
      (let ((*debugger-hook* nil))
        (funcall old-hook condition old-hook))))
  (%debugger condition))

#+ignore
(when (boundp 'sb-debug::*invoke-debugger-fun*)
  (setq sb-debug::*invoke-debugger-fun* #'debugger))

#+ignore
(defun print-condition (condition)
  (format *output* "~&Error: ~A~%" condition))

#+ignore
(defun print-condition-type (condition)
  (format *output* "~&  [Condition type: ~A]~%" (type-of condition)))

#+ignore
(defun %debugger (condition)
  (print-condition condition)
  (print-condition-type condition)
  (princ #\newline *output*)
  (print-restarts)
  (acldebug-loop))


#+ignore
(defun acldebug-loop ()
  (let ((continuable (continuable-break-p)))
    (if continuable
        (aclrepl :continuable t)
        (let ((level *break-level*))
          (with-simple-restart
              (abort "~@<Reduce debugger level (to debug level ~W).~@:>" level)
            (loop
             (repl)))))))

