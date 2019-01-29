;;;; Source location tracking macros.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

#| No calls to #'SOURCE-LOCATION must happen from this file because:
;; - it would imply lack of location information for the definition,
;;   since deferring the call past compile-time means we've already lost.
;; - it would be a style-warning to subsequently define the compiler-macro.
;; (DEFINE-COMPILER-MACRO SOURCE-LOCATION) does not itself use SOURCE-LOCATION
;; but this is possibly a mistake! Ordinary DEFMACRO supplies the location
;; to %DEFMACRO. Compiler macros should too. This extra form will be needed:
(eval-when (#+sb-xc :compile-toplevel)
  (setf (info :function :compiler-macro-function 'source-location)
        (lambda (form env)
          (declare (ignore form env))
          (make-definition-source-location)))) |#

#+sb-source-locations
(progn
  #-sb-xc-host
  (define-compiler-macro source-location ()
    (make-definition-source-location))
  ;; We need a regular definition of SOURCE-LOCATION for calls processed
  ;; during LOAD on a source file while *EVALUATOR-MODE* is :INTERPRET.
  (defun source-location ()
    #-sb-xc-host (make-definition-source-location)))

#-sb-source-locations
(defun source-location () nil)

#-sb-xc-host
(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (entry '#.sb-vm::!per-thread-c-interface-symbols)
    (let ((symbol (if (consp entry) (car entry) entry)))
      (declare (notinline info (setf info)))
      ;; CURRENT-{CATCH/UWP}-BLOCK are thread slots,
      ;; so the TLS indices were already assigned.
      ;; There may be other symbols too.
      (unless (info :variable :wired-tls symbol)
        (setf (info :variable :wired-tls symbol) :always-thread-local))
      (unless (info :variable :always-bound symbol)
        (setf (info :variable :always-bound symbol) :always-bound)))))
