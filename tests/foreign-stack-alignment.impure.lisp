;;;; Testing the stack alignment of foreign calls. Uses stack-alignment-offset.c.

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

(use-package :sb-alien)

;;; Callbacks are not part of the exported interface yet -- when they are this can
;;; go away.
(import 'sb-alien::alien-lambda)

(defun run (program &rest arguments)
  (let* ((proc nil)
         (output
          (with-output-to-string (s)
            (setf proc (run-program program arguments
                                    :output s)))))
    (unless (zerop (process-exit-code proc))
      (error "Bad exit code: ~S~%Output:~% ~S"
             (process-exit-code proc)
             output))
    output))

(defvar *required-alignment*
  #+arm 8
  #+mips 8
  #+(and ppc darwin) 16
  #+(and ppc (not darwin)) 8
  #+(or arm64 x86-64) 16
  #+(and x86 (not darwin)) 4
  #+(and x86 darwin) 16
  #+sparc 8
  #+alpha 16
  #+hppa 64
  #-(or arm arm64 x86 x86-64 mips ppc sparc alpha hppa)
  (error "Unknown platform"))

;;;; Build the offset-tool as regular excutable, and run it with
;;;; fork/exec, so that no lisp is on the stack. This is our known-good
;;;; number.

#-win32
(progn
  (run "/bin/sh" "run-compiler.sh" "-sbcl-pic"
       "stack-alignment-offset.c" "-o" "stack-alignment-offset")

  (defparameter *good-offset*
    (parse-integer (run "./stack-alignment-offset"
                        (princ-to-string *required-alignment*))))

  ;; Build the tool again, this time as a shared object, and load it

  (run "/bin/sh" "run-compiler.sh" "-sbcl-pic" "-sbcl-shared"
       "stack-alignment-offset.c" "-o" "stack-alignment-offset.so")

  (load-shared-object (truename "stack-alignment-offset.so"))

  (define-alien-routine stack-alignment-offset int (alignment int))
  #+alien-callbacks
  (define-alien-routine trampoline int (callback (function int))))

;;;; Now get the offset by calling from lisp, first with a regular foreign function
;;;; call, then with an intervening callback.

(with-test (:name :regular :fails-on :win32)
  (assert (= *good-offset* (stack-alignment-offset *required-alignment*))))

#+alien-callbacks
(with-test (:name :callback :fails-on :win32)
  (assert (= *good-offset*
             (trampoline (alien-lambda int ()
                           (stack-alignment-offset *required-alignment*))))))

(when (probe-file "stack-alignment-offset.so")
  (delete-file "stack-alignment-offset")
  (delete-file "stack-alignment-offset.so"))

;;;; success!
