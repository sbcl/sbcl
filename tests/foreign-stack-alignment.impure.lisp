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
  (let* ((stringstream (make-string-output-stream))
         (proc (run-program program arguments
                            :output stringstream :error sb-sys:*tty*
                            :search (or #+win32 t)))
         (output (get-output-stream-string stringstream)))
    (unless (zerop (process-exit-code proc))
      (error "Bad exit code: ~S~%Output:~% ~S"
             (process-exit-code proc)
             output))
    output))
(defun cc (&rest arguments)
  (apply #'run #+unix "./run-compiler.sh" #+win32 "gcc" arguments))

(defvar *required-alignment*
  (or #+arm 8
      #+mips 8
      #+(and ppc darwin) 16
      #+(and ppc (not darwin)) 8
      #+(or arm64 x86 x86-64 riscv ppc64) 16
      #+sparc 8
      (error "Unknown platform")))

;;;; Build the offset-tool as regular excutable, and run it with
;;;; fork/exec, so that no lisp is on the stack. This is our known-good
;;;; number.

(defvar *exename* (format nil "stackalign-test~a" (or #+win32 ".exe" "")))
(defvar *soname*  (format nil "stackalign-test~a" (or #+win32 ".dll" ".so")))

(progn
  (cc #+unix "-sbcl-pic" "-o" *exename* "stack-alignment-offset.c")

  (defparameter *good-offset*
    (parse-integer (run (format nil "./~a"  *exename*)
                        (princ-to-string *required-alignment*))))
  (format t "~s is ~d~%" '*good-offset* *good-offset*)
  ;; Build the tool again, this time as a shared object, and load it

  #+unix  (cc "-sbcl-shared" "-sbcl-pic" "-o" *soname* "stack-alignment-offset.c")
  #+win32 (cc "-shared" "-o" *soname* "stack-alignment-offset.c")

  (load-shared-object (truename *soname*))

  (define-alien-routine stack-alignment-offset int (alignment int))
  #+alien-callbacks
  (define-alien-routine trampoline int (callback (function int))))

;;;; Now get the offset by calling from lisp, first with a regular foreign function
;;;; call, then with an intervening callback.

(with-test (:name :regular)
  (assert (= *good-offset* (stack-alignment-offset *required-alignment*))))

#+alien-callbacks
(with-test (:name :callback)
  (assert (= *good-offset*
             (trampoline (alien-lambda int ()
                           (stack-alignment-offset *required-alignment*))))))

(ignore-errors (delete-file *exename*))
(ignore-errors (delete-file *soname*))

;;;; success!
