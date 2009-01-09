;;;; Testing swap_lispobjs.

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

#-(or x86 x86-64)
(sb-ext:quit :unix-status 104)

(defun run (program &rest arguments)
  (let* ((proc nil)
         (output
          (with-output-to-string (s)
            (setf proc (run-program program arguments
                                    :search (not (eql #\. (char program 0)))
                                    :output s)))))
    (unless (zerop (process-exit-code proc))
      (error "Bad exit code: ~S~%Output:~% ~S"
             (process-exit-code proc)
             output))
    output))

(run "cc" "-O3"
     "-I" "../src/runtime/"
     "swap-lispobjs.c"
     #+(and (or linux freebsd) (or x86-64 ppc mips)) "-fPIC"
     #+(and x86-64 darwin) "-arch" #+(and x86-64 darwin) "x86_64"
     #+darwin "-bundle" #-darwin "-shared"
     "-o" "swap-lispobjs.so")

(load-shared-object (truename "swap-lispobjs.so"))

(define-alien-routine try-to-zero-with-swap-lispobjs int
  (lispobj-adress unsigned-long))

(with-test (:name :swap-lispobjs)
  (let ((x (cons 13 27)))
    (try-to-zero-with-swap-lispobjs
     (logandc2 (sb-kernel:get-lisp-obj-address x)
               sb-vm:lowtag-mask))
    (assert (equal x (cons 0 27)))))

(delete-file "swap-lispobjs.so")
