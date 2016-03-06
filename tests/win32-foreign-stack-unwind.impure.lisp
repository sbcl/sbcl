;;;; Testing the behavior of foreign calls trying to unwind the stack.  Uses win32-stack-unwind.c.

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

#-win32 (exit :code 104) ;; This is extremely win32-specific.
#-x86   (exit :code 104) ;; And our AMD64 backend does not aim to support it.

(use-package :sb-alien)

;;; Callbacks are not part of the exported interface yet -- when they are this can
;;; go away.
(import 'sb-alien::alien-lambda)

;;; XXX XXX this should change to use run-compiler.sh, now that we have it
(defun run-compiler ()
  (let ((proc (run-program "gcc" '("win32-stack-unwind.c" "-shared"
                                   "-o" "win32-stack-unwind.dll")
                           :search t)))
    (unless (zerop (process-exit-code proc))
      (error "Bad exit code: ~S"
             (process-exit-code proc)))))

(run-compiler)

(load-shared-object (truename "win32-stack-unwind.dll"))


(defvar *current-test-callback*)

(defparameter *test-callback-thunk*
  (sb-alien::alien-callback
   (function void)
   #'(lambda () (funcall *current-test-callback*))))

(defun establish-return-frame (callback)
  "Establish an SEH frame for use as a target with PERFORM-TEST-UNWIND and invoke CALLBACK via FUNCALL"
  ;; We don't use a LET here because we don't want to accidentally
  ;; correct a blown binding stack pointer just yet.
  (setf *current-test-callback* callback)
  (alien-funcall (extern-alien "establish_return_frame"
                               (function void (* (function void))))
                 (alien-sap *test-callback-thunk*))
  (makunbound '*current-test-callback*)
  (values))

(defun perform-test-unwind ()
  "Perform an RtlUnwind to the surrounding ESTABLISH-RETURN-FRAME frame."
  (alien-funcall (extern-alien "perform_test_unwind" (function void))))


;;; An attempt to detect and clean up latent fatalities in the
;;; post-test environent.

(defmacro with-test-environment (args &body body)
  (declare (ignore args))
  (let ((old-bsp (gensym))
        (old-cuwp (gensym))
        (old-ccb (gensym))
        (old-asp (gensym)))
    `(let ((*standard-input* *standard-input*))
      (let ((,old-bsp (+ sb-vm::*binding-stack-pointer* 2))
            (,old-cuwp sb-vm::*current-unwind-protect-block*)
            (,old-ccb sb-vm:*current-catch-block*)
            (,old-asp sb-vm::*alien-stack-pointer*))
        (handler-case
            (let ((result (progn ,@body))
                  extra-results)
              (when (not (eql ,old-bsp sb-vm::*binding-stack-pointer*))
                #+(or)
                (format t "~A ~A~%" ,old-bsp sb-vm::*binding-stack-pointer*)
                (push :bsp-fail extra-results))
              (when (not (eql ,old-cuwp sb-vm::*current-unwind-protect-block*))
                (push :cuwp-fail extra-results))
              (when (not (eql ,old-ccb sb-vm:*current-catch-block*))
                (push :ccb-fail extra-results))
              (when (not (eql ,old-asp sb-vm::*alien-stack-pointer*))
                (push :asp-fail extra-results))
              (setf sb-vm::*current-unwind-protect-block* ,old-cuwp)
              (setf sb-vm:*current-catch-block* ,old-ccb)
              (setf sb-vm::*alien-stack-pointer* ,old-asp)
              (list* result extra-results))
          (error ()
            :error))))))


;;; Test cases.

(with-test (:name #1=:base-case)
  ;; Tests that the unwind test machinery works.
  (let ((result
         (with-test-environment ()
           (establish-return-frame (lambda () (perform-test-unwind)))
           :success)))
    (format t "~S result: ~S~%" #1# result)
    (assert (eql :success (car result)))))

(with-test (:name #1=:special-binding)
  ;; Tests that special bindings are undone properly during
  ;; unwind.
  (let ((result
         (with-test-environment ()
           (let ((foo :success))
             (declare (special foo))
             (establish-return-frame (lambda ()
                                       (let ((foo nil))
                                         (declare (special foo))
                                         (perform-test-unwind))))
             foo))))
    (format t "~S result: ~S~%" #1# result)
    (assert (eql :success (car result)))))

(with-test (:name #1=:unwind-protect)
  ;; Tests that unwind-protect forms are run during unwind.
  (let ((result
         (with-test-environment ()
           (let (result)
             (establish-return-frame (lambda ()
                                       (unwind-protect
                                            (perform-test-unwind)
                                         (setf result :success))))
             result))))
    (format t "~S result: ~S~%" #1# result)
    (assert (eql :success (car result)))))

(with-test (:name #1=:unwind-protect-nlx)
  ;; Tests that unwind-protect forms that are run during unwind
  ;; can do a non-local exit to abort the unwind.
  (let ((result
         (with-test-environment ()
           (let (result)
             (establish-return-frame (lambda ()
                                       (block nil
                                         (unwind-protect
                                              (perform-test-unwind)
                                           (return)))
                                       (setf result :success)))
             result))))
    (format t "~S result: ~S~%" #1# result)
    (assert (eql :success (car result)))))

(with-test (:name #1=:no-unwind)
  ;; Basic smoke test of establish-return-frame.
  (let ((result
         (with-test-environment ()
           (establish-return-frame (lambda ()))
           :success)))
    (format t "~S result: ~S~%" #1# result)
    (assert (eql :success (car result)))))

(with-test (:name #1=:no-unwind-error)
  ;; Tests that EXCEPTION_BREAKPOINT is caught and handled
  ;; correctly within callbacks.
  (let ((result
         (with-test-environment ()
           (establish-return-frame (lambda ()
                                     (handler-case
                                         (some-undefined-function)
                                       (undefined-function ()))))
           :success)))
    (format t "~S result: ~S~%" #1# result)
    (assert (eql :success (car result)))))

(with-test (:name #1=:unwind-foreign-frame)
  ;; Tests that unwinding a foreign SEH frame isn't completely
  ;; broken.
  (let ((result
         (with-test-environment ()
           (block nil
             (establish-return-frame (lambda () (return :success)))))))
    (format t "~S result: ~S~%" #1# result)
    (assert (eql :success (car result)))))

(with-test (:name #1=:unwind-protect-unwind-foreign-frame)
  ;; Tests that an unwind-protect block is allowed to unwind
  ;; past the original unwind target.
  (let ((result
         (with-test-environment ()
           (block nil
             (establish-return-frame (lambda ()
                                       (unwind-protect
                                            (perform-test-unwind)
                                         (return :success))))))))
    (format t "~S result: ~S~%" #1# result)
    (assert (eql :success (car result)))))

(with-test (:name #1=:unwind-error)
  ;; Another test for unwinding an SEH frame.
  (let ((result
         (with-test-environment ()
           (handler-case
               (establish-return-frame (lambda ()
                                         (error "Foo!")))
             (error ()
               :success)))))
    (format t "~S result: ~S~%" #1# result)
    (assert (eql :success (car result)))))

;;;; success!
