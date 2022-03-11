;;;; Structured exception handling. Uses win64-exceptions.c.

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

#-(and win32 x86-64) (exit :code 104) ;; This is extremely win64-specific.

(with-scratch-file (dll "dll")
  (run-program "gcc" `("-shared" "-o" ,dll "win64-exceptions.c")
               :search t)
  (load-shared-object dll))

(define-alien-routine "unsafe_div" int
  (dividend int)
  (divisor int))

(define-alien-routine "raise_exception" void
  (code int))

(define-alien-routine "raise_int_divide_by_zero" int
  (handle int))

(define-alien-routine "raise_access_violation" int
  (handle int))

(define-alien-routine "allocate_readonly_int" (* int))

(define-alien-routine "free_readonly_int" int
  (pointer (* int)))

(with-test (:name :unsafe-div)
  (assert (eql 3 (unsafe-div 12 4)))
  (multiple-value-bind (result error)
      (ignore-errors (unsafe-div 12 0))
    (assert (null result))
    (assert (string= (princ-to-string error)
                     "EXCEPTION_INT_DIVIDE_BY_ZERO"))))

(with-test (:name :raise-exception)
  (let ((e (nth-value 1 (ignore-errors (raise-exception 42)))))
    (assert (typep e 'sb-win32:exception))
    (assert (eql 42 (sb-win32:exception-code e)))))

(with-test (:name :raise-int-divide-by-zero)
  (assert (eql 1 (raise-int-divide-by-zero 1)))
  (multiple-value-bind (result error)
      (ignore-errors (raise-int-divide-by-zero 0))
    (assert (null result))
    (assert (string= (princ-to-string error)
                     "EXCEPTION_INT_DIVIDE_BY_ZERO"))))

(with-test (:name :raise-access-violation)
  (assert (eql 1 (raise-access-violation 1)))
  (assert-error (raise-access-violation 0) sb-sys:memory-fault-error))

(with-test (:name :access-violation-in-lisp)
  (let ((p (allocate-readonly-int)))
    (assert-error (setf (deref p) 42) sb-sys:memory-fault-error)
    (free-readonly-int p)))

;;; Not a very robust test since neither free() nor HeapFree() document this
;;; exception. The main result we wish for is not having the process abruptly
;;; terminated.
(with-test (:name :heap-corruption)
  (let ((bad-pointer (sb-alien:sap-alien (sb-sys:int-sap 42) (* (unsigned 8)))))
    (assert-error (sb-alien:free-alien bad-pointer)
                  sb-sys:foreign-heap-corruption)))
