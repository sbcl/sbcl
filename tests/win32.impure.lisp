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

#-win32 (exit :code 104)

(with-test (:name :dbg-print-exception-c)
  (handler-case
      (alien-funcall (extern-alien "OutputDebugStringA" (function (values) c-string)) "debug-test")
    (warning (c)
      (assert (equal (princ-to-string c)
                     "DBG_PRINTEXCEPTION_C: debug-test")))))
