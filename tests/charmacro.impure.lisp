;;;; miscellaneous side-effectful tests involving read macros

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

;;;; Note that the MOP is not in an entirely supported state.
;;;; However, this seems a good a way as any of ensuring that we have
;;;; no regressions.

(with-test (:name :charmacro-reassign-constituentp)
  ;; Set up a non-terminating macro.
  ;; I really wish they had named the NON-TERMINATING-P argument as CONSTITUENTP.
  ;; Negated booleans args have no reason for being.
  (set-macro-character #\! (lambda (stream char) stream char "{bang}") t)
  ;; #\! is a macro if starts a READ
  (assert (equal (read-from-string "(foo !thing)") '(FOO "{bang}" THING)))
  ;; but is a constituent if it is within a token
  (assert (equal (read-from-string "(foo some!thing)") '(FOO SOME\!THING)))
  ;; Change #\! to terminate tokens
  (set-macro-character #\! (lambda (stream char) stream char "{bang}") nil)
  (assert (equal (read-from-string "(foo some!thing)")
                 '(FOO SOME "{bang}" THING)))
  (assert (equal (read-from-string "(foo some\\!thing)")
                 '(FOO |SOME!THING|))))

(defun cashbang-reader (stream char arg)
  (declare (ignore stream char arg))
  "{cashbang}")

(with-test (:name :dispcharmacro-reassign-constituentp)
  (make-dispatch-macro-character #\$ t) ; NON-TERMINATING-P: Same degree of hate as above
  (set-dispatch-macro-character #\$ #\! #'cashbang-reader)
  (set-dispatch-macro-character #\$ #\$ 'double-cash)
  ;; #\$ is a macro if starts a READ
  (assert (equal (read-from-string "(foo $!thing)") '(FOO "{cashbang}" THING)))
  ;; but is a constituent if it is within a token
  (assert (equal (read-from-string "(foo some$thing)") '(FOO SOME\$THING)))
  ;; Change #\$ to terminate tokens
  (make-dispatch-macro-character #\$) ; let NON-TERMINATING-P take its default of T
  ;; sub-characters should still exist
  (assert (eq (get-dispatch-macro-character #\$ #\$) 'double-cash))
  (assert (equal (read-from-string "(foo some$!thing)")
                 '(FOO SOME "{cashbang}" THING)))
  (assert (equal (read-from-string "(foo some\\$thing)")
                 '(FOO |SOME$THING|))))

(with-test (:name (:charmacro-output-symbol :escape t))
  (let* ((expr (read-from-string "(foo some\\!thing)"))
         (stringified (write-to-string expr :escape t))
         (readback (read-from-string stringified)))
    (print stringified)
    (assert (equal readback expr))))

(with-test (:name (:charmacro-output-symbol :readably t))
  (let* ((expr (read-from-string "(foo some\\!thing)"))
         (stringified (write-to-string expr :readably t))
         (readback (let ((*readtable* (copy-readtable nil)))
                     (read-from-string stringified))))
    (assert (equal readback expr))))
