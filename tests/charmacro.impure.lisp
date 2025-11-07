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
  (assert (eq (set-dispatch-macro-character #\$ #\$) 'double-cash))
  (assert (equal (read-from-string "(foo some$!thing)")
                 '(FOO SOME "{cashbang}" THING)))
  (assert (equal (read-from-string "(foo some\\$thing)")
                 '(FOO |SOME$THING|))))

(with-test (:name :charmacro-printer-bug :fails-on :sbcl)
  ;; - ECL, CLISP, and GNU Common Lisp all print the test case as (FOO |SOME!THING|)
  ;; - Closzure prints as (FOO SOME\!THING)
  ;; either is acceptable.
  (let* ((expr (read-from-string "(foo some\\!thing)"))
         ;; ":escape t" should be strong enough, but just in case we think
         ;; it demands ":readably t" do that, not that it fixes the problem.
         (stringified (write-to-string expr :readably t))
         (readback (read-from-string stringified)))
    (assert (equal readback '(FOO |SOME!THING|))))
  ;; This is the root cause. Of course the test has already failed
  ;; (assert (sb-impl::symbol-quotep "SOME!THING" *readtable*))
  )
