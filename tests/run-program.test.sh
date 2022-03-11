#!/bin/sh

# tests related to SB-EXT:RUN-PROGRAM

# This software is part of the SBCL system. See the README file for
# more information.
#
# While most of SBCL is derived from the CMU CL system, the test
# files (like this one) were written from scratch after the fork
# from CMU CL.
#
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for
# more information.

. ./subr.sh

# Make sure that there's at least something in the environment (for
# one of the tests below).
SOMETHING_IN_THE_ENVIRONMENT='yes there is'
export SOMETHING_IN_THE_ENVIRONMENT
PATH=/some/path/that/does/not/exist:${PATH}
export PATH

# This should probably be broken up into separate pieces.
run_sbcl --eval "(defvar *exit-ok* $EXIT_LISP_WIN)" <<'EOF'
  ;; test that $PATH is searched
  (assert (zerop (sb-ext:process-exit-code
                  (sb-ext:run-program "true" () :search t :wait t))))
  (assert (not (zerop (sb-ext:process-exit-code
                       (sb-ext:run-program "false" () :search t :wait t)))))
  (let ((string (with-output-to-string (stream)
                  (sb-ext:run-program "/bin/echo"
                                      '("foo" "bar")
                                      :output stream))))
    (assert (string= string "foo bar
")))
  ;; Unix environment strings are ordinarily passed with SBCL convention
  ;; (instead of CMU CL alist-of-keywords convention).
  (let ((string (with-output-to-string (stream)
                  (sb-ext:run-program "/usr/bin/env" ()
                                      :output stream
                                      :environment '("FEEFIE=foefum")))))
    (assert (equal string "FEEFIE=foefum
")))

 ;; Unicode strings
 (flet ((try (sb-impl::*default-external-format* x y)
         (let* ((process (run-program
                          "/bin/sh" (list "-c" (format nil "echo ~c, $SB_TEST_FOO." x))
                          :environment (list (format nil "SB_TEST_FOO=~c" y))
                          :output :stream
                          :wait t))
                (output (read-line (process-output process)))
                (wanted (format nil "~c, ~c." x y)))
           (unless (equal output wanted)
             (error "wanted ~S, got ~S" wanted output))
           (process-close process))))
   (try :ascii #\s #\b)
   (try :latin-1 (code-char 197) (code-char 229))
   #+sb-unicode
   (try :utf-8 #\GREEK_CAPITAL_LETTER_OMEGA #\GREEK_SMALL_LETTER_OMEGA))

  ;; The default Unix environment for the subprocess is the same as
  ;; for the parent process. (I.e., we behave like perl and lots of
  ;; other programs, but not like CMU CL.)
  (let* ((sb-impl::*default-external-format* :latin-1)
         (sb-alien::*default-c-string-external-format* :latin-1)   
         (string (with-output-to-string (stream)
                  (sb-ext:run-program "/usr/bin/env" ()
                                      :output stream)))
         (expected (apply #'concatenate
                         'string
                         (mapcar (lambda (environ-string)
                                   (concatenate 'string
                                                environ-string
                                                (string #\newline)))
                                 (sb-ext:posix-environ)))))
    (assert (string= string expected))
    ;; That's not just because POSIX-ENVIRON is having a bad hair
    ;; day and returning NIL, is it?
    (assert (plusp (length (sb-ext:posix-environ)))))
  ;; make sure that a stream input argument is basically reasonable.
  (let ((string (let ((i (make-string-input-stream "abcdef")))
                  (with-output-to-string (stream)
                    (sb-ext:run-program "/bin/cat" ()
                                        :input i :output stream)))))
    (assert (= (length string) 6))
    (assert (string= string "abcdef")))

  ;;; Test the bookkeeping involved in decoding the child's output:

  ;; repeated short, properly-encoded reads exposed one bug.  (But
  ;; note: this test will be inconclusive if the child's stderr is
  ;; fully buffered.)
  (let ((str (with-output-to-string (s)
               (run-program "/bin/sh"
                            '("-c" "(echo Foo; sleep 2; echo Bar)>&2")
                            :output s :search t :error :output :wait t))))
    (assert (string= str (format nil "Foo~%Bar~%"))))

  ;; end of file in the middle of a UTF-8 character
  (typep (nth-value 1 (ignore-errors
                       (let ((sb-impl::*default-external-format* :utf-8))
                         (with-output-to-string (s)
                           (run-program "printf" '("\\316")
                                        :output s :search t :wait t)))))
         'error)

  ;; success convention for this Lisp program run as part of a larger script
  (sb-ext:quit :unix-status *exit-ok*)))
EOF
check_status_maybe_lose "run program tests" $?

exit $EXIT_TEST_WIN
