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

# Make sure that there's at least something in the environment (for
# one of the tests below).
SOMETHING_IN_THE_ENVIRONMENT='yes there is'
export SOMETHING_IN_THE_ENVIRONMENT
export PATH=/some/path/that/does/not/exist:${PATH}

${SBCL:-sbcl} <<EOF
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
    (assert (string= string "FEEFIE=foefum
")))
  ;; The default Unix environment for the subprocess is the same as
  ;; for the parent process. (I.e., we behave like perl and lots of
  ;; other programs, but not like CMU CL.)
  (let ((string (with-output-to-string (stream)
                  (sb-ext:run-program "/usr/bin/env" ()
		                      :output stream)))
	(expected (apply #'concatenate
	                 'string
			 (mapcar (lambda (environ-string)
			           (concatenate 'string
				                environ-string
						(string #\newline)))
                                 (sb-ext:posix-environ)))))
    (assert (string= string expected)))
  ;; That's not just because POSIX-ENVIRON is having a bad hair
  ;; day and returning NIL, is it?
  (assert (plusp (length (sb-ext:posix-environ))))
  ;; success convention for this Lisp program run as part of a larger script
  (sb-ext:quit :unix-status 52)))
EOF
if [ $? != 52 ]; then
    echo test failed: $?
    exit 1
fi

# success convention
exit 104
