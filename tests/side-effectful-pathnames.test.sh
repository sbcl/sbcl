#!/bin/sh

# FIXME: MNA wrote the tests below to work with the new
# lp-test-file.lisp file in place. It'd be good to replace them either
# with code which uses an existing distribution file instead, or with
# code which creates a new file in $TMPDIR and uses that. Meanwhile,
# we just return success immediately instead of doing anything.
exit 104

;;; loading files w/ logical pathnames
(setf (logical-pathname-translations "TEST")
        '(("**;*.*.*"
           #.(concatenate 'string
              (namestring (sb-int:default-directory))
              "**/*.*"))
          ("**;*.*.*"
           #.(concatenate 'string
              (namestring (sb-int:default-directory))
              "**/*.*.*"))))
(assert (equal (namestring (translate-logical-pathname
                            "test:lp-test-file.lisp"))
               #.(concatenate 'string
                              (namestring (sb-int:default-directory))
                              "lp-test-file.lisp")))
(load "TEST:LP-TEST-FILE")
(let ((compiled-file-name (namestring (compile-file "TEST:LP-TEST-FILE")))
      (should-be-file-name
        #.(concatenate 'string
                       (namestring (sb-int:default-directory))
                       "lp-test-file.x86f")))
  (assert (equal compiled-file-name should-be-file-name)))
