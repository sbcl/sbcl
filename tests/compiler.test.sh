#!/bin/sh

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

. ./expect.sh

base_tmpfilename="compiler-test-$$-tmp"
tmpfilename="$base_tmpfilename.lisp"
compiled_tmpfilename="$base_tmpfilename.fasl"

# This should fail, as type inference should show that the call to FOO
# will return something of the wrong type.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (list x))
    (defun bar (x) (1+ (foo x)))
EOF
expect_failed_compile $tmpfilename

# This should fail, as we define a function multiply in the same file
# (CLHS 3.2.2.3).
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (list x))
    (defun foo (x) (cons x x))
EOF
expect_failed_compile $tmpfilename

# This shouldn't fail, as the inner FLETs should not be treated as
# having the same name.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) 
      (flet ((baz (y) (load y)))
        (declare (notinline baz))
        (baz x)))
    (defun bar (x) 
      (flet ((baz (y) (load y)))
        (declare (notinline baz))
        (baz x)))
EOF
expect_clean_compile $tmpfilename

# This shouldn't fail despite the apparent type mismatch, because of
# the NOTINLINE declamation.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (list x))
    (declaim (notinline foo))
    (defun bar (x) (1+ (foo x)))
EOF
expect_clean_compile $tmpfilename

# This shouldn't fail despite the apparent type mismatch, because of
# the NOTINLINE declaration.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (list x))
    (defun bar (x) 
      (declare (notinline foo))
      (1+ (foo x)))
EOF
expect_clean_compile $tmpfilename

# This in an ideal world would fail (that is, return with FAILURE-P
# set), but at present it doesn't.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (list x))
    (defun bar (x)
      (declare (notinline foo))
      (locally
        (declare (inline foo))
        (1+ (foo x))))
EOF
# expect_failed_compile $tmpfilename

# This used to not warn, because the VALUES derive-type optimizer was
# insufficiently precise.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (declare (ignore x)) (values))
    (defun bar (x) (1+ (foo x)))
EOF
expect_failed_compile $tmpfilename

# Even after making the VALUES derive-type optimizer more precise, the
# following should still be clean.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (declare (ignore x)) (values))
    (defun bar (x) (car x))
EOF
expect_clean_compile $tmpfilename

# NOTINLINE on known functions shouldn't inhibit type inference
# (spotted by APD sbcl-devel 2003-06-14)
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x)
      (declare (notinline list))
      (1+ (list x)))
EOF
expect_failed_compile $tmpfilename

# ERROR wants to check its format string for sanity...
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x)
      (when x
        (error "~S")))
EOF
expect_failed_compile $tmpfilename

# ... but it (ERROR) shouldn't complain about being unable to optimize
# when it's uncertain about its argument's type
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x)
      (error x))
EOF
fail_on_compiler_note $tmpfilename

# test case from Rudi for some CLOS WARNINGness that shouldn't have
# been there
cat > $tmpfilename <<EOF
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (defstruct buffer-state 
        (output-index 0)))
    
    (defclass buffered-stream-mixin ()
      ((buffer-state :initform (make-buffer-state))))
    
    (defgeneric frob (stream))
    (defmethod frob ((stream t))
      nil)
    (defmethod frob ((stream buffered-stream-mixin))
      (symbol-macrolet
            ((index (buffer-state-output-index (slot-value stream 'buffer-state))))
          (setf index 0))
      (call-next-method))
EOF
expect_clean_compile $tmpfilename

rm $tmpfilename
rm $compiled_tmpfilename

# success 
exit 104
