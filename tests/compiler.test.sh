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

# FIXME: the functions below should be in their own file, sourced by
# each of the *.test.sh scripts.

# Check that compiling and loading the file $1 generates an error
# at load time; also that just loading it directly (into the
# interpreter) generates an error.
expect_load_error ()
{
    # Test compiling and loading.
    $SBCL <<EOF
	(compile-file "$1")
	;;; But loading the file should fail.
	(multiple-value-bind (value0 value1) (ignore-errors (load *))
	    (assert (null value0))
	    (format t "VALUE1=~S (~A)~%" value1 value1)
	    (assert (typep value1 'error)))
	(sb-ext:quit :unix-status 52)
EOF
    if [ $? != 52 ]; then
	echo compile-and-load $1 test failed: $?
	exit 1
    fi

    # Test loading into the interpreter.
    $SBCL <<EOF
	(multiple-value-bind (value0 value1) (ignore-errors (load "$1"))
	    (assert (null value0))
	    (format t "VALUE1=~S (~A)~%" value1 value1)
	    (assert (typep value1 'error)))
	(sb-ext:quit :unix-status 52)
EOF
    if [ $? != 52 ]; then
	echo load-into-interpreter $1 test failed: $?
	exit 1
    fi
}

# Test that a file compiles cleanly, with no ERRORs, WARNINGs or
# STYLE-WARNINGs.
expect_clean_compile () 
{
    $SBCL <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$1")
          (declare (ignore pathname))
          (assert (not warnings-p))
          (assert (not failure-p))
          (sb-ext:quit :unix-status 52))
EOF
    if [ $? != 52 ]; then
        echo clean-compile $1 test failed: $?
        exit 1
    fi
}

expect_warned_compile ()
{
    $SBCL <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$1")
          (declare (ignore pathname))
          (assert warnings-p)
          (assert (not failure-p))
          (sb-ext:quit :unix-status 52))
EOF
    if [ $? != 52 ]; then
        echo warn-compile $1 test failed: $?
        exit 1
    fi
}

expect_failed_compile ()
{
    $SBCL <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$1")
          (declare (ignore pathname warnings-p))
          (assert failure-p)
          (sb-ext:quit :unix-status 52))
EOF
    if [ $? != 52 ]; then
        echo fail-compile $1 test failed: $?
        exit 1
    fi
}

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

rm $tmpfilename
rm $compiled_tmpfilename

# success 
exit 104
