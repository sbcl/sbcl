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

tmpfilename="clos-test-$$-tmp.lisp"

# This should fail, but didn't until sbcl-0.6.12.7, with Martin
# Atzmueller's port of Pierre Mai's fixes.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    ;; This definition has too many qualifiers, so loading the
    ;; DEFMETHOD should fail.
    (defmethod zut progn :around ((x integer)) (print "integer"))
EOF
expect_load_error $tmpfilename

# Even before sbcl-0.6.12.7, this would fail as it should. Let's
# make sure that it still does.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defgeneric zut (x) (:method-combination progn))
    ;; This definition is missing the PROGN qualifier, and so the
    ;; DEFMETHOD should fail.
    (defmethod zut ((x integer)) (print "integer"))
EOF
expect_load_error $tmpfilename

# Even before sbcl-0.6.12.7, this would fail as it should, but Martin
# Atzmueller's port of Pierre Mai's fixes caused it to generate more
# correct text in the error message. We can't check that in a regression
# test until AI gets a mite stronger, but at least we can check that
# the problem is still detected.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defgeneric zut (x) (:method-combination progn))
    ;; This definition has too many qualifiers, so loading the
    ;; DEFMETHOD should fail.
    (defmethod zut progn :around ((x integer)) (print "integer"))
EOF
expect_load_error $tmpfilename

rm $tmpfilename

# success 
exit 104
