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

use_test_subdirectory

tmpfilename="$TEST_FILESTEM.lisp"

# This should fail, but didn't until sbcl-0.6.12.7, with Martin
# Atzmueller's port of Pierre Mai's fixes.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    ;; This definition has too many qualifiers, so loading the
    ;; DEFMETHOD should fail.
    (defmethod zut progn :around ((x integer)) (print "integer"))
    (zut 1)
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
    (zut 1)
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
    (zut 1)
EOF
expect_load_error $tmpfilename

# Until sbcl-0.7.6.21, PCL signalled spurious STYLE-WARNINGs on
# compilation of this form; the report (bug #191a.) and a patch
# suppressing these were provided by Alexey Dejenka in quick
# succession.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defclass another-class-with-slots ()
      (a-new-slot-name))
    (defun foo (x)
      (values (setf (slot-value x 'a-new-slot-name) 2)
              (slot-value x 'a-new-slot-name)))
EOF
expect_clean_compile $tmpfilename

# Test that slots with similar names don't trigger warnings if neither
# name is exported.

cat > $tmpfilename <<EOF
    (defpackage "INT"
      (:use "CL")
      (:export "EX"))
    (in-package "INT")
    (defclass ex ()
      ((a-slot :initarg :a-slot)))
    (in-package :cl-user)
    (handler-bind ((warning (lambda (c) (error "caught warning: ~A" c))))
      (defclass why (int:ex)
        ((a-slot :initarg :a-slot)))
      (sb-mop:finalize-inheritance (find-class 'why)))
EOF
expect_clean_cload $tmpfilename

# success
exit $EXIT_TEST_WIN
