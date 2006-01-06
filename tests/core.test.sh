#!/bin/sh

# tests related to .core files

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

tmpcore="core-test-sh-$$.core"
rm -f $tmpcore

# In sbcl-0.7.7 SAVE-LISP-AND-DIE didn't work at all because of
# flakiness caused by consing/GC/purify twice-and-at-least-twice
# mismatch grot.
#
# "serves yall right for fiddling with too much stuff"
#   -- Eric Marsden, <http://tunes.org/~nef/logs/lisp/02.09.15>
#
# diagnosed and fixed by Dan Barlow in sbcl-0.7.7.29
$SBCL <<EOF
  (defun foo (x) (+ x 11))
  (save-lisp-and-die "$tmpcore")
EOF
$SBCL_ALLOWING_CORE --core "$tmpcore" \
--userinit /dev/null --sysinit /dev/null <<EOF
  (quit :unix-status (foo 10))
EOF
if [ $? = 21 ]; then
    echo "/Basic SAVE-LISP-AND-DIE worked, good."
else
    echo "failure in basic SAVE-LISP-AND-DIE: $?"
    exit 1
fi

# In sbcl-0.9.8 saving cores with callbacks didn't work on gencgc platforms
$SBCL <<EOF
  (defun bar () 
    (format t "~&Callbacks not supported, skipping~%")
    (quit :unix-status 42))
  #+alien-callbacks
  (progn
    (sb-alien::define-alien-callback foo int () 42)
    (defun bar () (quit :unix-status (alien-funcall foo))))
  (save-lisp-and-die "$tmpcore")
EOF
$SBCL_ALLOWING_CORE --core "$tmpcore" \
--userinit /dev/null --sysinit /dev/null <<EOF
  (bar)
EOF
if [ $? = 42 ]; then
    echo "/Callbacks after SAVE-LISP-AND-DIE worked, good."
else
    echo "failure in basic SAVE-LISP-AND-DIE: $?"
    exit 1
fi

rm -f $tmpcore
echo "/returning success from core.test.sh"
exit 104
