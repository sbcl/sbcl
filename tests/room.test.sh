#!/bin/sh

# testing ROOM in a fresh SBCL.

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

echo //entering room.test.sh

${SBCL:-sbcl} --eval "(progn (defvar *a* (make-array (expt 2 27))) (room) (sb-ext:quit :unix-status 52))"
if [ $? = 52 ]; then
    true # nop
else
    exit 1
fi

${SBCL:-sbcl} --eval "(progn (dotimes (i 10) (dotimes (j 10) (room)) (gc)) (sb-ext:quit :unix-status 52))"
if [ $? = 52 ]; then
    true # nop
else
    exit 1
fi

# success convention for script
exit 104
