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

flag="condition-wait-sigcont.tmp"
touch $flag

$SBCL --load condition-wait-sigcont.lisp &
sb_pid=$!

while [ -f $flag ]; do sleep 1; done
sleep 1
kill -STOP $sb_pid
kill -CONT $sb_pid

sleep 2
kill -KILL $sb_pid

if [ -f $flag ]
then
    rm $flag
    exit 1 # error
else
    exit 104 # success
fi
