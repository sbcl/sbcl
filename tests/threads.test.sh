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

. ./subr.sh
use_test_subdirectory

run_sbcl --eval '(sb-thread:return-from-thread t :allow-exit t)'
check_status_maybe_lose "return from main thread" $? 0 "ok"

run_sbcl --eval '(sb-thread:abort-thread :allow-exit t)'
check_status_maybe_lose "abort main thread" $? 1 "ok"

run_sbcl --eval '#+sb-thread (sb-thread:join-thread (sb-thread:make-thread (lambda () (sb-ext:exit :code 77)))) #-sb-thread (sb-ext:exit :code 77)'
check_status_maybe_lose "exit from normal thread" $? 77 "ok"

flag="condition-wait-sigcont.tmp"
touch $flag

# $! is not set correctly when calling run_sbcl, do it directly
"$SBCL_RUNTIME" --core "$SBCL_CORE" $SBCL_ARGS \
    --load "$SBCL_PWD/condition-wait-sigcont.lisp" &
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
