#!/bin/sh

# tests related to --script

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

tmpscript=$TEST_FILESTEM.lisp-script
tmpout=$TEST_FILESTEM.lisp-out
tmperr=$TEST_FILESTEM.lisp-err

echo '(quit :unix-status 7)' > $tmpscript
run_sbcl --script $tmpscript
check_status_maybe_lose "--script exit status from QUIT" $? 7 "(quit status good)"

echo '(error "oops")' > $tmpscript
run_sbcl --script $tmpscript 1> $tmpout 2> $tmperr
check_status_maybe_lose "--script exit status from ERROR" $? 1 "(error implies 1)"
grep BACKTRACE $tmpout > /dev/null
check_status_maybe_lose "--script backtrace not to stdout" $? 1 "(ok)"
grep BACKTRACE $tmperr > /dev/null
check_status_maybe_lose "--script backtrace to stderr" $? 0 "(ok)"

echo 'nil'> $tmpscript
run_sbcl --script $tmpscript
check_status_maybe_lose "--script exit status from normal exit" $? 0 "(everything ok)"

cat > $tmpscript <<EOF
#+darwin
(progn
  (format t "silently skipping known failure in script.test.sh~%")
  (sb-ext:quit :unix-status 3))
(setf *standard-output* (open "/dev/stdout"))
(close *standard-output*)
(sb-ext:quit :unix-status 3)
EOF
run_sbcl --script $tmpscript >/dev/null
check_status_maybe_lose "--script exit status from QUIT when stdout closed" $? 3 "(as given)"
run_sbcl --load $tmpscript >/dev/null
check_status_maybe_lose "--load exit status from QUIT when stdout closed" $? 3 "(as given)"

cat > $tmpscript <<EOF
(loop (write-line (read-line)))
EOF
echo ONE | run_sbcl --script $tmpscript 1> $tmpout 2> $tmperr
check_status_maybe_lose "--script exit status when stdin closed" $? 0 "(as given)"
if [ -s $tmperr ] || [ "ONE" != `cat $tmpout` ]
then
    echo "--script outputs wrong"
    exit $EXIT_LOSE
fi

cat > $tmpscript <<EOF
(loop (write-line "foo"))
EOF
run_sbcl --script $tmpscript 2> $tmperr | head -n1 > $tmpout
check_status_maybe_lose "--script exit status when stdout closed" $? 0 "(as given)"
if [ -s $tmperr ] || [ "foo" != `cat $tmpout` ]
then
    echo "--script unexpected error output"
    exit $EXIT_LOSE
fi
echo '(write-line "Ok!")' | run_sbcl --script 1>$tmpout 2>$tmperr
check_status_maybe_lose "--script exit status script from stdin" $? 0 "(ok)"
if [ -s $tmperr ] || [ "Ok!" != `cat $tmpout` ]
then
    echo "--script unexpected error output"
    exit $EXIT_LOSE
fi

# --script
cat > $tmpscript <<EOF
(print :script-ok)
EOF
run_sbcl --script $tmpscript --eval foo \
  < /dev/null > $tmpout
if [ "`grep -c :SCRIPT-OK $tmpout`" != 1 ] ; then
   echo "failed --script test using PRINT"
   exit $EXIT_LOSE
fi

rm -f $tmpscript $tmpout $tmperr

exit $EXIT_TEST_WIN
