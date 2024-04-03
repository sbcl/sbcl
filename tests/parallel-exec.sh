#!/bin/sh

logdir=${SBCL_PAREXEC_TMP:-$HOME}/sbcl-test-logs-$$
echo ==== Writing logs to $logdir ====
# FIXME: junkdir isn't getting removed
junkdir=${SBCL_PAREXEC_TMP:-/tmp}/junk
mkdir -p $junkdir $logdir

case `uname` in
    CYGWIN* | WindowsNT | MINGW* | MSYS*)
        if [ $# -ne 1 ]
        then
            echo $0: Need arg
            exit 1
        fi
        echo ";; Using -j$1"
        echo "LOGDIR=$logdir" >$logdir/Makefile
        ../run-sbcl.sh --script genmakefile.lisp >>$logdir/Makefile
        exec $GNUMAKE -k -j $1 -f $logdir/Makefile
        ;;
esac

export TEST_LOGDIR TEST_DIRECTORY SBCL_HOME
TEST_LOGDIR=$logdir TEST_DIRECTORY=$junkdir SBCL_HOME=../obj/sbcl-home \
  exec ../src/runtime/sbcl \
  --noinform --core ../output/sbcl.core \
  --no-userinit --no-sysinit --noprint --disable-debugger $* < parallel-exec.lisp

