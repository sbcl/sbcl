# To be sourced by shell scripts in the test suite.

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

# Before sbcl-1.0.13 or so, we set up some environment variables to
# the absolute (POSIX) pathname naming the SBCL runtime, core, and
# home; but this runs afoul of the Bourne shell's repeated
# tokenization of its inputs, so now we use some shell functions.
. ../sbcl-pwd.sh
sbcl_pwd

# Make the shell bomb out whenever an unset shell variable is used.
# Note that scripts may toggle this if necessary.
set -u

# Initialize variables.
set -a # export all variables at assignment-time.
# Note: any script that uses the variables that name files should
# quote them (with double quotes), to contend with whitespace.
SBCL_HOME="${TEST_SBCL_HOME:-$SBCL_PWD/../obj/sbcl-home}"
SBCL_CORE="${TEST_SBCL_CORE:-$SBCL_PWD/../output/sbcl.core}"
SBCL_RUNTIME="${TEST_SBCL_RUNTIME:-$SBCL_PWD/../src/runtime/sbcl}"
SBCL_ARGS="${TEST_SBCL_ARGS:---disable-ldb --noinform --no-sysinit --no-userinit --noprint --disable-debugger}"


# Tests should probably not care about their own name.
script_basename=`basename $0`
# Scripts that use this variable should quote it.
TEST_FILESTEM=`basename "${script_basename}" | sed -e 's/\.sh$//' -e 's/\./-/g'`

TEST_DIRECTORY="${SBCL_PWD}/${TEST_FILESTEM}-$$"
export TEST_DIRECTORY

# "Ten four" is the closest numerical slang I can find to "OK", so
# it's the Unix status value that we expect from a successful test.
# (Of course, zero is the usual success value, but we don't want to
# use that because SBCL returns that by default, so we might think
# we passed a test when in fact some error caused us to exit SBCL
# in a weird unexpected way. In contrast, 104 is unlikely to be
# returned unless we exit through the intended explicit "test
# successful" path.
EXIT_TEST_WIN=104
# Shell scripts in this test suite also return 104, so we need a
# convention for distinguishing successful execution of SBCL in one of
# our scripts.
EXIT_LISP_WIN=52
# Any test that exits with status 1 is an explicit failure.
EXIT_LOSE=1

LANG=C
LC_ALL=C
set +a

run_sbcl () (
    set -u
    if [ $# -gt 0 ]; then
	"$SBCL_RUNTIME" --lose-on-corruption --core "$SBCL_CORE" $SBCL_ARGS --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})" "$@"
    else
	"$SBCL_RUNTIME" --lose-on-corruption --core "$SBCL_CORE" $SBCL_ARGS --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})"
    fi
)

run_sbcl_with_args () (
    set -u
    "$SBCL_RUNTIME" --lose-on-corruption --core "$SBCL_CORE" "$@"
)

run_sbcl_with_core () (
    set -u
    core="$1"
    shift
    if [ $# -gt 0 ]; then
	"$SBCL_RUNTIME" --lose-on-corruption --core "$core" "$@"
    else
	"$SBCL_RUNTIME" --lose-on-corruption --core "$core" $SBCL_ARGS --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})"
    fi
)

# Most tests that run an SBCL have to check whether the child's exit
# status.  Our convention is that SBCL exits with status
# $EXIT_LISP_WIN to indicate a successful run; but some tests can't do
# this (e.g., ones that end in S-L-A-D), or need to indicate some
# other ways of succeeding.  So this routine takes a test name, the
# exit status of the child, and then an arbitrary number extra
# arguments that will be treated as status-code/message pairs for
# unusual successful ways for the inferior SBCL to exit.  If the exit
# code of the SBCL isn't found in the status-codes, the calling script
# will exit with a failure code.
check_status_maybe_lose () {
    testname=$1
    status=$2
    lose=1
    if [ $status = $EXIT_LISP_WIN ]; then
	echo "test $testname ok"
	lose=0
    else
	shift; shift;
	while [ $# -gt 0 ]; do
	    if [ $status = $1 ]; then
		shift;
		echo "test $testname ok $1"
		lose=0
		break
	    fi
	    shift; shift
	done
    fi
    if [ $lose = 1 ]; then
        echo "test $testname failed: $status"
	exit $EXIT_LOSE
    fi
    unset lose
    unset status
    unset testname
}

# Picking an output dir is delayed until actually needed.
# In particular we can't read "$SBCL_SOFTWARE_TYPE" until after test-util
# assigns it into the environment, but this script is also sourced
# by run-tests.sh itself, which means that test-util hasn't done its thing.
#
pick_random_output_dir() {
    # Avoid writing into the source tree as much as possible.
    if [ "$SBCL_SOFTWARE_TYPE" != OpenBSD ]; then
        # Don't try to run sbcl from /tmp on openbsd as it's unlikely to be
        # mounted with wxallowed
        # If a test doesn't create an executable core, it would work to use /tmp,
        # but this utility is unaware of the intent of each test.
        # This means that some tests might flake our, and/or leave junk in your tree.
        base_dir=${TMPDIR:-/tmp}
    else
        base_dir="$SBCL_PWD"
    fi
    TEST_DIRECTORY="${base_dir}/${TEST_FILESTEM}-$$"
}

# Not every test needs to touch the file system, but enough do to have
# them consistently do so in subdirectories.  Note that such tests
# should not change their exit action, or do so only very carefully.
use_test_subdirectory () { # not a "subdirectory" now, but don't feel like renaming
    if test -d "$TEST_DIRECTORY"
    then
        cleanup_test_subdirectory
    fi
    pick_random_output_dir
    mkdir "$TEST_DIRECTORY"
    cd "$TEST_DIRECTORY"
    trap "cleanup_test_subdirectory" EXIT
}

# FIXME: Is it really true that a test must alter the source tree? How pathetic.
# In particular, failures can occur in the :READDIR/DIRENT-NAME test of sb-posix
# when using parallel-exec, because "run-sbcl-test-NNNN" randomly appears
# in either the SB-POSIX:READDIR call or CL:DIRECTORY but not both.
use_test_subdirectory_in_source_tree () { # DON'T USE THIS!
    if test -d "$TEST_DIRECTORY"
    then
        cleanup_test_subdirectory
    fi
    mkdir "$TEST_DIRECTORY"
    cd "$TEST_DIRECTORY"
    trap "cleanup_test_subdirectory" EXIT
}

# Do the above but without changing the current directory
create_test_subdirectory () {
    if test -d "$TEST_DIRECTORY"
    then
        cleanup_test_subdirectory
    fi
    pick_random_output_dir
    mkdir "$TEST_DIRECTORY"
    trap "cleanup_test_subdirectory" EXIT
}

cleanup_test_subdirectory () {
    cd "$SBCL_PWD"
    ( set -f; rm -r "$TEST_DIRECTORY" )
}
