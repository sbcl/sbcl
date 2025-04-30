# Automated platform feature testing
cd ./tools-for-build > /dev/null
. ./android_run.sh

# FIXME: Use this to test for dlopen presence and hence
# load-shared-object buildability

# Assumes the presence of $1-test.c, which when built and
# run should return with 104 if the feature is present.
# We presumes that the build machine matches the target machine
# in terms of a whether each feature presence test should pass.
featurep() {
    bin="$1-test"
    featurename=${2:-$1}
    rm -f $bin
    if $android
    then
        $CC -I../src/runtime -ldl -o $bin $bin.c > /dev/null 2>&1
	exit_code=`android_run_for_exit_code $bin`
    else
        $GNUMAKE $bin -I ../src/runtime > /dev/null 2>&1 && echo "input" | ./$bin> /dev/null 2>&1
	exit_code="$?"
    fi
    if [ "$exit_code" -eq 104 ]
    then
        printf " :$featurename"
    fi
    rm -f $bin
}

# Adding a nonexistent link library to Config.*-win32 will fail,
# so we pass -lSynchronization to the featurep test specifically
# and not in the general make rule.
# It will get added in by Config.*-win32 only if LISP_FEATURE_SB_FUTEX.
if [ "$sbcl_os" = win32 ] ; then
   LOADLIBES=-lSynchronization featurep os-provides-wakebyaddr sb-futex
   featurep ucrt
fi

featurep os-provides-dlopen

featurep os-provides-dladdr

featurep os-provides-blksize-t

featurep os-provides-suseconds-t

featurep os-provides-getprotoby-r

featurep os-provides-poll

featurep os-provides-close-range-wrapper

featurep os-provides-posix-spawn

if [ "$sbcl_os" = linux ] ; then
    case "$sbcl_arch" in
        arm | x86 | ppc | mips | sparc | riscv32)
            featurep 64-bit-time
    esac
fi

if [ "$sbcl_arch" = arm ] ; then
   featurep arm-softfp
fi

case "$sbcl_arch" in
    riscv*)
        featurep os-provides-flush-icache
esac
