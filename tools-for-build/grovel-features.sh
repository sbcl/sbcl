# Automated platform feature testing
cd ./tools-for-build > /dev/null

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
    $GNUMAKE $bin -I ../src/runtime > /dev/null 2>&1 && echo "input" | ./$bin> /dev/null 2>&1
    if [ "$?" -eq 104 ]
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
fi

# KLUDGE: ppc/darwin dlopen is special cased in make-config.sh, as
# we fake it with a shim.
featurep os-provides-dlopen

featurep os-provides-dladdr

featurep os-provides-blksize-t

featurep os-provides-suseconds-t

featurep os-provides-getprotoby-r

featurep os-provides-poll

if [ "$sbcl_arch" = arm ] ; then
   featurep arm-softfp
fi
