# Automated platform feature testing
cd ./tools-for-build > /dev/null

# FIXME: Use this to test for dlopen presence and hence
# load-shared-object buildability

# Assumes the presence of $1-test.c, which when built and
# run should return with 104 if the feature is present.
featurep() {
    bin="$1-test"
    rm -f $bin
    $GNUMAKE $bin -I ../src/runtime > /dev/null 2>&1 && echo "input" | ./$bin> /dev/null 2>&1
    if [ "$?" -eq 104 ]
    then
        printf " :$1"
    fi
    rm -f $bin
}

# Adding a nonexistent link library to Config.*-win32 will fail,
# so this needs its own recipe to detect the library.
if [ "$sbcl_os" = win32 ] ; then
    bin=os-provides-wakebyaddr-test
    rm -f $bin
    # pass -f /dev/null to use only builtin Make recipes
    LOADLIBES=-lSynchronization $GNUMAKE -f /dev/null $bin > /dev/null 2>&1
    if [ "$?" -eq 0 ]
    then
        printf " :sb-futex"
    fi
    rm -f $bin
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
