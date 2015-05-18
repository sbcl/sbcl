OUT=$(mktemp -d)
mkdir $OUT/sbcl $OUT/ccl32 $OUT/ccl64 $OUT/clisp

./make.sh --arch=x86 --xc-host='sbcl' "$@" && tar cf - run-sbcl.sh src/runtime/sbcl obj/from-xc output | tar -C $OUT/sbcl -xf -

./make.sh --arch=x86 --xc-host='/home/csr21/src/lisp/ccl/lx86cl -b' "$@" && tar cf - run-sbcl.sh src/runtime/sbcl obj/from-xc output | tar -C $OUT/ccl32 -xf -

./make.sh --arch=x86 --xc-host='/home/csr21/src/lisp/ccl/lx86cl64 -b' "$@" && tar cf - run-sbcl.sh src/runtime/sbcl obj/from-xc output | tar -C $OUT/ccl64 -xf -

./make.sh --arch=x86 --xc-host='clisp -ansi -on-error abort' "$@" && tar cf - run-sbcl.sh src/runtime/sbcl obj/from-xc output | tar -C $OUT/clisp -xf -

echo done: cd $OUT
