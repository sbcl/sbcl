export TEST_BASEDIR=${TMPDIR:-/tmp}
. ./subr.sh

sourcefile=`pwd -P`/heap-reloc/embiggen.lisp
use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

# Expose potential failure that could happen in save-lisp-and-die in an image
# that was restarted from one that underwent number coalescing during a
# previous save-lisp-and-die: A bignum as a layout bitmap can be forwarded
# while using that bignum as the bitmap to decide what to scan in that selfsame
# instance. Aside from random failure, this could be detected by enabling
# 'verify_gens' which printed "Ptr sees free page" after GC failed to scavenge
# all pointer slots. I believe that it was a coincidence that my test croaked
# specifically while scanning layout-of-layout. It could have been any
# structure having a slot holding a bignum EQ to its own layout-bitmap.
run_sbcl --load ${sourcefile} <<EOF
  #+gencgc (setf (extern-alien "verify_gens" char) 0)
  (save-lisp-and-die "$tmpcore")
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit --eval "(exit)"
check_status_maybe_lose "Crash GC" $? 0 "(saved core ran)"

exit $EXIT_TEST_WIN
