#!/bin/sh
set -em

# --load argument skips compilation.
#
# This is a script to be run as part of make.sh. The only time you'd
# want to run it by itself is if you're trying to cross-compile the
# system or if you're doing some kind of troubleshooting.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

echo //entering make-target-2.sh

LANG=C
LC_ALL=C
export LANG LC_ALL

# Load our build configuration
. output/build-config

if [ -n "$SBCL_HOST_LOCATION" ]; then
    echo //copying host-2 files to target
    rsync -a "$SBCL_HOST_LOCATION/output/" output/
fi

# Do warm init stuff, e.g. building and loading CLOS, and stuff which
# can't be done until CLOS is running.
#
# Note that it's normal for the newborn system to think rather hard at
# the beginning of this process (e.g. using nearly 100Mb of virtual memory
# and >30 seconds of CPU time on a 450MHz CPU), and unless you built the
# system with the :SB-SHOW feature enabled, it does it rather silently,
# without trying to tell you about what it's doing. So unless it hangs
# for much longer than that, don't worry, it's likely to be normal.
warm_compile=yes
devel=""
if [ "$1" = --load ]; then
    warm_compile=no
elif [ "$1" = --load-with-sb-devel ]; then
    warm_compile=no
    devel="(pushnew :sb-devel *features*)"
elif [ "x$1" != x ]; then
    echo Unknown option \'"$1"\' to make-target-2
    exit 1
fi
if [ "$warm_compile" = yes ]; then
    echo //doing warm init - compilation phase
    ./src/runtime/sbcl --core output/cold-sbcl.core \
     --lose-on-corruption $SBCL_MAKE_TARGET_2_OPTIONS --no-sysinit --no-userinit \
     --eval '(sb-fasl::!warm-load "src/cold/warm.lisp")' --quit
fi
echo //doing warm init - load and dump phase
./src/runtime/sbcl --noinform --core output/cold-sbcl.core \
                   --lose-on-corruption $SBCL_MAKE_TARGET_2_OPTIONS \
                   --no-sysinit --no-userinit --noprint <<EOF
(progn ${devel})
(sb-fasl::!warm-load "make-target-2-load.lisp")
(setf (extern-alien "gc_coalesce_string_literals" char) 2)
;;; Use the historical (bad) convention for *compile-file-pathname*
(setf sb-c::*merge-pathnames* t)
;;; and for storing pathname namestrings in fasls too.
(setq sb-c::*name-context-file-path-selector* 'truename)
(let ((sb-ext:*invoke-debugger-hook* (prog1 sb-ext:*invoke-debugger-hook* (sb-ext:enable-debugger))))
 (sb-ext:save-lisp-and-die "output/sbcl.core"))
EOF

# Confirm that default evaluation strategy is :INTERPRET if sb-fasteval was built
src/runtime/sbcl --core output/sbcl.core --lose-on-corruption --noinform \
  --no-sysinit --no-userinit --disable-debugger \
  --eval '(when (find-package "SB-INTERPRETER") (assert (eq *evaluator-mode* :interpret)))' \
  --quit

echo //checking for leftover cold-init symbols
./src/runtime/sbcl --core output/sbcl.core \
 --lose-on-corruption --noinform $SBCL_MAKE_TARGET_2_OPTIONS --no-sysinit --no-userinit --eval '
    (restart-case
      (let (l1 l2)
        (sb-vm:map-allocated-objects
         (lambda (obj type size)
           (declare (ignore size))
           (when (and (= type sb-vm:symbol-widetag) (not (symbol-package obj))
                      (search "!" (string obj)))
             (push obj l1))
           (when (and (= type sb-vm:fdefn-widetag)
                      (not (symbol-package
                            (sb-int:fun-name-block-name
                             (sb-kernel:fdefn-name obj)))))
             (push obj l2)))
         :all)
        (when l1 (format t "Found ~D:~%~S~%" (length l1) l1))
        (sb-int:awhen
          (mapcan (quote apropos-list)
           (quote ("DEFINE-INFO-TYPE" "LVAR-TYPE-USING"
                   "TWO-ARG-+/-"
                   "PPRINT-TAGBODY-GUTS" "WITH-DESCRIPTOR-HANDLERS"
                   "SUBTRACT-BIGNUM-LOOP" "BIGNUM-REPLACE" "WITH-BIGNUM-BUFFERS"
                   "GCD-ASSERT" "MODULARLY" "BIGNUM-NEGATE-LOOP"
                   "SHIFT-RIGHT-UNALIGNED"
                   "STRING-LESS-GREATER-EQUAL-TESTS")))
         (format t "~&Leftover from [disabled?] tree-shaker:~%~S~%" sb-int:it))
        (when l2 
           (format t "Found ~D fdefns named by uninterned symbols:~%~S~%" (length l2) l2)))
    (abort-build ()
      :report "Abort building SBCL."
      (sb-ext:exit :code 1)))' --quit
