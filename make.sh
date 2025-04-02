#!/bin/sh
set -e

LANG=C
LC_ALL=C
export LANG LC_ALL

# "When we build software, it's a good idea to have a reliable method
# for getting an executable from it. We want any two reconstructions
# starting from the same source to end up in the same result. That's
# just a basic intellectual premise."
#     -- Christian Queinnec, in _Lisp In Small Pieces_, p. 313

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

# If you're cross-compiling, make-config.sh should "do the right
# thing" when run on the target machine, with the minor caveat that
# any --xc-host parameter should be suitable for the host machine
# instead of the target.
sh make-config.sh "$@" --check-host-lisp || exit $?

. output/prefix.def
. output/build-config

echo //building host tools
# Build the perfect-hash-generator. It's actually OK if this fails,
# as long as xperfecthash.lisp-expr is up-to-date
$GNUMAKE -C tools-for-build perfecthash || true

build_started=`date`
echo "//Starting build: $build_started"
# Apparently option parsing succeeded. Print out the results.
echo "//Options: --prefix='$SBCL_PREFIX' --xc-host='$SBCL_XC_HOST'"

# Enforce the source policy for no bogus whitespace
$SBCL_XC_HOST < tools-for-build/canonicalize-whitespace.lisp || exit 1

# The make-host-*.sh scripts are run on the cross-compilation host,
# and the make-target-*.sh scripts are run on the target machine. In
# ordinary compilation, we just do these phases consecutively on the
# same machine, but if you wanted to cross-compile from one machine
# which supports Common Lisp to another which does not (yet:-) support
# Common Lisp, you could do something like this:
#   Create copies of the source tree on both the host and the target.
#   Read the make-config.sh script carefully and emulate it by hand
#     on both machines (e.g. creating "target"-named symlinks to
#     identify the target architecture).
#   On the host system:
#     SBCL_XC_HOST=<whatever> sh make-host-1.sh
#   Copy src/runtime/genesis/*.h from the host system to the target
#     system.
#   On the target system:
#     sh make-target-1.sh
#   Copy output/stuff-groveled-from-headers.lisp
#     from the target system to the host system.
#   On the host system:
#     SBCL_XC_HOST=<whatever> sh make-host-2.sh
#   Copy output/cold-sbcl.core from the host system to the target system.
#   On the target system:
#     sh make-target-2.sh
#     sh make-target-contrib.sh
# Or, if you can set up the files somewhere shared (with NFS, AFS, or
# whatever) between the host machine and the target machine, the basic
# procedure above should still work, but you can skip the "copy" steps.
# If you can use rsync on the host machine, you can call make-config.sh
# with:
# --host-location=user@host-machine:<rsync path to host sbcl directory>
# and the make-target-*.sh scripts will take care of transferring the
# necessary files.
maybetime() {
    if command -v time > /dev/null ; then
        time $@
    else
        $@
    fi
}
maybetime sh make-host-1.sh
maybetime sh make-target-1.sh
maybetime sh make-host-2.sh
maybetime sh make-target-2.sh
maybetime sh make-target-contrib.sh

# Confirm that default evaluation strategy is :INTERPRET if sb-fasteval was built
src/runtime/sbcl --core output/sbcl.core --lose-on-corruption --noinform \
  --no-sysinit --no-userinit --disable-debugger \
  --eval '(when (find-package "SB-INTERPRETER") (assert (eq *evaluator-mode* :interpret)))' \
  --quit

./src/runtime/sbcl --core output/sbcl.core \
 --lose-on-corruption --noinform $SBCL_MAKE_TARGET_2_OPTIONS --no-sysinit --no-userinit --eval '
    (progn
      #-sb-devel
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
            ;; Assert that a chosen few symbols not named using the ! convention are removed
            ;; by tree-shaking. This list was made by hand-checking various macros that seemed
            ;; not to be needed after the build. I would have thought
            ;; (EVAL-WHEN (:COMPILE-TOPLEVEL)) to be preferable, but revision fb1ba6de5e makes
            ;; a case for not doing that. Either way is less than fabulous.
            (sb-int:awhen
                (mapcan (quote apropos-list)
                        (quote ("DEFINE-INFO-TYPE" "LVAR-TYPE-USING"
                                                   "TWO-ARG-+/-"
                                                   "PPRINT-TAGBODY-GUTS" "WITH-DESCRIPTOR-HANDLERS"
                                                   "SUBTRACT-BIGNUM-LOOP" "BIGNUM-REPLACE" "WITH-BIGNUM-BUFFERS"
                                                   "GCD-ASSERT" "BIGNUM-NEGATE-LOOP"
                                                   "SHIFT-RIGHT-UNALIGNED"
                                                   "STRING-LESS-GREATER-EQUAL-TESTS")))
              (format t "~&Leftover from [disabled?] tree-shaker:~%~S~%" sb-int:it))
            (when l2
              (format t "Found ~D fdefns named by uninterned symbols:~%~S~%" (length l2) l2)))
        (abort-build ()
          :report "Abort building SBCL."
          (sb-ext:exit :code 1))))' --quit

# contrib/Makefile shouldn't be counted in NCONTRIBS nor should asdf and uiop.
# The asdf directory produces 2 fasls, so is unlike all our other contribs
# and would therefore mess up the accounting here if included.
NPASSED=`ls obj/sbcl-home/contrib/sb-*.fasl | wc -l`
echo
echo "The build seems to have finished successfully, including $NPASSED"
echo "contributed modules. If you would like to run more extensive tests on"
echo "the new SBCL, you can try:"
echo
echo "  cd ./tests && sh ./run-tests.sh"
echo
echo "To build documentation:"
echo
echo "  cd ./doc/manual && make"
echo
echo "To install SBCL (more information in INSTALL):"
echo
echo "  sh install.sh"

build_finished=`date`
echo
echo "//build started:  $build_started"
echo "//build finished: $build_finished"
