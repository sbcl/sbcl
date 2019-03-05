#!/bin/sh

# Load our build configuration
. output/build-config

# This script is supposed to be host-agnostic, but unfortunately in testing
# I have found it to be sensitive to the host in the following ways:
#  -  using CCL it gets "Error: Incorrect keyword arguments in (:TEST EQ T)."
#  -  using CLISP it prints "WARNING: cross-compiler type ambiguity" 540 times.
# We could make this use the target SBCL as the new host,
# but that somewhat defeats the purpose.

time $SBCL_XC_HOST --noinform --no-userinit --no-sysinit --noprint <<EOF
(load "src/cold/shared.lisp")
(load "src/cold/set-up-cold-packages.lisp")
(load "tools-for-build/corefile.lisp")
(in-package "SB-COLD")
(in-host-compilation-mode
 (lambda (&aux (sb-xc:*features* (cons :c-headers-only sb-xc:*features*)))
  (do-stems-and-flags (stem flags 1)
    (when (member :c-headers flags)
       (handler-bind ((style-warning (function muffle-warning)))
         (load (merge-pathnames (stem-remap-target stem) ".lisp")))))
  (load "src/compiler/generic/genesis.lisp")))
(genesis :c-header-dir-name "src/runtime/genesis/" :verbose nil)
EOF

diff -r output/genesis-2 src/runtime/genesis

(cd src/runtime ; make)
