#!/bin/sh

# Load our build configuration
. output/build-config

# This script has been tested with CLISP, ECL and SBCL (of course).
# Running with CCL it gets:
#            > Error: Class is not yet defined or was undefined: CTYPE
#            > While executing: COMPILER-LAYOUT-OR-LOSE, in process listener(1).
time $SBCL_XC_HOST <<EOF
(load "src/cold/shared.lisp")
(load "src/cold/set-up-cold-packages.lisp")
(load "tools-for-build/corefile.lisp")
(in-package "SB-COLD")
(defvar *target-sbcl-version* (read-from-file "version.lisp-expr"))
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

(cd src/runtime ; $GNUMAKE)
