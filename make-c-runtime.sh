#!/bin/sh

# Load our build configuration
. output/build-config

time $SBCL_XC_HOST <<EOF
(load "src/cold/shared.lisp")
(load "src/cold/set-up-cold-packages.lisp")
(load "tools-for-build/corefile.lisp")
(in-package "SB-COLD")
(in-host-compilation-mode
 (lambda (&aux (*features* (cons :c-headers-only *features*)))
  (do-stems-and-flags (stem flags)
    (when (member :c-headers flags)
       (handler-bind ((style-warning (function muffle-warning)))
         (load (merge-pathnames (stem-remap-target stem) ".lisp")))))
  (load "src/compiler/generic/genesis.lisp")))
(genesis :c-header-dir-name "src/runtime/genesis/" :verbose nil)
EOF

diff -r output/genesis-2 src/runtime/genesis

(cd src/runtime ; make)
