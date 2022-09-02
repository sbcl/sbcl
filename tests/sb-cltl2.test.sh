#!/bin/sh
. ./subr.sh
run_sbcl <<EOF
  (require :sb-cltl2)
  (push :sb-testing-contrib *features*)
  (require :asdf)
  (let ((*package* (find-package "ASDF"))) (load "../contrib/sb-cltl2/sb-cltl2.asd"))
  (asdf:test-system "sb-cltl2/tests")
  (exit :code $EXIT_TEST_WIN)
EOF
