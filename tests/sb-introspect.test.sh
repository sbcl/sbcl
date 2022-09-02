#!/bin/sh
. ./subr.sh
run_sbcl <<EOF
  (setq *features* (append sb-impl:+internal-features+ *features*))
  (push :sb-testing-contrib *features*)
  (require :asdf)
  (let ((*package* (find-package "ASDF"))) (load "../contrib/sb-introspect/sb-introspect.asd"))
  (asdf:test-system "sb-introspect/tests")
  (exit :code $EXIT_TEST_WIN)
EOF
