#!/bin/sh

sbcl <<EOF
    (compile-file "WHN/stress-gc.lisp")
    (load *)
    (time (stress-gc ${1:-100000} ${2:-3000}))
    (format t "~&test completed successfully~%")
EOF
