# This file run a regression test for a bug in loading
# forward-referenced layouts.

FILES='"undefined-classoid-bug-1.lisp" "undefined-classoid-bug-2.lisp"'
FASLS='"undefined-classoid-bug-1.fasl" "undefined-classoid-bug-2.fasl"'

${SBCL:-sbcl} <<EOF
(let ((files (list $FILES)))
  (mapc #'load files)
  (mapc #'compile-file files))
(quit :unix-status 52)
EOF

${SBCL:-sbcl} <<EOF
(mapc #'load (list $FASLS))
(quit :unix-status 52)
EOF

if [ $? != 52 ]; then
    rm $FASLS
    echo undefined-classoid-bug test failed: $?
    exit 1 # Failure
fi

# success convention for script
exit 104
