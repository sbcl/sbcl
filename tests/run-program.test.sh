#!/bin/sh

# tests related to SB-EXT:RUN-PROGRAM

sbcl --noinform --noprint --sysinit /dev/null --userinit /dev/null <<EOF
  (let ((string (with-output-to-string (stream)
                  (sb-ext:run-program "/bin/echo"
                                      '("foo" "bar")
                                      :output stream))))
    (when (string= string "foo bar
")
      (sb-ext:quit :unix-status 52)))
EOF
if [ $? != 52 ]; then
    echo test failed: $?
    exit 1
fi

# known bugs:
#
#   sbcl-0.6.8:
#
#     (SB-EXT:RUN-PROGRAM "echo" NIL)
#     => error in function SB-IMPL::%ENUMERATE-SEARCH-LIST:
#        Undefined search list: path
#
#     (SB-EXT:RUN-PROGRAM "/bin/uname" '("-a") :OUTPUT :STREAM)
#     doesn't return a STREAM (the way doc string claims)

# success convention
exit 104
