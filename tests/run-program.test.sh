#!/bin/sh

# tests related to SB-EXT:RUN-PROGRAM

# This software is part of the SBCL system. See the README file for
# more information.
#
# While most of SBCL is derived from the CMU CL system, the test
# files (like this one) were written from scratch after the fork
# from CMU CL.
# 
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for
# more information.

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
