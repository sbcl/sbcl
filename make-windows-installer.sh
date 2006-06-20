#!/bin/sh

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

WIX_PATH="${WIX_PATH:-$PROGRAMFILES\WiX}"

. ./sbcl-pwd.sh
sbcl_pwd

cd output

"$SBCL_PWD/src/runtime/sbcl" --noinform --core "$SBCL_PWD/output/sbcl.core" \
  --disable-debugger --no-sysinit --no-userinit \
  --load ../tools-for-build/rtf.lisp \
  --load ../tools-for-build/wxs.lisp \
  --eval '(progn 
            (write-rtf (read-text "../COPYING") "License.rtf")
            (write-wxs "sbcl.wxs")
            (with-open-file (f "version.txt" 
                               :direction :output
                               :if-exists :supersede)
             (write-line (lisp-implementation-version) f))
            (quit))'

"$WIX_PATH/candle" sbcl.wxs
"$WIX_PATH/light" sbcl.wixobj "$WIX_PATH/wixui.wixlib" \
   -loc "$WIX_PATH/WixUI_en-us.wxl" \
   -out sbcl-`cat version.txt`.msi

