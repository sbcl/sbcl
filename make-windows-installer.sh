#!/bin/sh

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

WIX_PATH=$WIX/bin

SBCL_TOP=../
cd ./output

"$SBCL_TOP/src/runtime/sbcl" --noinform --core "$SBCL_TOP/output/sbcl.core" \
  --disable-debugger --no-sysinit --no-userinit \
  --load ../tools-for-build/rtf.lisp \
  --load ../tools-for-build/wxs.lisp \
  --eval '(progn
            (write-rtf (read-text "../COPYING") "License.rtf")
            (write-wxs "sbcl.wxs")
            (with-open-file (f "version.txt"
                               :direction :output
                               :if-exists :supersede)
             (format f "~a-~a-windows-binary"
                       (lisp-implementation-version)
                       #+x86 "x86" #+x86-64 "x86-64"))
            (exit))'

"$WIX_PATH/candle" sbcl.wxs
"$WIX_PATH/light" sbcl.wixobj \
   -ext "$WIX_PATH/WixUIExtension.dll" -cultures:en-us \
   -out sbcl-`cat version.txt`.msi
