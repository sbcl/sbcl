#!/bin/sh

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

if test -n "$WIX_PATH"; then
    :
elif test -d "$PROGRAMFILES/WiX Toolset v3.7"; then
    WIX_PATH="$PROGRAMFILES/WiX Toolset v3.7/bin"
elif test -d "$PROGRAMFILES/Windows Installer XML v3.5"; then
    WIX_PATH="$PROGRAMFILES/Windows Installer XML v3.5/bin"
fi
echo "using $WIX_PATH"

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
             (format f "~a-~a-windows-binary"
                       (lisp-implementation-version)
                       #+x86 "x86" #+x86-64 "x86-64"))
            (exit))'

"$WIX_PATH/candle" sbcl.wxs
"$WIX_PATH/light" sbcl.wixobj \
   -ext "$WIX_PATH/WixUIExtension.dll" -cultures:en-us \
   -out sbcl-`cat version.txt`.msi
