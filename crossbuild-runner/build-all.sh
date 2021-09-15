#!/bin/sh

set -e
# This script just checks that it's possible to build and run
# the cross-compiler for all backends using a predetermined
# 'local-target-features' for each.
# There is no dependence on the C runtime,
# since we don't assume the presence of either a C cross-compiler
# or a host machine on which to run its native compiler.

if [ -z "$*" ]; then
    targets="arm arm64 mips ppc ppc64 riscv sparc x86 x86-64"
else
    targets="$@"
fi

for arch in $targets
do
  echo TESTING $arch
  ltf=local-target-features.lisp-expr
  # Whether any of the :OS-PROVIDES-* features are present is mostly immaterial
  # to this cross-build test. Some of the provisions are only for C code which
  # we don't compile. Or else it doesn't matter much which lisp code is compiled.
  # Assume that dlopen() is provided so that we don't try to read sbcl.nm though.
  echo '(lambda (features) (union features (list :os-provides-dlopen ' > $ltf
  echo ":$arch" >> $ltf
  # x86 and x86-64 are tested as if #+win32. Unix is otherwise plenty tested.
  if [ $arch = x86 -o $arch = x86-64 ]; then
    echo ':win32 :sb-thread :sb-safepoint' >> $ltf
  else
    echo ':unix :linux :elf' >> $ltf
  fi
  cat crossbuild-runner/backends/$arch/features >> $ltf
  cat crossbuild-runner/backends/$arch/local-target-features >> $ltf
  echo ')))' >> $ltf

  cp -fv crossbuild-runner/backends/$arch/stuff-groveled-from-headers.lisp \
         output/stuff-groveled-from-headers.lisp
  sh make-host-1.sh
  sh make-host-2.sh
done
