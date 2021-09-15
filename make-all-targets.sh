#!/bin/sh

mkdir -p obj/xbuild

make -k -j9 -f crossbuild-runner/Makefile \
     output/arm.core output/arm64.core \
     output/mips.core \
     output/ppc.core output/ppc64.core \
     output/riscv.core \
     output/sparc.core \
     output/x86.core output/x86-64.core
