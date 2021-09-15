all: output/arm.core output/arm64.core output/mips.core output/ppc.core output/ppc64.core \
 output/riscv.core output/sparc.core output/x86.core output/x86-64.core

SBCL=src/runtime/sbcl
ARGS=--core output/sbcl.core --noinform --disable-debugger --noprint --no-userinit --no-sysinit
SCRIPT1=crossbuild-runner/pass-1.lisp
SCRIPT2=crossbuild-runner/pass-2.lisp
DEPS1=crossbuild-runner/pass-1.lisp src/cold/build-order.lisp-expr # FIXME: and all the sources, of course

obj/xbuild/arm/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) arm < $(SCRIPT1)
output/arm.core: obj/xbuild/arm/xc.core
	$(SBCL) $(ARGS) arm < $(SCRIPT2)

obj/xbuild/arm64/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) arm64 < $(SCRIPT1)
output/arm64.core: obj/xbuild/arm64/xc.core
	$(SBCL) $(ARGS) arm64 < $(SCRIPT2)

obj/xbuild/mips/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) mips < $(SCRIPT1)
output/mips.core: obj/xbuild/mips/xc.core
	$(SBCL) $(ARGS) mips < $(SCRIPT2)

obj/xbuild/ppc/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) ppc < $(SCRIPT1)
output/ppc.core: obj/xbuild/ppc/xc.core
	$(SBCL) $(ARGS) ppc < $(SCRIPT2)

obj/xbuild/ppc64/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) ppc64 < $(SCRIPT1)
output/ppc64.core: obj/xbuild/ppc64/xc.core
	$(SBCL) $(ARGS) ppc64 < $(SCRIPT2)

obj/xbuild/riscv/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) riscv < $(SCRIPT1)
output/riscv.core: obj/xbuild/riscv/xc.core
	$(SBCL) $(ARGS) riscv < $(SCRIPT2)

obj/xbuild/sparc/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) sparc < $(SCRIPT1)
output/sparc.core: obj/xbuild/sparc/xc.core
	$(SBCL) $(ARGS) sparc < $(SCRIPT2)

obj/xbuild/x86/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86 < $(SCRIPT1)
output/x86.core: obj/xbuild/x86/xc.core
	$(SBCL) $(ARGS) x86 < $(SCRIPT2)

obj/xbuild/x86-64/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-64 < $(SCRIPT1)
output/x86-64.core: obj/xbuild/x86-64/xc.core
	$(SBCL) $(ARGS) x86-64 < $(SCRIPT2)
