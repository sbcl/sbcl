SBCL=src/runtime/sbcl
ARGS=--core output/sbcl.core --noinform --disable-debugger --noprint --no-userinit --no-sysinit
SCRIPT1=crossbuild-runner/pass-1.lisp
SCRIPT2=crossbuild-runner/pass-2.lisp
DEPS1=crossbuild-runner/pass-1.lisp src/cold/build-order.lisp-expr

obj/xbuild/arm/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) arm arm "(:LITTLE-ENDIAN :LARGEFILE)" < $(SCRIPT1)
obj/xbuild/arm.core: obj/xbuild/arm/xc.core
	$(SBCL) $(ARGS) arm < $(SCRIPT2)

obj/xbuild/arm64/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) arm64 arm64 "(:LITTLE-ENDIAN :SB-THREAD)" < $(SCRIPT1)
obj/xbuild/arm64.core: obj/xbuild/arm64/xc.core
	$(SBCL) $(ARGS) arm64 < $(SCRIPT2)

obj/xbuild/arm64-darwin/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) arm64-darwin arm64 "(:LITTLE-ENDIAN :SB-THREAD :DARWIN-JIT)" < $(SCRIPT1)
obj/xbuild/arm64-darwin.core: obj/xbuild/arm64-darwin/xc.core
	$(SBCL) $(ARGS) arm64-darwin < $(SCRIPT2)

obj/xbuild/mips/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) mips mips "(:LARGEFILE :LITTLE-ENDIAN)" < $(SCRIPT1)
obj/xbuild/mips.core: obj/xbuild/mips/xc.core
	$(SBCL) $(ARGS) mips < $(SCRIPT2)

obj/xbuild/ppc/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) ppc ppc "(:BIG-ENDIAN)" < $(SCRIPT1)
obj/xbuild/ppc.core: obj/xbuild/ppc/xc.core
	$(SBCL) $(ARGS) ppc < $(SCRIPT2)

obj/xbuild/ppc-thread/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) ppc-thread ppc "(:BIG-ENDIAN :SB-THREAD)" < $(SCRIPT1)
obj/xbuild/ppc-thread.core: obj/xbuild/ppc-thread/xc.core
	$(SBCL) $(ARGS) ppc-thread < $(SCRIPT2)

obj/xbuild/ppc64/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) ppc64 ppc64 "(:PPC64 :BIG-ENDIAN)" < $(SCRIPT1)
obj/xbuild/ppc64.core: obj/xbuild/ppc64/xc.core
	$(SBCL) $(ARGS) ppc64 < $(SCRIPT2)

obj/xbuild/riscv/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) riscv riscv "(:64-BIT :LITTLE-ENDIAN :SB-THREAD)" < $(SCRIPT1)
obj/xbuild/riscv.core: obj/xbuild/riscv/xc.core
	$(SBCL) $(ARGS) riscv < $(SCRIPT2)

obj/xbuild/sparc/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) sparc sparc "(:BIG-ENDIAN)" < $(SCRIPT1)
obj/xbuild/sparc.core: obj/xbuild/sparc/xc.core
	$(SBCL) $(ARGS) sparc < $(SCRIPT2)

obj/xbuild/x86/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86 x86 "(:LITTLE-ENDIAN :LARGEFILE)" < $(SCRIPT1)
obj/xbuild/x86.core: obj/xbuild/x86/xc.core
	$(SBCL) $(ARGS) x86 < $(SCRIPT2)

obj/xbuild/x86-thread/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-thread x86 "(:LITTLE-ENDIAN :LARGEFILE :SB-THREAD)" < $(SCRIPT1)
obj/xbuild/x86-thread.core: obj/xbuild/x86-thread/xc.core
	$(SBCL) $(ARGS) x86-thread < $(SCRIPT2)

obj/xbuild/x86-64/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-64 x86-64 "(:LITTLE-ENDIAN :AVX2 :GENCGC :SB-SIMD-PACK :SB-SIMD-PACK-256)" < $(SCRIPT1)
obj/xbuild/x86-64.core: obj/xbuild/x86-64/xc.core
	$(SBCL) $(ARGS) x86-64 < $(SCRIPT2)

obj/xbuild/x86-64-imm/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-64-imm x86-64 "(:LITTLE-ENDIAN :AVX2 :GENCGC :SB-SIMD-PACK :SB-SIMD-PACK-256 :IMMOBILE-SPACE)" < $(SCRIPT1)
obj/xbuild/x86-64-imm.core: obj/xbuild/x86-64-imm/xc.core
	$(SBCL) $(ARGS) x86-64-imm < $(SCRIPT2)

obj/xbuild/x86-64-permgen/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-64-permgen x86-64 "(:LITTLE-ENDIAN :AVX2 :GENCGC :SB-SIMD-PACK :SB-SIMD-PACK-256 :PERMGEN)" < $(SCRIPT1)
obj/xbuild/x86-64-permgen.core: obj/xbuild/x86-64-permgen/xc.core
	$(SBCL) $(ARGS) x86-64-permgen < $(SCRIPT2)
