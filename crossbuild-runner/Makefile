SBCL=src/runtime/sbcl
ARGS=--core output/sbcl.core --noinform --disable-debugger --noprint --no-userinit --no-sysinit
SCRIPT1=crossbuild-runner/pass-1.lisp
SCRIPT2=crossbuild-runner/pass-2.lisp
DEPS1=crossbuild-runner/pass-1.lisp src/cold/build-order.lisp-expr

obj/xbuild/arm/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) arm arm "(:UNIX :LINUX :ELF :LITTLE-ENDIAN :LARGEFILE)" < $(SCRIPT1)
obj/xbuild/arm.core: obj/xbuild/arm/xc.core
	$(SBCL) $(ARGS) arm < $(SCRIPT2)

obj/xbuild/arm64/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) arm64 arm64 "(:UNIX :LINUX :ELF :LITTLE-ENDIAN :SB-THREAD)" < $(SCRIPT1)
obj/xbuild/arm64.core: obj/xbuild/arm64/xc.core
	$(SBCL) $(ARGS) arm64 < $(SCRIPT2)

obj/xbuild/arm64-darwin/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) arm64-darwin arm64 "(:DARWIN :BSD :UNIX :MACH-O :LITTLE-ENDIAN :SB-THREAD :DARWIN-JIT)" < $(SCRIPT1)
obj/xbuild/arm64-darwin.core: obj/xbuild/arm64-darwin/xc.core
	$(SBCL) $(ARGS) arm64-darwin < $(SCRIPT2)

obj/xbuild/arm64-reloc/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) arm64-reloc arm64 "(:UNIX :LINUX :ELF :LITTLE-ENDIAN :SB-THREAD :RELOCATABLE-STATIC-SPACE)" < $(SCRIPT1)
obj/xbuild/arm64-reloc.core: obj/xbuild/arm64-reloc/xc.core
	$(SBCL) $(ARGS) arm64-reloc < $(SCRIPT2)

obj/xbuild/arm64-immobile-space/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) arm64-immobile-space arm64 "(:UNIX :LINUX :ELF :LITTLE-ENDIAN :SB-THREAD :IMMOBILE-SPACE)" < $(SCRIPT1)
obj/xbuild/arm64-immobile-space.core: obj/xbuild/arm64-immobile-space/xc.core
	$(SBCL) $(ARGS) arm64-immobile-space < $(SCRIPT2)

obj/xbuild/mips/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) mips mips "(:UNIX :LINUX :ELF :LARGEFILE :LITTLE-ENDIAN)" < $(SCRIPT1)
obj/xbuild/mips.core: obj/xbuild/mips/xc.core
	$(SBCL) $(ARGS) mips < $(SCRIPT2)

obj/xbuild/ppc/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) ppc ppc "(:UNIX :LINUX :ELF :BIG-ENDIAN)" < $(SCRIPT1)
obj/xbuild/ppc.core: obj/xbuild/ppc/xc.core
	$(SBCL) $(ARGS) ppc < $(SCRIPT2)

obj/xbuild/ppc-thread/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) ppc-thread ppc "(:UNIX :LINUX :ELF :BIG-ENDIAN :SB-THREAD)" < $(SCRIPT1)
obj/xbuild/ppc-thread.core: obj/xbuild/ppc-thread/xc.core
	$(SBCL) $(ARGS) ppc-thread < $(SCRIPT2)

obj/xbuild/ppc64/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) ppc64 ppc64 "(:UNIX :LINUX :ELF :PPC64 :BIG-ENDIAN)" < $(SCRIPT1)
obj/xbuild/ppc64.core: obj/xbuild/ppc64/xc.core
	$(SBCL) $(ARGS) ppc64 < $(SCRIPT2)

obj/xbuild/riscv/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) riscv riscv "(:UNIX :LINUX :ELF :64-BIT :LITTLE-ENDIAN :SB-THREAD)" < $(SCRIPT1)
obj/xbuild/riscv.core: obj/xbuild/riscv/xc.core
	$(SBCL) $(ARGS) riscv < $(SCRIPT2)

obj/xbuild/sparc/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) sparc sparc "(:UNIX :LINUX :ELF :BIG-ENDIAN)" < $(SCRIPT1)
obj/xbuild/sparc.core: obj/xbuild/sparc/xc.core
	$(SBCL) $(ARGS) sparc < $(SCRIPT2)

obj/xbuild/sparc-sunos/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) sparc-sunos sparc "(:BIG-ENDIAN :UNIX :SUNOS :ELF)" < $(SCRIPT1)
obj/xbuild/sparc-sunos.core: obj/xbuild/sparc-sunos/xc.core
	$(SBCL) $(ARGS) sparc-sunos < $(SCRIPT2)

obj/xbuild/x86/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86 x86 "(:WIN32 :SB-THREAD :SB-SAFEPOINT :LITTLE-ENDIAN :LARGEFILE (NOT :SB-EVAL) :SB-FASTEVAL)" < $(SCRIPT1)
obj/xbuild/x86.core: obj/xbuild/x86/xc.core
	$(SBCL) $(ARGS) x86 < $(SCRIPT2)

obj/xbuild/x86-ascii/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-ascii x86 "(:WIN32 :SB-THREAD :SB-SAFEPOINT :LITTLE-ENDIAN :LARGEFILE (NOT :SB-UNICODE))" < $(SCRIPT1)
obj/xbuild/x86-ascii.core: obj/xbuild/x86-ascii/xc.core
	$(SBCL) $(ARGS) x86-ascii < $(SCRIPT2)

obj/xbuild/x86-thread/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-thread x86 "(:WIN32 :SB-THREAD :SB-SAFEPOINT :LITTLE-ENDIAN :LARGEFILE :SB-THREAD)" < $(SCRIPT1)
obj/xbuild/x86-thread.core: obj/xbuild/x86-thread/xc.core
	$(SBCL) $(ARGS) x86-thread < $(SCRIPT2)

obj/xbuild/x86-linux/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-linux x86 "(:LITTLE-ENDIAN :LARGEFILE :SB-THREAD :LINUX :UNIX :ELF :SB-THREAD)" < $(SCRIPT1)
obj/xbuild/x86-linux.core: obj/xbuild/x86-linux/xc.core
	$(SBCL) $(ARGS) x86-linux < $(SCRIPT2)

obj/xbuild/x86-64/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-64 x86-64 "(:WIN32 :SB-THREAD :SB-SAFEPOINT :LITTLE-ENDIAN :AVX2 :GENCGC :SB-SIMD-PACK :SB-SIMD-PACK-256)" < $(SCRIPT1)
obj/xbuild/x86-64.core: obj/xbuild/x86-64/xc.core
	$(SBCL) $(ARGS) x86-64 < $(SCRIPT2)

obj/xbuild/x86-64-linux/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-64-linux x86-64 "(:LINUX :UNIX :ELF :LITTLE-ENDIAN :AVX2 :GENCGC :SB-SIMD-PACK :SB-SIMD-PACK-256 (NOT :SB-EVAL) :SB-FASTEVAL)" < $(SCRIPT1)
obj/xbuild/x86-64-linux.core: obj/xbuild/x86-64-linux/xc.core
	$(SBCL) $(ARGS) x86-64-linux < $(SCRIPT2)

obj/xbuild/x86-64-darwin/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-64-darwin x86-64 "(:DARWIN :BSD :UNIX :MACH-O :LITTLE-ENDIAN :AVX2 :GENCGC :SB-SIMD-PACK :SB-SIMD-PACK-256)" < $(SCRIPT1)
obj/xbuild/x86-64-darwin.core: obj/xbuild/x86-64-darwin/xc.core
	$(SBCL) $(ARGS) x86-64-darwin < $(SCRIPT2)

obj/xbuild/x86-64-imm/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-64-imm x86-64 "(:WIN32 :SB-THREAD :SB-SAFEPOINT :LITTLE-ENDIAN :AVX2 :GENCGC :SB-SIMD-PACK :SB-SIMD-PACK-256 :IMMOBILE-SPACE)" < $(SCRIPT1)
obj/xbuild/x86-64-imm.core: obj/xbuild/x86-64-imm/xc.core
	$(SBCL) $(ARGS) x86-64-imm < $(SCRIPT2)

obj/xbuild/x86-64-permgen/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) x86-64-permgen x86-64 "(:WIN32 :SB-THREAD :SB-SAFEPOINT :LITTLE-ENDIAN :AVX2 :GENCGC :SB-SIMD-PACK :SB-SIMD-PACK-256 :PERMGEN)" < $(SCRIPT1)
obj/xbuild/x86-64-permgen.core: obj/xbuild/x86-64-permgen/xc.core
	$(SBCL) $(ARGS) x86-64-permgen < $(SCRIPT2)
