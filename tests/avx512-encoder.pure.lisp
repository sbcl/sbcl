;;;; Potentially side-effectful tests of the simd-pack infrastructure.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

#-sb-simd-pack-512 (invoke-restart 'run-tests::skip-file)

(when (zerop (sb-alien:extern-alien "avx512_supported" int))
  (format t "~&INFO: simd-pack-512 not supported")
  (invoke-restart 'run-tests::skip-file))

;; Like simd-pack-512.pure.lisp's own %TEST-* functions: these need to
;; live directly in SB-VM (not this pure.lisp file's default throwaway
;; package) to see INLINE-VOP, INST, and the unqualified register/
;; mnemonic names unqualified. Unlike that file, DEFINE-EVEX-DISASM-TEST
;; calls stay interleaved with their DEFUNs here rather than being
;; split into a separate TEST-UTIL-package section at the end, so each
;; call is qualified as TEST-UTIL::DEFINE-EVEX-DISASM-TEST instead.
(cl:in-package "SB-VM")

;; REG-TN builds a register TN for a given storage-class name and
;; offset - the same way SBCL's own predefined GPR TNs (RAX-TN and
;; friends, in src/compiler/x86-64/vm.lisp) are built, just done at
;; runtime here since these tests need registers from every SC
;; (XMM/YMM/ZMM under all three type interpretations, plus K
;; registers), not only the ones already predefined.
(declaim (inline reg-tn))

(defun reg-tn (sc-name offset)
  (make-random-tn (sc-or-lose sc-name) offset))

;; ASSEMBLE-AND-DISASSEMBLE hands INSTRUCTIONS (a list of raw (mnemonic
;; operand...) forms, as SB-ASSEM:INST* would take them) straight to the
;; assembler and disassembles the resulting bytes back out.
;; assembler-and-disassembler-only mechanism as ASSEMBLER.PURE.LISP's
;; TEST-ASSEMBLE (test-util:assemble, an existing exported utility, does the raw
;; assembly part). Returns the disassembly text.
(defun assemble-and-disassemble (instructions)
  (let ((buf (test-util:assemble instructions)))
    (with-output-to-string (s)
      (let ((sb-disassem:*disassem-location-column-width* 0))
        (sb-sys:with-pinned-objects (buf)
          (sb-disassem:disassemble-memory
           (sb-sys:sap-int (sb-sys:vector-sap buf))
           (length buf)
           :stream s))))))

(defmacro assemble-instructions (&body instructions)
  (labels ((expand-operand (op)
             (cond
               ((and (consp op) (eq (car op) 'ea))
                `(ea ,@(mapcar (lambda (x) 
                                 (if (consp x) 
                                     `(reg-tn ',(car x) ,(cadr x)) 
                                     x)) 
                               (cdr op))))
               ((consp op)
                `(reg-tn ',(car op) ,(cadr op)))
               (t op))))
    `(assemble-and-disassemble
      (list ,@(loop for inst in instructions
                    collect `(list ',(car inst)
                                   ,@(mapcar #'expand-operand (cdr inst))))))))

;; XMM-N/YMM-N/ZMM-N of the same N alias the same physical register, so
;; testing all three widths in one function needs three DISJOINT
;; offset ranges - reusing 1-3 for all three (as if e.g. XMM1 and
;; YMM1 were independent) makes the register allocator see two live
;; temporaries pinned to the same physical register and abort with
;; "is wired to location 1 ... that it conflicts with".
(defun %test-vnni-basic ()
  (assemble-instructions
   (vpdpbusd (single-reg 1) (single-reg 2) (single-reg 3))
   (vpdpbusds (single-reg 1) (single-reg 2) (single-reg 3))
   (vpdpwssd (single-reg 1) (single-reg 2) (single-reg 3))
   (vpdpwssds (single-reg 1) (single-reg 2) (single-reg 3))
   (vpdpbusd (int-avx2-reg 4) (int-avx2-reg 5) (int-avx2-reg 6))
   (vpdpbusds (int-avx2-reg 4) (int-avx2-reg 5) (int-avx2-reg 6))
   (vpdpwssd (int-avx2-reg 4) (int-avx2-reg 5) (int-avx2-reg 6))
   (vpdpwssds (int-avx2-reg 4) (int-avx2-reg 5) (int-avx2-reg 6))
   (vpdpbusd (int-avx512-reg 7) (int-avx512-reg 8) (int-avx512-reg 9))
   (vpdpbusds (int-avx512-reg 7) (int-avx512-reg 8) (int-avx512-reg 9))
   (vpdpwssd (int-avx512-reg 7) (int-avx512-reg 8) (int-avx512-reg 9))
   (vpdpwssds (int-avx512-reg 7) (int-avx512-reg 8) (int-avx512-reg 9))))

(test-util::define-evex-disasm-test :evex-vnni-basic %test-vnni-basic
  ("VPDPBUSD" "XMM1" "XMM2" "XMM3")
  ("VPDPBUSDS" "XMM1" "XMM2" "XMM3")
  ("VPDPWSSD" "XMM1" "XMM2" "XMM3")
  ("VPDPWSSDS" "XMM1" "XMM2" "XMM3")
  ("VPDPBUSD" "YMM4" "YMM5" "YMM6")
  ("VPDPBUSDS" "YMM4" "YMM5" "YMM6")
  ("VPDPWSSD" "YMM4" "YMM5" "YMM6")
  ("VPDPWSSDS" "YMM4" "YMM5" "YMM6")
  ("VPDPBUSD" "ZMM7" "ZMM8" "ZMM9")
  ("VPDPBUSDS" "ZMM7" "ZMM8" "ZMM9")
  ("VPDPWSSD" "ZMM7" "ZMM8" "ZMM9")
  ("VPDPWSSDS" "ZMM7" "ZMM8" "ZMM9"))

(defun %test-vnni-memory ()
  (assemble-instructions
   (vpdpbusd (int-avx512-reg 1) (int-avx512-reg 2) (ea (unsigned-reg rax-offset)))
   (vpdpbusds (int-avx512-reg 1) (int-avx512-reg 2) (ea 64 (unsigned-reg rax-offset)))
   (vpdpwssd (int-avx512-reg 1) (int-avx512-reg 2) (ea 4096 (unsigned-reg rax-offset)))
   (vpdpwssds (int-avx512-reg 1) (int-avx512-reg 2) (ea 65 (unsigned-reg rax-offset)))))

(test-util::define-evex-disasm-test :evex-vnni-memory %test-vnni-memory
  ("VPDPBUSD" "ZMM1" "ZMM2" "[RAX]")
  ("VPDPBUSDS" "ZMM1" "ZMM2" "[RAX+64]")
  ("VPDPWSSD" "ZMM1" "ZMM2" "[RAX+4096]")
  ("VPDPWSSDS" "ZMM1" "ZMM2" "[RAX+65]"))

(defun %test-vnni-masked ()
  (assemble-instructions
   (vpdpbusd-masked (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3) (mask-reg 1))
   (vpdpbusds-masked (int-avx512-reg 1) (int-avx512-reg 2) (ea 64 (unsigned-reg rax-offset)) (mask-reg 1))
   (vpdpwssd-masked (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3) (mask-reg 1) :z)
   (vpdpwssds-masked (int-avx512-reg 1) (int-avx512-reg 2) (ea 64 (unsigned-reg rax-offset)) (mask-reg 1) :z)))

(test-util::define-evex-disasm-test :evex-vnni-masked %test-vnni-masked
  ("VPDPBUSD" "ZMM1" "{K1}" "ZMM2" "ZMM3")
  ("VPDPBUSDS" "ZMM1" "{K1}" "ZMM2" "[RAX+64]")
  ("VPDPWSSD" "ZMM1" "{K1}" "{z}" "ZMM2" "ZMM3")
  ("VPDPWSSDS" "ZMM1" "{K1}" "{z}" "ZMM2" "[RAX+64]"))

;; Disjoint offset ranges across XMM/YMM/ZMM -- see %TEST-VNNI-BASIC's
;; comment for why (same-numbered XMM/YMM/ZMM alias one register).
(defun %test-bw-basic ()
  (assemble-instructions
   (vpaddb (single-reg 1) (single-reg 2) (single-reg 3))
   (vpaddw (single-reg 1) (single-reg 2) (single-reg 3))
   (vpsubb (single-reg 1) (single-reg 2) (single-reg 3))
   (vpsubw (single-reg 1) (single-reg 2) (single-reg 3))
   (vpaddb (int-avx2-reg 4) (int-avx2-reg 5) (int-avx2-reg 6))
   (vpaddw (int-avx2-reg 4) (int-avx2-reg 5) (int-avx2-reg 6))
   (vpsubb (int-avx2-reg 4) (int-avx2-reg 5) (int-avx2-reg 6))
   (vpsubw (int-avx2-reg 4) (int-avx2-reg 5) (int-avx2-reg 6))
   (vpaddb (int-avx512-reg 7) (int-avx512-reg 8) (int-avx512-reg 9))
   (vpaddw (int-avx512-reg 7) (int-avx512-reg 8) (int-avx512-reg 9))
   (vpsubb (int-avx512-reg 7) (int-avx512-reg 8) (int-avx512-reg 9))
   (vpsubw (int-avx512-reg 7) (int-avx512-reg 8) (int-avx512-reg 9))))

(test-util::define-evex-disasm-test :evex-bw-basic %test-bw-basic
  ("VPADDB" "XMM1" "XMM2" "XMM3")
  ("VPADDW" "XMM1" "XMM2" "XMM3")
  ("VPSUBB" "XMM1" "XMM2" "XMM3")
  ("VPSUBW" "XMM1" "XMM2" "XMM3")
  ("VPADDB" "YMM4" "YMM5" "YMM6")
  ("VPADDW" "YMM4" "YMM5" "YMM6")
  ("VPSUBB" "YMM4" "YMM5" "YMM6")
  ("VPSUBW" "YMM4" "YMM5" "YMM6")
  ("VPADDB" "ZMM7" "ZMM8" "ZMM9")
  ("VPADDW" "ZMM7" "ZMM8" "ZMM9")
  ("VPSUBB" "ZMM7" "ZMM8" "ZMM9")
  ("VPSUBW" "ZMM7" "ZMM8" "ZMM9"))

(defun %test-bw-memory ()
  (assemble-instructions
   (vpaddb (int-avx512-reg 1) (int-avx512-reg 2) (ea (unsigned-reg rax-offset)))
   (vpaddw (int-avx512-reg 1) (int-avx512-reg 2) (ea 64 (unsigned-reg rax-offset)))
   (vpsubb (int-avx512-reg 1) (int-avx512-reg 2) (ea 4096 (unsigned-reg rax-offset)))
   (vpsubw (int-avx512-reg 1) (int-avx512-reg 2) (ea 65 (unsigned-reg rax-offset)))))

(test-util::define-evex-disasm-test :evex-bw-memory %test-bw-memory
  ("VPADDB" "ZMM1" "ZMM2" "[RAX]")
  ("VPADDW" "ZMM1" "ZMM2" "[RAX+64]")
  ("VPSUBB" "ZMM1" "ZMM2" "[RAX+4096]")
  ("VPSUBW" "ZMM1" "ZMM2" "[RAX+65]"))

(defun %test-bw-masked ()
  (assemble-instructions
   (vpaddb-masked (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3) (mask-reg 1))
   (vpaddw-masked (int-avx512-reg 1) (int-avx512-reg 2) (ea 64 (unsigned-reg rax-offset)) (mask-reg 1))
   (vpsubb-masked (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3) (mask-reg 1) :z)
   (vpsubw-masked (int-avx512-reg 1) (int-avx512-reg 2) (ea 64 (unsigned-reg rax-offset)) (mask-reg 1) :z)))

(test-util::define-evex-disasm-test :evex-bw-masked %test-bw-masked
  ("VPADDB" "ZMM1" "{K1}" "ZMM2" "ZMM3")
  ("VPADDW" "ZMM1" "{K1}" "ZMM2" "[RAX+64]")
  ("VPSUBB" "ZMM1" "{K1}" "{z}" "ZMM2" "ZMM3")
  ("VPSUBW" "ZMM1" "{K1}" "{z}" "ZMM2" "[RAX+64]"))

;; 1. ARITHMETIC (Float & Integer)
(defun %test-avx512f-arithmetic ()
  (assemble-instructions
   (vaddps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vaddpd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vsubps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vsubpd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vmulps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vmulpd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vdivps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vdivpd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vsqrtps (int-avx512-reg 1) (int-avx512-reg 2))
   (vsqrtpd (int-avx512-reg 1) (int-avx512-reg 2))
   (vpaddd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpaddq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpsubd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpsubq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpmulld (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpmuludq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))))

(test-util::define-evex-disasm-test :evex-avx512f-arithmetic %test-avx512f-arithmetic
  ("VADDPS" "ZMM1" "ZMM2" "ZMM3")
  ("VADDPD" "ZMM1" "ZMM2" "ZMM3")
  ("VSUBPS" "ZMM1" "ZMM2" "ZMM3")
  ("VSUBPD" "ZMM1" "ZMM2" "ZMM3")
  ("VMULPS" "ZMM1" "ZMM2" "ZMM3")
  ("VMULPD" "ZMM1" "ZMM2" "ZMM3")
  ("VDIVPS" "ZMM1" "ZMM2" "ZMM3")
  ("VDIVPD" "ZMM1" "ZMM2" "ZMM3")
  ("VSQRTPS" "ZMM1" "ZMM2")
  ("VSQRTPD" "ZMM1" "ZMM2")
  ("VPADDD" "ZMM1" "ZMM2" "ZMM3")
  ("VPADDQ" "ZMM1" "ZMM2" "ZMM3")
  ("VPSUBD" "ZMM1" "ZMM2" "ZMM3")
  ("VPSUBQ" "ZMM1" "ZMM2" "ZMM3")
  ("VPMULLD" "ZMM1" "ZMM2" "ZMM3")
  ("VPMULUDQ" "ZMM1" "ZMM2" "ZMM3"))

;; 2. LOGICAL (Bitwise)
(defun %test-avx512f-logical ()
  (assemble-instructions
   (vandps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vandpd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vandnps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vandnpd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vorps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vorpd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vxorps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vxorpd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpandd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpandq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpandnd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpandnq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpord (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vporq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpxord (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpxorq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))))

(test-util::define-evex-disasm-test :evex-avx512f-logical %test-avx512f-logical
  ("VANDPS" "ZMM1" "ZMM2" "ZMM3")
  ("VANDPD" "ZMM1" "ZMM2" "ZMM3")
  ("VANDNPS" "ZMM1" "ZMM2" "ZMM3")
  ("VANDNPD" "ZMM1" "ZMM2" "ZMM3")
  ("VORPS" "ZMM1" "ZMM2" "ZMM3")
  ("VORPD" "ZMM1" "ZMM2" "ZMM3")
  ("VXORPS" "ZMM1" "ZMM2" "ZMM3")
  ("VXORPD" "ZMM1" "ZMM2" "ZMM3")
  ("VPANDD" "ZMM1" "ZMM2" "ZMM3")
  ("VPANDQ" "ZMM1" "ZMM2" "ZMM3")
  ("VPANDND" "ZMM1" "ZMM2" "ZMM3")
  ("VPANDNQ" "ZMM1" "ZMM2" "ZMM3")
  ("VPORD" "ZMM1" "ZMM2" "ZMM3")
  ("VPORQ" "ZMM1" "ZMM2" "ZMM3")
  ("VPXORD" "ZMM1" "ZMM2" "ZMM3")
  ("VPXORQ" "ZMM1" "ZMM2" "ZMM3"))

;; 3. FUSED MULTIPLY-ADD (FMA)
(defun %test-avx512f-fma ()
  (assemble-instructions
   (vfmadd132ps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vfmadd213ps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vfmadd231ps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vfmadd132pd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vfmadd213pd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vfmadd231pd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vfnmadd132ps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vfnmadd132pd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))))

(test-util::define-evex-disasm-test :evex-avx512f-fma %test-avx512f-fma
  ("VFMADD132PS" "ZMM1" "ZMM2" "ZMM3")
  ("VFMADD213PS" "ZMM1" "ZMM2" "ZMM3")
  ("VFMADD231PS" "ZMM1" "ZMM2" "ZMM3")
  ("VFMADD132PD" "ZMM1" "ZMM2" "ZMM3")
  ("VFMADD213PD" "ZMM1" "ZMM2" "ZMM3")
  ("VFMADD231PD" "ZMM1" "ZMM2" "ZMM3")
  ("VFNMADD132PS" "ZMM1" "ZMM2" "ZMM3")
  ("VFNMADD132PD" "ZMM1" "ZMM2" "ZMM3"))

;; 4. SHIFTS (Immediate and Vector)
(defun %test-avx512f-shift ()
  (assemble-instructions
   (vpslld (int-avx512-reg 1) (int-avx512-reg 2) 4)
   (vpsllq (int-avx512-reg 1) (int-avx512-reg 2) 4)
   (vpsrld (int-avx512-reg 1) (int-avx512-reg 2) 4)
   (vpsrlq (int-avx512-reg 1) (int-avx512-reg 2) 4)
   (vpsrad (int-avx512-reg 1) (int-avx512-reg 2) 4)
   (vpsraq (int-avx512-reg 1) (int-avx512-reg 2) 4)
   (vpsllvd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpsllvq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpsrlvd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpsrlvq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpsravd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpsravq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))))

(test-util::define-evex-disasm-test :evex-avx512f-shift %test-avx512f-shift
  ("VPSLLD" "ZMM1" "ZMM2" "4")
  ("VPSLLQ" "ZMM1" "ZMM2" "4")
  ("VPSRLD" "ZMM1" "ZMM2" "4")
  ("VPSRLQ" "ZMM1" "ZMM2" "4")
  ("VPSRAD" "ZMM1" "ZMM2" "4")
  ("VPSRAQ" "ZMM1" "ZMM2" "4")
  ("VPSLLVD" "ZMM1" "ZMM2" "ZMM3")
  ("VPSLLVQ" "ZMM1" "ZMM2" "ZMM3")
  ("VPSRLVD" "ZMM1" "ZMM2" "ZMM3")
  ("VPSRLVQ" "ZMM1" "ZMM2" "ZMM3")
  ("VPSRAVD" "ZMM1" "ZMM2" "ZMM3")
  ("VPSRAVQ" "ZMM1" "ZMM2" "ZMM3"))

;; 1. AVX-512CD (Conflict Detection)
(defun %test-avx512cd-basic ()
  (assemble-instructions
   (vpconflictd (int-avx512-reg 1) (int-avx512-reg 2))
   (vpconflictq (int-avx512-reg 1) (int-avx512-reg 2))
   (vplzcntd (int-avx512-reg 1) (int-avx512-reg 2))
   (vplzcntq (int-avx512-reg 1) (int-avx512-reg 2))))

(test-util::define-evex-disasm-test :evex-avx512cd-basic %test-avx512cd-basic
  ("VPCONFLICTD" "ZMM1" "ZMM2")
  ("VPCONFLICTQ" "ZMM1" "ZMM2")
  ("VPLZCNTD" "ZMM1" "ZMM2")
  ("VPLZCNTQ" "ZMM1" "ZMM2"))


;; 2. AVX-512DQ (Doubleword/Quadword)
(defun %test-avx512dq-basic ()
  (assemble-instructions
   (vpmullq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vrangeps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3) 0)
   (vrangepd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3) 0)
   (vreduceps (int-avx512-reg 1) (int-avx512-reg 2) 0)
   (vreducepd (int-avx512-reg 1) (int-avx512-reg 2) 0)
   (vfpclassps (mask-reg 1) (int-avx512-reg 2) 0)
   (vfpclasspd (mask-reg 1) (int-avx512-reg 2) 0)))

(test-util::define-evex-disasm-test :evex-avx512dq-basic %test-avx512dq-basic
  ("VPMULLQ" "ZMM1" "ZMM2" "ZMM3")
  ("VRANGEPS" "ZMM1" "ZMM2" "ZMM3" "0")
  ("VRANGEPD" "ZMM1" "ZMM2" "ZMM3" "0")
  ("VREDUCEPS" "ZMM1" "ZMM2" "0")
  ("VREDUCEPD" "ZMM1" "ZMM2" "0")
  ("VFPCLASSPS" "K1" "ZMM2" "0")
  ("VFPCLASSPD" "K1" "ZMM2" "0"))

;; 3. AVX-512VL (Vector Length - EVEX features on 128/256-bit)
;; Disjoint XMM/YMM offset ranges -- see %TEST-VNNI-BASIC's comment for
;; why (same-numbered XMM/YMM alias one register); K1 is a separate
;; register file (mask-reg) so it doesn't need to avoid 1-3 too.
(defun %test-avx512vl-masked ()
  (assemble-instructions
   (vaddps-masked (single-reg 1) (single-reg 2) (single-reg 3) (mask-reg 1) :z)
   (vsubps-masked (single-reg 1) (single-reg 2) (ea 16 (unsigned-reg rax-offset)) (mask-reg 1))
   (vaddpd-masked (int-avx2-reg 4) (int-avx2-reg 5) (int-avx2-reg 6) (mask-reg 1) :z)
   (vsubpd-masked (int-avx2-reg 4) (int-avx2-reg 5) (ea 32 (unsigned-reg rax-offset)) (mask-reg 1))))

(test-util::define-evex-disasm-test :evex-avx512vl-masked %test-avx512vl-masked
  ("VADDPS" "XMM1" "{K1}" "{z}" "XMM2" "XMM3")
  ("VSUBPS" "XMM1" "{K1}" "XMM2" "[RAX+16]")
  ("VADDPD" "YMM4" "{K1}" "{z}" "YMM5" "YMM6")
  ("VSUBPD" "YMM4" "{K1}" "YMM5" "[RAX+32]"))

(defun %test-avx512vbmi-basic ()
  (assemble-instructions
   (vpermb (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpermt2b (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpmultishiftqb (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))))

(test-util::define-evex-disasm-test :evex-avx512vbmi-basic %test-avx512vbmi-basic
  ("VPERMB" "ZMM1" "ZMM2" "ZMM3")
  ("VPERMT2B" "ZMM1" "ZMM2" "ZMM3")
  ("VPMULTISHIFTQB" "ZMM1" "ZMM2" "ZMM3"))

(defun %test-avx512vbmi2-basic ()
  (assemble-instructions
   (vpshldd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3) 4)
   (vpshrdd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3) 4)
   (vpshldvd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpshrdvd (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))))

(test-util::define-evex-disasm-test :evex-avx512vbmi2-basic %test-avx512vbmi2-basic
  ("VPSHLDD" "ZMM1" "ZMM2" "ZMM3" "4")
  ("VPSHRDD" "ZMM1" "ZMM2" "ZMM3" "4")
  ("VPSHLDVD" "ZMM1" "ZMM2" "ZMM3")
  ("VPSHRDVD" "ZMM1" "ZMM2" "ZMM3"))

(defun %test-avx512ifma-basic ()
  (assemble-instructions
   (vpmadd52luq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vpmadd52huq (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))))

(test-util::define-evex-disasm-test :evex-avx512ifma-basic %test-avx512ifma-basic
  ("VPMADD52LUQ" "ZMM1" "ZMM2" "ZMM3")
  ("VPMADD52HUQ" "ZMM1" "ZMM2" "ZMM3"))

(defun %test-avx512bitalg-basic ()
  (assemble-instructions
   (vpopcntb (int-avx512-reg 1) (int-avx512-reg 2))
   (vpopcntw (int-avx512-reg 1) (int-avx512-reg 2))
   (vpshufbitqmb (mask-reg 1) (int-avx512-reg 1) (int-avx512-reg 2))))

(test-util::define-evex-disasm-test :evex-avx512bitalg-basic %test-avx512bitalg-basic
  ("VPOPCNTB" "ZMM1" "ZMM2")
  ("VPOPCNTW" "ZMM1" "ZMM2")
  ("VPSHUFBITQMB" "K1" "ZMM1" "ZMM2"))

(defun %test-avx512bf16-basic ()
  (assemble-instructions
   (vdpbf16ps (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))
   (vcvtne2ps2bf16 (int-avx512-reg 1) (int-avx512-reg 2) (int-avx512-reg 3))))

(test-util::define-evex-disasm-test :evex-avx512bf16-basic %test-avx512bf16-basic
  ("VDPBF16PS" "ZMM1" "ZMM2" "ZMM3")
  ("VCVTNE2PS2BF16" "ZMM1" "ZMM2" "ZMM3"))

;; Below: assembler/disassembler-only encoding checks for AVX-512
;; instructions. These used to go through DEFINE-VOP + :TRANSLATE
;; (needing a DEFKNOWN stub too, so the compiler had something to
;; open-code in place of), purely so that DEFINE-EVEX-DISASM-TEST could
;; get a disassemblable compiled function out of it. None of these ever
;; checked a computed value or a compiler decision - every one is a
;; disassembly-text check, so they go straight through the assembler
;; and disassembler (ASSEMBLE-INSTRUCTIONS / REG-TN, defined above)
;; with no VOP, :TRANSLATE, DEFKNOWN, or register allocator involved at
;; all.

;; ---- KANDQ ----
(defun %test-kandq-disassembly ()
  (assemble-instructions
   (kandq (mask-reg 3) (mask-reg 1) (mask-reg 2))))

;; Ensure the disassembler decodes this as KANDQ and not raw VEX bytes.
(test-util::define-evex-disasm-test :evex-kandq-disassembly %test-kandq-disassembly
  ("KANDQ")
  :unexpected ("BYTE #XC4"))

;; ---- KMOVQ ----
(defun %test-kmovq-disassembly ()
  (assemble-instructions
   (kmovq (mask-reg 1) rax-tn)
   (kmovq (mask-reg 2) (mask-reg 1))
   (kmovq rax-tn (mask-reg 2))))

;; All three KMOVQ operand shapes (GPR->mask, mask->mask, mask->GPR)
;; must decode properly, not fall back to raw VEX bytes.
(test-util::define-evex-disasm-test :evex-kmovq-disassembly %test-kmovq-disassembly
  ("KMOVQ")
  :unexpected ("BYTE #XC4"))

;; ---- KSHIFTRQ ----
(defun %test-kshiftrq-disassembly ()
  (assemble-instructions
   (kshiftrq (mask-reg 3) (mask-reg 1) 1)))

(test-util::define-evex-disasm-test :evex-kshiftrq-disassembly %test-kshiftrq-disassembly
  ("KSHIFTRQ")
  :unexpected ("BYTE #XC4"))

;; ---- VADDPD ----
(defun %test-auto-promoted-vaddpd ()
  (assemble-instructions
   (vaddpd (double-avx512-reg 16) (double-avx512-reg 17) (double-avx512-reg 18))))

;; Auto-promoted VADDPD must remain W=1
(test-util::define-evex-disasm-test
    :auto-promoted-evex-vaddpd-disasm
    sb-vm::%test-auto-promoted-vaddpd
  ("VADDPD" "ZMM16" "ZMM17" "ZMM18")
  :unexpected ("VADDPS"))

(defun %test-vaddpd-masked-zmm-disp8 ()
  (assemble-instructions
   (vaddpd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 1)))

;; Masked double arithmetic: vaddpd-masked ZMM, W=1 -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vaddpd-masked-zmm-compressed-disp8
    sb-vm::%test-vaddpd-masked-zmm-disp8
  ("VADDPD" "ZMM0" "ZMM1" "[RSP+64]" "{K1}")
  :unexpected ("[RSP+1]"))

(defun %test-vaddpd-masked-z-disp8 ()
  (assemble-instructions
   (vaddpd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 1 :z)))

;; Zeroing masked double precision
(test-util::define-evex-disasm-test
    :evex-vaddpd-masked-z-compressed-disp8
    sb-vm::%test-vaddpd-masked-z-disp8
  ("VADDPD" "ZMM0" "ZMM1" "[RSP+64]" "{K1} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vaddpd-bcast-zmm-disp8 ()
  (assemble-instructions
   (vaddpd-bcast (double-avx512-reg 0) (double-avx512-reg 1) (ea 8 rsp-tn))))

;; Embedded broadcast: vaddpd ZMM {1to8}
(test-util::define-evex-disasm-test
    :evex-vaddpd-bcast-zmm
    sb-vm::%test-vaddpd-bcast-zmm-disp8
  ("VADDPD-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VADDPS ----
(defun %test-evex-high-regs ()
  (assemble-instructions
   (vaddps (single-avx512-reg 16) (single-avx512-reg 17) (single-avx512-reg 18))))

(test-util::define-evex-disasm-test
    :evex-high-register-disassembly
    sb-vm::%test-evex-high-regs
  ("ZMM16" "ZMM17" "ZMM18" "VADDPS")
  :unexpected ("VADDPS-MASKED" "VADDPD"))

(defun %test-vaddps-masked-zmm-disp8 ()
  (assemble-instructions
   (vaddps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 1)))

;; Masked arithmetic: vaddps-masked ZMM, W=0 -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vaddps-masked-zmm-compressed-disp8
    sb-vm::%test-vaddps-masked-zmm-disp8
  ("VADDPS" "ZMM0" "ZMM1" "[RSP+64]" "{K1}")
  :unexpected ("[RSP+1]"))

(defun %test-vaddps-masked-zmm-disp32 ()
  (assemble-instructions
   (vaddps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 65 rsp-tn) 1)))

;; Masked arithmetic: vaddps-masked ZMM disp32 fallback
(test-util::define-evex-disasm-test
    :evex-vaddps-masked-zmm-disp32-fallback
    sb-vm::%test-vaddps-masked-zmm-disp32
  ("VADDPS" "ZMM0" "ZMM1" "[RSP+65]" "{K1}"))

(defun %test-vaddps-masked-z-disp8 ()
  (assemble-instructions
   (vaddps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 1 :z)))

;; Zeroing masked arithmetic: single precision
(test-util::define-evex-disasm-test
    :evex-vaddps-masked-z-compressed-disp8
    sb-vm::%test-vaddps-masked-z-disp8
  ("VADDPS" "ZMM0" "ZMM1" "[RSP+64]" "{K1} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vaddps-masked-z-disp32 ()
  (assemble-instructions
   (vaddps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 65 rsp-tn) 1 :z)))

;; Zeroing masked arithmetic: single precision, disp32 fallback
(test-util::define-evex-disasm-test
    :evex-vaddps-masked-z-disp32-fallback
    sb-vm::%test-vaddps-masked-z-disp32
  ("VADDPS" "ZMM0" "ZMM1" "[RSP+65]" "{K1} {z}"))

(defun %test-vaddps-bcast-zmm-disp8 ()
  (assemble-instructions
   (vaddps-bcast (single-avx512-reg 0) (single-avx512-reg 1) (ea 4 rsp-tn))))

;; Embedded broadcast: vaddps ZMM {1to16}
(test-util::define-evex-disasm-test
    :evex-vaddps-bcast-zmm
    sb-vm::%test-vaddps-bcast-zmm-disp8
  ("VADDPS-BCAST" "ZMM0" "ZMM1" "[RSP+4]" "{1to16}")
  :unexpected ("[RSP+1]"))

(defun %test-vaddps-bcast-xmm-disp8 ()
  (assemble-instructions
   (vaddps-bcast (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn))))

;; Embedded broadcast: vaddps XMM {1to4}
(test-util::define-evex-disasm-test
    :evex-vaddps-bcast-xmm
    sb-vm::%test-vaddps-bcast-xmm-disp8
  ("VADDPS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VAESDEC ----
(defun %test-vaesdec-masked-xmm-disp8 ()
  (assemble-instructions
   (vaesdec-masked (int-sse-reg 0) (int-sse-reg 1) (ea 16 rsp-tn) 4)))

;; Masked VAESDEC
(test-util::define-evex-disasm-test
    :evex-vaesdec-masked-xmm-disp8
    sb-vm::%test-vaesdec-masked-xmm-disp8
  ("VAESDEC" "XMM0" "XMM1" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

(defun %test-vaesdec-evex-zmm ()
  (assemble-instructions
   (vaesdec (int-avx512-reg 0) (int-avx512-reg 1) (int-avx512-reg 2))))

;; Unmasked EVEX VAESDEC ZMM
(test-util::define-evex-disasm-test
    :evex-vaesdec-unmasked-zmm
    sb-vm::%test-vaesdec-evex-zmm
  ("VAESDEC" "ZMM0" "ZMM1" "ZMM2"))

;; ---- VAESDECLAST ----
(defun %test-vaesdeclast-masked-z-ymm-disp8 ()
  (assemble-instructions
   (vaesdeclast-masked (int-avx2-reg 1) (int-avx2-reg 2) (ea 32 rsp-tn) 5 :z)))

;; Zeroing VAESDECLAST
(test-util::define-evex-disasm-test
    :evex-vaesdeclast-masked-z-ymm-disp8
    sb-vm::%test-vaesdeclast-masked-z-ymm-disp8
  ("VAESDECLAST" "YMM1" "YMM2" "[RSP+32]" "{K5} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vaesdeclast-evex-zmm ()
  (assemble-instructions
   (vaesdeclast (int-avx512-reg 0) (int-avx512-reg 1) (int-avx512-reg 2))))

;; Unmasked EVEX VAESDECLAST ZMM
(test-util::define-evex-disasm-test
    :evex-vaesdeclast-unmasked-zmm
    sb-vm::%test-vaesdeclast-evex-zmm
  ("VAESDECLAST" "ZMM0" "ZMM1" "ZMM2"))

;; ---- VAESENC ----
(defun %test-vaesenc-masked-zmm-disp8 ()
  (assemble-instructions
   (vaesenc-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2)))

;; Masked VAESENC
(test-util::define-evex-disasm-test
    :evex-vaesenc-masked-zmm-disp8
    sb-vm::%test-vaesenc-masked-zmm-disp8
  ("VAESENC" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vaesenc-evex-zmm ()
  (assemble-instructions
   (vaesenc (int-avx512-reg 0) (int-avx512-reg 1) (int-avx512-reg 2))))

;; Unmasked EVEX VAESENC ZMM
(test-util::define-evex-disasm-test
    :evex-vaesenc-unmasked-zmm
    sb-vm::%test-vaesenc-evex-zmm
  ("VAESENC" "ZMM0" "ZMM1" "ZMM2"))

;; ---- VAESENCLAST ----
(defun %test-vaesenclast-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vaesenclast-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 :z)))

;; Zeroing VAESENCLAST
(test-util::define-evex-disasm-test
    :evex-vaesenclast-masked-z-zmm-disp8
    sb-vm::%test-vaesenclast-masked-z-zmm-disp8
  ("VAESENCLAST" "ZMM0" "ZMM1" "[RSP+64]" "{K3} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vaesenclast-evex-zmm ()
  (assemble-instructions
   (vaesenclast (int-avx512-reg 0) (int-avx512-reg 1) (int-avx512-reg 2))))

;; Unmasked EVEX VAESENCLAST ZMM
(test-util::define-evex-disasm-test
    :evex-vaesenclast-unmasked-zmm
    sb-vm::%test-vaesenclast-evex-zmm
  ("VAESENCLAST" "ZMM0" "ZMM1" "ZMM2"))

;; ---- VALIGND ----
(defun %test-valignd-xmm-disp8 ()
  (assemble-instructions
   (valignd (single-sse-reg 0) (single-sse-reg 1) (ea 16 rsp-tn) 0)))

;; Packed 3-operand immediate: valignd XMM -> disp-n=16
(test-util::define-evex-disasm-test
    :evex-valignd-xmm-compressed-disp8
    sb-vm::%test-valignd-xmm-disp8
  ("VALIGND" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-valignd-ymm-disp8 ()
  (assemble-instructions
   (valignd (int-avx2-reg 1) (int-avx2-reg 2) (ea 32 rsp-tn) 0)))

;; Packed 3-operand immediate: valignd YMM -> disp-n=32
(test-util::define-evex-disasm-test
    :evex-valignd-ymm-compressed-disp8
    sb-vm::%test-valignd-ymm-disp8
  ("VALIGND" "YMM1" "YMM2" "[RSP+32]")
  :unexpected ("[RSP+1]"))

(defun %test-valignd-zmm-disp8 ()
  (assemble-instructions
   (valignd (int-avx512-reg 1) (int-avx512-reg 2) (ea 64 rsp-tn) 0)))

;; Packed 3-operand immediate: valignd ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-valignd-zmm-compressed-disp8
    sb-vm::%test-valignd-zmm-disp8
  ("VALIGND" "ZMM1" "ZMM2" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-valignd-zmm-disp32 ()
  (assemble-instructions
   (valignd (int-avx512-reg 1) (int-avx512-reg 2) (ea 65 rsp-tn) 0)))

;; Packed 3-operand immediate: non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-valignd-zmm-disp32-fallback
    sb-vm::%test-valignd-zmm-disp32
  ("VALIGND" "ZMM1" "ZMM2" "[RSP+65]"))

;; ---- VBLENDMPS ----
(defun %test-evex-vblendmps ()
  (assemble-instructions
   (vblendmps (single-avx512-reg 16) (single-avx512-reg 17) (single-avx512-reg 18))))

;; Explicit EVEX: VBLENDMPS with high regs
(test-util::define-evex-disasm-test
    :evex-explicit-vblendmps-disasm
    sb-vm::%test-evex-vblendmps
  ("VBLENDMPS" "ZMM16" "ZMM17" "ZMM18"))

(defun %test-vblendmps-zmm-disp8 ()
  (assemble-instructions
   (vblendmps (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn))))

;; Blend with mask: vblendmps ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vblendmps-zmm-compressed-disp8
    sb-vm::%test-vblendmps-zmm-disp8
  ("VBLENDMPS" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vblendmps-zmm-disp32 ()
  (assemble-instructions
   (vblendmps (single-avx512-reg 0) (single-avx512-reg 1) (ea 65 rsp-tn))))

;; Blend with mask: vblendmps disp32 fallback
(test-util::define-evex-disasm-test
    :evex-vblendmps-zmm-disp32-fallback
    sb-vm::%test-vblendmps-zmm-disp32
  ("VBLENDMPS" "ZMM0" "ZMM1" "[RSP+65]"))

;; ---- VBROADCASTF128 ----
(defun %test-vbroadcastf128-zmm-disasm ()
  (assemble-instructions
   (vbroadcastf128 (single-avx512-reg 0) (ea 16 rsp-tn))))

;; vbroadcastf128 skip: explicit VBROADCASTF32X4 should be used
(test-util::define-evex-disasm-test
    :auto-evex-skip-vbroadcastf128
    sb-vm::%test-vbroadcastf128-zmm-disasm
  ("VBROADCASTF32X4" "ZMM0" "[RSP+16]")
  :unexpected ("VBROADCASTF128"))

;; ---- VBROADCASTF32X4 ----
(defun %test-broadcast-f32x4-disp8 ()
  (assemble-instructions
   (vbroadcastf32x4 (single-avx512-reg 0) (ea 16 rsp-tn))))

;; Block broadcast: VBROADCASTF32X4 uses disp-n=16
(test-util::define-evex-disasm-test
    :evex-broadcast-f32x4-compressed-disp8
    sb-vm::%test-broadcast-f32x4-disp8
  ("VBROADCASTF32X4" "ZMM0" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-broadcast-f32x4-disp32 ()
  (assemble-instructions
   (vbroadcastf32x4 (single-avx512-reg 0) (ea 17 rsp-tn))))

;; Block broadcast: VBROADCASTF32X4 disp32 fallback
(test-util::define-evex-disasm-test
    :evex-broadcast-f32x4-disp32-fallback
    sb-vm::%test-broadcast-f32x4-disp32
  ("VBROADCASTF32X4" "ZMM0" "[RSP+17]"))

;; ---- VBROADCASTF64X4 ----
(defun %test-broadcast-f64x4-disp8 ()
  (assemble-instructions
   (vbroadcastf64x4 (double-avx512-reg 0) (ea 32 rsp-tn))))

;; Block broadcast: VBROADCASTF64X4 uses disp-n=32
(test-util::define-evex-disasm-test
    :evex-broadcast-f64x4-compressed-disp8
    sb-vm::%test-broadcast-f64x4-disp8
  ("VBROADCASTF64X4" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

(defun %test-broadcast-f64x4-disp32 ()
  (assemble-instructions
   (vbroadcastf64x4 (double-avx512-reg 0) (ea 33 rsp-tn))))

;; Block broadcast: VBROADCASTF64X4 disp32 fallback
(test-util::define-evex-disasm-test
    :evex-broadcast-f64x4-disp32-fallback
    sb-vm::%test-broadcast-f64x4-disp32
  ("VBROADCASTF64X4" "ZMM0" "[RSP+33]"))

;; ---- VCOMPRESSPD ----
(defun %test-vcompresspd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vcompresspd-masked-z (ea 64 rsp-tn) (double-avx512-reg 0) 3)))

;; Zeroing compress double
(test-util::define-evex-disasm-test
    :evex-vcompresspd-masked-z-zmm-disp8
    sb-vm::%test-vcompresspd-masked-z-zmm-disp8
  ("VCOMPRESSPD" "[RSP+64]" "ZMM0" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VCOMPRESSPS ----
(defun %test-compress-disp8 ()
  (assemble-instructions
   (vcompressps (ea 64 rsp-tn) (single-avx512-reg 0))))

;; Compress: VCOMPRESSPS uses disp-n=64
(test-util::define-evex-disasm-test
    :evex-compress-compressed-disp8
    sb-vm::%test-compress-disp8
  ("VCOMPRESSPS" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-compress-disp32 ()
  (assemble-instructions
   (vcompressps (ea 65 rsp-tn) (single-avx512-reg 0))))

;; Compress: disp32 fallback
(test-util::define-evex-disasm-test
    :evex-compress-disp32-fallback
    sb-vm::%test-compress-disp32
  ("VCOMPRESSPS" "ZMM0" "[RSP+65]"))

(defun %test-vcompressps-masked-zmm-disp8 ()
  (assemble-instructions
   (vcompressps-masked (ea 64 rsp-tn) (single-avx512-reg 0) 2)))

;; Masked compress single
(test-util::define-evex-disasm-test
    :evex-vcompressps-masked-zmm-disp8
    sb-vm::%test-vcompressps-masked-zmm-disp8
  ("VCOMPRESSPS" "[RSP+64]" "ZMM0" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VCVTDQ2PD ----
(defun %test-auto-promoted-vcvtdq2pd-disp8 ()
  (assemble-instructions
   (vcvtdq2pd (double-avx512-reg 0) (ea 32 rsp-tn))))

;; Auto-promoted widening conversion: vcvtdq2pd ZMM -> disp-n=32
(test-util::define-evex-disasm-test
    :auto-promoted-vcvtdq2pd-compressed-disp8
    sb-vm::%test-auto-promoted-vcvtdq2pd-disp8
  ("VCVTDQ2PD" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

(defun %test-auto-promoted-vcvtdq2pd-disp32 ()
  (assemble-instructions
   (vcvtdq2pd (double-avx512-reg 0) (ea 33 rsp-tn))))

;; Auto-promoted widening conversion non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :auto-promoted-vcvtdq2pd-disp32-fallback
    sb-vm::%test-auto-promoted-vcvtdq2pd-disp32
  ("VCVTDQ2PD" "ZMM0" "[RSP+33]"))

;; ---- VCVTNE2PS2BF16 ----
(defun %test-vcvtne2ps2bf16-zmm-disp8 ()
  (assemble-instructions
   (vcvtne2ps2bf16 (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vcvtne2ps2bf16-zmm-compressed-disp8
    sb-vm::%test-vcvtne2ps2bf16-zmm-disp8
  ("VCVTNE2PS2BF16" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtne2ps2bf16-masked-zmm-disp8 ()
  (assemble-instructions
   (vcvtne2ps2bf16-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vcvtne2ps2bf16-masked-zmm-disp8
    sb-vm::%test-vcvtne2ps2bf16-masked-zmm-disp8
  ("VCVTNE2PS2BF16" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtne2ps2bf16-bcast-zmm-disp8 ()
  (assemble-instructions
   (vcvtne2ps2bf16-bcast (int-avx512-reg 0) (int-avx512-reg 1) (ea 4 rsp-tn))))

;; Broadcast BF16 conversion
(test-util::define-evex-disasm-test
    :evex-vcvtne2ps2bf16-bcast-zmm-disp8
    sb-vm::%test-vcvtne2ps2bf16-bcast-zmm-disp8
  ("VCVTNE2PS2BF16-BCAST" "ZMM0" "ZMM1" "[RSP+4]" "{1to16}")
  :unexpected ("[RSP+1]"))

;; ---- VCVTNEPS2BF16 ----
(defun %test-vcvtneps2bf16-zmm-disp8 ()
  (assemble-instructions
   (vcvtneps2bf16 (int-avx512-reg 0) (ea 64 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vcvtneps2bf16-zmm-compressed-disp8
    sb-vm::%test-vcvtneps2bf16-zmm-disp8
  ("VCVTNEPS2BF16" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtneps2bf16-masked-zmm-disp8 ()
  (assemble-instructions
   (vcvtneps2bf16-masked (int-avx512-reg 0) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vcvtneps2bf16-masked-zmm-disp8
    sb-vm::%test-vcvtneps2bf16-masked-zmm-disp8
  ("VCVTNEPS2BF16" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtneps2bf16-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vcvtneps2bf16-masked-z (int-avx512-reg 0) (ea 64 rsp-tn) 3)))

(test-util::define-evex-disasm-test
    :evex-vcvtneps2bf16-masked-z-zmm-disp8
    sb-vm::%test-vcvtneps2bf16-masked-z-zmm-disp8
  ("VCVTNEPS2BF16" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VCVTPD2QQ ----
(defun %test-vcvtpd2qq-zmm-disp8 ()
  (assemble-instructions
   (vcvtpd2qq (int-avx512-reg 0) (ea 64 rsp-tn))))

;; Full-width conversion: double -> qword, ZMM memory = 64 bytes, disp-n=64
(test-util::define-evex-disasm-test
    :evex-vcvtpd2qq-zmm-compressed-disp8
    sb-vm::%test-vcvtpd2qq-zmm-disp8
  ("VCVTPD2QQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VCVTPH2PS ----
(defun %test-vcvtph2ps-zmm-disp8 ()
  (assemble-instructions
   (vcvtph2ps (single-avx512-reg 0) (ea 32 rsp-tn))))

;; F16C load: widening, disp-n=32 for ZMM
(test-util::define-evex-disasm-test
    :evex-vcvtph2ps-zmm-compressed-disp8
    sb-vm::%test-vcvtph2ps-zmm-disp8
  ("VCVTPH2PS" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtph2ps-masked-zmm-disp8 ()
  (assemble-instructions
   (vcvtph2ps-masked (single-avx512-reg 0) (ea 32 rsp-tn) 2)))

;; Masked vcvtph2ps ZMM
(test-util::define-evex-disasm-test
    :evex-vcvtph2ps-masked-zmm-disp8
    sb-vm::%test-vcvtph2ps-masked-zmm-disp8
  ("VCVTPH2PS" "ZMM0" "[RSP+32]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtph2ps-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vcvtph2ps-masked-z (single-avx512-reg 0) (ea 32 rsp-tn) 3)))

;; Zeroing vcvtph2ps ZMM
(test-util::define-evex-disasm-test
    :evex-vcvtph2ps-masked-z-zmm-disp8
    sb-vm::%test-vcvtph2ps-masked-z-zmm-disp8
  ("VCVTPH2PS" "ZMM0" "[RSP+32]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtph2ps-masked-xmm-disp8 ()
  (assemble-instructions
   (vcvtph2ps-masked (single-sse-reg 0) (ea 8 rsp-tn) 4)))

;; Masked vcvtph2ps XMM
(test-util::define-evex-disasm-test
    :evex-vcvtph2ps-masked-xmm-disp8
    sb-vm::%test-vcvtph2ps-masked-xmm-disp8
  ("VCVTPH2PS" "XMM0" "[RSP+8]" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VCVTPS2DQ ----
(defun %test-auto-promoted-vcvtps2dq-disp8 ()
  (assemble-instructions
   (vcvtps2dq (int-avx512-reg 0) (ea 64 rsp-tn))))

;; Auto-promoted same-width conversion: vcvtps2dq ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :auto-promoted-vcvtps2dq-compressed-disp8
    sb-vm::%test-auto-promoted-vcvtps2dq-disp8
  ("VCVTPS2DQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-auto-promoted-vcvtps2dq-disp32 ()
  (assemble-instructions
   (vcvtps2dq (int-avx512-reg 0) (ea 65 rsp-tn))))

;; Auto-promoted same-width conversion non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :auto-promoted-vcvtps2dq-disp32-fallback
    sb-vm::%test-auto-promoted-vcvtps2dq-disp32
  ("VCVTPS2DQ" "ZMM0" "[RSP+65]"))

;; ---- VCVTPS2PH ----
(defun %test-vcvtps2ph-zmm-disp8 ()
  (assemble-instructions
   (vcvtps2ph (ea 32 rsp-tn) (single-avx512-reg 0) 0)))

;; F16C store: narrowing, disp-n=32 for ZMM
(test-util::define-evex-disasm-test
    :evex-vcvtps2ph-zmm-compressed-disp8
    sb-vm::%test-vcvtps2ph-zmm-disp8
  ("VCVTPS2PH" "[RSP+32]" "ZMM0")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtps2ph-masked-zmm-disp8 ()
  (assemble-instructions
   (vcvtps2ph-masked (ea 32 rsp-tn) (single-avx512-reg 0) 2 0)))

;; Masked vcvtps2ph ZMM
(test-util::define-evex-disasm-test
    :evex-vcvtps2ph-masked-zmm-disp8
    sb-vm::%test-vcvtps2ph-masked-zmm-disp8
  ("VCVTPS2PH" "[RSP+32]" "ZMM0" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtps2ph-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vcvtps2ph-masked-z (ea 32 rsp-tn) (single-avx512-reg 0) 3 0)))

;; Zeroing vcvtps2ph ZMM
(test-util::define-evex-disasm-test
    :evex-vcvtps2ph-masked-z-zmm-disp8
    sb-vm::%test-vcvtps2ph-masked-z-zmm-disp8
  ("VCVTPS2PH" "[RSP+32]" "ZMM0" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtps2ph-masked-xmm-disp8 ()
  (assemble-instructions
   (vcvtps2ph-masked (ea 8 rsp-tn) (single-sse-reg 0) 4 0)))

;; Masked vcvtps2ph XMM
(test-util::define-evex-disasm-test
    :evex-vcvtps2ph-masked-xmm-disp8
    sb-vm::%test-vcvtps2ph-masked-xmm-disp8
  ("VCVTPS2PH" "[RSP+8]" "XMM0" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VCVTPS2QQ ----
(defun %test-vcvtps2qq-zmm-disp8 ()
  (assemble-instructions
   (vcvtps2qq (int-avx512-reg 0) (ea 32 rsp-tn))))

;; Narrowing conversion: single -> qword, ZMM memory = 32 bytes, disp-n=32
(test-util::define-evex-disasm-test
    :evex-vcvtps2qq-zmm-compressed-disp8
    sb-vm::%test-vcvtps2qq-zmm-disp8
  ("VCVTPS2QQ" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtps2qq-zmm-disp32 ()
  (assemble-instructions
   (vcvtps2qq (int-avx512-reg 0) (ea 33 rsp-tn))))

;; Narrowing conversion non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vcvtps2qq-zmm-disp32-fallback
    sb-vm::%test-vcvtps2qq-zmm-disp32
  ("VCVTPS2QQ" "ZMM0" "[RSP+33]"))

;; ---- VCVTPS2UDQ ----
(defun %test-vcvtps2udq-zmm-disp8 ()
  (assemble-instructions
   (vcvtps2udq (int-avx512-reg 0) (ea 64 rsp-tn))))

;; Full-vector unsigned conversion: vcvtps2udq ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vcvtps2udq-zmm-compressed-disp8
    sb-vm::%test-vcvtps2udq-zmm-disp8
  ("VCVTPS2UDQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtps2udq-zmm-disp32 ()
  (assemble-instructions
   (vcvtps2udq (int-avx512-reg 0) (ea 65 rsp-tn))))

;; Full-vector unsigned conversion: non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vcvtps2udq-zmm-disp32-fallback
    sb-vm::%test-vcvtps2udq-zmm-disp32
  ("VCVTPS2UDQ" "ZMM0" "[RSP+65]"))

;; ---- VCVTQQ2PS ----
(defun %test-vcvtqq2ps-zmm-disp8 ()
  (assemble-instructions
   (vcvtqq2ps (single-avx512-reg 0) (ea 64 rsp-tn))))

;; Full-width conversion: qword -> single, ZMM memory = 64 bytes, disp-n=64
(test-util::define-evex-disasm-test
    :evex-vcvtqq2ps-zmm-compressed-disp8
    sb-vm::%test-vcvtqq2ps-zmm-disp8
  ("VCVTQQ2PS" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtqq2ps-zmm-disp32 ()
  (assemble-instructions
   (vcvtqq2ps (single-avx512-reg 0) (ea 65 rsp-tn))))

;; Full-width conversion non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vcvtqq2ps-zmm-disp32-fallback
    sb-vm::%test-vcvtqq2ps-zmm-disp32
  ("VCVTQQ2PS" "ZMM0" "[RSP+65]"))

;; ---- VCVTSD2USI ----
(defun %test-vcvtsd2usi-disp8 ()
  (assemble-instructions
   (vcvtsd2usi rax-tn (ea 8 rsp-tn))))

;; Scalar unsigned conversion: double precision uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vcvtsd2usi-compressed-disp8
    sb-vm::%test-vcvtsd2usi-disp8
  ("VCVTSD2USI" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; ---- VCVTSS2USI ----
(defun %test-vcvtss2usi-disp8 ()
  (assemble-instructions
   (vcvtss2usi rax-tn (ea 4 rsp-tn))))

;; Scalar unsigned conversion: single precision uses disp-n=4
(test-util::define-evex-disasm-test
    :evex-vcvtss2usi-compressed-disp8
    sb-vm::%test-vcvtss2usi-disp8
  ("VCVTSS2USI" "[RSP+4]")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtss2usi-disp32 ()
  (assemble-instructions
   (vcvtss2usi rax-tn (ea 5 rsp-tn))))

;; Scalar unsigned conversion: single precision non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vcvtss2usi-disp32-fallback
    sb-vm::%test-vcvtss2usi-disp32
  ("VCVTSS2USI" "[RSP+5]"))

;; ---- VCVTTSD2USI ----
(defun %test-vcvttsd2usi-disp8 ()
  (assemble-instructions
   (vcvttsd2usi rax-tn (ea 8 rsp-tn))))

;; Scalar truncating unsigned conversion: double precision uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vcvttsd2usi-compressed-disp8
    sb-vm::%test-vcvttsd2usi-disp8
  ("VCVTTSD2USI" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; ---- VCVTTSS2USI ----
(defun %test-vcvttss2usi-disp8 ()
  (assemble-instructions
   (vcvttss2usi rax-tn (ea 4 rsp-tn))))

;; Scalar truncating unsigned conversion: single precision uses disp-n=4
(test-util::define-evex-disasm-test
    :evex-vcvttss2usi-compressed-disp8
    sb-vm::%test-vcvttss2usi-disp8
  ("VCVTTSS2USI" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; ---- VCVTUDQ2PD ----
(defun %test-vcvtudq2pd-zmm-disp8 ()
  (assemble-instructions
   (vcvtudq2pd (double-avx512-reg 0) (ea 32 rsp-tn))))

;; Widening unsigned conversion: vcvtudq2pd ZMM source -> disp-n=32
(test-util::define-evex-disasm-test
    :evex-vcvtudq2pd-zmm-compressed-disp8
    sb-vm::%test-vcvtudq2pd-zmm-disp8
  ("VCVTUDQ2PD" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtudq2pd-xmm-disp8 ()
  (assemble-instructions
   (vcvtudq2pd (double-sse-reg 0) (ea 8 rsp-tn))))

;; Widening unsigned conversion: XMM source -> disp-n=8
(test-util::define-evex-disasm-test
    :evex-vcvtudq2pd-xmm-compressed-disp8
    sb-vm::%test-vcvtudq2pd-xmm-disp8
  ("VCVTUDQ2PD" "XMM0" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; ---- VCVTUSI2SD ----
(defun %test-vcvtusi2sd-disp8 ()
  (assemble-instructions
   (vcvtusi2sd (double-sse-reg 0) (double-sse-reg 0) (ea 8 rsp-tn))))

;; Scalar unsigned convert to double, default W=1 -> disp-n=8
(test-util::define-evex-disasm-test
    :evex-vcvtusi2sd-compressed-disp8
    sb-vm::%test-vcvtusi2sd-disp8
  ("VCVTUSI2SD" "XMM0" "XMM0" "[RSP+8]")
  :unexpected ("[RSP+1]"))

(defun %test-vcvtusi2sd-disp32 ()
  (assemble-instructions
   (vcvtusi2sd (double-sse-reg 0) (double-sse-reg 0) (ea 9 rsp-tn))))

;; Scalar unsigned convert to double, non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vcvtusi2sd-disp32-fallback
    sb-vm::%test-vcvtusi2sd-disp32
  ("VCVTUSI2SD" "XMM0" "XMM0" "[RSP+9]"))

;; ---- VCVTUSI2SS ----
(defun %test-vcvtusi2ss-disp8 ()
  (assemble-instructions
   (vcvtusi2ss (single-sse-reg 0) (single-sse-reg 0) (ea 8 rsp-tn))))

;; Scalar unsigned convert to single, default W=1 -> disp-n=8
(test-util::define-evex-disasm-test
    :evex-vcvtusi2ss-compressed-disp8
    sb-vm::%test-vcvtusi2ss-disp8
  ("VCVTUSI2SS" "XMM0" "XMM0" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; ---- VDBPSADBW ----
(defun %test-vdbpsadbw-zmm-disp8 ()
  (assemble-instructions
   (vdbpsadbw (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 0)))

;; Double-block SAD: ZMM full-vector -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vdbpsadbw-zmm-compressed-disp8
    sb-vm::%test-vdbpsadbw-zmm-disp8
  ("VDBPSADBW" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vdbpsadbw-masked-zmm-disp8 ()
  (assemble-instructions
   (vdbpsadbw-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2 0)))

;; Masked vdbpsadbw
(test-util::define-evex-disasm-test
    :evex-vdbpsadbw-masked-zmm-disp8
    sb-vm::%test-vdbpsadbw-masked-zmm-disp8
  ("VDBPSADBW" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vdbpsadbw-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vdbpsadbw-masked-z (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 0)))

;; Zeroing vdbpsadbw
(test-util::define-evex-disasm-test
    :evex-vdbpsadbw-masked-z-zmm-disp8
    sb-vm::%test-vdbpsadbw-masked-z-zmm-disp8
  ("VDBPSADBW" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VDIVPD ----
(defun %test-vdivpd-masked-zmm-disp8 ()
  (assemble-instructions
   (vdivpd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 3)))

;; Masked divide double precision
(test-util::define-evex-disasm-test
    :evex-vdivpd-masked-zmm-disp8
    sb-vm::%test-vdivpd-masked-zmm-disp8
  ("VDIVPD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}")
  :unexpected ("[RSP+1]"))

(defun %test-vdivpd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vdivpd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 5 :z)))

;; Zeroing divide double precision
(test-util::define-evex-disasm-test
    :evex-vdivpd-masked-z-zmm-disp8
    sb-vm::%test-vdivpd-masked-z-zmm-disp8
  ("VDIVPD" "ZMM0" "ZMM1" "[RSP+64]" "{K5} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vdivpd-bcast-zmm-disp8 ()
  (assemble-instructions
   (vdivpd-bcast (double-avx512-reg 0) (double-avx512-reg 1) (ea 8 rsp-tn))))

;; Broadcast divide double precision
(test-util::define-evex-disasm-test
    :evex-vdivpd-bcast-zmm-disp8
    sb-vm::%test-vdivpd-bcast-zmm-disp8
  ("VDIVPD-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VDIVPS ----
(defun %test-vdivps-masked-zmm-disp8 ()
  (assemble-instructions
   (vdivps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 2)))

;; Masked divide single precision
(test-util::define-evex-disasm-test
    :evex-vdivps-masked-zmm-disp8
    sb-vm::%test-vdivps-masked-zmm-disp8
  ("VDIVPS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vdivps-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vdivps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 4 :z)))

;; Zeroing divide single precision
(test-util::define-evex-disasm-test
    :evex-vdivps-masked-z-zmm-disp8
    sb-vm::%test-vdivps-masked-z-zmm-disp8
  ("VDIVPS" "ZMM0" "ZMM1" "[RSP+64]" "{K4} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vdivps-bcast-xmm-disp8 ()
  (assemble-instructions
   (vdivps-bcast (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn))))

;; Broadcast divide single precision
(test-util::define-evex-disasm-test
    :evex-vdivps-bcast-xmm-disp8
    sb-vm::%test-vdivps-bcast-xmm-disp8
  ("VDIVPS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VDPBF16PS ----
(defun %test-vdpbf16ps-xmm-disp8 ()
  (assemble-instructions
   (vdpbf16ps (int-sse-reg 0) (int-sse-reg 1) (ea 16 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vdpbf16ps-xmm-compressed-disp8
    sb-vm::%test-vdpbf16ps-xmm-disp8
  ("VDPBF16PS" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-vdpbf16ps-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vdpbf16ps-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 :z)))

(test-util::define-evex-disasm-test
    :evex-vdpbf16ps-masked-z-zmm-disp8
    sb-vm::%test-vdpbf16ps-masked-z-zmm-disp8
  ("VDPBF16PS" "ZMM0" "ZMM1" "[RSP+64]" "{K3} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vdpbf16ps-bcast-xmm-disp8 ()
  (assemble-instructions
   (vdpbf16ps-bcast (int-sse-reg 0) (int-sse-reg 1) (ea 4 rsp-tn))))

;; Broadcast BF16 dot product
(test-util::define-evex-disasm-test
    :evex-vdpbf16ps-bcast-xmm-disp8
    sb-vm::%test-vdpbf16ps-bcast-xmm-disp8
  ("VDPBF16PS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VEXPANDPD ----
(defun %test-vexpandpd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vexpandpd-masked-z (double-avx512-reg 0) (ea 64 rsp-tn) 3)))

;; Zeroing expand double
(test-util::define-evex-disasm-test
    :evex-vexpandpd-masked-z-zmm-disp8
    sb-vm::%test-vexpandpd-masked-z-zmm-disp8
  ("VEXPANDPD" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VEXPANDPS ----
(defun %test-vexpandps-masked-zmm-disp8 ()
  (assemble-instructions
   (vexpandps-masked (single-avx512-reg 0) (ea 64 rsp-tn) 2)))

;; Masked expand single
(test-util::define-evex-disasm-test
    :evex-vexpandps-masked-zmm-disp8
    sb-vm::%test-vexpandps-masked-zmm-disp8
  ("VEXPANDPS" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VEXTRACTF32X4 ----
(defun %test-vextractf32x4-zmm-disp8 ()
  (assemble-instructions
   (vextractf32x4 (ea 16 rsp-tn) (single-avx512-reg 0) 0)))

;; Extract 128-bit lane: disp-n=16
(test-util::define-evex-disasm-test
    :evex-vextractf32x4-zmm-compressed-disp8
    sb-vm::%test-vextractf32x4-zmm-disp8
  ("VEXTRACTF32X4" "[RSP+16]" "ZMM0")
  :unexpected ("[RSP+1]"))

;; ---- VEXTRACTF32X8 ----
(defun %test-vextractf32x8-zmm-disp8 ()
  (assemble-instructions
   (vextractf32x8 (ea 32 rsp-tn) (single-avx512-reg 0) 0)))

;; Extract 256-bit lane: disp-n=32
(test-util::define-evex-disasm-test
    :evex-vextractf32x8-zmm-compressed-disp8
    sb-vm::%test-vextractf32x8-zmm-disp8
  ("VEXTRACTF32X8" "[RSP+32]" "ZMM0")
  :unexpected ("[RSP+1]"))

;; ---- VFIXUPIMMPD ----
(defun %test-vfixupimmpd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vfixupimmpd-masked-z (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 7 0)))

(test-util::define-evex-disasm-test
    :evex-vfixupimmpd-masked-z-zmm-disp8
    sb-vm::%test-vfixupimmpd-masked-z-zmm-disp8
  ("VFIXUPIMMPD" "ZMM0" "ZMM1" "[RSP+64]" "{K7}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VFIXUPIMMPS ----
(defun %test-vfixupimmps-xmm-disp8 ()
  (assemble-instructions
   (vfixupimmps (single-sse-reg 0) (single-sse-reg 1) (ea 16 rsp-tn) 0)))

;; Packed fixup: XMM full-vector -> disp-n=16
(test-util::define-evex-disasm-test
    :evex-vfixupimmps-xmm-compressed-disp8
    sb-vm::%test-vfixupimmps-xmm-disp8
  ("VFIXUPIMMPS" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-vfixupimmps-zmm-disp8 ()
  (assemble-instructions
   (vfixupimmps (single-avx512-reg 1) (single-avx512-reg 2) (ea 64 rsp-tn) 0)))

;; Packed fixup: ZMM full-vector -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vfixupimmps-zmm-compressed-disp8
    sb-vm::%test-vfixupimmps-zmm-disp8
  ("VFIXUPIMMPS" "ZMM1" "ZMM2" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vfixupimmps-zmm-disp32 ()
  (assemble-instructions
   (vfixupimmps (single-avx512-reg 1) (single-avx512-reg 2) (ea 65 rsp-tn) 0)))

;; Packed fixup: non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vfixupimmps-zmm-disp32-fallback
    sb-vm::%test-vfixupimmps-zmm-disp32
  ("VFIXUPIMMPS" "ZMM1" "ZMM2" "[RSP+65]"))

(defun %test-vfixupimmps-masked-zmm-disp8 ()
  (assemble-instructions
   (vfixupimmps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 6 0)))

;; Fixup
(test-util::define-evex-disasm-test
    :evex-vfixupimmps-masked-zmm-disp8
    sb-vm::%test-vfixupimmps-masked-zmm-disp8
  ("VFIXUPIMMPS" "ZMM0" "ZMM1" "[RSP+64]" "{K6}")
  :unexpected ("[RSP+1]"))

(defun %test-vfixupimmps-bcast-xmm-disp8 ()
  (assemble-instructions
   (vfixupimmps-bcast (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn) 0)))

(test-util::define-evex-disasm-test
    :evex-vfixupimmps-bcast-xmm-disp8
    sb-vm::%test-vfixupimmps-bcast-xmm-disp8
  ("VFIXUPIMMPS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VFIXUPIMMSD ----
(defun %test-vfixupimmsd-disp8 ()
  (assemble-instructions
   (vfixupimmsd (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn) 0)))

;; Scalar fixup: double precision uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vfixupimmsd-compressed-disp8
    sb-vm::%test-vfixupimmsd-disp8
  ("VFIXUPIMMSD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; ---- VFIXUPIMMSS ----
(defun %test-vfixupimmss-disp8 ()
  (assemble-instructions
   (vfixupimmss (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn) 0)))

;; Scalar fixup: single precision uses disp-n=4
(test-util::define-evex-disasm-test
    :evex-vfixupimmss-compressed-disp8
    sb-vm::%test-vfixupimmss-disp8
  ("VFIXUPIMMSS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

(defun %test-vfixupimmss-disp32 ()
  (assemble-instructions
   (vfixupimmss (single-sse-reg 0) (single-sse-reg 1) (ea 5 rsp-tn) 0)))

;; Scalar fixup: single precision non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vfixupimmss-disp32-fallback
    sb-vm::%test-vfixupimmss-disp32
  ("VFIXUPIMMSS" "XMM0" "XMM1" "[RSP+5]"))

(defun %test-vfixupimmss-masked-disp8 ()
  (assemble-instructions
   (vfixupimmss-masked (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn) 4 0)))

;; Masked scalar fixup
(test-util::define-evex-disasm-test
    :evex-vfixupimmss-masked-disp8
    sb-vm::%test-vfixupimmss-masked-disp8
  ("VFIXUPIMMSS" "XMM0" "XMM1" "[RSP+4]" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VFMADD132PS ----
(defun %test-vfmadd132ps-zmm-disp8 ()
  (assemble-instructions
   (vfmadd132ps (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn))))

;; FMA packed: full-vector, disp-n=64
(test-util::define-evex-disasm-test
    :evex-vfmadd132ps-zmm-compressed-disp8
    sb-vm::%test-vfmadd132ps-zmm-disp8
  ("VFMADD132PS" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vfmadd132ps-masked-zmm-disp8 ()
  (assemble-instructions
   (vfmadd132ps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vfmadd132ps-masked-zmm-disp8
    sb-vm::%test-vfmadd132ps-masked-zmm-disp8
  ("VFMADD132PS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vfmadd132ps-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vfmadd132ps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 4 :z)))

(test-util::define-evex-disasm-test
    :evex-vfmadd132ps-masked-z-zmm-disp8
    sb-vm::%test-vfmadd132ps-masked-z-zmm-disp8
  ("VFMADD132PS" "ZMM0" "ZMM1" "[RSP+64]" "{K4} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vfmadd132ps-bcast-xmm-disp8 ()
  (assemble-instructions
   (vfmadd132ps-bcast (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vfmadd132ps-bcast-xmm-disp8
    sb-vm::%test-vfmadd132ps-bcast-xmm-disp8
  ("VFMADD132PS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VFMADD132SS ----
(defun %test-vfmadd132ss-disp8 ()
  (assemble-instructions
   (vfmadd132ss (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn))))

;; FMA scalar: fixed disp-n=4
(test-util::define-evex-disasm-test
    :evex-vfmadd132ss-compressed-disp8
    sb-vm::%test-vfmadd132ss-disp8
  ("VFMADD132SS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

(defun %test-vfmadd132ss-masked-disp8 ()
  (assemble-instructions
   (vfmadd132ss-masked (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vfmadd132ss-masked-disp8
    sb-vm::%test-vfmadd132ss-masked-disp8
  ("VFMADD132SS" "XMM0" "XMM1" "[RSP+4]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VFMADD213PD ----
(defun %test-vfmadd213pd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vfmadd213pd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 5 :z)))

(test-util::define-evex-disasm-test
    :evex-vfmadd213pd-masked-z-zmm-disp8
    sb-vm::%test-vfmadd213pd-masked-z-zmm-disp8
  ("VFMADD213PD" "ZMM0" "ZMM1" "[RSP+64]" "{K5} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vfmadd213pd-bcast-zmm-disp8 ()
  (assemble-instructions
   (vfmadd213pd-bcast (double-avx512-reg 0) (double-avx512-reg 1) (ea 8 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vfmadd213pd-bcast-zmm-disp8
    sb-vm::%test-vfmadd213pd-bcast-zmm-disp8
  ("VFMADD213PD-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VFMADD231PD ----
(defun %test-vfmadd231pd-masked-zmm-disp8 ()
  (assemble-instructions
   (vfmadd231pd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 3)))

(test-util::define-evex-disasm-test
    :evex-vfmadd231pd-masked-zmm-disp8
    sb-vm::%test-vfmadd231pd-masked-zmm-disp8
  ("VFMADD231PD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}")
  :unexpected ("[RSP+1]"))

;; ---- VFMADDSUB132PS ----
(defun %test-vfmaddsub132ps-masked-zmm-disp8 ()
  (assemble-instructions
   (vfmaddsub132ps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vfmaddsub132ps-masked-zmm-disp8
    sb-vm::%test-vfmaddsub132ps-masked-zmm-disp8
  ("VFMADDSUB132PS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VFMADDSUB213PD ----
(defun %test-vfmaddsub213pd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vfmaddsub213pd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 3 :z)))

(test-util::define-evex-disasm-test
    :evex-vfmaddsub213pd-masked-z-zmm-disp8
    sb-vm::%test-vfmaddsub213pd-masked-z-zmm-disp8
  ("VFMADDSUB213PD" "ZMM0" "ZMM1" "[RSP+64]" "{K3} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VFMADDSUB231PS ----
(defun %test-vfmaddsub231ps-bcast-xmm-disp8 ()
  (assemble-instructions
   (vfmaddsub231ps-bcast (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vfmaddsub231ps-bcast-xmm-disp8
    sb-vm::%test-vfmaddsub231ps-bcast-xmm-disp8
  ("VFMADDSUB231PS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VFMSUB132PS ----
(defun %test-vfmsub132ps-masked-zmm-disp8 ()
  (assemble-instructions
   (vfmsub132ps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vfmsub132ps-masked-zmm-disp8
    sb-vm::%test-vfmsub132ps-masked-zmm-disp8
  ("VFMSUB132PS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VFMSUB213PD ----
(defun %test-vfmsub213pd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vfmsub213pd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 3 :z)))

(test-util::define-evex-disasm-test
    :evex-vfmsub213pd-masked-z-zmm-disp8
    sb-vm::%test-vfmsub213pd-masked-z-zmm-disp8
  ("VFMSUB213PD" "ZMM0" "ZMM1" "[RSP+64]" "{K3} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VFMSUB213SD ----
(defun %test-vfmsub213sd-masked-z-disp8 ()
  (assemble-instructions
   (vfmsub213sd-masked-z (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn) 3)))

(test-util::define-evex-disasm-test
    :evex-vfmsub213sd-masked-z-disp8
    sb-vm::%test-vfmsub213sd-masked-z-disp8
  ("VFMSUB213SD" "XMM0" "XMM1" "[RSP+8]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VFMSUBADD132PS ----
(defun %test-vfmsubadd132ps-masked-zmm-disp8 ()
  (assemble-instructions
   (vfmsubadd132ps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vfmsubadd132ps-masked-zmm-disp8
    sb-vm::%test-vfmsubadd132ps-masked-zmm-disp8
  ("VFMSUBADD132PS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VFMSUBADD213PD ----
(defun %test-vfmsubadd213pd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vfmsubadd213pd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 3 :z)))

(test-util::define-evex-disasm-test
    :evex-vfmsubadd213pd-masked-z-zmm-disp8
    sb-vm::%test-vfmsubadd213pd-masked-z-zmm-disp8
  ("VFMSUBADD213PD" "ZMM0" "ZMM1" "[RSP+64]" "{K3} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VFMSUBADD231PS ----
(defun %test-vfmsubadd231ps-bcast-xmm-disp8 ()
  (assemble-instructions
   (vfmsubadd231ps-bcast (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vfmsubadd231ps-bcast-xmm-disp8
    sb-vm::%test-vfmsubadd231ps-bcast-xmm-disp8
  ("VFMSUBADD231PS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VFNMADD231PS ----
(defun %test-vfnmadd231ps-bcast-xmm-disp8 ()
  (assemble-instructions
   (vfnmadd231ps-bcast (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vfnmadd231ps-bcast-xmm-disp8
    sb-vm::%test-vfnmadd231ps-bcast-xmm-disp8
  ("VFNMADD231PS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VFPCLASSPS ----
(defun %test-vfpclassps-zmm-disp8 ()
  (assemble-instructions
   (vfpclassps (mask-reg 1) (ea 64 rsp-tn) #x1)))

;; vfpclassps packed: full-vector source, disp-n=64
(test-util::define-evex-disasm-test
    :evex-vfpclassps-zmm-compressed-disp8
  sb-vm::%test-vfpclassps-zmm-disp8
  ("VFPCLASSPS" "K1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VFPCLASSSS ----
(defun %test-vfpclassss-disp8 ()
  (assemble-instructions
   (vfpclassss (mask-reg 1) (ea 4 rsp-tn) #x1)))

;; vfpclassss scalar: fixed disp-n=4
(test-util::define-evex-disasm-test
    :evex-vfpclassss-compressed-disp8
    sb-vm::%test-vfpclassss-disp8
  ("VFPCLASSSS" "K1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; ---- VGATHERDPS ----
(defun %test-vsib-no-base-low-index ()
  (assemble-instructions
   (vgatherdps-z (single-avx512-reg 0) (ea 64 nil (single-avx512-reg 1) 4) 1)))

;; VSIB No base, low index, displacement
(test-util::define-evex-disasm-test
    :evex-vsib-no-base-low-index
    sb-vm::%test-vsib-no-base-low-index
  ("VGATHERDPS" "ZMM0" "[ZMM1*4+64]" "K1"))

(defun %test-vsib-high-index ()
  (assemble-instructions
   (vgatherdps-z (single-avx512-reg 0) (ea 0 (unsigned-reg rax-offset) (single-avx512-reg 16) 4) 1)))

(test-util::define-evex-disasm-test
    :evex-vsib-high-index-disasm
    sb-vm::%test-vsib-high-index
  ("VGATHERDPS" "ZMM16")
  :unexpected ())

(defun %test-vsib-high-base-high-index ()
  (assemble-instructions
   (vgatherdps-z (single-avx512-reg 0) (ea 0 (unsigned-reg r8-offset) (single-avx512-reg 16) 8) 2)))

;; VSIB High base and high vector index
(test-util::define-evex-disasm-test
    :evex-vsib-high-base-high-index
    sb-vm::%test-vsib-high-base-high-index
  ("VGATHERDPS" "ZMM0" "ZMM16*8" "K2"))

(defun %test-vsib-scale-2 ()
  (assemble-instructions
   (vgatherdps-z (single-avx512-reg 0) (ea 0 (unsigned-reg rbx-offset) (single-avx512-reg 5) 2) 4)))

;; VSIB Scale 2
(test-util::define-evex-disasm-test
    :evex-vsib-scale-2
    sb-vm::%test-vsib-scale-2
  ("VGATHERDPS" "ZMM0" "ZMM5*2" "K4"))

(defun %test-vsib-scale-8 ()
  (assemble-instructions
   (vgatherdps-z (single-avx512-reg 0) (ea 0 (unsigned-reg rdx-offset) (single-avx512-reg 7) 8) 5)))

;; VSIB Scale 8
(test-util::define-evex-disasm-test
    :evex-vsib-scale-8
    sb-vm::%test-vsib-scale-8
  ("VGATHERDPS" "ZMM0" "ZMM7*8" "K5"))

(defun %test-vsib-disp8 ()
  (assemble-instructions
   (vgatherdps-z (single-avx512-reg 0) (ea 8 (unsigned-reg rax-offset) (single-avx512-reg 2) 4) 6)))

;; VSIB Compressed displacement (disp8)
(test-util::define-evex-disasm-test
    :evex-vsib-disp8
    sb-vm::%test-vsib-disp8
  ("VGATHERDPS" "ZMM0" "ZMM2*4+8" "K6"))

(defun %test-vsib-disp8-compressed ()
  (assemble-instructions
   (vgatherdps-z (single-avx512-reg 0) (ea 4 (unsigned-reg rax-offset) (single-avx512-reg 1) 4) 1)))

(test-util::define-evex-disasm-test
    :evex-vsib-disp8-compressed
    sb-vm::%test-vsib-disp8-compressed
  ("VGATHERDPS" "ZMM0" "ZMM1*4+4" "K1"))

(defun %test-vsib-nonmultiple-disp32 ()
  (assemble-instructions
   (vgatherdps-z (single-avx512-reg 0) (ea 5 (unsigned-reg rcx-offset) (single-avx512-reg 3) 4) 3)))

;; Non-multiple displacement falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vsib-nonmultiple-disp32
    sb-vm::%test-vsib-nonmultiple-disp32
  ("VGATHERDPS" "ZMM0" "ZMM3*4+5" "K3"))

(defun %test-vsib-high-index-disp8-compressed ()
  (assemble-instructions
   (vgatherdps-z (single-avx512-reg 0) (ea 4 (unsigned-reg rdx-offset) (single-avx512-reg 16) 4) 4)))

;; High vector index with compressed displacement
(test-util::define-evex-disasm-test
    :evex-vsib-high-index-disp8-compressed
    sb-vm::%test-vsib-high-index-disp8-compressed
  ("VGATHERDPS" "ZMM0" "ZMM16*4+4" "K4"))

(defun %test-vsib-no-disp ()
  (assemble-instructions
   (vgatherdps-z (single-avx512-reg 0) (ea 0 (unsigned-reg rax-offset) (single-avx512-reg 1) 4) 1)))

;; Ensure zero displacement with base+index does not print "+0"
(test-util::define-evex-disasm-test
    :evex-vsib-no-disp
    sb-vm::%test-vsib-no-disp
  ("VGATHERDPS" "ZMM0" "ZMM1*4" "K1")
  :unexpected ("+0"))

;; ---- VGATHERDPS-Z-ZERO ----
(defun %test-vgatherdps-z-zero ()
  (assemble-instructions
   (vgatherdps-z-zero (single-avx512-reg 0) (ea 0 (unsigned-reg rax-offset) (single-avx512-reg 1) 4) 1)))

(test-util::define-evex-disasm-test
    :evex-vgatherdps-z-zero
    sb-vm::%test-vgatherdps-z-zero
  ("VGATHERDPS" "ZMM0" "ZMM1*4" "{K1}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VGETEXPPD ----
(defun %test-vgetexppd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vgetexppd-masked-z (double-avx512-reg 0) (ea 64 rsp-tn) 3)))

;; Zeroing get exponent double
(test-util::define-evex-disasm-test
    :evex-vgetexppd-masked-z-zmm-disp8
    sb-vm::%test-vgetexppd-masked-z-zmm-disp8
  ("VGETEXPPD" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VGETEXPPS ----
(defun %test-vgetexpps-masked-zmm-disp8 ()
  (assemble-instructions
   (vgetexpps-masked (single-avx512-reg 0) (ea 64 rsp-tn) 2)))

;; Masked get exponent
(test-util::define-evex-disasm-test
    :evex-vgetexpps-masked-zmm-disp8
    sb-vm::%test-vgetexpps-masked-zmm-disp8
  ("VGETEXPPS" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VGETEXPSD ----
(defun %test-vgetexpsd-disp8 ()
  (assemble-instructions
   (vgetexpsd (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn))))

;; Scalar getexp: double precision uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vgetexpsd-compressed-disp8
    sb-vm::%test-vgetexpsd-disp8
  ("VGETEXPSD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; ---- VGETEXPSS ----
(defun %test-vgetexpss-disp8 ()
  (assemble-instructions
   (vgetexpss (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn))))

;; Scalar getexp: single precision uses disp-n=4
(test-util::define-evex-disasm-test
    :evex-vgetexpss-compressed-disp8
    sb-vm::%test-vgetexpss-disp8
  ("VGETEXPSS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

(defun %test-vgetexpss-disp32 ()
  (assemble-instructions
   (vgetexpss (single-sse-reg 0) (single-sse-reg 1) (ea 5 rsp-tn))))

;; Scalar getexp: single precision non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vgetexpss-disp32-fallback
    sb-vm::%test-vgetexpss-disp32
  ("VGETEXPSS" "XMM0" "XMM1" "[RSP+5]"))

(defun %test-vgetexpss-masked-disp8 ()
  (assemble-instructions
   (vgetexpss-masked (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vgetexpss-masked-disp8
    sb-vm::%test-vgetexpss-masked-disp8
  ("VGETEXPSS" "XMM0" "XMM1" "[RSP+4]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VGETMANTPD ----
(defun %test-vgetmantpd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vgetmantpd-masked-z (double-avx512-reg 0) (ea 64 rsp-tn) 5 0)))

;; Zeroing get mantissa double
(test-util::define-evex-disasm-test
    :evex-vgetmantpd-masked-z-zmm-disp8
    sb-vm::%test-vgetmantpd-masked-z-zmm-disp8
  ("VGETMANTPD" "ZMM0" "[RSP+64]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VGETMANTPS ----
(defun %test-vgetmantps-masked-zmm-disp8 ()
  (assemble-instructions
   (vgetmantps-masked (single-avx512-reg 0) (ea 64 rsp-tn) 4 0)))

;; Masked get mantissa
(test-util::define-evex-disasm-test
    :evex-vgetmantps-masked-zmm-disp8
    sb-vm::%test-vgetmantps-masked-zmm-disp8
  ("VGETMANTPS" "ZMM0" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VGETMANTSD ----
(defun %test-vgetmantsd-disp8 ()
  (assemble-instructions
   (vgetmantsd (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn) 0)))

;; Scalar getmant: double precision uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vgetmantsd-compressed-disp8
    sb-vm::%test-vgetmantsd-disp8
  ("VGETMANTSD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; ---- VGETMANTSS ----
(defun %test-vgetmantss-disp8 ()
  (assemble-instructions
   (vgetmantss (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn) 0)))

;; Scalar getmant: single precision uses disp-n=4
(test-util::define-evex-disasm-test
    :evex-vgetmantss-compressed-disp8
    sb-vm::%test-vgetmantss-disp8
  ("VGETMANTSS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

(defun %test-vgetmantss-disp32 ()
  (assemble-instructions
   (vgetmantss (single-sse-reg 0) (single-sse-reg 1) (ea 5 rsp-tn) 0)))

;; Scalar getmant: single precision non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vgetmantss-disp32-fallback
    sb-vm::%test-vgetmantss-disp32
  ("VGETMANTSS" "XMM0" "XMM1" "[RSP+5]"))

(defun %test-vgetmantss-masked-disp8 ()
  (assemble-instructions
   (vgetmantss-masked (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn) 2 0)))

;; Masked scalar getmant
(test-util::define-evex-disasm-test
    :evex-vgetmantss-masked-disp8
    sb-vm::%test-vgetmantss-masked-disp8
  ("VGETMANTSS" "XMM0" "XMM1" "[RSP+4]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VGF2P8AFFINEINVQB ----
(defun %test-vgf2p8affineinvqb-masked-zmm-disp8 ()
  (assemble-instructions
   (vgf2p8affineinvqb-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 4 0)))

;; Masked GFNI affine INVQB
(test-util::define-evex-disasm-test
    :evex-vgf2p8affineinvqb-masked-zmm-disp8
    sb-vm::%test-vgf2p8affineinvqb-masked-zmm-disp8
  ("VGF2P8AFFINEINVQB" "ZMM0" "ZMM1" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VGF2P8AFFINEQB ----
(defun %test-vgf2p8affineqb-zmm-disp8 ()
  (assemble-instructions
   (vgf2p8affineqb (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 0)))

;; GFNI affine: full-vector, disp-n=64
(test-util::define-evex-disasm-test
    :evex-vgf2p8affineqb-zmm-compressed-disp8
    sb-vm::%test-vgf2p8affineqb-zmm-disp8
  ("VGF2P8AFFINEQB" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vgf2p8affineqb-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vgf2p8affineqb-masked-z (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 0)))

;; Zeroing GFNI affine QB
(test-util::define-evex-disasm-test
    :evex-vgf2p8affineqb-masked-z-zmm-disp8
    sb-vm::%test-vgf2p8affineqb-masked-z-zmm-disp8
  ("VGF2P8AFFINEQB" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VGF2P8MULB ----
(defun %test-vgf2p8mulb-zmm-disp8 ()
  (assemble-instructions
   (vgf2p8mulb (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; GFNI mulb: full-vector, disp-n=64
(test-util::define-evex-disasm-test
    :evex-vgf2p8mulb-zmm-compressed-disp8
    sb-vm::%test-vgf2p8mulb-zmm-disp8
  ("VGF2P8MULB" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vgf2p8mulb-masked-zmm-disp8 ()
  (assemble-instructions
   (vgf2p8mulb-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2)))

;; Masked GFNI multiply
(test-util::define-evex-disasm-test
    :evex-vgf2p8mulb-masked-zmm-disp8
    sb-vm::%test-vgf2p8mulb-masked-zmm-disp8
  ("VGF2P8MULB" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VINSERTF32X4 ----
(defun %test-vinsertf32x4-zmm-disp8 ()
  (assemble-instructions
   (vinsertf32x4 (single-avx512-reg 0) (single-avx512-reg 1) (ea 16 rsp-tn) 0)))

;; Insert 128-bit lane: disp-n=16
(test-util::define-evex-disasm-test
    :evex-vinsertf32x4-zmm-compressed-disp8
    sb-vm::%test-vinsertf32x4-zmm-disp8
  ("VINSERTF32X4" "ZMM0" "ZMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-vinsertf32x4-masked-zmm-disp8 ()
  (assemble-instructions
   (vinsertf32x4-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 16 rsp-tn) 2 0)))

;; Masked insert 128-bit lane single
(test-util::define-evex-disasm-test
    :evex-vinsertf32x4-masked-zmm-disp8
    sb-vm::%test-vinsertf32x4-masked-zmm-disp8
  ("VINSERTF32X4" "ZMM0" "ZMM1" "[RSP+16]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vinsertf32x4-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vinsertf32x4-masked-z (single-avx512-reg 0) (single-avx512-reg 1) (ea 16 rsp-tn) 3 0)))

;; Zeroing insert 128-bit lane single
(test-util::define-evex-disasm-test
    :evex-vinsertf32x4-masked-z-zmm-disp8
    sb-vm::%test-vinsertf32x4-masked-z-zmm-disp8
  ("VINSERTF32X4" "ZMM0" "ZMM1" "[RSP+16]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VINSERTF32X8 ----
(defun %test-vinsertf32x8-zmm-disp8 ()
  (assemble-instructions
   (vinsertf32x8 (single-avx512-reg 0) (single-avx512-reg 1) (ea 32 rsp-tn) 0)))

;; Insert 256-bit lane: disp-n=32
(test-util::define-evex-disasm-test
    :evex-vinsertf32x8-zmm-compressed-disp8
    sb-vm::%test-vinsertf32x8-zmm-disp8
  ("VINSERTF32X8" "ZMM0" "ZMM1" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; ---- VINSERTF64X2 ----
(defun %test-vinsertf64x2-masked-xmm-disp8 ()
  (assemble-instructions
   (vinsertf64x2-masked (double-sse-reg 0) (double-sse-reg 1) (ea 16 rsp-tn) 4 0)))

;; Masked insert 128-bit lane double
(test-util::define-evex-disasm-test
    :evex-vinsertf64x2-masked-xmm-disp8
    sb-vm::%test-vinsertf64x2-masked-xmm-disp8
  ("VINSERTF64X2" "XMM0" "XMM1" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

(defun %test-vinsertf64x2-masked-z-ymm-disp8 ()
  (assemble-instructions
   (vinsertf64x2-masked-z (double-avx2-reg 1) (double-avx2-reg 2) (ea 16 rsp-tn) 5 0)))

;; Zeroing insert 128-bit lane double YMM
(test-util::define-evex-disasm-test
    :evex-vinsertf64x2-masked-z-ymm-disp8
    sb-vm::%test-vinsertf64x2-masked-z-ymm-disp8
  ("VINSERTF64X2" "YMM1" "YMM2" "[RSP+16]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VINSERTI32X4 ----
(defun %test-vinserti32x4-masked-zmm-disp8 ()
  (assemble-instructions
   (vinserti32x4-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 16 rsp-tn) 6 0)))

;; Masked integer insert 128-bit lane
(test-util::define-evex-disasm-test
    :evex-vinserti32x4-masked-zmm-disp8
    sb-vm::%test-vinserti32x4-masked-zmm-disp8
  ("VINSERTI32X4" "ZMM0" "ZMM1" "[RSP+16]" "{K6}")
  :unexpected ("[RSP+1]"))

(defun %test-vinserti32x4-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vinserti32x4-masked-z (int-avx512-reg 0) (int-avx512-reg 1) (ea 16 rsp-tn) 7 0)))

;; Zeroing integer insert 128-bit lane
(test-util::define-evex-disasm-test
    :evex-vinserti32x4-masked-z-zmm-disp8
    sb-vm::%test-vinserti32x4-masked-z-zmm-disp8
  ("VINSERTI32X4" "ZMM0" "ZMM1" "[RSP+16]" "{K7}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VINSERTI32X8 ----
(defun %test-vinserti32x8-masked-ymm-disp8 ()
  (assemble-instructions
   (vinserti32x8-masked (int-avx2-reg 1) (int-avx2-reg 2) (ea 32 rsp-tn) 2 0)))

;; Masked integer insert 256-bit lane
(test-util::define-evex-disasm-test
    :evex-vinserti32x8-masked-ymm-disp8
    sb-vm::%test-vinserti32x8-masked-ymm-disp8
  ("VINSERTI32X8" "YMM1" "YMM2" "[RSP+32]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vinserti32x8-masked-z-ymm-disp8 ()
  (assemble-instructions
   (vinserti32x8-masked-z (int-avx2-reg 1) (int-avx2-reg 2) (ea 32 rsp-tn) 3 0)))

;; Zeroing integer insert 256-bit lane
(test-util::define-evex-disasm-test
    :evex-vinserti32x8-masked-z-ymm-disp8
    sb-vm::%test-vinserti32x8-masked-z-ymm-disp8
  ("VINSERTI32X8" "YMM1" "YMM2" "[RSP+32]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VMAXPD ----
(defun %test-vmaxpd-masked-zmm-disp8 ()
  (assemble-instructions
   (vmaxpd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 3)))

;; Masked max double
(test-util::define-evex-disasm-test
    :evex-vmaxpd-masked-zmm-disp8
    sb-vm::%test-vmaxpd-masked-zmm-disp8
  ("VMAXPD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}")
  :unexpected ("[RSP+1]"))

(defun %test-vmaxpd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vmaxpd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 5 :z)))

;; Zeroing max double
(test-util::define-evex-disasm-test
    :evex-vmaxpd-masked-z-zmm-disp8
    sb-vm::%test-vmaxpd-masked-z-zmm-disp8
  ("VMAXPD" "ZMM0" "ZMM1" "[RSP+64]" "{K5} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vmaxpd-bcast-zmm-disp8 ()
  (assemble-instructions
   (vmaxpd-bcast (double-avx512-reg 0) (double-avx512-reg 1) (ea 8 rsp-tn))))

;; Broadcast max double
(test-util::define-evex-disasm-test
    :evex-vmaxpd-bcast-zmm-disp8
    sb-vm::%test-vmaxpd-bcast-zmm-disp8
  ("VMAXPD-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VMINPS ----
(defun %test-vminps-masked-zmm-disp8 ()
  (assemble-instructions
   (vminps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 2)))

;; Masked min single
(test-util::define-evex-disasm-test
    :evex-vminps-masked-zmm-disp8
    sb-vm::%test-vminps-masked-zmm-disp8
  ("VMINPS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vminps-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vminps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 4 :z)))

;; Zeroing min single
(test-util::define-evex-disasm-test
    :evex-vminps-masked-z-zmm-disp8
    sb-vm::%test-vminps-masked-z-zmm-disp8
  ("VMINPS" "ZMM0" "ZMM1" "[RSP+64]" "{K4} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vminps-bcast-ymm-disp8 ()
  (assemble-instructions
   (vminps-bcast (single-avx2-reg 0) (single-avx2-reg 1) (ea 4 rsp-tn))))

;; Broadcast min single
(test-util::define-evex-disasm-test
    :evex-vminps-bcast-ymm-disp8
    sb-vm::%test-vminps-bcast-ymm-disp8
  ("VMINPS-BCAST" "YMM0" "YMM1" "[RSP+4]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VMOVAPS ----
(defun %test-auto-promoted-vmovaps-disp8 ()
  (assemble-instructions
   (vmovaps (single-avx512-reg 0) (ea 64 rsp-tn))))

;; Auto-promoted VMOVAPS with compressed disp8
(test-util::define-evex-disasm-test
    :auto-promoted-vmovaps-disp8-disasm
    sb-vm::%test-auto-promoted-vmovaps-disp8
  ("VMOVAPS" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-auto-promoted-vmovaps-disp-nonmultiple ()
  (assemble-instructions
   (vmovaps (single-avx512-reg 0) (ea 65 rsp-tn))))

;; Auto-promoted VMOVAPS falls back to disp32 for non-multiple
(test-util::define-evex-disasm-test
    :auto-promoted-vmovaps-disp-nonmultiple-disasm
    sb-vm::%test-auto-promoted-vmovaps-disp-nonmultiple
  ("VMOVAPS" "ZMM0" "[RSP+65]"))

;; ---- VMOVDQA ----
(defun %test-vmovdqa-zmm-disasm ()
  (assemble-instructions
   (vmovdqa (single-avx512-reg 0) (ea 64 rsp-tn))))

;; vmovdqa skip: explicit VMOVDQA32 should be used
(test-util::define-evex-disasm-test
    :auto-evex-skip-vmovdqa
    sb-vm::%test-vmovdqa-zmm-disasm
  ("VMOVDQA32" "ZMM0" "[RSP+64]")
  :unexpected ("VMOVDQA "))

;; ---- VMOVDQU ----
(defun %test-auto-promoted-vmovdqu ()
  (assemble-instructions
   (vmovdqu (single-avx512-reg 0) (ea 64 rsp-tn))))

;; VMOVDQU skip prevents conflict with explicit VMOVDQU32
(test-util::define-evex-disasm-test
    :auto-promoted-vmovdqu-skip-conflict-disasm
    sb-vm::%test-auto-promoted-vmovdqu
  ("VMOVDQU32" "ZMM0" "[RSP+64]"))

;; ---- VMOVDQU64 ----
(defun %test-evex-disp8 ()
  (assemble-instructions
   (vmovdqu64 (single-avx512-reg 0) (ea 64 rsp-tn))))

;; ZMM full-vector compressed displacement: disp8*64
(test-util::define-evex-disasm-test
    :evex-compressed-displacement
    sb-vm::%test-evex-disp8
  ("VMOVDQU64 ZMM0, [RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-evex-disp-vector-lengths ()
  (assemble-instructions
   (vmovdqu64 (single-sse-reg 0) (ea 16 rsp-tn))
   (vmovdqu64 (single-avx2-reg 1) (ea 32 rsp-tn))
   (vmovdqu64 (single-avx512-reg 2) (ea 64 rsp-tn))))

;; Full-vector compressed displacement at XMM/YMM/ZMM widths
(test-util::define-evex-disasm-test
    :evex-compressed-displacement-vector-lengths
    sb-vm::%test-evex-disp-vector-lengths
  ("VMOVDQU64 XMM0, [RSP+16]"
   "VMOVDQU64 YMM1, [RSP+32]"
   "VMOVDQU64 ZMM2, [RSP+64]"))

(defun %test-evex-disp-negative ()
  (assemble-instructions
   (vmovdqu64 (single-avx512-reg 0) (ea -64 rsp-tn))))

;; Negative compressed displacement
(test-util::define-evex-disasm-test
    :evex-compressed-displacement-negative
    sb-vm::%test-evex-disp-negative
  ("VMOVDQU64 ZMM0, [RSP-64]"))

(defun %test-evex-disp-nonmultiple ()
  (assemble-instructions
   (vmovdqu64 (single-avx512-reg 0) (ea 65 rsp-tn))))

;; Non-multiple displacement falls back to disp32
(test-util::define-evex-disasm-test
    :evex-compressed-displacement-nonmultiple
    sb-vm::%test-evex-disp-nonmultiple
  ("VMOVDQU64 ZMM0, [RSP+65]"))

(defun %test-evex-disp-large ()
  (assemble-instructions
   (vmovdqu64 (single-avx512-reg 0) (ea 8192 rsp-tn))))

;; Too large for disp8 falls back to disp32
(test-util::define-evex-disasm-test
    :evex-compressed-displacement-large
    sb-vm::%test-evex-disp-large
  ("VMOVDQU64 ZMM0, [RSP+8192]"))

;; ---- VMULPS ----
(defun %test-vmulps-bcast-ymm-disp8 ()
  (assemble-instructions
   (vmulps-bcast (single-avx2-reg 1) (single-avx2-reg 2) (ea 4 rsp-tn))))

;; Embedded broadcast: vmulps YMM {1to8}
(test-util::define-evex-disasm-test
    :evex-vmulps-bcast-ymm
    sb-vm::%test-vmulps-bcast-ymm-disp8
  ("VMULPS-BCAST" "YMM1" "YMM2" "[RSP+4]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VP2INTERSECTD ----
(defun %test-vp2intersectd ()
  (assemble-instructions
   (vp2intersectd (mask-reg 1) (mask-reg 2) (int-avx512-reg 0) (ea 64 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vp2intersectd
    sb-vm::%test-vp2intersectd
  ("VP2INTERSECTD" "K1" "K2" "ZMM0" "[RSP+64]"))

;; ---- VP2INTERSECTQ ----
(defun %test-vp2intersectq ()
  (assemble-instructions
   (vp2intersectq (mask-reg 3) (mask-reg 4) (int-avx512-reg 2) (ea 64 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vp2intersectq
    sb-vm::%test-vp2intersectq
  ("VP2INTERSECTQ" "K3" "K4" "ZMM2" "[RSP+64]"))

;; ---- VPABSQ ----
(defun %test-vpabsq-zmm-disp8 ()
  (assemble-instructions
   (vpabsq (int-avx512-reg 3) (ea 64 rsp-tn))))

;; Integer full-vector group with W=1: vpabsq
(test-util::define-evex-disasm-test
    :evex-vpabsq-zmm-compressed-disp8
    sb-vm::%test-vpabsq-zmm-disp8
  ("VPABSQ" "ZMM3" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpabsq-masked-zmm-disp8 ()
  (assemble-instructions
   (vpabsq-masked (int-avx512-reg 0) (ea 64 rsp-tn) 6)))

;; Masked absolute value qword
(test-util::define-evex-disasm-test
    :evex-vpabsq-masked-zmm-disp8
    sb-vm::%test-vpabsq-masked-zmm-disp8
  ("VPABSQ" "ZMM0" "[RSP+64]" "{K6}")
  :unexpected ("[RSP+1]"))

;; ---- VPADDB ----
(defun %test-vpaddb-masked-zmm-disp8 ()
  (assemble-instructions
   (vpaddb-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vpaddb-masked-zmm-disp8
    sb-vm::%test-vpaddb-masked-zmm-disp8
  ("VPADDB" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VPADDD ----
(defun %test-vpaddd-bcast-zmm-disp8 ()
  (assemble-instructions
   (vpaddd-bcast (int-avx512-reg 0) (int-avx512-reg 1) (ea 4 rsp-tn))))

;; Broadcast integer add dword
(test-util::define-evex-disasm-test
    :evex-vpaddd-bcast-zmm-disp8
    sb-vm::%test-vpaddd-bcast-zmm-disp8
  ("VPADDD-BCAST" "ZMM0" "ZMM1" "[RSP+4]" "{1to16}")
  :unexpected ("[RSP+1]"))

;; ---- VPADDQ ----
(defun %test-vpaddq-high-ymm-disasm ()
  (assemble-instructions
   (vpaddq (int-avx2-reg 20) (int-avx2-reg 21) (int-avx2-reg 22))))

;; The same extended-register-number fix, exercised at YMM width instead
;; of XMM, using three distinct registers (20, 21, 22).
(test-util::define-evex-disasm-test
    :evex-high-register-ymm-vpaddq
    sb-vm::%test-vpaddq-high-ymm-disasm
  ("VPADDQ" "YMM20" "YMM21" "YMM22"))

(defun %test-vpaddq-masked-zmm-disp8 ()
  (assemble-instructions
   (vpaddq-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 1)))

;; Masked integer arithmetic: vpaddq-masked ZMM, W=1 -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpaddq-masked-zmm-compressed-disp8
    sb-vm::%test-vpaddq-masked-zmm-disp8
  ("VPADDQ" "ZMM0" "ZMM1" "[RSP+64]" "{K1}")
  :unexpected ("[RSP+1]"))

(defun %test-vpaddq-masked-z-disp8 ()
  (assemble-instructions
   (vpaddq-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 1 :z)))

;; Zeroing masked integer arithmetic: qword
(test-util::define-evex-disasm-test
    :evex-vpaddq-masked-z-compressed-disp8
    sb-vm::%test-vpaddq-masked-z-disp8
  ("VPADDQ" "ZMM0" "ZMM1" "[RSP+64]" "{K1} {z}")
  :unexpected ("[RSP+1]"))

(defun %test-vpaddq-bcast-zmm-disp8 ()
  (assemble-instructions
   (vpaddq-bcast (int-avx512-reg 0) (int-avx512-reg 1) (ea 8 rsp-tn))))

;; Broadcast integer add qword
(test-util::define-evex-disasm-test
    :evex-vpaddq-bcast-zmm-disp8
    sb-vm::%test-vpaddq-bcast-zmm-disp8
  ("VPADDQ-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VPAND ----
(defun %test-vpand-zmm-disasm ()
  (assemble-instructions
   (vpand (int-avx512-reg 0) (int-avx512-reg 1) (int-avx512-reg 2))))

;; vpand skip: explicit VPANDD should be used, not auto-promoted VPAND
(test-util::define-evex-disasm-test
    :auto-evex-skip-vpand
    sb-vm::%test-vpand-zmm-disasm
  ("VPANDD" "ZMM0" "ZMM1" "ZMM2")
  :unexpected ("VPAND "))

(defun %test-vpand-high-xmm-disasm ()
  (assemble-instructions
   (vpand (complex-double-reg 30) (complex-double-reg 30) (complex-double-reg 30))))

;; Same idea for a plain 3-register ALU op: VPAND auto-promotes to the
;; explicit EVEX VPANDD encoding once any operand needs EVEX for any reason,
;; not just because the vector is ZMM-wide.
(test-util::define-evex-disasm-test
    :evex-high-register-xmm-vpand
    sb-vm::%test-vpand-high-xmm-disasm
  ("VPANDD" "XMM30" "XMM30" "XMM30")
  :unexpected ("VPAND "))

;; ---- VPANDD ----
(defun %test-vpandd-bcast-xmm-disp8 ()
  (assemble-instructions
   (vpandd-bcast (int-sse-reg 0) (int-sse-reg 1) (ea 4 rsp-tn))))

;; Broadcast logical and dword
(test-util::define-evex-disasm-test
    :evex-vpandd-bcast-xmm-disp8
    sb-vm::%test-vpandd-bcast-xmm-disp8
  ("VPANDD-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VPBLENDMB ----
(defun %test-vpblendmb-zmm-disp8 ()
  (assemble-instructions
   (vpblendmb (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; Blend byte: ZMM full-vector -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpblendmb-zmm-compressed-disp8
    sb-vm::%test-vpblendmb-zmm-disp8
  ("VPBLENDMB" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VPBLENDMQ ----
(defun %test-vpblendmq-zmm-disp8 ()
  (assemble-instructions
   (vpblendmq (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; Blend with mask (integer): vpblendmq ZMM, W=1 -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpblendmq-zmm-compressed-disp8
    sb-vm::%test-vpblendmq-zmm-disp8
  ("VPBLENDMQ" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VPBROADCASTQ ----
(defun %test-auto-promoted-vpbroadcastq ()
  (assemble-instructions
   (vpbroadcastq (int-avx512-reg 0) (ea 64 rsp-tn))))

;; Auto-promoted VPBROADCASTQ uses W=1 and compressed disp8
(test-util::define-evex-disasm-test
    :auto-promoted-evex-vpbroadcastq-disasm
    sb-vm::%test-auto-promoted-vpbroadcastq
  ("VPBROADCASTQ" "ZMM0" "[RSP+64]"))

;; ---- VPCMPB ----
(defun %test-vpcmpb-zmm-disp8 ()
  (assemble-instructions
   (vpcmpb (mask-reg 1) (int-avx512-reg 0) (ea 64 rsp-tn) #x1)))

;; Compare byte to k: ZMM full-vector -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpcmpb-zmm-compressed-disp8
    sb-vm::%test-vpcmpb-zmm-disp8
  ("VPCMPB" "K1" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VPCMPD ----
(defun %test-evex-vpcmpd ()
  (assemble-instructions
   (vpcmpd (mask-reg 1) (int-avx512-reg 0) (int-avx512-reg 1) #x1)))

;; Explicit EVEX: VPCMPD with opmask
(test-util::define-evex-disasm-test
    :evex-explicit-vpcmpd-disasm
    sb-vm::%test-evex-vpcmpd
  ("VPCMPD" "K1" "ZMM0" "ZMM1"))

(defun %test-vpcmpd-disp8 ()
  (assemble-instructions
   (vpcmpd (mask-reg 1) (int-avx512-reg 0) (ea 64 rsp-tn) #x1)))

;; Compare-to-k with compressed disp8
(test-util::define-evex-disasm-test
    :evex-vpcmpd-compressed-disp8
    sb-vm::%test-vpcmpd-disp8
  ("VPCMPD" "K1" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpcmpd-disp32 ()
  (assemble-instructions
   (vpcmpd (mask-reg 1) (int-avx512-reg 0) (ea 65 rsp-tn) #x1)))

;; Compare-to-k falls back to disp32 for non-multiple
(test-util::define-evex-disasm-test
    :evex-vpcmpd-disp32-fallback
    sb-vm::%test-vpcmpd-disp32
  ("VPCMPD" "K1" "ZMM0" "[RSP+65]"))

;; ---- VPCOMPRESSB ----
(defun %test-vpcompressb-zmm-disp8 ()
  (assemble-instructions
   (vpcompressb (ea 64 rsp-tn) (int-avx512-reg 0))))

;; VBMI2 compress: vpcompressb ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpcompressb-zmm-compressed-disp8
    sb-vm::%test-vpcompressb-zmm-disp8
  ("VPCOMPRESSB" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VPCOMPRESSD ----
(defun %test-vpcompressd-masked-xmm-disp8 ()
  (assemble-instructions
   (vpcompressd-masked (ea 16 rsp-tn) (int-sse-reg 0) 4)))

;; Masked compress dword
(test-util::define-evex-disasm-test
    :evex-vpcompressd-masked-xmm-disp8
    sb-vm::%test-vpcompressd-masked-xmm-disp8
  ("VPCOMPRESSD" "[RSP+16]" "XMM0" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VPCONFLICTD ----
(defun %test-vpconflictd-zmm-disp8 ()
  (assemble-instructions
   (vpconflictd (int-avx512-reg 0) (ea 64 rsp-tn))))

;; VPCONFLICTD ZMM full-vector
(test-util::define-evex-disasm-test
    :evex-vpconflictd-zmm-compressed-disp8
    sb-vm::%test-vpconflictd-zmm-disp8
  ("VPCONFLICTD" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpconflictd-masked-zmm-disp8 ()
  (assemble-instructions
   (vpconflictd-masked (int-avx512-reg 0) (ea 64 rsp-tn) 2)))

;; Masked conflict detection dword
(test-util::define-evex-disasm-test
    :evex-vpconflictd-masked-zmm-disp8
    sb-vm::%test-vpconflictd-masked-zmm-disp8
  ("VPCONFLICTD" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VPCONFLICTQ ----
(defun %test-vpconflictq-zmm-disp8 ()
  (assemble-instructions
   (vpconflictq (int-avx512-reg 0) (ea 64 rsp-tn))))

;; VPCONFLICTQ ZMM full-vector, W=1
(test-util::define-evex-disasm-test
    :evex-vpconflictq-zmm-compressed-disp8
    sb-vm::%test-vpconflictq-zmm-disp8
  ("VPCONFLICTQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpconflictq-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpconflictq-masked (int-avx512-reg 0) (ea 64 rsp-tn) 3 :z)))

;; Zeroing conflict detection qword
(test-util::define-evex-disasm-test
    :evex-vpconflictq-masked-z-zmm-disp8
    sb-vm::%test-vpconflictq-masked-z-zmm-disp8
  ("VPCONFLICTQ" "ZMM0" "[RSP+64]" "{K3} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VPDPBUSD ----
(defun %test-vpdpbusd-zmm-disp8 ()
  (assemble-instructions
   (vpdpbusd (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; VPDPBUSD ZMM full-vector
(test-util::define-evex-disasm-test
    :evex-vpdpbusd-zmm-compressed-disp8
    sb-vm::%test-vpdpbusd-zmm-disp8
  ("VPDPBUSD" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpdpbusd-masked-zmm-disp8 ()
  (assemble-instructions
   (vpdpbusd-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vpdpbusd-masked-zmm-disp8
    sb-vm::%test-vpdpbusd-masked-zmm-disp8
  ("VPDPBUSD" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vpdpbusd-bcast-xmm-disp8 ()
  (assemble-instructions
   (vpdpbusd-bcast (int-sse-reg 0) (int-sse-reg 1) (ea 4 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vpdpbusd-bcast-xmm-disp8
    sb-vm::%test-vpdpbusd-bcast-xmm-disp8
  ("VPDPBUSD-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VPDPBUSDS ----
(defun %test-vpdpbusds-zmm-disp8 ()
  (assemble-instructions
   (vpdpbusds (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; VPDPBUSDS ZMM full-vector
(test-util::define-evex-disasm-test
    :evex-vpdpbusds-zmm-compressed-disp8
    sb-vm::%test-vpdpbusds-zmm-disp8
  ("VPDPBUSDS" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VPDPWSSD ----
(defun %test-vpdpwssd-xmm-disp8 ()
  (assemble-instructions
   (vpdpwssd (int-sse-reg 0) (int-sse-reg 1) (ea 16 rsp-tn))))

;; VPDPWSSD XMM full-vector
(test-util::define-evex-disasm-test
    :evex-vpdpwssd-xmm-compressed-disp8
    sb-vm::%test-vpdpwssd-xmm-disp8
  ("VPDPWSSD" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-vpdpwssd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpdpwssd-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 :z)))

(test-util::define-evex-disasm-test
    :evex-vpdpwssd-masked-z-zmm-disp8
    sb-vm::%test-vpdpwssd-masked-z-zmm-disp8
  ("VPDPWSSD" "ZMM0" "ZMM1" "[RSP+64]" "{K3} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VPDPWSSDS ----
(defun %test-vpdpwssds-zmm-disp32 ()
  (assemble-instructions
   (vpdpwssds (int-avx512-reg 0) (int-avx512-reg 1) (ea 65 rsp-tn))))

;; VPDPWSSDS ZMM with disp32 fallback
(test-util::define-evex-disasm-test
    :evex-vpdpwssds-zmm-disp32-fallback
    sb-vm::%test-vpdpwssds-zmm-disp32
  ("VPDPWSSDS" "ZMM0" "ZMM1" "[RSP+65]"))

;; ---- VPERMB ----
(defun %test-vpermb-zmm-disp8 ()
  (assemble-instructions
   (vpermb (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; VBMI permute: vpermb ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpermb-zmm-compressed-disp8
    sb-vm::%test-vpermb-zmm-disp8
  ("VPERMB" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpermb-masked-zmm-disp8 ()
  (assemble-instructions
   (vpermb-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 4)))

(test-util::define-evex-disasm-test
    :evex-vpermb-masked-zmm-disp8
    sb-vm::%test-vpermb-masked-zmm-disp8
  ("VPERMB" "ZMM0" "ZMM1" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VPERMT2D ----
(defun %test-evex-vpermt2d ()
  (assemble-instructions
   (vpermt2d (int-avx512-reg 0) (int-avx512-reg 1) (int-avx512-reg 2))))

;; Explicit EVEX: VPERMT2D
(test-util::define-evex-disasm-test
    :evex-explicit-vpermt2d-disasm
    sb-vm::%test-evex-vpermt2d
  ("VPERMT2D" "ZMM0" "ZMM1" "ZMM2"))

(defun %test-vpermt2d-zmm-disp8 ()
  (assemble-instructions
   (vpermt2d (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn))))

;; Full-vector 3-operand NDS: vpermt2d ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpermt2d-zmm-compressed-disp8
    sb-vm::%test-vpermt2d-zmm-disp8
  ("VPERMT2D" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpermt2d-zmm-disp32 ()
  (assemble-instructions
   (vpermt2d (single-avx512-reg 0) (single-avx512-reg 1) (ea 65 rsp-tn))))

;; Full-vector 3-operand NDS: vpermt2d disp32 fallback
(test-util::define-evex-disasm-test
    :evex-vpermt2d-zmm-disp32-fallback
    sb-vm::%test-vpermt2d-zmm-disp32
  ("VPERMT2D" "ZMM0" "ZMM1" "[RSP+65]"))

(defun %test-vpermt2d-masked-zmm-disp8 ()
  (assemble-instructions
   (vpermt2d-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vpermt2d-masked-zmm-disp8
    sb-vm::%test-vpermt2d-masked-zmm-disp8
  ("VPERMT2D" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VPERMT2Q ----
(defun %test-vpermt2q-zmm-disp8 ()
  (assemble-instructions
   (vpermt2q (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; Full-vector 3-operand NDS: vpermt2q ZMM, W=1 -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpermt2q-zmm-compressed-disp8
    sb-vm::%test-vpermt2q-zmm-disp8
  ("VPERMT2Q" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpermt2q-zmm-disp32 ()
  (assemble-instructions
   (vpermt2q (int-avx512-reg 0) (int-avx512-reg 1) (ea 65 rsp-tn))))

;; vpermt2q disp32 fallback
(test-util::define-evex-disasm-test
    :evex-vpermt2q-zmm-disp32-fallback
    sb-vm::%test-vpermt2q-zmm-disp32
  ("VPERMT2Q" "ZMM0" "ZMM1" "[RSP+65]"))

(defun %test-vpermt2q-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpermt2q-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 :z)))

(test-util::define-evex-disasm-test
    :evex-vpermt2q-masked-z-zmm-disp8
    sb-vm::%test-vpermt2q-masked-z-zmm-disp8
  ("VPERMT2Q" "ZMM0" "ZMM1" "[RSP+64]" "{K3} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VPERMW ----
(defun %test-vpermw-zmm-disp8 ()
  (assemble-instructions
   (vpermw (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; Permute word: ZMM full-vector -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpermw-zmm-compressed-disp8
    sb-vm::%test-vpermw-zmm-disp8
  ("VPERMW" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpermw-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpermw-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 5 :z)))

(test-util::define-evex-disasm-test
    :evex-vpermw-masked-z-zmm-disp8
    sb-vm::%test-vpermw-masked-z-zmm-disp8
  ("VPERMW" "ZMM0" "ZMM1" "[RSP+64]" "{K5} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VPEXPANDD ----
(defun %test-vpexpandd-masked-xmm-disp8 ()
  (assemble-instructions
   (vpexpandd-masked (int-sse-reg 0) (ea 16 rsp-tn) 4)))

;; Masked expand dword
(test-util::define-evex-disasm-test
    :evex-vpexpandd-masked-xmm-disp8
    sb-vm::%test-vpexpandd-masked-xmm-disp8
  ("VPEXPANDD" "XMM0" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VPEXPANDQ ----
(defun %test-expand-disp8 ()
  (assemble-instructions
   (vpexpandq (int-avx512-reg 0) (ea 64 rsp-tn))))

;; Expand: VPEXPANDQ uses disp-n=64
(test-util::define-evex-disasm-test
    :evex-expand-compressed-disp8
    sb-vm::%test-expand-disp8
  ("VPEXPANDQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-expand-disp32 ()
  (assemble-instructions
   (vpexpandq (int-avx512-reg 0) (ea 65 rsp-tn))))

;; Expand: disp32 fallback
(test-util::define-evex-disasm-test
    :evex-expand-disp32-fallback
    sb-vm::%test-expand-disp32
  ("VPEXPANDQ" "ZMM0" "[RSP+65]"))

;; ---- VPGATHERQQ ----
(defun %test-vsib-qword-disp8-compressed ()
  (assemble-instructions
   (vpgatherqq-z (int-avx512-reg 0) (ea 8 (unsigned-reg rbx-offset) (int-avx512-reg 2) 8) 2)))

;; Qword gather uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vsib-qword-disp8-compressed
    sb-vm::%test-vsib-qword-disp8-compressed
  ("VPGATHERQQ" "ZMM0" "ZMM2*8+8" "K2"))

;; ---- VPINSRQ ----
(defun %test-vpinsrq-high-xmm-disasm ()
  (assemble-instructions
   (vpinsrq (complex-double-reg 30) (complex-double-reg 30) (unsigned-reg rax-offset) 1)))

;; VPINSRQ mixes two XMM operands with a plain GPR operand and an immediate.
;; The GPR (RAX) must decode normally -- it must not be pulled into the
;; extended vector-register-numbering path meant for the XMM operands.
(test-util::define-evex-disasm-test
    :evex-high-register-xmm-vpinsrq
    sb-vm::%test-vpinsrq-high-xmm-disasm
  ("VPINSRQ" "XMM30" "XMM30" "RAX" "1"))

;; ---- VPLZCNTD ----
(defun %test-vplzcntd-xmm-disp8 ()
  (assemble-instructions
   (vplzcntd (int-sse-reg 0) (ea 16 rsp-tn))))

;; VPLZCNTD XMM full-vector
(test-util::define-evex-disasm-test
    :evex-vplzcntd-xmm-compressed-disp8
    sb-vm::%test-vplzcntd-xmm-disp8
  ("VPLZCNTD" "XMM0" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-vplzcntd-masked-xmm-disp8 ()
  (assemble-instructions
   (vplzcntd-masked (int-sse-reg 0) (ea 16 rsp-tn) 4)))

;; Masked leading zero count dword
(test-util::define-evex-disasm-test
    :evex-vplzcntd-masked-xmm-disp8
    sb-vm::%test-vplzcntd-masked-xmm-disp8
  ("VPLZCNTD" "XMM0" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VPLZCNTQ ----
(defun %test-vplzcntq-zmm-disp8 ()
  (assemble-instructions
   (vplzcntq (int-avx512-reg 0) (ea 64 rsp-tn))))

;; VPLZCNTQ ZMM full-vector, W=1
(test-util::define-evex-disasm-test
    :evex-vplzcntq-zmm-compressed-disp8
    sb-vm::%test-vplzcntq-zmm-disp8
  ("VPLZCNTQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vplzcntq-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vplzcntq-masked (int-avx512-reg 0) (ea 64 rsp-tn) 5 :z)))

;; Zeroing leading zero count qword
(test-util::define-evex-disasm-test
    :evex-vplzcntq-masked-z-zmm-disp8
    sb-vm::%test-vplzcntq-masked-z-zmm-disp8
  ("VPLZCNTQ" "ZMM0" "[RSP+64]" "{K5} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VPMADD52LUQ ----
(defun %test-vpmadd52luq-zmm-disp8 ()
  (assemble-instructions
   (vpmadd52luq (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; IFMA: vpmadd52luq ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpmadd52luq-zmm-compressed-disp8
    sb-vm::%test-vpmadd52luq-zmm-disp8
  ("VPMADD52LUQ" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpmadd52luq-masked-zmm-disp8 ()
  (assemble-instructions
   (vpmadd52luq-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vpmadd52luq-masked-zmm-disp8
    sb-vm::%test-vpmadd52luq-masked-zmm-disp8
  ("VPMADD52LUQ" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VPMAXSD ----
(defun %test-vpmaxsd-bcast-xmm-disp8 ()
  (assemble-instructions
   (vpmaxsd-bcast (int-sse-reg 0) (int-sse-reg 1) (ea 4 rsp-tn))))

;; Broadcast signed dword max
(test-util::define-evex-disasm-test
    :evex-vpmaxsd-bcast-xmm-disp8
    sb-vm::%test-vpmaxsd-bcast-xmm-disp8
  ("VPMAXSD-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VPMAXSQ ----
(defun %test-vpmaxsq-zmm-disp8 ()
  (assemble-instructions
   (vpmaxsq (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; Full-vector 3-operand NDS: vpmaxsq ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpmaxsq-zmm-compressed-disp8
    sb-vm::%test-vpmaxsq-zmm-disp8
  ("VPMAXSQ" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpmaxsq-masked-zmm-disp8 ()
  (assemble-instructions
   (vpmaxsq-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3)))

;; Masked signed qword max
(test-util::define-evex-disasm-test
    :evex-vpmaxsq-masked-zmm-disp8
    sb-vm::%test-vpmaxsq-masked-zmm-disp8
  ("VPMAXSQ" "ZMM0" "ZMM1" "[RSP+64]" "{K3}")
  :unexpected ("[RSP+1]"))

(defun %test-vpmaxsq-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpmaxsq-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 5 :z)))

;; Zeroing signed qword max
(test-util::define-evex-disasm-test
    :evex-vpmaxsq-masked-z-zmm-disp8
    sb-vm::%test-vpmaxsq-masked-z-zmm-disp8
  ("VPMAXSQ" "ZMM0" "ZMM1" "[RSP+64]" "{K5} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VPMAXUQ ----
(defun %test-vpmaxuq-bcast-zmm-disp8 ()
  (assemble-instructions
   (vpmaxuq-bcast (int-avx512-reg 0) (int-avx512-reg 1) (ea 8 rsp-tn))))

;; Broadcast unsigned qword max
(test-util::define-evex-disasm-test
    :evex-vpmaxuq-bcast-zmm-disp8
    sb-vm::%test-vpmaxuq-bcast-zmm-disp8
  ("VPMAXUQ-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VPMINSD ----
(defun %test-vpminsd-bcast-zmm-disp8 ()
  (assemble-instructions
   (vpminsd-bcast (int-avx512-reg 0) (int-avx512-reg 1) (ea 4 rsp-tn))))

;; Broadcast signed dword min
(test-util::define-evex-disasm-test
    :evex-vpminsd-bcast-zmm-disp8
    sb-vm::%test-vpminsd-bcast-zmm-disp8
  ("VPMINSD-BCAST" "ZMM0" "ZMM1" "[RSP+4]" "{1to16}")
  :unexpected ("[RSP+1]"))

(defun %test-vpminsd-masked-zmm-disp8 ()
  (assemble-instructions
   (vpminsd-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2)))

;; Masked signed dword min
(test-util::define-evex-disasm-test
    :evex-vpminsd-masked-zmm-disp8
    sb-vm::%test-vpminsd-masked-zmm-disp8
  ("VPMINSD" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vpminsd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpminsd-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 4 :z)))

;; Zeroing signed dword min
(test-util::define-evex-disasm-test
    :evex-vpminsd-masked-z-zmm-disp8
    sb-vm::%test-vpminsd-masked-z-zmm-disp8
  ("VPMINSD" "ZMM0" "ZMM1" "[RSP+64]" "{K4} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VPMINUD ----
(defun %test-vpminud-bcast-ymm-disp8 ()
  (assemble-instructions
   (vpminud-bcast (int-avx2-reg 1) (int-avx2-reg 2) (ea 4 rsp-tn))))

;; Broadcast unsigned dword min
(test-util::define-evex-disasm-test
    :evex-vpminud-bcast-ymm-disp8
    sb-vm::%test-vpminud-bcast-ymm-disp8
  ("VPMINUD-BCAST" "YMM1" "YMM2" "[RSP+4]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VPMOVQD ----
(defun %test-evex-vpmovqd ()
  (assemble-instructions
   (vpmovqd (ea 32 rsp-tn) (int-avx512-reg 0))))

;; Explicit EVEX: VPMOVQD reversed store with compressed disp
(test-util::define-evex-disasm-test
    :evex-explicit-vpmovqd-disasm
    sb-vm::%test-evex-vpmovqd
  ("VPMOVQD" "[RSP+32]" "ZMM0"))

(defun %test-vpmovqd-xmm-disp8 ()
  (assemble-instructions
   (vpmovqd (ea 8 rsp-tn) (single-sse-reg 0))))

;; Down-convert store: XMM source has disp-n=8
(test-util::define-evex-disasm-test
    :evex-vpmovqd-xmm-compressed-disp8
    sb-vm::%test-vpmovqd-xmm-disp8
  ("VPMOVQD" "XMM0" "[RSP+8]")
  :unexpected ("[RSP+1]"))

(defun %test-vpmovqd-ymm-disp8 ()
  (assemble-instructions
   (vpmovqd (ea 16 rsp-tn) (single-avx2-reg 1))))

;; Down-convert store: YMM source has disp-n=16
(test-util::define-evex-disasm-test
    :evex-vpmovqd-ymm-compressed-disp8
    sb-vm::%test-vpmovqd-ymm-disp8
  ("VPMOVQD" "YMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-vpmovqd-zmm-disp8 ()
  (assemble-instructions
   (vpmovqd (ea 32 rsp-tn) (int-avx512-reg 2))))

;; Down-convert store: ZMM source has disp-n=32
(test-util::define-evex-disasm-test
    :evex-vpmovqd-zmm-compressed-disp8
    sb-vm::%test-vpmovqd-zmm-disp8
  ("VPMOVQD" "ZMM2" "[RSP+32]")
  :unexpected ("[RSP+1]"))

(defun %test-vpmovqd-zmm-disp32 ()
  (assemble-instructions
   (vpmovqd (ea 33 rsp-tn) (int-avx512-reg 2))))

;; Down-convert store: ZMM falls back to disp32 for non-multiple
(test-util::define-evex-disasm-test
    :evex-vpmovqd-zmm-disp32-fallback
    sb-vm::%test-vpmovqd-zmm-disp32
  ("VPMOVQD" "ZMM2" "[RSP+33]"))

(defun %test-vpmovqd-masked-zmm-disp8 ()
  (assemble-instructions
   (vpmovqd-masked (ea 32 rsp-tn) (int-avx512-reg 0) 2)))

;; Masked down-convert store: vpmovqd ZMM
(test-util::define-evex-disasm-test
    :evex-vpmovqd-masked-zmm-disp8
    sb-vm::%test-vpmovqd-masked-zmm-disp8
  ("VPMOVQD" "[RSP+32]" "ZMM0" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vpmovqd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpmovqd-masked-z (ea 32 rsp-tn) (int-avx512-reg 0) 3)))

;; Zeroing down-convert store: vpmovqd ZMM
(test-util::define-evex-disasm-test
    :evex-vpmovqd-masked-z-zmm-disp8
    sb-vm::%test-vpmovqd-masked-z-zmm-disp8
  ("VPMOVQD" "[RSP+32]" "ZMM0" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VPMOVSQB ----
(defun %test-vpmovsqb-xmm-disp8 ()
  (assemble-instructions
   (vpmovsqb (ea 2 rsp-tn) (single-sse-reg 0))))

;; Saturating truncation: XMM source, byte result -> disp-n=2
(test-util::define-evex-disasm-test
    :evex-vpmovsqb-xmm-compressed-disp8
    sb-vm::%test-vpmovsqb-xmm-disp8
  ("VPMOVSQB" "XMM0" "[RSP+2]")
  :unexpected ("[RSP+1]"))

(defun %test-vpmovsqb-ymm-disp8 ()
  (assemble-instructions
   (vpmovsqb (ea 4 rsp-tn) (int-avx2-reg 1))))

;; Saturating truncation: YMM source, byte result -> disp-n=4
(test-util::define-evex-disasm-test
    :evex-vpmovsqb-ymm-compressed-disp8
    sb-vm::%test-vpmovsqb-ymm-disp8
  ("VPMOVSQB" "YMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

(defun %test-vpmovsqb-zmm-disp8 ()
  (assemble-instructions
   (vpmovsqb (ea 8 rsp-tn) (int-avx512-reg 2))))

;; Saturating truncation: ZMM source, byte result -> disp-n=8
(test-util::define-evex-disasm-test
    :evex-vpmovsqb-zmm-compressed-disp8
    sb-vm::%test-vpmovsqb-zmm-disp8
  ("VPMOVSQB" "ZMM2" "[RSP+8]")
  :unexpected ("[RSP+1]"))

(defun %test-vpmovsqb-zmm-disp32 ()
  (assemble-instructions
   (vpmovsqb (ea 9 rsp-tn) (int-avx512-reg 2))))

;; Saturating truncation: non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vpmovsqb-zmm-disp32-fallback
    sb-vm::%test-vpmovsqb-zmm-disp32
  ("VPMOVSQB" "ZMM2" "[RSP+9]"))

(defun %test-vpmovsqb-masked-xmm-disp8 ()
  (assemble-instructions
   (vpmovsqb-masked (ea 2 rsp-tn) (single-sse-reg 0) 4)))

;; Masked saturating truncation: vpmovsqb XMM
(test-util::define-evex-disasm-test
    :evex-vpmovsqb-masked-xmm-disp8
    sb-vm::%test-vpmovsqb-masked-xmm-disp8
  ("VPMOVSQB" "[RSP+2]" "XMM0" "{K4}")
  :unexpected ("[RSP+1]"))

(defun %test-vpmovsqb-masked-z-ymm-disp8 ()
  (assemble-instructions
   (vpmovsqb-masked-z (ea 4 rsp-tn) (int-avx2-reg 1) 5)))

;; Zeroing saturating truncation: vpmovsqb YMM
(test-util::define-evex-disasm-test
    :evex-vpmovsqb-masked-z-ymm-disp8
    sb-vm::%test-vpmovsqb-masked-z-ymm-disp8
  ("VPMOVSQB" "[RSP+4]" "YMM1" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VPMOVSXBW ----
(defun %test-auto-promoted-vpmovsxbw-disp8 ()
  (assemble-instructions
   (vpmovsxbw (int-avx512-reg 0) (ea 32 rsp-tn))))

;; Auto-promoted widening conversion: vpmovsxbw ZMM -> disp-n=32
(test-util::define-evex-disasm-test
    :auto-promoted-vpmovsxbw-compressed-disp8
    sb-vm::%test-auto-promoted-vpmovsxbw-disp8
  ("VPMOVSXBW" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; ---- VPMOVZXBD ----
(defun %test-vpmovzxbd-high-zmm-disasm ()
  (assemble-instructions
   (vpmovzxbd (int-avx512-reg 25) (int-avx512-reg 25))))

;; Combines two fixes at once: VPMOVZXBD's register-direct source must
;; print one size down from the (extended-numbered) ZMM destination, i.e.
;; XMM25, not ZMM25.
(test-util::define-evex-disasm-test
    :evex-high-register-zmm-vpmovzxbd
    sb-vm::%test-vpmovzxbd-high-zmm-disasm
  ("VPMOVZXBD" "ZMM25" "XMM25")
  :unexpected ("ZMM25, ZMM25"))

;; ---- VPMULLQ ----
(defun %test-vpmullq-zmm-disp8 ()
  (assemble-instructions
   (vpmullq (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; vpmullq full-vector: disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpmullq-zmm-compressed-disp8
    sb-vm::%test-vpmullq-zmm-disp8
  ("VPMULLQ" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VPMULTISHIFTQB ----
(defun %test-vpmultishiftqb-masked-zmm-disp8 ()
  (assemble-instructions
   (vpmultishiftqb-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 5)))

;; Masked multishift QB
(test-util::define-evex-disasm-test
    :evex-vpmultishiftqb-masked-zmm-disp8
    sb-vm::%test-vpmultishiftqb-masked-zmm-disp8
  ("VPMULTISHIFTQB" "ZMM0" "ZMM1" "[RSP+64]" "{K5}")
  :unexpected ("[RSP+1]"))

;; ---- VPOPCNTD ----
(defun %test-vpopcntd-zmm-disp8 ()
  (assemble-instructions
   (vpopcntd (int-avx512-reg 0) (ea 64 rsp-tn))))

;; VPOPCNTDQ: vpopcntd ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpopcntd-zmm-compressed-disp8
    sb-vm::%test-vpopcntd-zmm-disp8
  ("VPOPCNTD" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpopcntd-masked-zmm-disp8 ()
  (assemble-instructions
   (vpopcntd-masked (int-avx512-reg 0) (ea 64 rsp-tn) 4)))

(test-util::define-evex-disasm-test
    :evex-vpopcntd-masked-zmm-disp8
    sb-vm::%test-vpopcntd-masked-zmm-disp8
  ("VPOPCNTD" "ZMM0" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VPOPCNTQ ----
(defun %test-vpopcntq-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpopcntq-masked-z (int-avx512-reg 0) (ea 64 rsp-tn) 5)))

(test-util::define-evex-disasm-test
    :evex-vpopcntq-masked-z-zmm-disp8
    sb-vm::%test-vpopcntq-masked-z-zmm-disp8
  ("VPOPCNTQ" "ZMM0" "[RSP+64]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VPORQ ----
(defun %test-vporq-bcast-ymm-disp8 ()
  (assemble-instructions
   (vporq-bcast (int-avx2-reg 1) (int-avx2-reg 2) (ea 8 rsp-tn))))

;; Broadcast logical or qword
(test-util::define-evex-disasm-test
    :evex-vporq-bcast-ymm-disp8
    sb-vm::%test-vporq-bcast-ymm-disp8
  ("VPORQ-BCAST" "YMM1" "YMM2" "[RSP+8]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VPROLVD ----
(defun %test-vprolvd-xmm-disp8 ()
  (assemble-instructions
   (vprolvd (int-sse-reg 0) (int-sse-reg 1) (ea 16 rsp-tn))))

;; Full-vector 3-operand NDS: vprolvd XMM -> disp-n=16
(test-util::define-evex-disasm-test
    :evex-vprolvd-xmm-compressed-disp8
    sb-vm::%test-vprolvd-xmm-disp8
  ("VPROLVD" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; ---- VPSHLDD ----
(defun %test-vpshldd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpshldd-masked-z (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 0)))

;; Zeroing shift double dword left
(test-util::define-evex-disasm-test
    :evex-vpshldd-masked-z-zmm-disp8
    sb-vm::%test-vpshldd-masked-z-zmm-disp8
  ("VPSHLDD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VPSHLDVW ----
(defun %test-vpshldvw-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpshldvw-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 :z)))

(test-util::define-evex-disasm-test
    :evex-vpshldvw-masked-z-zmm-disp8
    sb-vm::%test-vpshldvw-masked-z-zmm-disp8
  ("VPSHLDVW" "ZMM0" "ZMM1" "[RSP+64]" "{K3} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VPSHLDW ----
(defun %test-vpshldw-zmm-disp8 ()
  (assemble-instructions
   (vpshldw (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 0)))

;; VBMI2 shift immediate: vpshldw ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpshldw-zmm-compressed-disp8
    sb-vm::%test-vpshldw-zmm-disp8
  ("VPSHLDW" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vpshldw-masked-zmm-disp8 ()
  (assemble-instructions
   (vpshldw-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2 0)))

;; Masked variable shift double word left
(test-util::define-evex-disasm-test
    :evex-vpshldw-masked-zmm-disp8
    sb-vm::%test-vpshldw-masked-zmm-disp8
  ("VPSHLDW" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VPSHRDQ ----
(defun %test-vpshrdq-masked-zmm-disp8 ()
  (assemble-instructions
   (vpshrdq-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 4 0)))

;; Masked shift double qword right
(test-util::define-evex-disasm-test
    :evex-vpshrdq-masked-zmm-disp8
    sb-vm::%test-vpshrdq-masked-zmm-disp8
  ("VPSHRDQ" "ZMM0" "ZMM1" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VPSHUFBITQMB ----
(defun %test-vpshufbitqmb-zmm-disp8 ()
  (assemble-instructions
   (vpshufbitqmb (mask-reg 1) (int-avx512-reg 0) (ea 64 rsp-tn))))

;; BITALG: vpshufbitqmb ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpshufbitqmb-zmm-compressed-disp8
    sb-vm::%test-vpshufbitqmb-zmm-disp8
  ("VPSHUFBITQMB" "K1" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VPSLLDQ ----
;; COMPLEX-DOUBLE-REG is XMM-width regardless of its offset, and INT-AVX2-REG
;; is YMM-width regardless of its offset (see PERFORM-OPERAND-LOWERING) --
;; forcing an offset of 16-31 exercises a register that can only be
;; addressed via EVEX even though the operation itself is only XMM/YMM-wide.
(defun %test-vpslldq-high-xmm-disasm ()
  (assemble-instructions
   (vpslldq (complex-double-reg 30) (complex-double-reg 30) 1)))

;; Extended (16-31) register numbers require EVEX even at XMM/YMM width,
;; where nothing about the operation itself (vector width, opcode map)
;; would otherwise force EVEX encoding.
;; VPSLLDQ is a register-only shift: the format's only vector operand
;; besides VVVV is a ModRM.rm-decoded, B-extended-only "REG" field
;; (unlike a generic reg/mem operand), which needed its own EVEX B' fix.
(test-util::define-evex-disasm-test
    :evex-high-register-xmm-vpslldq
    sb-vm::%test-vpslldq-high-xmm-disasm
  ("VPSLLDQ" "XMM30" "1"))

;; ---- VPSLLVD ----
(defun %test-vpsllvd-masked-zmm-disp8 ()
  (assemble-instructions
   (vpsllvd-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vpsllvd-masked-zmm-disp8
    sb-vm::%test-vpsllvd-masked-zmm-disp8
  ("VPSLLVD" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VPSLLVW ----
(defun %test-vpsllvw-zmm-disp8 ()
  (assemble-instructions
   (vpsllvw (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn))))

;; Variable shift word: ZMM full-vector -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vpsllvw-zmm-compressed-disp8
    sb-vm::%test-vpsllvw-zmm-disp8
  ("VPSLLVW" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VPSRAVD ----
(defun %test-vpsravd-masked-zmm-disp8 ()
  (assemble-instructions
   (vpsravd-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 4)))

(test-util::define-evex-disasm-test
    :evex-vpsravd-masked-zmm-disp8
    sb-vm::%test-vpsravd-masked-zmm-disp8
  ("VPSRAVD" "ZMM0" "ZMM1" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VPSRLVQ ----
(defun %test-vpsrlvq-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpsrlvq-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 :z)))

(test-util::define-evex-disasm-test
    :evex-vpsrlvq-masked-z-zmm-disp8
    sb-vm::%test-vpsrlvq-masked-z-zmm-disp8
  ("VPSRLVQ" "ZMM0" "ZMM1" "[RSP+64]" "{K3} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VPSUBW ----
(defun %test-vpsubw-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpsubw-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 :z)))

(test-util::define-evex-disasm-test
    :evex-vpsubw-masked-z-zmm-disp8
    sb-vm::%test-vpsubw-masked-z-zmm-disp8
  ("VPSUBW" "ZMM0" "ZMM1" "[RSP+64]" "{K3} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VPTERNLOGD ----
(defun %test-evex-vpternlogd ()
  (assemble-instructions
   (vpternlogd (int-avx512-reg 0) (int-avx512-reg 1) (int-avx512-reg 2) #xFF)))

;; Explicit EVEX: VPTERNLOGD with immediate
(test-util::define-evex-disasm-test
    :evex-explicit-vpternlogd-disasm
    sb-vm::%test-evex-vpternlogd
  ("VPTERNLOGD" "ZMM0" "ZMM1" "ZMM2" "255"))

(defun %test-vpternlogd-masked-zmm-disp8 ()
  (assemble-instructions
   (vpternlogd-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2 0)))

;; Masked ternary logic dword ZMM
(test-util::define-evex-disasm-test
    :evex-vpternlogd-masked-zmm-disp8
    sb-vm::%test-vpternlogd-masked-zmm-disp8
  ("VPTERNLOGD" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vpternlogd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vpternlogd-masked-z (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 0)))

;; Zeroing ternary logic dword ZMM
(test-util::define-evex-disasm-test
    :evex-vpternlogd-masked-z-zmm-disp8
    sb-vm::%test-vpternlogd-masked-z-zmm-disp8
  ("VPTERNLOGD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VPTERNLOGQ ----
(defun %test-vpternlogq-masked-xmm-disp8 ()
  (assemble-instructions
   (vpternlogq-masked (int-sse-reg 0) (int-sse-reg 1) (ea 16 rsp-tn) 4 0)))

;; Masked ternary logic qword XMM
(test-util::define-evex-disasm-test
    :evex-vpternlogq-masked-xmm-disp8
    sb-vm::%test-vpternlogq-masked-xmm-disp8
  ("VPTERNLOGQ" "XMM0" "XMM1" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

(defun %test-vpternlogq-masked-z-ymm-disp8 ()
  (assemble-instructions
   (vpternlogq-masked-z (int-avx2-reg 1) (int-avx2-reg 2) (ea 32 rsp-tn) 5 0)))

;; Zeroing ternary logic qword YMM
(test-util::define-evex-disasm-test
    :evex-vpternlogq-masked-z-ymm-disp8
    sb-vm::%test-vpternlogq-masked-z-ymm-disp8
  ("VPTERNLOGQ" "YMM1" "YMM2" "[RSP+32]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VPTESTMD ----
(defun %test-vptestmd-disp8 ()
  (assemble-instructions
   (vptestmd (mask-reg 1) (int-avx512-reg 0) (ea 64 rsp-tn))))

;; Test-to-k with compressed disp8
(test-util::define-evex-disasm-test
    :evex-vptestmd-compressed-disp8
    sb-vm::%test-vptestmd-disp8
  ("VPTESTMD" "K1" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vptestmd-disp32 ()
  (assemble-instructions
   (vptestmd (mask-reg 1) (int-avx512-reg 0) (ea 65 rsp-tn))))

;; Test-to-k falls back to disp32 for non-multiple
(test-util::define-evex-disasm-test
    :evex-vptestmd-disp32-fallback
    sb-vm::%test-vptestmd-disp32
  ("VPTESTMD" "K1" "ZMM0" "[RSP+65]"))

;; ---- VPTESTMW ----
(defun %test-vptestmw-zmm-disp8 ()
  (assemble-instructions
   (vptestmw (mask-reg 1) (int-avx512-reg 0) (ea 64 rsp-tn))))

;; Test word to k: ZMM full-vector -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vptestmw-zmm-compressed-disp8
    sb-vm::%test-vptestmw-zmm-disp8
  ("VPTESTMW" "K1" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VRANGEPD ----
(defun %test-vrangepd-zmm-disp8 ()
  (assemble-instructions
   (vrangepd (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 0)))

;; Range: vrangepd ZMM, W=1 -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vrangepd-zmm-compressed-disp8
    sb-vm::%test-vrangepd-zmm-disp8
  ("VRANGEPD" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vrangepd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vrangepd-masked-z (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 3 0)))

(test-util::define-evex-disasm-test
    :evex-vrangepd-masked-z-zmm-disp8
    sb-vm::%test-vrangepd-masked-z-zmm-disp8
  ("VRANGEPD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VRANGEPS ----
(defun %test-vrangeps-masked-zmm-disp8 ()
  (assemble-instructions
   (vrangeps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 2 0)))

(test-util::define-evex-disasm-test
    :evex-vrangeps-masked-zmm-disp8
    sb-vm::%test-vrangeps-masked-zmm-disp8
  ("VRANGEPS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vrangeps-bcast-xmm-disp8 ()
  (assemble-instructions
   (vrangeps-bcast (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn) 0)))

(test-util::define-evex-disasm-test
    :evex-vrangeps-bcast-xmm-disp8
    sb-vm::%test-vrangeps-bcast-xmm-disp8
  ("VRANGEPS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VRANGESD ----
(defun %test-vrangesd-disp8 ()
  (assemble-instructions
   (vrangesd (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn) 0)))

;; Scalar range: double precision uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vrangesd-compressed-disp8
    sb-vm::%test-vrangesd-disp8
  ("VRANGESD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

(defun %test-vrangesd-masked-z-disp8 ()
  (assemble-instructions
   (vrangesd-masked-z (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn) 3 0)))

;; Zeroing scalar range
(test-util::define-evex-disasm-test
    :evex-vrangesd-masked-z-disp8
    sb-vm::%test-vrangesd-masked-z-disp8
  ("VRANGESD" "XMM0" "XMM1" "[RSP+8]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VRANGESS ----
(defun %test-vrangess-disp8 ()
  (assemble-instructions
   (vrangess (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn) 0)))

;; Scalar range: single precision uses disp-n=4
(test-util::define-evex-disasm-test
    :evex-vrangess-compressed-disp8
    sb-vm::%test-vrangess-disp8
  ("VRANGESS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

(defun %test-vrangess-disp32 ()
  (assemble-instructions
   (vrangess (single-sse-reg 0) (single-sse-reg 1) (ea 5 rsp-tn) 0)))

;; Scalar range: single precision non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vrangess-disp32-fallback
    sb-vm::%test-vrangess-disp32
  ("VRANGESS" "XMM0" "XMM1" "[RSP+5]"))

;; ---- VRCP14PD ----
(defun %test-vrcp14pd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vrcp14pd-masked-z (double-avx512-reg 0) (ea 64 rsp-tn) 3)))

(test-util::define-evex-disasm-test
    :evex-vrcp14pd-masked-z-zmm-disp8
    sb-vm::%test-vrcp14pd-masked-z-zmm-disp8
  ("VRCP14PD" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VRCP14PS ----
(defun %test-vrcp14ps-xmm-disp8 ()
  (assemble-instructions
   (vrcp14ps (single-sse-reg 0) (ea 16 rsp-tn))))

;; Full-vector reciprocal approximation: XMM -> disp-n=16
(test-util::define-evex-disasm-test
    :evex-vrcp14ps-xmm-compressed-disp8
    sb-vm::%test-vrcp14ps-xmm-disp8
  ("VRCP14PS" "XMM0" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-vrcp14ps-ymm-disp8 ()
  (assemble-instructions
   (vrcp14ps (single-avx2-reg 1) (ea 32 rsp-tn))))

;; Full-vector reciprocal approximation: YMM -> disp-n=32
(test-util::define-evex-disasm-test
    :evex-vrcp14ps-ymm-compressed-disp8
    sb-vm::%test-vrcp14ps-ymm-disp8
  ("VRCP14PS" "YMM1" "[RSP+32]")
  :unexpected ("[RSP+1]"))

(defun %test-vrcp14ps-zmm-disp8 ()
  (assemble-instructions
   (vrcp14ps (single-avx512-reg 2) (ea 64 rsp-tn))))

;; Full-vector reciprocal approximation: ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vrcp14ps-zmm-compressed-disp8
    sb-vm::%test-vrcp14ps-zmm-disp8
  ("VRCP14PS" "ZMM2" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vrcp14ps-zmm-disp32 ()
  (assemble-instructions
   (vrcp14ps (single-avx512-reg 2) (ea 65 rsp-tn))))

;; Full-vector reciprocal approximation: non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vrcp14ps-zmm-disp32-fallback
    sb-vm::%test-vrcp14ps-zmm-disp32
  ("VRCP14PS" "ZMM2" "[RSP+65]"))

(defun %test-vrcp14ps-masked-zmm-disp8 ()
  (assemble-instructions
   (vrcp14ps-masked (single-avx512-reg 0) (ea 64 rsp-tn) 2)))

(test-util::define-evex-disasm-test
    :evex-vrcp14ps-masked-zmm-disp8
    sb-vm::%test-vrcp14ps-masked-zmm-disp8
  ("VRCP14PS" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vrcp14ps-bcast-xmm-disp8 ()
  (assemble-instructions
   (vrcp14ps-bcast (single-sse-reg 0) (ea 4 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vrcp14ps-bcast-xmm-disp8
    sb-vm::%test-vrcp14ps-bcast-xmm-disp8
  ("VRCP14PS-BCAST" "XMM0" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VRCP14SD ----
(defun %test-vrcp14sd-disp8 ()
  (assemble-instructions
   (vrcp14sd (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn))))

;; Scalar reciprocal: double precision uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vrcp14sd-compressed-disp8
    sb-vm::%test-vrcp14sd-disp8
  ("VRCP14SD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; ---- VRCP14SS ----
(defun %test-vrcp14ss-disp8 ()
  (assemble-instructions
   (vrcp14ss (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn))))

;; Scalar reciprocal: single precision uses disp-n=4
(test-util::define-evex-disasm-test
    :evex-vrcp14ss-compressed-disp8
    sb-vm::%test-vrcp14ss-disp8
  ("VRCP14SS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

(defun %test-vrcp14ss-disp32 ()
  (assemble-instructions
   (vrcp14ss (single-sse-reg 0) (single-sse-reg 1) (ea 5 rsp-tn))))

;; Scalar reciprocal: single precision non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vrcp14ss-disp32-fallback
    sb-vm::%test-vrcp14ss-disp32
  ("VRCP14SS" "XMM0" "XMM1" "[RSP+5]"))

(defun %test-vrcp14ss-masked-disp8 ()
  (assemble-instructions
   (vrcp14ss-masked (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn) 2)))

;; masked scalar reciprocal
(test-util::define-evex-disasm-test
    :evex-vrcp14ss-masked-disp8
    sb-vm::%test-vrcp14ss-masked-disp8
  ("VRCP14SS" "XMM0" "XMM1" "[RSP+4]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VRCPPS ----
(defun %test-auto-promoted-vrcpps-disp8 ()
  (assemble-instructions
   (vrcpps (single-avx512-reg 0) (ea 64 rsp-tn))))

;; Auto-promoted full-vector same width: vrcpps ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :auto-promoted-vrcpps-compressed-disp8
    sb-vm::%test-auto-promoted-vrcpps-disp8
  ("VRCPPS" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; ---- VREDUCEPD ----
(defun %test-vreducepd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vreducepd-masked-z (double-avx512-reg 0) (ea 64 rsp-tn) 3 0)))

(test-util::define-evex-disasm-test
    :evex-vreducepd-masked-z-zmm-disp8
    sb-vm::%test-vreducepd-masked-z-zmm-disp8
  ("VREDUCEPD" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VREDUCEPS ----
(defun %test-vreduceps-xmm-disp8 ()
  (assemble-instructions
   (vreduceps (single-sse-reg 0) (ea 16 rsp-tn) 0)))

;; Reduce: vreduceps XMM -> disp-n=16
(test-util::define-evex-disasm-test
    :evex-vreduceps-xmm-compressed-disp8
    sb-vm::%test-vreduceps-xmm-disp8
  ("VREDUCEPS" "XMM0" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-vreduceps-zmm-disp8 ()
  (assemble-instructions
   (vreduceps (single-avx512-reg 0) (ea 64 rsp-tn) 0)))

;; Reduce: vreduceps ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vreduceps-zmm-compressed-disp8
    sb-vm::%test-vreduceps-zmm-disp8
  ("VREDUCEPS" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vreduceps-zmm-disp32 ()
  (assemble-instructions
   (vreduceps (single-avx512-reg 0) (ea 65 rsp-tn) 0)))

;; Reduce: vreduceps disp32 fallback
(test-util::define-evex-disasm-test
    :evex-vreduceps-zmm-disp32-fallback
    sb-vm::%test-vreduceps-zmm-disp32
  ("VREDUCEPS" "ZMM0" "[RSP+65]"))

(defun %test-vreduceps-masked-zmm-disp8 ()
  (assemble-instructions
   (vreduceps-masked (single-avx512-reg 0) (ea 64 rsp-tn) 2 0)))

;; Reduce
(test-util::define-evex-disasm-test
    :evex-vreduceps-masked-zmm-disp8
    sb-vm::%test-vreduceps-masked-zmm-disp8
  ("VREDUCEPS" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vreduceps-bcast-xmm-disp8 ()
  (assemble-instructions
   (vreduceps-bcast (single-sse-reg 0) (ea 4 rsp-tn) 0)))

(test-util::define-evex-disasm-test
    :evex-vreduceps-bcast-xmm-disp8
    sb-vm::%test-vreduceps-bcast-xmm-disp8
  ("VREDUCEPS-BCAST" "XMM0" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; ---- VREDUCESD ----
(defun %test-vreducesd-disp8 ()
  (assemble-instructions
   (vreducesd (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn) 0)))

;; Scalar reduce: double precision uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vreducesd-compressed-disp8
    sb-vm::%test-vreducesd-disp8
  ("VREDUCESD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; ---- VREDUCESS ----
(defun %test-vreducess-disp8 ()
  (assemble-instructions
   (vreducess (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn) 0)))

;; Scalar reduce: single precision uses disp-n=4
(test-util::define-evex-disasm-test
    :evex-vreducess-compressed-disp8
    sb-vm::%test-vreducess-disp8
  ("VREDUCESS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

(defun %test-vreducess-disp32 ()
  (assemble-instructions
   (vreducess (single-sse-reg 0) (single-sse-reg 1) (ea 5 rsp-tn) 0)))

;; Scalar reduce: single precision non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vreducess-disp32-fallback
    sb-vm::%test-vreducess-disp32
  ("VREDUCESS" "XMM0" "XMM1" "[RSP+5]"))

;; ---- VRNDSCALEPD ----
(defun %test-vrndscalepd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vrndscalepd-masked-z (double-avx512-reg 0) (ea 64 rsp-tn) 3 0)))

(test-util::define-evex-disasm-test
    :evex-vrndscalepd-masked-z-zmm-disp8
    sb-vm::%test-vrndscalepd-masked-z-zmm-disp8
  ("VRNDSCALEPD" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VRNDSCALEPS ----
(defun %test-vrndscaleps-xmm-disp8 ()
  (assemble-instructions
   (vrndscaleps (single-sse-reg 0) (ea 16 rsp-tn) 0)))

;; Packed round: XMM full-vector -> disp-n=16
(test-util::define-evex-disasm-test
    :evex-vrndscaleps-xmm-compressed-disp8
    sb-vm::%test-vrndscaleps-xmm-disp8
  ("VRNDSCALEPS" "XMM0" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-vrndscaleps-zmm-disp8 ()
  (assemble-instructions
   (vrndscaleps (single-avx512-reg 2) (ea 64 rsp-tn) 0)))

;; Packed round: ZMM full-vector -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vrndscaleps-zmm-compressed-disp8
    sb-vm::%test-vrndscaleps-zmm-disp8
  ("VRNDSCALEPS" "ZMM2" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vrndscaleps-zmm-disp32 ()
  (assemble-instructions
   (vrndscaleps (single-avx512-reg 2) (ea 65 rsp-tn) 0)))

;; Packed round: non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vrndscaleps-zmm-disp32-fallback
    sb-vm::%test-vrndscaleps-zmm-disp32
  ("VRNDSCALEPS" "ZMM2" "[RSP+65]"))

(defun %test-vrndscaleps-masked-zmm-disp8 ()
  (assemble-instructions
   (vrndscaleps-masked (single-avx512-reg 0) (ea 64 rsp-tn) 2 0)))

;; Round
(test-util::define-evex-disasm-test
    :evex-vrndscaleps-masked-zmm-disp8
    sb-vm::%test-vrndscaleps-masked-zmm-disp8
  ("VRNDSCALEPS" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VRNDSCALESD ----
(defun %test-vrndscalesd-disp8 ()
  (assemble-instructions
   (vrndscalesd (double-sse-reg 0) (ea 8 rsp-tn) 0)))

;; Scalar round: double precision uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vrndscalesd-compressed-disp8
    sb-vm::%test-vrndscalesd-disp8
  ("VRNDSCALESD" "XMM0" "[RSP+8]")
  :unexpected ("[RSP+1]"))

(defun %test-vrndscalesd-masked-z-disp8 ()
  (assemble-instructions
   (vrndscalesd-masked-z (double-sse-reg 0) (ea 8 rsp-tn) 3 0)))

;; Zeroing 2-operand round
(test-util::define-evex-disasm-test
    :evex-vrndscalesd-masked-z-disp8
    sb-vm::%test-vrndscalesd-masked-z-disp8
  ("VRNDSCALESD" "XMM0" "[RSP+8]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VRNDSCALESS ----
(defun %test-vrndscaless-disp8 ()
  (assemble-instructions
   (vrndscaless (single-sse-reg 0) (ea 4 rsp-tn) 0)))

;; Scalar round: single precision uses disp-n=4
(test-util::define-evex-disasm-test
    :evex-vrndscaless-compressed-disp8
    sb-vm::%test-vrndscaless-disp8
  ("VRNDSCALESS" "XMM0" "[RSP+4]")
  :unexpected ("[RSP+1]"))

(defun %test-vrndscaless-disp32 ()
  (assemble-instructions
   (vrndscaless (single-sse-reg 0) (ea 5 rsp-tn) 0)))

;; Scalar round: single precision non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vrndscaless-disp32-fallback
    sb-vm::%test-vrndscaless-disp32
  ("VRNDSCALESS" "XMM0" "[RSP+5]"))

(defun %test-vrndscaless-masked-disp8 ()
  (assemble-instructions
   (vrndscaless-masked (single-sse-reg 0) (ea 4 rsp-tn) 2 0)))

;; Masked 2-operand round
(test-util::define-evex-disasm-test
    :evex-vrndscaless-masked-disp8
    sb-vm::%test-vrndscaless-masked-disp8
  ("VRNDSCALESS" "XMM0" "[RSP+4]" "{K2}")
  :unexpected ("[RSP+1]"))

;; ---- VRSQRT14PD ----
(defun %test-vrsqrt14pd-bcast-zmm-disp8 ()
  (assemble-instructions
   (vrsqrt14pd-bcast (double-avx512-reg 0) (ea 8 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vrsqrt14pd-bcast-zmm-disp8
    sb-vm::%test-vrsqrt14pd-bcast-zmm-disp8
  ("VRSQRT14PD-BCAST" "ZMM0" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VRSQRT14PS ----
(defun %test-vrsqrt14ps-masked-zmm-disp8 ()
  (assemble-instructions
   (vrsqrt14ps-masked (single-avx512-reg 0) (ea 64 rsp-tn) 4)))

(test-util::define-evex-disasm-test
    :evex-vrsqrt14ps-masked-zmm-disp8
    sb-vm::%test-vrsqrt14ps-masked-zmm-disp8
  ("VRSQRT14PS" "ZMM0" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

;; ---- VRSQRT14SD ----
(defun %test-vrsqrt14sd-disp8 ()
  (assemble-instructions
   (vrsqrt14sd (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn))))

;; Scalar rsqrt: double precision uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vrsqrt14sd-compressed-disp8
    sb-vm::%test-vrsqrt14sd-disp8
  ("VRSQRT14SD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

(defun %test-vrsqrt14sd-masked-z-disp8 ()
  (assemble-instructions
   (vrsqrt14sd-masked-z (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn) 3)))

;; zeroing scalar sqrt
(test-util::define-evex-disasm-test
    :evex-vrsqrt14sd-masked-z-disp8
    sb-vm::%test-vrsqrt14sd-masked-z-disp8
  ("VRSQRT14SD" "XMM0" "XMM1" "[RSP+8]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VRSQRT14SS ----
(defun %test-vrsqrt14ss-disp8 ()
  (assemble-instructions
   (vrsqrt14ss (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn))))

;; Scalar rsqrt: single precision uses disp-n=4
(test-util::define-evex-disasm-test
    :evex-vrsqrt14ss-compressed-disp8
    sb-vm::%test-vrsqrt14ss-disp8
  ("VRSQRT14SS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; ---- VSCALEFPD ----
(defun %test-vscalefpd-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vscalefpd-masked (double-avx512-reg 0) (double-avx512-reg 1) (ea 64 rsp-tn) 5 :z)))

(test-util::define-evex-disasm-test
    :evex-vscalefpd-masked-z-zmm-disp8
    sb-vm::%test-vscalefpd-masked-z-zmm-disp8
  ("VSCALEFPD" "ZMM0" "ZMM1" "[RSP+64]" "{K5} {z}")
  :unexpected ("[RSP+1]"))

;; ---- VSCALEFPS ----
(defun %test-vscalefps-xmm-disp8 ()
  (assemble-instructions
   (vscalefps (single-sse-reg 0) (single-sse-reg 1) (ea 16 rsp-tn))))

;; Packed scale: XMM full-vector -> disp-n=16
(test-util::define-evex-disasm-test
    :evex-vscalefps-xmm-compressed-disp8
    sb-vm::%test-vscalefps-xmm-disp8
  ("VSCALEFPS" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-vscalefps-zmm-disp8 ()
  (assemble-instructions
   (vscalefps (single-avx512-reg 1) (single-avx512-reg 2) (ea 64 rsp-tn))))

;; Packed scale: ZMM full-vector -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vscalefps-zmm-compressed-disp8
    sb-vm::%test-vscalefps-zmm-disp8
  ("VSCALEFPS" "ZMM1" "ZMM2" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vscalefps-zmm-disp32 ()
  (assemble-instructions
   (vscalefps (single-avx512-reg 1) (single-avx512-reg 2) (ea 65 rsp-tn))))

;; Packed scale: non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vscalefps-zmm-disp32-fallback
    sb-vm::%test-vscalefps-zmm-disp32
  ("VSCALEFPS" "ZMM1" "ZMM2" "[RSP+65]"))

(defun %test-vscalefps-masked-zmm-disp8 ()
  (assemble-instructions
   (vscalefps-masked (single-avx512-reg 0) (single-avx512-reg 1) (ea 64 rsp-tn) 4)))

;; Scale
(test-util::define-evex-disasm-test
    :evex-vscalefps-masked-zmm-disp8
    sb-vm::%test-vscalefps-masked-zmm-disp8
  ("VSCALEFPS" "ZMM0" "ZMM1" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

(defun %test-vscalefps-bcast-ymm-disp8 ()
  (assemble-instructions
   (vscalefps-bcast (single-avx2-reg 0) (single-avx2-reg 1) (ea 4 rsp-tn))))

(test-util::define-evex-disasm-test
    :evex-vscalefps-bcast-ymm-disp8
    sb-vm::%test-vscalefps-bcast-ymm-disp8
  ("VSCALEFPS-BCAST" "YMM0" "YMM1" "[RSP+4]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VSCALEFSD ----
(defun %test-vscalefsd-disp8 ()
  (assemble-instructions
   (vscalefsd (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn))))

;; Scalar scale: double precision uses disp-n=8
(test-util::define-evex-disasm-test
    :evex-vscalefsd-compressed-disp8
    sb-vm::%test-vscalefsd-disp8
  ("VSCALEFSD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

(defun %test-vscalefsd-masked-z-disp8 ()
  (assemble-instructions
   (vscalefsd-masked-z (double-sse-reg 0) (double-sse-reg 1) (ea 8 rsp-tn) 3)))

(test-util::define-evex-disasm-test
    :evex-vscalefsd-masked-z-disp8
    sb-vm::%test-vscalefsd-masked-z-disp8
  ("VSCALEFSD" "XMM0" "XMM1" "[RSP+8]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VSCALEFSS ----
(defun %test-vscalefss-disp8 ()
  (assemble-instructions
   (vscalefss (single-sse-reg 0) (single-sse-reg 1) (ea 4 rsp-tn))))

;; Scalar scale: single precision uses disp-n=4
(test-util::define-evex-disasm-test
    :evex-vscalefss-compressed-disp8
    sb-vm::%test-vscalefss-disp8
  ("VSCALEFSS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

(defun %test-vscalefss-disp32 ()
  (assemble-instructions
   (vscalefss (single-sse-reg 0) (single-sse-reg 1) (ea 5 rsp-tn))))

;; Scalar scale: single precision non-multiple falls back to disp32
(test-util::define-evex-disasm-test
    :evex-vscalefss-disp32-fallback
    sb-vm::%test-vscalefss-disp32
  ("VSCALEFSS" "XMM0" "XMM1" "[RSP+5]"))

;; ---- VSCATTERDPS ----
(defun %test-vsib-scatter-low-index ()
  (assemble-instructions
   (vscatterdps-z (ea 0 (unsigned-reg rax-offset) (single-avx512-reg 3) 4) (single-avx512-reg 0) 3)))

;; VSIB Scatter, low index
(test-util::define-evex-disasm-test
    :evex-vsib-scatter-low-index
    sb-vm::%test-vsib-scatter-low-index
  ("VSCATTERDPS" "ZMM0" "ZMM3" "K3"))

(defun %test-vsib-scatter-disp8-compressed ()
  (assemble-instructions
   (vscatterdps-z (ea 4 (unsigned-reg rax-offset) (single-avx512-reg 1) 4) (single-avx512-reg 0) 1)))

;; Scatter with compressed displacement
(test-util::define-evex-disasm-test
    :evex-vsib-scatter-disp8-compressed
    sb-vm::%test-vsib-scatter-disp8-compressed
  ("VSCATTERDPS" "ZMM0" "ZMM1*4+4" "K1"))

;; ---- VSHUFF32X4 ----
(defun %test-vshuff32x4-zmm-disp8 ()
  (assemble-instructions
   (vshuff32x4 (int-avx512-reg 2) (int-avx512-reg 3) (ea 64 rsp-tn) 0)))

;; Cross-lane shuffle: vshuff32x4 ZMM -> disp-n=64
(test-util::define-evex-disasm-test
    :evex-vshuff32x4-zmm-compressed-disp8
    sb-vm::%test-vshuff32x4-zmm-disp8
  ("VSHUFF32X4" "ZMM2" "ZMM3" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(defun %test-vshuff32x4-zmm-disp32 ()
  (assemble-instructions
   (vshuff32x4 (int-avx512-reg 2) (int-avx512-reg 3) (ea 65 rsp-tn) 0)))

;; vshuff32x4 disp32 fallback
(test-util::define-evex-disasm-test
    :evex-vshuff32x4-zmm-disp32-fallback
    sb-vm::%test-vshuff32x4-zmm-disp32
  ("VSHUFF32X4" "ZMM2" "ZMM3" "[RSP+65]"))

(defun %test-vshuff32x4-masked-zmm-disp8 ()
  (assemble-instructions
   (vshuff32x4-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 2 0)))

;; Masked cross-lane shuffle: vshuff32x4 ZMM
(test-util::define-evex-disasm-test
    :evex-vshuff32x4-masked-zmm-disp8
    sb-vm::%test-vshuff32x4-masked-zmm-disp8
  ("VSHUFF32X4" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vshuff32x4-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vshuff32x4-masked-z (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 3 0)))

;; Zeroing cross-lane shuffle: vshuff32x4 ZMM
(test-util::define-evex-disasm-test
    :evex-vshuff32x4-masked-z-zmm-disp8
    sb-vm::%test-vshuff32x4-masked-z-zmm-disp8
  ("VSHUFF32X4" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VSHUFF64X2 ----
(defun %test-vshuff64x2-masked-xmm-disp8 ()
  (assemble-instructions
   (vshuff64x2-masked (int-sse-reg 0) (int-sse-reg 1) (ea 16 rsp-tn) 4 0)))

;; Masked cross-lane shuffle: vshuff64x2 XMM
(test-util::define-evex-disasm-test
    :evex-vshuff64x2-masked-xmm-disp8
    sb-vm::%test-vshuff64x2-masked-xmm-disp8
  ("VSHUFF64X2" "XMM0" "XMM1" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

(defun %test-vshuff64x2-masked-z-ymm-disp8 ()
  (assemble-instructions
   (vshuff64x2-masked-z (int-avx2-reg 1) (int-avx2-reg 2) (ea 32 rsp-tn) 5 0)))

;; Zeroing cross-lane shuffle: vshuff64x2 YMM
(test-util::define-evex-disasm-test
    :evex-vshuff64x2-masked-z-ymm-disp8
    sb-vm::%test-vshuff64x2-masked-z-ymm-disp8
  ("VSHUFF64X2" "YMM1" "YMM2" "[RSP+32]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VSHUFI32X4 ----
(defun %test-vshufi32x4-xmm-disp8 ()
  (assemble-instructions
   (vshufi32x4 (int-sse-reg 0) (int-sse-reg 1) (ea 16 rsp-tn) 0)))

;; Cross-lane shuffle: vshufi32x4 XMM -> disp-n=16
(test-util::define-evex-disasm-test
    :evex-vshufi32x4-xmm-compressed-disp8
    sb-vm::%test-vshufi32x4-xmm-disp8
  ("VSHUFI32X4" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(defun %test-vshufi32x4-masked-zmm-disp8 ()
  (assemble-instructions
   (vshufi32x4-masked (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 6 0)))

;; Masked cross-lane shuffle: vshufi32x4 ZMM
(test-util::define-evex-disasm-test
    :evex-vshufi32x4-masked-zmm-disp8
    sb-vm::%test-vshufi32x4-masked-zmm-disp8
  ("VSHUFI32X4" "ZMM0" "ZMM1" "[RSP+64]" "{K6}")
  :unexpected ("[RSP+1]"))

(defun %test-vshufi32x4-masked-z-zmm-disp8 ()
  (assemble-instructions
   (vshufi32x4-masked-z (int-avx512-reg 0) (int-avx512-reg 1) (ea 64 rsp-tn) 7 0)))

;; Zeroing cross-lane shuffle: vshufi32x4 ZMM
(test-util::define-evex-disasm-test
    :evex-vshufi32x4-masked-z-zmm-disp8
    sb-vm::%test-vshufi32x4-masked-z-zmm-disp8
  ("VSHUFI32X4" "ZMM0" "ZMM1" "[RSP+64]" "{K7}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VSHUFI64X2 ----
(defun %test-vshufi64x2-masked-xmm-disp8 ()
  (assemble-instructions
   (vshufi64x2-masked (int-sse-reg 0) (int-sse-reg 1) (ea 16 rsp-tn) 2 0)))

;; Masked cross-lane shuffle: vshufi64x2 XMM
(test-util::define-evex-disasm-test
    :evex-vshufi64x2-masked-xmm-disp8
    sb-vm::%test-vshufi64x2-masked-xmm-disp8
  ("VSHUFI64X2" "XMM0" "XMM1" "[RSP+16]" "{K2}")
  :unexpected ("[RSP+1]"))

(defun %test-vshufi64x2-masked-z-ymm-disp8 ()
  (assemble-instructions
   (vshufi64x2-masked-z (int-avx2-reg 1) (int-avx2-reg 2) (ea 32 rsp-tn) 3 0)))

;; Zeroing cross-lane shuffle: vshufi64x2 YMM
(test-util::define-evex-disasm-test
    :evex-vshufi64x2-masked-z-ymm-disp8
    sb-vm::%test-vshufi64x2-masked-z-ymm-disp8
  ("VSHUFI64X2" "YMM1" "YMM2" "[RSP+32]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; ---- VSUBPD ----
(defun %test-vsubpd-bcast-zmm-disp8 ()
  (assemble-instructions
   (vsubpd-bcast (double-avx512-reg 0) (double-avx512-reg 1) (ea 8 rsp-tn))))

;; Broadcast subtract double precision
(test-util::define-evex-disasm-test
    :evex-vsubpd-bcast-zmm-disp8
    sb-vm::%test-vsubpd-bcast-zmm-disp8
  ("VSUBPD-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; ---- VSUBPS ----
(defun %test-vsubps-bcast-ymm-disp8 ()
  (assemble-instructions
   (vsubps-bcast (single-avx2-reg 0) (single-avx2-reg 1) (ea 4 rsp-tn))))

;; Broadcast subtract single precision
(test-util::define-evex-disasm-test
    :evex-vsubps-bcast-ymm-disp8
    sb-vm::%test-vsubps-bcast-ymm-disp8
  ("VSUBPS-BCAST" "YMM0" "YMM1" "[RSP+4]" "{1to8}")
  :unexpected ("[RSP+1]"))
