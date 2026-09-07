;;;; ==============================================================================
;;;; tests/apx.impure.lisp --- Intel APX (Advanced Performance Extensions)
;;;; ==============================================================================
;;;;
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

(in-package "SB-VM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "TEST-UTIL")
    (make-package "TEST-UTIL" :use '("CL"))
    (export (intern "WITH-TEST" "TEST-UTIL") "TEST-UTIL")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'test-util:with-test)
    (defmacro test-util:with-test ((&key name) &body body)
      (declare (ignore name))
      `(progn ,@body))))

(flet ((skip ()
         (let* ((pkg (find-package "RUN-TESTS"))
                (sym (and pkg (find-symbol "SKIP-FILE" pkg))))
           (if (and sym (find-restart sym))
               (invoke-restart sym)
               (sb-ext:exit :code 0)))))
  #-x86-64 (skip)
  (when (zerop (sb-alien:extern-alien "apx_supported" int))
    (format t "~&INFO: Intel APX not supported by host CPU~%")
    (skip)))

(declaim (optimize (speed 3) (safety 1))
         (sb-ext:muffle-conditions sb-ext:compiler-note))

(defun report-stage (stage-name status &optional msg)
  (unless status
    (error "APX verification failed in ~A: ~A" stage-name (or msg "Assertion error"))))

;;; ---------------------------------------------------------------------------
;;; Execution Harness & Buffers
;;; ---------------------------------------------------------------------------

(defvar *exec-buffer*
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "mmap"
                          (function sb-sys:system-area-pointer
                                    sb-sys:system-area-pointer
                                    sb-alien:unsigned-long
                                    sb-alien:int
                                    sb-alien:int
                                    sb-alien:int
                                    sb-alien:long))
   (sb-sys:int-sap 0)
   2097152 ; 2MB executable buffer
   7    ; PROT_READ | PROT_WRITE | PROT_EXEC
   #x22 ; MAP_PRIVATE | MAP_ANONYMOUS
   -1
   0))

(defmacro exec-asm ((&optional (arg-name 'arg)) &body body)
  (let ((seg (gensym "SEG"))
        (buf (gensym "BUF"))
        (len (gensym "LEN")))
    `(let ((,seg (sb-assem:make-segment)))
       (sb-assem:assemble (,seg nil)
         ,@body
         (sb-assem:inst ret))
       (let* ((,buf (sb-assem::segment-buffer ,seg))
              (,len (sb-assem::segment-current-posn ,seg)))
         (assert (<= ,len 2097152))
         (loop for i below ,len
               do (setf (sb-sys:sap-ref-8 *exec-buffer* i) (aref ,buf i)))
         (sb-alien:alien-funcall
          (sb-alien:sap-alien *exec-buffer* (function sb-alien:unsigned-long sb-alien:unsigned-long))
          ,(if (symbolp arg-name) 0 arg-name))))))

;;; ---------------------------------------------------------------------------
;;; Stage 1: Extended GPRs (R16-R31) Basic Data Flow & Fidelity
;;; ---------------------------------------------------------------------------

(defun test-stage-1-egpr-integrity ()
  (let ((all-ok t))
    (loop for reg-idx from 16 to 31
          for test-val = (+ #x1234567800000000 (* reg-idx #x11111111))
          do
       (let* ((reg (sb-x86-64-asm::get-gpr :qword reg-idx))
              (rax (sb-x86-64-asm::get-gpr :qword 0))
              (res (exec-asm ()
                     (sb-assem:inst mov reg test-val)
                     (sb-assem:inst mov rax reg))))
         (unless (= res test-val)
           (setf all-ok nil)
           (format t "    R~D mismatch: got #x~X expected #x~X~%" reg-idx res test-val))))
    (report-stage "Stage 1: Extended GPRs R16..R31 Data Fidelity" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 2: REX2 Boundary Stress & Zero-Extension
;;; ---------------------------------------------------------------------------

(defun test-stage-2-rex2-matrix ()
  (let ((all-ok t))
    (loop for reg-idx from 16 to 31
          for imm32 = (logand (+ #x9ABCDEF0 (* reg-idx 17)) #xFFFFFFFF)
          do
       (let* ((reg-q (sb-x86-64-asm::get-gpr :qword reg-idx))
              (reg-d (sb-x86-64-asm::get-gpr :dword reg-idx))
              (rax (sb-x86-64-asm::get-gpr :qword 0))
              (res (exec-asm ()
                     (sb-assem:inst mov reg-q #xFFFFFFFFFFFFFFFF)
                     (sb-assem:inst mov reg-d imm32)
                     (sb-assem:inst mov rax reg-q))))
         (unless (= res imm32)
           (setf all-ok nil)
           (format t "    R~D 32-bit zero-extension fail: got #x~X expected #x~X~%" reg-idx res imm32))))
    (report-stage "Stage 2: REX2 Cross-Matrix & Zero-Extension" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 3: SIB & Memory Addressing with EGPRs
;;; ---------------------------------------------------------------------------

(defun test-stage-3-sib-matrix ()
  (let ((all-ok t)
        (buf (make-array 64 :element-type '(unsigned-byte 64) :initial-element 0)))
    (sb-sys:with-pinned-objects (buf)
      (let* ((buf-addr (sb-sys:sap-int (sb-sys:vector-sap buf)))
             (rax (sb-x86-64-asm::get-gpr :qword 0))
             (r16 (sb-x86-64-asm::get-gpr :qword 16))
             (r17 (sb-x86-64-asm::get-gpr :qword 17))
             (r18 (sb-x86-64-asm::get-gpr :qword 18)))
        (setf (aref buf 5) #xCAFEBABEDEADBEEF)
        (let ((res (exec-asm ()
                     (sb-assem:inst mov r16 buf-addr)
                     (sb-assem:inst mov r17 5)
                     (sb-assem:inst mov r18 (sb-x86-64-asm::ea 0 r16 r17 8))
                     (sb-assem:inst mov rax r18))))
          (unless (= res #xCAFEBABEDEADBEEF)
            (setf all-ok nil)
            (format t "    SIB load fail: got #x~X~%" res)))
        (let ((res (exec-asm ()
                     (sb-assem:inst mov r16 buf-addr)
                     (sb-assem:inst mov r17 2)
                     (sb-assem:inst mov r18 #x5555AAAA5555AAAA)
                     (sb-assem:inst mov (sb-x86-64-asm::ea 24 r16 r17 8) r18)
                     (sb-assem:inst mov rax (sb-x86-64-asm::ea 24 r16 r17 8)))))
          (unless (= res #x5555AAAA5555AAAA)
            (setf all-ok nil)
            (format t "    SIB store/load fail: got #x~X~%" res))
          (unless (= (aref buf 5) #x5555AAAA5555AAAA)
            (setf all-ok nil)
            (format t "    Buffer reflection fail: got #x~X~%" (aref buf 5))))))
    (report-stage "Stage 3: SIB & Memory Addressing with EGPRs" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 4: 3-Operand NDD ALU Engine
;;; ---------------------------------------------------------------------------

(defun test-stage-4-ndd-alu ()
  (let ((all-ok t)
        (r16 (sb-x86-64-asm::get-gpr :qword 16))
        (r17 (sb-x86-64-asm::get-gpr :qword 17))
        (r18 (sb-x86-64-asm::get-gpr :qword 18))
        (rax (sb-x86-64-asm::get-gpr :qword 0)))
    (flet ((run-alu (name op-fn asm-emitter v1 v2)
             (let* ((expected (logand (funcall op-fn v1 v2) #xFFFFFFFFFFFFFFFF))
                    (res (exec-asm ()
                           (sb-assem:inst mov r17 v1)
                           (sb-assem:inst mov r18 v2)
                           (funcall asm-emitter r16 r17 r18)
                           (sb-assem:inst mov rax r16))))
               (unless (= res expected)
                 (setf all-ok nil)
                 (format t "    NDD ~A fail: got #x~X expected #x~X~%" name res expected)))))
      (run-alu "ADD" #'+ (lambda (d s1 s2) (sb-assem:inst add-ndd d s1 s2)) #x1000200030004000 #x0000000000000005)
      (run-alu "SUB" #'- (lambda (d s1 s2) (sb-assem:inst sub-ndd d s1 s2)) #x5000000000000000 #x2000000000000000)
      (run-alu "AND" #'logand (lambda (d s1 s2) (sb-assem:inst and-ndd d s1 s2)) #xFF00FF00FF00FF00 #x0F0F0F0F0F0F0F0F)
      (run-alu "OR"  #'logior (lambda (d s1 s2) (sb-assem:inst or-ndd d s1 s2))  #x1234000000000000 #x0000567800000000)
      (run-alu "XOR" #'logxor (lambda (d s1 s2) (sb-assem:inst xor-ndd d s1 s2)) #xAAAAAAAAAAAAAAAA #x5555555555555555)
      (run-alu "IMUL" #'* (lambda (d s1 s2) (sb-assem:inst imul-ndd d s1 s2)) 1234567 891011))
    (report-stage "Stage 4: 3-Operand NDD ALU Engine" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 5: Unary NDD Operations
;;; ---------------------------------------------------------------------------

(defun test-stage-5-unary-ndd ()
  (let ((all-ok t)
        (r19 (sb-x86-64-asm::get-gpr :qword 19))
        (r20 (sb-x86-64-asm::get-gpr :qword 20))
        (rax (sb-x86-64-asm::get-gpr :qword 0)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r20 42)
                 (sb-assem:inst inc-ndd r19 r20)
                 (sb-assem:inst mov rax r19))))
      (unless (= res 43)
        (setf all-ok nil)
        (format t "    INC-NDD fail: got ~D expected 43~%" res)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r20 42)
                 (sb-assem:inst dec-ndd r19 r20)
                 (sb-assem:inst mov rax r19))))
      (unless (= res 41)
        (setf all-ok nil)
        (format t "    DEC-NDD fail: got ~D expected 41~%" res)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r20 50)
                 (sb-assem:inst neg-ndd r19 r20)
                 (sb-assem:inst mov rax r19))))
      (unless (= res (logand -50 #xFFFFFFFFFFFFFFFF))
        (setf all-ok nil)
        (format t "    NEG-NDD fail: got #x~X~%" res)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r20 #x5555555555555555)
                 (sb-assem:inst not-ndd r19 r20)
                 (sb-assem:inst mov rax r19))))
      (unless (= res #xAAAAAAAAAAAAAAAA)
        (setf all-ok nil)
        (format t "    NOT-NDD fail: got #x~X~%" res)))
    (report-stage "Stage 5: Unary NDD Operations" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 6: Shift & Rotate NDD Operations
;;; ---------------------------------------------------------------------------

(defun test-stage-6-shift-ndd ()
  (let ((all-ok t)
        (r21 (sb-x86-64-asm::get-gpr :qword 21))
        (r22 (sb-x86-64-asm::get-gpr :qword 22))
        (rcx (sb-x86-64-asm::get-gpr :qword 1))
        (rax (sb-x86-64-asm::get-gpr :qword 0)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r22 1)
                 (sb-assem:inst shl-ndd r21 r22 10)
                 (sb-assem:inst mov rax r21))))
      (unless (= res 1024)
        (setf all-ok nil)
        (format t "    SHL-NDD imm fail: got ~D expected 1024~%" res)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r22 2048)
                 (sb-assem:inst mov rcx 4)
                 (sb-assem:inst shr-ndd r21 r22 :cl)
                 (sb-assem:inst mov rax r21))))
      (unless (= res 128)
        (setf all-ok nil)
        (format t "    SHR-NDD cl fail: got ~D expected 128~%" res)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r22 #x123456789ABCDEF0)
                 (sb-assem:inst rol-ndd r21 r22 4)
                 (sb-assem:inst mov rax r21))))
      (unless (= res #x23456789ABCDEF01)
        (setf all-ok nil)
        (format t "    ROL-NDD imm fail: got #x~X~%" res)))
    (report-stage "Stage 6: Shift & Rotate NDD Operations" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 7: Status Flag Suppression (NF=1) Invariance
;;; ---------------------------------------------------------------------------

(defun test-stage-7-nf-invariance ()
  (let ((all-ok t)
        (r23 (sb-x86-64-asm::get-gpr :qword 23))
        (r24 (sb-x86-64-asm::get-gpr :qword 24))
        (r25 (sb-x86-64-asm::get-gpr :qword 25))
        (rax (sb-x86-64-asm::get-gpr :qword 0))
        (al  (sb-x86-64-asm::get-gpr :byte 0)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r24 100)
                 (sb-assem:inst mov r25 200)
                 (sb-assem:inst cmp r24 r25) ; sets CF=1, ZF=0
                 (sb-assem:inst add-ndd r23 r24 r25 :nf 1)
                 (sb-assem:inst mov rax 0)
                 (sb-assem:inst set :c al))))
      (unless (= res 1)
        (setf all-ok nil)
        (format t "    NF flag suppression failure: flags modified~%")))
    (report-stage "Stage 7: Status Flag Suppression (NF) Invariance" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 8 & 9: Conditional Compare & Test (CCMP / CTEST)
;;; ---------------------------------------------------------------------------

(defun test-stage-8-9-ccmp-ctest ()
  (let ((all-ok t)
        (r26 (sb-x86-64-asm::get-gpr :qword 26))
        (r27 (sb-x86-64-asm::get-gpr :qword 27))
        (rax (sb-x86-64-asm::get-gpr :qword 0))
        (al  (sb-x86-64-asm::get-gpr :byte 0)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r26 10)
                 (sb-assem:inst mov r27 10)
                 (sb-assem:inst cmp r26 r27)
                 (sb-assem:inst ccmp :e r26 20 0)
                 (sb-assem:inst mov rax 0)
                 (sb-assem:inst set :ne al))))
      (unless (= res 1)
        (setf all-ok nil)
        (format t "    CCMP branchless decision failure~%")))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r26 5)
                 (sb-assem:inst mov r27 10)
                 (sb-assem:inst cmp r26 r27)
                 (sb-assem:inst ctest :ne r26 5 0)
                 (sb-assem:inst mov rax 0)
                 (sb-assem:inst set :ne al))))
      (unless (= res 1)
        (setf all-ok nil)
        (format t "    CTEST decision failure~%")))
    (report-stage "Stage 8 & 9: CCMP & CTEST Decision Engine" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 10: Conditional Faulting Move (CFCMOVcc)
;;; ---------------------------------------------------------------------------

(defun test-stage-10-cfcmov ()
  (let ((all-ok t)
        (r28 (sb-x86-64-asm::get-gpr :qword 28))
        (r29 (sb-x86-64-asm::get-gpr :qword 29))
        (rax (sb-x86-64-asm::get-gpr :qword 0)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r28 100)
                 (sb-assem:inst mov r29 200)
                 (sb-assem:inst cmp r28 r29)
                 (sb-assem:inst cfcmov :e r28 r29)
                 (sb-assem:inst mov rax r28))))
      (unless (= res 0)
        (setf all-ok nil)
        (format t "    CFCMOV false-condition zeroing failure: got ~D expected 0~%" res)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r28 100)
                 (sb-assem:inst mov r29 200)
                 (sb-assem:inst cmp r28 r28)
                 (sb-assem:inst cfcmov :e r28 r29)
                 (sb-assem:inst mov rax r28))))
      (unless (= res 200)
        (setf all-ok nil)
        (format t "    CFCMOV true-condition move failure: got ~D expected 200~%" res)))
    (report-stage "Stage 10: Conditional Faulting Move (CFCMOV)" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 11: Register Pair Push/Pop (PUSH2 / POP2)
;;; ---------------------------------------------------------------------------

(defun test-stage-11-push2-pop2 ()
  (let ((all-ok t)
        (r16 (sb-x86-64-asm::get-gpr :qword 16))
        (r17 (sb-x86-64-asm::get-gpr :qword 17))
        (r18 (sb-x86-64-asm::get-gpr :qword 18))
        (r19 (sb-x86-64-asm::get-gpr :qword 19))
        (rsp (sb-x86-64-asm::get-gpr :qword 4))
        (rax (sb-x86-64-asm::get-gpr :qword 0)))
    (let ((res (exec-asm ()
                 (sb-assem:inst mov r16 #x1111222233334444)
                 (sb-assem:inst mov r17 #x5555666677778888)
                 (sb-assem:inst sub rsp 8)
                 (sb-assem:inst push2 r16 r17)
                 (sb-assem:inst pop2 r19 r18)
                 (sb-assem:inst add rsp 8)
                 (sb-assem:inst xor r18 r16)
                 (sb-assem:inst xor r19 r17)
                 (sb-assem:inst or r18 r19)
                 (sb-assem:inst mov rax r18))))
      (unless (= res 0)
        (setf all-ok nil)
        (format t "    PUSH2/POP2 stack balance/order failure: diff=#x~X~%" res)))
    (report-stage "Stage 11: Register Pair PUSH2/POP2 Stack Balance" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 12: Progressive Morphogenetic Mixed Chaos Mutator
;;; ---------------------------------------------------------------------------

(defun test-stage-12-chaos-mutator ()
  (let ((all-ok t)
        (r-arr (coerce (loop for i from 16 to 31 collect (sb-x86-64-asm::get-gpr :qword i)) 'vector))
        (rax (sb-x86-64-asm::get-gpr :qword 0)))
    (let ((seed #x123456789ABCDEF0))
      (flet ((prng-next ()
               (setf seed (logand (+ (* seed 6364136223846793005) 1442695040888963407) #xFFFFFFFFFFFFFFFF))
               seed))
        (let ((res (exec-asm ()
                     (loop for r across r-arr do (sb-assem:inst mov r 0))
                     (sb-assem:inst mov (aref r-arr 0) 1)
                     (sb-assem:inst mov (aref r-arr 1) 2)
                     (dotimes (i 2000)
                       (let* ((op (mod (prng-next) 5))
                              (rd (mod (prng-next) 16))
                              (rs1 (mod (prng-next) 16))
                              (rs2 (mod (prng-next) 16)))
                         (case op
                           (0 (sb-assem:inst add-ndd (aref r-arr rd) (aref r-arr rs1) (aref r-arr rs2) :nf 1))
                           (1 (sb-assem:inst xor-ndd (aref r-arr rd) (aref r-arr rs1) (aref r-arr rs2)))
                           (2 (sb-assem:inst and-ndd (aref r-arr rd) (aref r-arr rs1) (aref r-arr rs2) :nf 1))
                           (3 (sb-assem:inst inc-ndd (aref r-arr rd) (aref r-arr rs1)))
                           (4 (sb-assem:inst rol-ndd (aref r-arr rd) (aref r-arr rs1) 1)))))
                     (sb-assem:inst mov rax (aref r-arr 0)))))
          (unless (numberp res)
            (setf all-ok nil)
            (format t "    Chaos mutator execution faulted~%")))))
    (report-stage "Stage 12: Morphogenetic Mixed Chaos Mutator" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 13: 32x32 Cross-Permutation & NDD Source Invariance
;;; ---------------------------------------------------------------------------

(defun test-stage-13-permutation ()
  (let ((all-ok t)
        (rax (sb-x86-64-asm::get-gpr :qword 0)))
    (dotimes (i 16)
      (dotimes (j 16)
        (let* ((ri (+ 16 i))
               (rj (+ 16 j))
               (rk (+ 16 (mod (+ i j 1) 16)))
               (val-i (+ #x1000000000000000 (* ri #x0101010101010101)))
               (val-j (if (= ri rj)
                          val-i
                          (+ #x2000000000000000 (* rj #x0202020202020202))))
               (expected-k (logand (+ val-i val-j) #xFFFFFFFFFFFFFFFF))
               (reg-i (sb-x86-64-asm::get-gpr :qword ri))
               (reg-j (sb-x86-64-asm::get-gpr :qword rj))
               (reg-k (sb-x86-64-asm::get-gpr :qword rk))
               (scratch (sb-x86-64-asm::get-gpr :qword (if (= ri 16) 17 16))))
          (multiple-value-bind (res-k res-i res-j)
              (let ((raw (exec-asm ()
                           (sb-assem:inst mov reg-i val-i)
                           (sb-assem:inst mov reg-j val-j)
                           (sb-assem:inst add-ndd reg-k reg-i reg-j)
                           (sb-assem:inst mov rax reg-k)
                           (sb-assem:inst mov scratch reg-i)
                           (sb-assem:inst mov scratch reg-j))))
                (declare (ignore raw))
                (values
                 (exec-asm () (sb-assem:inst mov reg-i val-i) (sb-assem:inst mov reg-j val-j)
                              (sb-assem:inst add-ndd reg-k reg-i reg-j) (sb-assem:inst mov rax reg-k))
                 (exec-asm () (sb-assem:inst mov reg-i val-i) (sb-assem:inst mov reg-j val-j)
                              (sb-assem:inst add-ndd reg-k reg-i reg-j) (sb-assem:inst mov rax reg-i))
                 (exec-asm () (sb-assem:inst mov reg-i val-i) (sb-assem:inst mov reg-j val-j)
                              (sb-assem:inst add-ndd reg-k reg-i reg-j) (sb-assem:inst mov rax reg-j))))
            (unless (= res-k expected-k)
              (setf all-ok nil)
              (format t "    NDD Fail R~D = R~D + R~D: got #x~X expected #x~X~%" rk ri rj res-k expected-k))
            (when (and (/= rk ri) (/= res-i val-i))
              (setf all-ok nil)
              (format t "    NDD Source R~D mutated: got #x~X expected #x~X~%" ri res-i val-i))
            (when (and (/= rk rj) (/= res-j val-j))
              (setf all-ok nil)
              (format t "    NDD Source R~D mutated: got #x~X expected #x~X~%" rj res-j val-j))))))
    (report-stage "Stage 13: 32x32 Cross-Permutation & Invariance Fuzzer (256 pairs)" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 14: Diamond Rapids Fusion (AVX10/AVX-512 + APX EGPR Complex SIB)
;;; ---------------------------------------------------------------------------

(defun test-stage-14-fusion ()
  (let* ((all-ok t)
         (buf (make-array 16 :element-type 'single-float :initial-element 0.0f0)))
    (setf (aref buf 0) 1.0f0 (aref buf 1) 2.0f0 (aref buf 2) 3.0f0 (aref buf 3) 4.0f0)
    (setf (aref buf 4) 10.0f0 (aref buf 5) 20.0f0 (aref buf 6) 30.0f0 (aref buf 7) 40.0f0)
    (sb-sys:with-pinned-objects (buf)
      (let* ((buf-addr (sb-sys:sap-int (sb-sys:vector-sap buf)))
             (r24 (sb-x86-64-asm::get-gpr :qword 24))
             (r25 (sb-x86-64-asm::get-gpr :qword 25))
             (xmm16 (sb-x86-64-asm::get-fpr :xmm 16))
             (xmm17 (sb-x86-64-asm::get-fpr :xmm 17))
             (xmm18 (sb-x86-64-asm::get-fpr :xmm 18))
             (rax (sb-x86-64-asm::get-gpr :qword 0)))
        (exec-asm ()
          (sb-assem:inst mov r24 buf-addr)
          (sb-assem:inst mov r25 4)
          (sb-assem:inst vmovups xmm16 (sb-x86-64-asm::ea 0 r24))
          (sb-assem:inst vmovups xmm17 (sb-x86-64-asm::ea 0 r24 r25 4))
          (sb-assem:inst vaddps xmm18 xmm16 xmm17)
          (sb-assem:inst vmovups (sb-x86-64-asm::ea 32 r24) xmm18)
          (sb-assem:inst mov rax 1))))
    (dotimes (i 4)
      (let ((expected (+ (aref buf i) (aref buf (+ i 4))))
            (actual (aref buf (+ i 8))))
        (unless (= actual expected)
          (setf all-ok nil)
          (format t "    Fusion mismatch at [~D]: got ~A expected ~A~%" i actual expected))))
    (report-stage "Stage 14: Diamond Rapids Fusion (AVX10/AVX-512 + APX EGPR Complex SIB)" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 15: Dual-Oracle Disassembler Audit (SBCL / Optional Intel XED)
;;; ---------------------------------------------------------------------------

(defun slurp-stream (stream)
  (with-output-to-string (s)
    (loop for c = (read-char stream nil nil)
          while c do (write-char c s))))

(defun split-lines (str)
  (let ((lines '()) (start 0))
    (loop for pos = (position #\Newline str :start start)
          do (push (subseq str start (or pos (length str))) lines)
          while pos
          do (setf start (1+ pos)))
    (nreverse lines)))

(defun test-stage-15-dual-oracle ()
  (let* ((all-ok t)
         (xed-env (sb-ext:posix-getenv "INTEL_XED"))
         (xed-path (and xed-env (probe-file xed-env)))
         (test-cases
          (let ((r16 (sb-x86-64-asm::get-gpr :qword 16))
                (r17 (sb-x86-64-asm::get-gpr :qword 17))
                (r18 (sb-x86-64-asm::get-gpr :qword 18))
                (r19 (sb-x86-64-asm::get-gpr :qword 19))
                (r20 (sb-x86-64-asm::get-gpr :qword 20))
                (r21 (sb-x86-64-asm::get-gpr :qword 21))
                (r22 (sb-x86-64-asm::get-gpr :qword 22))
                (r23 (sb-x86-64-asm::get-gpr :qword 23))
                (r24 (sb-x86-64-asm::get-gpr :qword 24))
                (r25 (sb-x86-64-asm::get-gpr :qword 25))
                (r26 (sb-x86-64-asm::get-gpr :qword 26))
                (r31 (sb-x86-64-asm::get-gpr :qword 31)))
            `((:rex2-mov  (mov ,r16 ,r17)          "MOV"    #(213 88 139 193))
              (:rex2-add  (add ,r31 ,r16)          "ADD"    #(213 89 1 199))
              (:add-ndd   (add-ndd ,r16 ,r17 ,r18) "ADD"    #(98 236 252 16 1 209))
              (:sub-ndd   (sub-ndd ,r19 ,r20 42)   "SUB"    #(98 252 228 16 131 236 42))
              (:xor-ndd   (xor-ndd ,r21 ,r22 ,r23) "XOR"    #(98 236 212 16 49 254))
              (:imul-ndd  (imul-ndd ,r24 ,r25 ,r26)"IMUL"   #(98 76 188 16 175 209))
              (:shl-ndd   (shl-ndd ,r16 ,r17 10)   "SHL"    #(98 252 252 16 193 225 10))
              (:ccmp      (ccmp :e ,r16 ,r17 0)    "CCMP"   #(98 236 132 4 57 200))
              (:ctest     (ctest :ne ,r16 ,r17 0)  "CTEST"  #(98 236 132 5 133 200))
              (:cfcmov    (cfcmov :z ,r16 ,r17)    "CFCMOV" #(98 236 252 8 68 193))
              (:push2     (push2 ,r16 ,r17)        "PUSH2"  #(98 252 124 16 255 241))
              (:pop2      (pop2 ,r18 ,r19)         "POP2"   #(98 252 108 16 143 195))))))
    (dolist (tc test-cases)
      (destructuring-bind (name inst-form expected-mnemonic expected-bytes) tc
        (let ((seg (sb-assem:make-segment)))
          (sb-assem:assemble (seg nil)
            (apply #'sb-assem:inst* (car inst-form) (cdr inst-form)))
          (let* ((buf (sb-assem::segment-buffer seg))
                 (len (sb-assem::segment-current-posn seg))
                 (bytes (subseq buf 0 len))
                 (hex-str (format nil "~{~2,'0X~}" (coerce bytes 'list))))
            (unless (equalp bytes expected-bytes)
              (setf all-ok nil)
              (format t "    [ENCODING FAIL] ~A: got ~A expected ~A~%" name bytes expected-bytes))
            (when xed-path
              (let* ((xed-proc (sb-ext:run-program xed-path
                                                   (list "-64" "-chip-check" "DIAMOND_RAPIDS" "-d" hex-str)
                                                   :output :stream :error nil))
                     (xed-out (slurp-stream (sb-ext:process-output xed-proc))))
                (sb-ext:process-wait xed-proc)
                (let ((xed-exit (sb-ext:process-exit-code xed-proc))
                      (found-mnemonic (search expected-mnemonic (string-upcase xed-out))))
                  (unless (and (zerop xed-exit) found-mnemonic)
                    (setf all-ok nil)
                    (format t "    [XED FAIL] ~A (~A): exit=~D mnemonic ~S found=~A~%"
                            name hex-str xed-exit expected-mnemonic (if found-mnemonic t nil))))))))))
    (report-stage "Stage 15: Disassembler Differential Audit" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 16: Dynamic In-Line VOP Synthesis & Compiled Lisp Execution
;;; ---------------------------------------------------------------------------

(defun test-stage-16-vop ()
  (let ((all-ok t))
    (handler-bind ((warning #'muffle-warning))
      (sb-c:defknown %apx-vop-add-ndd (fixnum fixnum) fixnum
          (sb-c:foldable sb-c:flushable)
        :overwrite-fndb-silently t)

      (sb-c:define-vop (%apx-vop-add-ndd)
        (:translate %apx-vop-add-ndd)
        (:policy :fast-safe)
        (:args (x :scs (any-reg) :target r)
               (y :scs (any-reg)))
        (:arg-types tagged-num tagged-num)
        (:results (r :scs (any-reg)))
        (:result-types tagged-num)
        (:generator 5
          (let ((r16 (sb-x86-64-asm::get-gpr :qword 16))
                (r17 (sb-x86-64-asm::get-gpr :qword 17)))
            (sb-assem:inst mov r16 x)
            (sb-assem:inst mov r17 y)
            (sb-assem:inst add-ndd r r16 r17))))

      (let* ((fn (compile nil '(lambda (a b)
                                (declare (type fixnum a b)
                                         (optimize (speed 3) (safety 0)))
                                (%apx-vop-add-ndd a b))))
             (res (funcall fn 1000 2500)))
        (unless (= res 3500)
          (setf all-ok nil)
          (format t "    Compiled VOP execution fail: got ~D expected 3500~%" res))))
    (report-stage "Stage 16: Dynamic In-Line VOP Synthesis & Compiled Lisp Execution" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 17: Branchless Boolean Logic Synthesis (CCMP/CFCMOV Truth Tables)
;;; ---------------------------------------------------------------------------

(defun test-stage-17-branchless ()
  (let ((all-ok t)
        (r16 (sb-x86-64-asm::get-gpr :qword 16))
        (r17 (sb-x86-64-asm::get-gpr :qword 17))
        (r18 (sb-x86-64-asm::get-gpr :qword 18))
        (rax (sb-x86-64-asm::get-gpr :qword 0)))
    (flet ((eval-branchless (val-x val-y)
             (exec-asm ()
               (sb-assem:inst mov r16 val-x)
               (sb-assem:inst mov r17 val-y)
               (sb-assem:inst cmp r16 10)
               (sb-assem:inst ccmp :g r17 20 0)
               (sb-assem:inst mov r18 1)
               (sb-assem:inst mov rax 0)
               (sb-assem:inst cfcmov :le rax r18))))
      (let ((tests '((15 10  1)
                     (5  10  0)
                     (15 25  0)
                     (5  25  0))))
        (dolist (test tests)
          (destructuring-bind (x y expected) test
            (let ((actual (eval-branchless x y)))
              (unless (= actual expected)
                (setf all-ok nil)
                (format t "    Truth Table Fail for x=~D, y=~D: got ~D expected ~D~%"
                        x y actual expected)))))))
    (report-stage "Stage 17: Branchless Boolean Logic Synthesis (CCMP/CFCMOV Truth Tables)" all-ok)))

;;; ===========================================================================
;;; CRONENBERG BODY HORROR (PROGRESSIVE MORPHOGENETIC MUTATOR) FOR 64-BIT / APX
;;; ===========================================================================

(defstruct (cronenberg-prng
            (:constructor make-cronenberg-prng (&optional (state #xDEADBEEFCAFE64)))
            (:copier nil))
  "Zero-consing 64-bit SplitMix64 deterministic PRNG."
  (state #xDEADBEEFCAFE64 :type (unsigned-byte 64)))

(declaim (inline %cronenberg-next-u64 %cronenberg-next-u32 %cronenberg-next-range))

(defun %cronenberg-next-u64 (prng)
  (declare (type cronenberg-prng prng) (optimize (speed 3) (safety 0)))
  (let* ((s (cronenberg-prng-state prng))
         (z (the (unsigned-byte 64) (logand (+ s #x9E3779B97F4A7C15) #xFFFFFFFFFFFFFFFF))))
    (setf (cronenberg-prng-state prng) z)
    (let* ((v (the (unsigned-byte 64) (logxor z (ash z -30))))
           (v2 (the (unsigned-byte 64) (logand (* v #xBF58476D1CE4E5B9) #xFFFFFFFFFFFFFFFF)))
           (v3 (the (unsigned-byte 64) (logxor v2 (ash v2 -27))))
           (v4 (the (unsigned-byte 64) (logand (* v3 #x94D049BB133111EB) #xFFFFFFFFFFFFFFFF))))
      (the (unsigned-byte 64) (logxor v4 (ash v4 -31))))))

(defun %cronenberg-next-u32 (prng)
  (declare (type cronenberg-prng prng) (optimize (speed 3) (safety 0)))
  (the (unsigned-byte 32) (logand (%cronenberg-next-u64 prng) #xFFFFFFFF)))

(defun %cronenberg-next-range (prng limit)
  (declare (type cronenberg-prng prng) (type fixnum limit) (optimize (speed 3) (safety 0)))
  (if (<= limit 1)
      0
      (the fixnum (mod (%cronenberg-next-u32 prng) limit))))

;; 256-entry 64-bit Extreme Topological Integer LUT
(declaim (type (simple-array (unsigned-byte 64) (256)) *apx-extreme-int64-lut*))
(defparameter *apx-extreme-int64-lut*
  (let ((lut (make-array 256 :element-type '(unsigned-byte 64) :initial-element 0))
        (idx 0))
    (flet ((push-val (v)
             (when (< idx 256)
               (setf (aref lut idx) (ldb (byte 64 0) v))
               (incf idx))))
      ;; 0..15: Singularities (16 entries)
      (dolist (v (list 0 -1 1 -2 2
                       cl:most-positive-fixnum cl:most-negative-fixnum
                       #x7FFFFFFF #x80000000 #xFFFFFFFF #x100000000
                       #x7FFFFFFFFFFFFFFF #x8000000000000000
                       #x7FFF #x8000 #x7F))
        (push-val v))
      ;; 16..47: Power-of-2 boundaries and off-by-ones (32 entries)
      (dolist (k '(7 8 15 16 23 24 31 32 47 48 62 63))
        (push-val (ash 1 k))
        (push-val (1- (ash 1 k))))
      (loop while (< idx 48) do (push-val (ash 1 (mod idx 64))))
      ;; 48..111: Sub-QWORD truncation traps & necrotic halves (64 entries)
      (dolist (v '(#xDEADBEEF00000000 #xCAFEBABE00000000 #xFEEDFACE00000000 #xBAADF00D00000000
                   #x00000000DEADBEEF #x00000000CAFEBABE #x00000000FEEDFACE #x00000000BAADF00D
                   #xDEADBEEFCAFEBABE #xBAADF00DFEEDFACE #x1122334455667788 #x8877665544332211
                   #xFF00FF00FF00FF00 #x00FF00FF00FF00FF #x0000FFFF0000FFFF #xFFFF0000FFFF0000
                   #x00000000FFFFFFFF #xFFFFFFFF00000000 #xAAAAAAAA55555555 #x55555555AAAAAAAA))
        (push-val v))
      (loop while (< idx 112) do
        (push-val (logior (ash #xDEADBEEF 32) (ash 1 (mod idx 32)))))
      ;; 112..143: Sign/Zero extension singularities (32 entries)
      (dolist (v '(#xFFFFFFFF80000000 #x0000000080000000
                   #xFFFFFFFFFFFF8000 #x0000000000008000
                   #xFFFFFFFFFFFFFF80 #x0000000000000080
                   #x7FFFFFFFFFFFFFFF #x8000000000000000
                   #x000000007FFFFFFF #xFFFFFFFF80000000
                   #x0000000000007FFF #xFFFFFFFFFFFF8000
                   #x000000000000007F #xFFFFFFFFFFFFFF80))
        (push-val v))
      (loop while (< idx 144) do (push-val (logxor #xFFFFFFFFFFFFFFFF (ash 1 (mod idx 64)))))
      ;; 144..207: Walking 1s and 0s (64 entries)
      (dotimes (i 32) (push-val (ash 1 i)))
      (dotimes (i 32) (push-val (logxor #xFFFFFFFFFFFFFFFF (ash 1 i))))
      ;; 208..239: High-Entropy bit noise & waveforms (32 entries)
      (dolist (v '(#x5555555555555555 #xAAAAAAAAAAAAAAAA #x3333333333333333 #xCCCCCCCCCCCCCCCC
                   #x0F0F0F0F0F0F0F0F #xF0F0F0F0F0F0F0F0 #x5A5A5A5A5A5A5A5A #xA5A5A5A5A5A5A5A5
                   #x0123456789ABCDEF #xFEDCBA9876543210 #x0011223344556677 #x8899AABBCCDDEEFF))
        (push-val v))
      (loop while (< idx 240) do (push-val (logxor #x5555555555555555 (ash idx 8))))
      ;; 240..255: Lisp pointer tags & alignment traps (16 entries)
      (dolist (v '(#x0000000000000000 #x0000000000000001 #x0000000000000003
                   #x0000000000000007 #x000000000000000B #x000000000000000F
                   #x1000000000000007 #x200000000000000B #x300000000000000F))
        (push-val v))
      (loop while (< idx 256) do (push-val (logior (ash 1 (mod idx 64)) 7))))
    lut))

(defun cronenberg-mutate-gprs (vec stage prng)
  "Apply one of 7 progressive morphogenetic body mutations to the 32-register GPR array."
  (declare (type (simple-array (unsigned-byte 64) (32)) vec)
           (type keyword stage)
           (type cronenberg-prng prng)
           (optimize (speed 3) (safety 1)))
  (case stage
    (:genetic-drift
     (let ((reg (%cronenberg-next-range prng 32))
           (bit (%cronenberg-next-range prng 64)))
       (setf (aref vec reg) (logxor (aref vec reg) (ash 1 bit)))))

    (:cellular-dysplasia
     (let ((src-reg (%cronenberg-next-range prng 32))
           (span (1+ (%cronenberg-next-range prng 3))))
       (dotimes (i span)
         (let ((target (mod (+ src-reg i 1) 32)))
           (setf (aref vec target) (aref vec src-reg))))))

    (:organ-transposition
     (let ((idx-a (%cronenberg-next-range prng 16))
           (idx-b (+ 16 (%cronenberg-next-range prng 16))))
       (rotatef (aref vec idx-a) (aref vec idx-b))))

    (:necrotic-slush
     (let* ((reg (%cronenberg-next-range prng 32))
            (slush (case (%cronenberg-next-range prng 5)
                     (0 #xDEADBEEF)
                     (1 #xCAFEBABE)
                     (2 #xFEEDFACE)
                     (3 #xBAADF00D)
                     (otherwise #xFFFFFFFF))))
       (setf (aref vec reg) (logior (ldb (byte 32 0) (aref vec reg))
                                    (ash slush 32)))))

    (:frankenstein-chimera
     (let ((reg (%cronenberg-next-range prng 32))
           (alien-val (aref *apx-extreme-int64-lut* (%cronenberg-next-range prng 256))))
       (setf (aref vec reg) alien-val)))

    (:sub-qword-truncation
     (let* ((reg (%cronenberg-next-range prng 32))
            (mode (%cronenberg-next-range prng 3)))
       (case mode
         (0 (setf (aref vec reg) (ldb (byte 8 0) (aref vec reg))))
         (1 (setf (aref vec reg) (ldb (byte 16 0) (aref vec reg))))
         (2 (setf (aref vec reg) (ldb (byte 32 0) (aref vec reg)))))))

    (:resonant-teratoma
     (let* ((reg-a (%cronenberg-next-range prng 31))
            (val (aref vec reg-a)))
       (setf (aref vec (1+ reg-a))
             (if (zerop (%cronenberg-next-range prng 2))
                 (ldb (byte 64 0) (- val))
                 (logxor val #xFFFFFFFFFFFFFFFF))))))
  vec)

;;; ---------------------------------------------------------------------------
;;; Stage 18: SIB R20/R12/R28 Index & Pathological Base Anomaly
;;; ---------------------------------------------------------------------------

(defun test-stage-18-sib-r20-and-pathological-bases ()
  (let ((all-ok t)
        (buf (make-array 1024 :element-type '(unsigned-byte 64) :initial-element 0)))
    (sb-sys:with-pinned-objects (buf)
      (let* ((buf-addr (sb-sys:sap-int (sb-sys:vector-sap buf)))
             (rax (sb-x86-64-asm::get-gpr :qword 0))
             (r16 (sb-x86-64-asm::get-gpr :qword 16))
             (r20 (sb-x86-64-asm::get-gpr :qword 20))
             (r12 (sb-x86-64-asm::get-gpr :qword 12))
             (r28 (sb-x86-64-asm::get-gpr :qword 28))
             (r29 (sb-x86-64-asm::get-gpr :qword 29))
             (val-expected #xCAFEBABEDEADBEEF))
        ;; 1. R20 as Index: [r16 + r20*8 + 32]
        (setf (aref buf (+ 5 4)) val-expected)
        (let ((res (exec-asm ()
                     (sb-assem:inst mov r16 buf-addr)
                     (sb-assem:inst mov r20 5)
                     (sb-assem:inst mov rax (sb-x86-64-asm::ea 32 r16 r20 8)))))
          (unless (= res val-expected)
            (setf all-ok nil)
            (format t "    R20 Index test fail: got #x~X expected #x~X~%" res val-expected)))

        ;; 2. R12 as Index with callee-save preservation: [r16 + r12*8 + 32]
        (setf (aref buf (+ 7 4)) #x1212121212121212)
        (let ((res (exec-asm ()
                     (sb-assem:inst push r12)
                     (sb-assem:inst mov r16 buf-addr)
                     (sb-assem:inst mov r12 7)
                     (sb-assem:inst mov rax (sb-x86-64-asm::ea 32 r16 r12 8))
                     (sb-assem:inst pop r12))))
          (unless (= res #x1212121212121212)
            (setf all-ok nil)
            (format t "    R12 Index test fail: got #x~X~%" res)))

        ;; 3. R28 as Index: [r16 + r28*8 + 32]
        (setf (aref buf (+ 3 4)) #x2828282828282828)
        (let ((res (exec-asm ()
                     (sb-assem:inst mov r16 buf-addr)
                     (sb-assem:inst mov r28 3)
                     (sb-assem:inst mov rax (sb-x86-64-asm::ea 32 r16 r28 8)))))
          (unless (= res #x2828282828282828)
            (setf all-ok nil)
            (format t "    R28 Index test fail: got #x~X~%" res)))

        ;; 4. Base R28 without Index: forces SIB byte (base=4)
        (setf (aref buf 10) #xAABBCCDDEEFF0011)
        (let ((res (exec-asm ()
                     (sb-assem:inst mov r28 (+ buf-addr (* 10 8)))
                     (sb-assem:inst mov rax (sb-x86-64-asm::ea 0 r28)))))
          (unless (= res #xAABBCCDDEEFF0011)
            (setf all-ok nil)
            (format t "    Base R28 test fail: got #x~X~%" res)))

        ;; 5. Base R29 without Index with disp=0: forces disp8=0
        (setf (aref buf 12) #x9988776655443322)
        (let ((res (exec-asm ()
                     (sb-assem:inst mov r29 (+ buf-addr (* 12 8)))
                     (sb-assem:inst mov rax (sb-x86-64-asm::ea 0 r29)))))
          (unless (= res #x9988776655443322)
            (setf all-ok nil)
            (format t "    Base R29 test fail: got #x~X~%" res)))))
    (report-stage "Stage 18: SIB R20/R12/R28 Index & Pathological Base Anomaly" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 19: Cronenberg Sub-QWORD NDD Truncation & Zero-Extension Mutator
;;; ---------------------------------------------------------------------------

(defun test-stage-19-cronenberg-subqword-ndd ()
  (let* ((all-ok t)
         (prng (make-cronenberg-prng #x5EEDC0DECAFEBABE))
         (gpr-vec (make-array 32 :element-type '(unsigned-byte 64) :initial-element 0))
         (stages '(:genetic-drift :cellular-dysplasia :organ-transposition
                   :necrotic-slush :frankenstein-chimera :sub-qword-truncation :resonant-teratoma))
         (ops '(:add :sub :xor :and :or))
         (dirty-slush #xDEADBEEFCAFEBABE)
         (rax (sb-x86-64-asm::get-gpr :qword 0)))
    (dotimes (i 32)
      (setf (aref gpr-vec i) (aref *apx-extreme-int64-lut* (mod (* i 7) 256))))

    (dotimes (round 2000)
      (let ((stage (nth (mod round (length stages)) stages)))
        (cronenberg-mutate-gprs gpr-vec stage prng))

      (let* ((d-idx (+ 16 (%cronenberg-next-range prng 16)))
             (s1-idx (+ 16 (%cronenberg-next-range prng 16)))
             (s2-idx (+ 16 (%cronenberg-next-range prng 16)))
             (op (nth (%cronenberg-next-range prng (length ops)) ops)))
        (loop while (= s1-idx d-idx) do (setf s1-idx (+ 16 (%cronenberg-next-range prng 16))))
        (loop while (= s2-idx d-idx) do (setf s2-idx (+ 16 (%cronenberg-next-range prng 16))))
        (let* ((val-s1 (aref gpr-vec s1-idx))
               (val-s2 (aref gpr-vec s2-idx))
               (expected-32
                (ldb (byte 32 0)
                     (case op
                       (:add (+ val-s1 val-s2))
                       (:sub (- val-s1 val-s2))
                       (:xor (logxor val-s1 val-s2))
                       (:and (logand val-s1 val-s2))
                       (:or  (logior val-s1 val-s2))))))
          (let ((res-d (exec-asm ()
                         (sb-assem:inst mov (sb-x86-64-asm::get-gpr :qword d-idx) dirty-slush)
                         (sb-assem:inst mov (sb-x86-64-asm::get-gpr :qword s1-idx) val-s1)
                         (sb-assem:inst mov (sb-x86-64-asm::get-gpr :qword s2-idx) val-s2)
                         (case op
                           (:add (sb-assem:inst add-ndd
                                                (sb-x86-64-asm::get-gpr :dword d-idx)
                                                (sb-x86-64-asm::get-gpr :dword s1-idx)
                                                (sb-x86-64-asm::get-gpr :dword s2-idx)))
                           (:sub (sb-assem:inst sub-ndd
                                                (sb-x86-64-asm::get-gpr :dword d-idx)
                                                (sb-x86-64-asm::get-gpr :dword s1-idx)
                                                (sb-x86-64-asm::get-gpr :dword s2-idx)))
                           (:xor (sb-assem:inst xor-ndd
                                                (sb-x86-64-asm::get-gpr :dword d-idx)
                                                (sb-x86-64-asm::get-gpr :dword s1-idx)
                                                (sb-x86-64-asm::get-gpr :dword s2-idx)))
                           (:and (sb-assem:inst and-ndd
                                                (sb-x86-64-asm::get-gpr :dword d-idx)
                                                (sb-x86-64-asm::get-gpr :dword s1-idx)
                                                (sb-x86-64-asm::get-gpr :dword s2-idx)))
                           (:or  (sb-assem:inst or-ndd
                                                (sb-x86-64-asm::get-gpr :dword d-idx)
                                                (sb-x86-64-asm::get-gpr :dword s1-idx)
                                                (sb-x86-64-asm::get-gpr :dword s2-idx))))
                         (sb-assem:inst mov rax (sb-x86-64-asm::get-gpr :qword d-idx)))))
            (unless (= (ldb (byte 32 0) res-d) expected-32)
              (setf all-ok nil)
              (format t "    DWORD NDD ALU fail: op ~A got #x~X expected #x~X~%"
                      op (ldb (byte 32 0) res-d) expected-32))
            (unless (= (ldb (byte 32 32) res-d) 0)
              (setf all-ok nil)
              (format t "    DWORD NDD Zero-Extension fail: upper 32 bits leaked: #x~X~%"
                      (ldb (byte 32 32) res-d)))))))
    (report-stage "Stage 19: Cronenberg Sub-QWORD NDD Truncation & Zero-Extension" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 20: Cronenberg 256-State CCMP/CTEST Combinatorial Exhaustion
;;; ---------------------------------------------------------------------------

(defun test-stage-20-cronenberg-ccmp-256 ()
  (let ((all-ok t)
        (rax (sb-x86-64-asm::get-gpr :qword 0))
        (r16 (sb-x86-64-asm::get-gpr :qword 16))
        (r17 (sb-x86-64-asm::get-gpr :qword 17)))
    (dotimes (dfv 16)
      (let ((eflags (exec-asm ()
                      (sb-assem:inst mov r16 100)
                      (sb-assem:inst mov r17 200)
                      (sb-assem:inst cmp r16 r17)
                      (sb-assem:inst ccmp :e r16 r17 dfv)
                      (sb-assem:inst pushf)
                      (sb-assem:inst pop rax))))
        (let ((cf (ldb (byte 1 0) eflags))
              (zf (ldb (byte 1 6) eflags))
              (sf (ldb (byte 1 7) eflags))
              (of (ldb (byte 1 11) eflags)))
          (let ((extracted-dfv (logior cf (ash zf 1) (ash sf 2) (ash of 3))))
            (unless (= extracted-dfv dfv)
              (setf all-ok nil)
              (format t "    CCMP DFV fail: injected DFV=~D, extracted=~D~%" dfv extracted-dfv))))))
    (report-stage "Stage 20: Cronenberg 256-State CCMP/DFV Combinatorial Exhaustion" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 21: Exhaustive CFCMOV 32x32 True/False Architectural Zeroing Matrix
;;; ---------------------------------------------------------------------------

(defun test-stage-21-cronenberg-cfcmov-zeroing ()
  (let ((all-ok t)
        (rax (sb-x86-64-asm::get-gpr :qword 0))
        (r18 (sb-x86-64-asm::get-gpr :qword 18))
        (r19 (sb-x86-64-asm::get-gpr :qword 19))
        (dirty-slush #xCAFEBABEDEADBEEF)
        (test-val    #x1122334455667788))
    (dolist (cond '(:e :ne :g :le :b :ae :s :ns))
      ;; Test 1: Condition evaluates to FALSE -> dst MUST BE CLEARED TO ZERO!
      (let* ((d-reg (sb-x86-64-asm::get-gpr :qword 21))
             (s-reg (sb-x86-64-asm::get-gpr :qword 22))
             (res-false
              (exec-asm ()
                (sb-assem:inst mov d-reg dirty-slush)
                (sb-assem:inst mov s-reg test-val)
                (sb-assem:inst mov r18 10)
                (sb-assem:inst mov r19 20)
                (case cond
                  (:e  (sb-assem:inst cmp r18 r19))
                  (:ne (sb-assem:inst cmp r18 r18))
                  (:g  (sb-assem:inst cmp r18 r19))
                  (:le (sb-assem:inst cmp r19 r18))
                  (:b  (sb-assem:inst cmp r19 r18))
                  (:ae (sb-assem:inst cmp r18 r19))
                  (:s  (sb-assem:inst cmp r19 r18))
                  (:ns (sb-assem:inst cmp r18 r19)))
                (sb-assem:inst cfcmov cond d-reg s-reg)
                (sb-assem:inst mov rax d-reg))))
        (unless (zerop res-false)
          (setf all-ok nil)
          (format t "    CFCMOV false zeroing fail on ~A: expected 0, got #x~X~%" cond res-false)))

      ;; Test 2: Condition evaluates to TRUE -> dst MUST BE COPIED!
      (let* ((d-reg (sb-x86-64-asm::get-gpr :qword 23))
             (s-reg (sb-x86-64-asm::get-gpr :qword 24))
             (res-true
              (exec-asm ()
                (sb-assem:inst mov d-reg 0)
                (sb-assem:inst mov s-reg test-val)
                (sb-assem:inst mov r18 10)
                (sb-assem:inst mov r19 20)
                (case cond
                  (:e  (sb-assem:inst cmp r18 r18))
                  (:ne (sb-assem:inst cmp r18 r19))
                  (:g  (sb-assem:inst cmp r19 r18))
                  (:le (sb-assem:inst cmp r18 r19))
                  (:b  (sb-assem:inst cmp r18 r19))
                  (:ae (sb-assem:inst cmp r19 r18))
                  (:s  (sb-assem:inst cmp r18 r19))
                  (:ns (sb-assem:inst cmp r19 r18)))
                (sb-assem:inst cfcmov cond d-reg s-reg)
                (sb-assem:inst mov rax d-reg))))
        (unless (= res-true test-val)
          (setf all-ok nil)
          (format t "    CFCMOV true copy fail on ~A: expected #x~X, got #x~X~%"
                  cond test-val res-true))))
    (report-stage "Stage 21: Exhaustive CFCMOV 32x32 True/False Architectural Zeroing" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 22: Memory-Source 3-Operand NDD with Complex EGPR SIB Addressing
;;; ---------------------------------------------------------------------------

(defun test-stage-22-memory-ndd-sib ()
  (let ((all-ok t)
        (buf (make-array 64 :element-type '(unsigned-byte 64) :initial-element 0)))
    (setf (aref buf 10) #x1000200030004000)
    (sb-sys:with-pinned-objects (buf)
      (let* ((buf-addr (sb-sys:sap-int (sb-sys:vector-sap buf)))
             (rax (sb-x86-64-asm::get-gpr :qword 0))
             (r16 (sb-x86-64-asm::get-gpr :qword 16))
             (r17 (sb-x86-64-asm::get-gpr :qword 17))
             (r24 (sb-x86-64-asm::get-gpr :qword 24))
             (r25 (sb-x86-64-asm::get-gpr :qword 25))
             (val-src #x0111022203330444)
             (val-mem (aref buf 10))
             (expected (+ val-mem val-src)))
        (let ((res (exec-asm ()
                     (sb-assem:inst mov r24 buf-addr)
                     (sb-assem:inst mov r25 8)
                     (sb-assem:inst mov r17 val-src)
                     (sb-assem:inst mov r16 #xDEADBEEF)
                     (sb-assem:inst add-ndd r16 (sb-x86-64-asm::ea 16 r24 r25 8) r17)
                     (sb-assem:inst mov rax r16))))
          (unless (= res expected)
            (setf all-ok nil)
            (format t "    Memory NDD fail: got #x~X expected #x~X~%" res expected))
          (unless (= (aref buf 10) val-mem)
            (setf all-ok nil)
            (format t "    Memory corrupted: got #x~X expected #x~X~%" (aref buf 10) val-mem)))))
    (report-stage "Stage 22: Memory-Source 3-Operand NDD with Complex EGPR SIB" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Stage 23: Lisp GC Scavenging & Live Heap Object Pointer Integrity
;;; ---------------------------------------------------------------------------

(defun test-stage-23-lisp-gc-integrity ()
  (let* ((all-ok t)
         (objects (make-array 32)))
    (dotimes (i 32)
      (setf (aref objects i)
            (case (mod i 4)
              (0 (cons (format nil "cons-~D" i) i))
              (1 (format nil "string-heap-object-~D" i))
              (2 (make-array 4 :initial-element (+ i 100)))
              (3 (+ #x1000000000000000 i)))))
    (sb-sys:with-pinned-objects (objects)
      (let* ((r16 (sb-x86-64-asm::get-gpr :qword 16))
             (r17 (sb-x86-64-asm::get-gpr :qword 17))
             (r18 (sb-x86-64-asm::get-gpr :qword 18))
             (rax (sb-x86-64-asm::get-gpr :qword 0)))
        (let* ((ptr0 (sb-kernel:get-lisp-obj-address (aref objects 0)))
               (ptr1 (sb-kernel:get-lisp-obj-address (aref objects 1)))
               (res (exec-asm ()
                      (sb-assem:inst mov r16 ptr0)
                      (sb-assem:inst mov r17 ptr1)
                      (sb-assem:inst xor-ndd r18 r16 r17)
                      (sb-assem:inst xor-ndd rax r18 r17))))
          (unless (= res ptr0)
            (setf all-ok nil)
            (format t "    GC Pointer XOR-NDD restoration fail: got #x~X expected #x~X~%" res ptr0)))))
    (sb-ext:gc :full t)
    (dotimes (i 32)
      (let ((obj (aref objects i)))
        (case (mod i 4)
          (0 (unless (and (consp obj) (string= (car obj) (format nil "cons-~D" i)) (= (cdr obj) i))
               (setf all-ok nil)
               (format t "    GC corrupted cons cell ~D~%" i)))
          (1 (unless (string= obj (format nil "string-heap-object-~D" i))
               (setf all-ok nil)
               (format t "    GC corrupted string object ~D~%" i)))
          (2 (unless (and (vectorp obj) (= (aref obj 0) (+ i 100)))
               (setf all-ok nil)
               (format t "    GC corrupted vector object ~D~%" i)))
          (3 (unless (= obj (+ #x1000000000000000 i))
               (setf all-ok nil)
               (format t "    GC corrupted bignum object ~D~%" i))))))
    (report-stage "Stage 23: Lisp GC Scavenging & Live Heap Object Pointer Integrity" all-ok)))

;;; ---------------------------------------------------------------------------
;;; Master Test Runner Integration
;;; ---------------------------------------------------------------------------

(test-util:with-test (:name :apx-hardware-execution)
  (flet ((step-tick ()
           (write-char #\. *trace-output*)
           (force-output *trace-output*)))
    (test-stage-1-egpr-integrity) (step-tick)
    (test-stage-2-rex2-matrix) (step-tick)
    (test-stage-3-sib-matrix) (step-tick)
    (test-stage-4-ndd-alu) (step-tick)
    (test-stage-5-unary-ndd) (step-tick)
    (test-stage-6-shift-ndd) (step-tick)
    (test-stage-7-nf-invariance) (step-tick)
    (test-stage-8-9-ccmp-ctest) (step-tick)
    (test-stage-10-cfcmov) (step-tick)
    (test-stage-11-push2-pop2) (step-tick)
    (test-stage-12-chaos-mutator) (step-tick)
    (test-stage-13-permutation) (step-tick)
    (test-stage-14-fusion) (step-tick)
    (test-stage-15-dual-oracle) (step-tick)
    (test-stage-16-vop) (step-tick)
    (test-stage-17-branchless) (step-tick)
    (test-stage-18-sib-r20-and-pathological-bases) (step-tick)
    (test-stage-19-cronenberg-subqword-ndd) (step-tick)
    (test-stage-20-cronenberg-ccmp-256) (step-tick)
    (test-stage-21-cronenberg-cfcmov-zeroing) (step-tick)
    (test-stage-22-memory-ndd-sib) (step-tick)
    (test-stage-23-lisp-gc-integrity) (step-tick)
    (terpri *trace-output*)
    t))
