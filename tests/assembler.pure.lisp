;;;; tests for assembler/disassembler

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

(cl:in-package "SB-VM")

(test-util:enable-test-parallelism)

;; this is architecture-agnostic
(defun test-assemble (inst expect)
  (let ((segment (sb-assem:make-segment)))
    (sb-assem:assemble (segment 'nil)
        (apply #'sb-assem:inst* (car inst) (cdr inst)))
    (let* ((buf (sb-assem:segment-buffer segment))
           (string
             (with-output-to-string (stream)
               (with-pinned-objects (buf)
                 (let ((sb-disassem:*disassem-location-column-width* 0))
                   (sb-disassem:disassemble-memory
                    (sap-int (vector-sap buf))
                    (sb-assem::segment-current-posn segment)
                    :stream stream)))))
           (line (string-left-trim'(#\; #\ )
                                  (subseq string (1+ (position #\newline string))
                                          (1- (length string)))))) ; chop final newline
      (assert (string= line expect)))))

(defun check-does-not-assemble (instruction)
  (handler-case (test-assemble instruction "")
    (error nil)
    (:no-error (x) x (error "Should not assemble"))))

;;; Create some special variables that are needed for tests
;;; since they no longer exist as part of the vm definition.
#+x86-64
(macrolet ((define (name qword-tn size)
             `(defvar ,name
                (sb-x86-64-asm::sized-thing (sb-x86-64-asm::tn-reg ,qword-tn)
                                            ,size))))
  (define al rax-tn :byte)
  (define bl rbx-tn :byte)
  (define cl rcx-tn :byte)
  (define dl rdx-tn :byte)
  (define dil rdi-tn :byte)
  (define r8b r8-tn :byte)

  (define ax rax-tn :word)
  (define bx rbx-tn :word)
  (define cx rcx-tn :word)
  (define r8w r8-tn :word)

  (define eax rax-tn :dword)
  (define ebx rbx-tn :dword)
  (define ecx rcx-tn :dword)
  (define edx rdx-tn :dword)
  (define edi rdi-tn :dword)
  (define r8d r8-tn :dword)
  (define r9d r9-tn :dword))
#+x86
(progn (defglobal al al-tn) (defglobal ax ax-tn) (defglobal eax eax-tn)
       (defglobal bl bl-tn) (defglobal bx bx-tn) (defglobal ebx ebx-tn)
       (defglobal cl cl-tn) (defglobal cx cx-tn) (defglobal ecx ecx-tn)
       (defglobal dl dl-tn)
       (defglobal edi edi-tn))
(test-util:with-test (:name :assemble-movnti-instruction :skipped-on (not :x86-64))
  (flet ((test-movnti (dst src expect)
           (test-assemble `(movnti ,dst ,src) expect)))
    (test-movnti (ea 57 rdi-tn) eax "0FC34739         MOVNTI [RDI+57], EAX")
    (test-movnti (ea rax-tn) r11-tn "4C0FC318         MOVNTI [RAX], R11")))

(test-util:with-test (:name :assemble-crc32 :skipped-on (not :x86-64))
  ;; Destination size = :DWORD
  (test-assemble `(crc32 :byte ,eax ,(ea rbp-tn))
                 "F20F38F04500     CRC32 EAX, BYTE PTR [RBP]")
  (test-assemble `(crc32 :byte ,eax (,rcx-tn . :high-byte))
                 "F20F38F0C5       CRC32 EAX, CH")
  (test-assemble `(crc32 :byte ,eax ,dil)
                 "F2400F38F0C7     CRC32 EAX, DIL")
  (test-assemble `(crc32 :word ,eax ,(ea rbp-tn))
                 "66F20F38F14500   CRC32 EAX, WORD PTR [RBP]")
  (test-assemble `(crc32 :dword ,eax ,(ea rbp-tn))
                 "F20F38F14500     CRC32 EAX, DWORD PTR [RBP]")
  ;; these check that the presence of REX does not per se change the width.
  (test-assemble `(crc32 :byte ,r9d ,(ea r14-tn r15-tn))
                 "F2470F38F00C3E   CRC32 R9D, BYTE PTR [R14+R15]")
  (test-assemble `(crc32 :word ,r9d ,(ea r14-tn r15-tn))
                 "66F2470F38F10C3E CRC32 R9D, WORD PTR [R14+R15]")
  (test-assemble `(crc32 :dword ,r9d ,(ea r14-tn r15-tn))
                 "F2470F38F10C3E   CRC32 R9D, DWORD PTR [R14+R15]")
  ;; Destination size = :QWORD
  (test-assemble `(crc32 :byte ,rax-tn ,(ea rbp-tn))
                 "F2480F38F04500   CRC32 RAX, BYTE PTR [RBP]")
  (test-assemble `(crc32 :qword ,rax-tn ,(ea rbp-tn))
                 "F2480F38F14500   CRC32 RAX, QWORD PTR [RBP]")
  ;; now with high regs
  (test-assemble `(crc32 :byte ,r9-tn ,(ea r14-tn r15-tn))
                 "F24F0F38F00C3E   CRC32 R9, BYTE PTR [R14+R15]")
  (test-assemble `(crc32 :qword ,r9-tn ,(ea r14-tn r15-tn))
                 "F24F0F38F10C3E   CRC32 R9, QWORD PTR [R14+R15]"))

(test-util:with-test (:name :assemble-unsigned-qword-imm-to-mem :skipped-on (not :x86-64))
  ;; unsigned bits cast as signed bits
  (let ((const #xffffffff801234BB))
    (test-assemble `(mov :qword ,(ea rcx-tn) ,const)
                   "48C701BB341280   MOV QWORD PTR [RCX], -2146290501")
    ;; Do not truncate to just the lower bits
    (dolist (size '(:byte :word :dword))
      (check-does-not-assemble `(mov ,size ,(ea rcx-tn) ,const)))))

(test-util:with-test (:name :unsigned-as-signed-imm8 :skipped-on (not :x86-64))
  ;; PUSH
  (test-assemble `(push #xfffffffffffffffc) "6AFC             PUSH -4")

  ;; ADD/SUB/etc
  (test-assemble `(and ,rax-tn #xffffffffffffff8c)
                 "4883E08C         AND RAX, -116")
  (test-assemble `(sub ,eax #xfffffffc) "83E8FC           SUB EAX, -4")
  ;; Register AX could use the special 1-byte opcode and non-sign-extended
  ;; imm16 operand; the encoding length is the same either way.
  (test-assemble `(or ,ax #xfff7) "6683C8F7         OR AX, -9"))

(test-util:with-test (:name :assemble-movsx :skipped-on (not :x86-64))
  ;; source = :BYTE, signed
  (check-does-not-assemble `(movsx (:byte :byte) ,r8b ,cl))
  (test-assemble `(movsx (:byte :word)  ,r8w ,cl) "66440FBEC1       MOVSX R8W, CL")
  (test-assemble `(movsx (:byte :dword) ,r8d ,cl) "440FBEC1         MOVSX R8D, CL")
  (test-assemble `(movsx (:byte :qword) ,r8-tn  ,cl) "4C0FBEC1         MOVSX R8, CL")
  ;; source = :BYTE, unsigned
  (check-does-not-assemble `(movzx (:byte :byte) ,r8b ,cl))
  (test-assemble `(movzx (:byte :word)  ,r8w ,cl) "66440FB6C1       MOVZX R8W, CL")
  (test-assemble `(movzx (:byte :dword) ,r8d ,cl) "440FB6C1         MOVZX R8D, CL")
  (test-assemble `(movzx (:byte :qword) ,r8-tn  ,cl) "4C0FB6C1         MOVZX R8, CL")
  ;; source = :WORD, signed
  (test-assemble `(movsx (:word :dword) ,r8d ,cx) "440FBFC1         MOVSX R8D, CX")
  (test-assemble `(movsx (:word :qword) ,r8-tn ,cx)  "4C0FBFC1         MOVSX R8, CX")
  ;; source = :WORD, unsigned
  (test-assemble `(movzx (:word :dword) ,r8d ,cx) "440FB7C1         MOVZX R8D, CX")
  (test-assemble `(movzx (:word :qword) ,r8-tn ,cx)  "4C0FB7C1         MOVZX R8, CX")
  ;; source = :DWORD, signed and unsigned
  (test-assemble `(movsx (:dword :qword) ,r8-tn ,ecx) "4C63C1           MOVSX R8, ECX"))

(test-util:with-test (:name :disassemble-movabs-instruction :skipped-on (not :x86-64))
  (let* ((bytes (coerce '(#x48 #xA1 8 7 6 5 4 3 2 1
                          #xA1 8 7 6 5 4 3 2 1
                          #x66 #xA1 8 7 6 5 4 3 2 1
                          #xA0 8 7 6 5 4 3 2 1)
                        '(array (unsigned-byte 8) 1)))
         (lines
          (test-util:split-string
           (with-output-to-string (s)
             (sb-sys:with-pinned-objects (bytes)
               (sb-disassem:disassemble-memory
                (sb-sys:sap-int (sb-sys:vector-sap bytes))
                (length bytes)
                :stream s)))
           #\newline)))
    (pop lines)
    (dolist (dest-reg '("RAX" "EAX" "AX" "AL"))
      (assert (search (format nil "MOVABS ~A, [#x102030405060708]" dest-reg)
                      (pop lines))))))

(test-util:with-test (:name :disassemble-arith-insts :skipped-on (not (or :x86 :x86-64)))
  (flet ((try (inst expect)
           (let ((p (search "$fp" expect)))
             (when p
               (setq expect
                     (concatenate 'string (subseq expect 0 p)
                                  #+x86 "EBP" #+x86-64 "RBP"
                                  (subseq expect (+ p 3))))))
           (destructuring-bind (opcode operand1 operand2 . more) inst
             (when (or (typep operand1 '(cons (eql memref)))
                       (typep operand2 '(cons (eql memref))))
               #+x86-64
               (let ((prefix (second (if (consp operand1) operand1 operand2))))
                 (flet ((new-ea (operand) (if (consp operand) (ea rbp-tn) operand)))
                   (setf inst
                         (list* opcode prefix (new-ea operand1) (new-ea operand2) more))))
               #+x86
               (flet ((new-ea (operand)
                        (if (consp operand) (make-ea (second operand) :base ebp-tn) operand)))
                 (setf inst (list* opcode (new-ea operand1) (new-ea operand2) more)))))
           (test-assemble inst expect)))
    (try `(bt (memref :word)  ,ax)  "660FA34500       BT WORD PTR [$fp], AX")
    (try `(bt (memref :dword) ,eax) "0FA34500         BT DWORD PTR [$fp], EAX")
    #+x86-64
    (try `(bt (memref :qword) ,rax-tn) "480FA34500       BT QWORD PTR [$fp], RAX")
    (try `(bt (memref :word)  3) "660FBA650003     BT WORD PTR [$fp], 3")
    (try `(bt (memref :dword) 3) "0FBA650003       BT DWORD PTR [$fp], 3")
    #+x86-64
    (try `(bt (memref :qword) 3) "480FBA650003     BT QWORD PTR [$fp], 3")
    ;;
    (try `(shld ,eax ,ebx :cl) "0FA5D8           SHLD EAX, EBX, CL")
    (try `(shld (memref :word)  ,bx 6)  "660FA45D0006     SHLD [$fp], BX, 6")
    (try `(shld (memref :dword) ,ebx 6) "0FA45D0006       SHLD [$fp], EBX, 6")
    #+x86-64
    (try `(shld (memref :qword) ,rbx-tn 6) "480FA45D0006     SHLD [$fp], RBX, 6")
    ;;
    (try `(add ,al  #x7f)       "047F             ADD AL, 127")
    (try `(add ,ax  #x7fff)     "6605FF7F         ADD AX, 32767")
    (try `(add ,eax #x7fffffff) "05FFFFFF7F       ADD EAX, 2147483647")
    #+x86-64
    (try `(add ,rax-tn #x7fffffff) "4805FFFFFF7F     ADD RAX, 2147483647")
    ;;
    (try `(add ,bl  #x7f)       "80C37F           ADD BL, 127")
    (try `(add ,bx  #x7fff)     "6681C3FF7F       ADD BX, 32767")
    (try `(add ,ebx #x7fffffff) "81C3FFFFFF7F     ADD EBX, 2147483647")
    #+x86-64
    (try `(add ,rbx-tn #x7fffffff) "4881C3FFFFFF7F   ADD RBX, 2147483647")
    ;;
    (try `(add ,ax  #x7f)       "6683C07F         ADD AX, 127")
    (try `(add ,eax #x7f)       "83C07F           ADD EAX, 127")
    #+x86-64
    (try `(add ,rax-tn #x7f)       "4883C07F         ADD RAX, 127")
    ;;
    (try `(add (memref :byte) ,cl)   "004D00           ADD [$fp], CL")
    (try `(add (memref :word) ,cx)   "66014D00         ADD [$fp], CX")
    (try `(add (memref :dword) ,ecx) "014D00           ADD [$fp], ECX")
    #+x86-64
    (try `(add (memref :qword) ,rcx-tn) "48014D00         ADD [$fp], RCX")
    (try `(add ,cl (memref :byte))   "024D00           ADD CL, [$fp]")
    (try `(add ,cx (memref :word))   "66034D00         ADD CX, [$fp]")
    (try `(add ,ecx (memref :dword)) "034D00           ADD ECX, [$fp]")
    #+x86-64
    (try `(add ,rcx-tn (memref :qword)) "48034D00         ADD RCX, [$fp]")
    ))

(test-util:with-test (:name :disassemble-imul :skipped-on (not (or :x86 :x86-64)))
  (test-assemble `(imul ,dl)  "F6EA             IMUL DL")
  (test-assemble `(imul ,cx)  "66F7E9           IMUL CX")
  (test-assemble `(imul ,ebx) "F7EB             IMUL EBX")
  (test-assemble `(imul ,edi 92) "6BFF5C           IMUL EDI, EDI, 92"))

(test-util:with-test (:name :disassemble-fs-prefix :skipped-on (not (or :x86-64)))
  (let ((bytes (coerce '(#x64 #xF0 #x44 #x08 #x04 #x25 #x00 #x04 #x10 #x20)
                       '(array (unsigned-byte 8) 1)))
        (s (make-string-output-stream)))
    (sb-sys:with-pinned-objects (bytes)
      (sb-disassem::disassemble-memory (sb-sys:sap-int (sb-sys:vector-sap bytes))
                                       (length bytes)
                                       :stream s))
    (assert (search "LOCK OR FS:[#x20100400], R8B"
                    (get-output-stream-string s)))))

(test-util:with-test (:name :cast-reg-to-size :skipped-on (not :x86-64))
  (test-assemble `(mov :byte ,rsi-tn ,rdi-tn)
                 "408AF7           MOV SIL, DIL")
  (test-assemble `(movsx (:byte :word) ,rax-tn ,rdi-tn)
                 "66400FBEC7       MOVSX AX, DIL")
  (test-assemble `(cmpxchg :byte ,(ea rax-tn) ,rdi-tn)
                 "400FB038         CMPXCHG [RAX], DIL")
  (test-assemble `(cmp :byte ,rdi-tn ,rsi-tn)
                 "4038F7           CMP DIL, SIL")
  (test-assemble `(not :byte ,rsi-tn)
                 "40F6D6           NOT SIL")
  (test-assemble `(rol :byte ,rsi-tn 2)
                 "40C0C602         ROL SIL, 2")
  (test-assemble `(test :byte ,rsi-tn 15)
                 "40F6C60F         TEST SIL, 15")
  (let ((float0 (sb-c:make-random-tn (sb-c:sc-or-lose 'double-reg) 0)))
    (test-assemble `(movd ,float0 ,rax-tn)
                   "660F6EC0         MOVD XMM0, EAX")
    (test-assemble `(movq ,float0 ,eax)
                   "66480F6EC0       MOVQ XMM0, RAX")))

(test-util:with-test (:name :assemble-high-byte-regs :skipped-on (not :x86-64))
  (test-assemble `(cmp (,rdx-tn . :high-byte) 1)
                 "80FE01           CMP DH, 1")
  (test-assemble `(mov (,rdx-tn . :high-byte) (,rcx-tn . :high-byte))
                 "8AF5             MOV DH, CH")
  ;; can not use legacy high byte reg in a REX-prefixed instruction
  (check-does-not-assemble `(movsx (:byte :qword) ,rax-tn (,rbx-tn . :high-byte))))

(defun try (inst)
  (let ((segment (sb-assem:make-segment)))
    (sb-assem:assemble (segment 'nil)
        (apply #'sb-assem:inst* (car inst) (cdr inst)))
    (let* ((buf (sb-assem:segment-buffer segment))
           (string
             (with-output-to-string (stream)
               (with-pinned-objects (buf)
                 (let ((sb-disassem:*disassem-location-column-width* 0))
                   (sb-disassem:disassemble-memory
                    (sap-int (vector-sap buf))
                    (sb-assem::segment-current-posn segment)
                    :stream stream)))))
           (line (string-left-trim'(#\; #\ )
                                  (subseq string (1+ (position #\newline string))
                                          (1- (length string)))))) ; chop final newline
      (declare (ignorable line))
      ;;(print line)
      )))

#+x86-64
(test-util:with-test (:name :muldiv)
  ;; This just assserts that we can assemble. It doesn't check
  ;; against the expected encoding or disassembly.
  (dolist (size '(:byte :word :dword :qword nil))
    (dolist (op '(mul div idiv))
      (if size
          (try `(,op ,size ,rbx-tn))
          (try `(,op ,rbx-tn))))))

#+x86-64
(test-util:with-test (:name :imul)
  (dolist (reg `(,r9-tn)) ;
    ;; 1-operand form yielding a double-width result into rAX:rDX
    (dolist (size '(:byte :word :dword :qword))
      (try `(imul ,size ,reg))
      (try `(imul ,size ,(ea reg)))
      (try `(imul ,size ,(ea #x1000))))
    (try `(imul ,reg)) ; default to :QWORD
    ;; 2-operand form. There is no :BYTE size
    (dolist (size '(:word :dword :qword))
      (try `(imul ,size ,reg ,reg))
      (try `(imul ,size ,reg ,(ea reg)))
      (try `(imul ,size ,reg ,(ea #x1000))))
    (try `(imul ,reg ,r10-tn)) ; default to :QWORD
    ;; 3-operand form with 8-bit signed imm
    (try `(imul :word ,rbx-tn ,(ea rdx-tn) -128))
    (try `(imul :dword ,rbx-tn ,(ea rdx-tn) -128))
    (try `(imul :qword ,rbx-tn ,(ea rdx-tn) -128))
    ;; 3-operand form with 16-bit signed imm
    (try `(imul :word ,rbx-tn ,(ea rdx-tn) -32768))
    ;; 3-operand form with 32-bit signed imm
    (try `(imul :dword ,rbx-tn ,(ea rdx-tn) #xbaba))
    (try `(imul :qword ,rbx-tn ,(ea rdx-tn) #xbaba))))

(test-util:with-test (:name :mxcsr-loadstore :skipped-on (not :x86-64))
  ;; This just assserts that we can assemble
  (try `(ldmxcsr ,(ea rax-tn)))
  (try `(stmxcsr ,(ea rax-tn))))

#+x86-64
(test-util:with-test (:name :avx512-evex-instructions)
  (let ((k1 (sb-x86-64-asm::get-fpr :kreg 1))
        (k2 (sb-x86-64-asm::get-fpr :kreg 2))
        (zmm0 (sb-x86-64-asm::get-fpr :zmm 0))
        (zmm1 (sb-x86-64-asm::get-fpr :zmm 1))
        (zmm2 (sb-x86-64-asm::get-fpr :zmm 2))
        (ymm0 (sb-x86-64-asm::get-fpr :ymm 0))
        (xmm1 (sb-x86-64-asm::get-fpr :xmm 1)))
    ;; Compare to mask (both forms)
    (try `(vcmpps :eq ,k1 ,zmm1 ,zmm2))
    (try `(vcmpps :eq ,k1 ,zmm1 ,zmm2 ,k2))
    (try `(vcmpps ,k1 ,zmm1 ,zmm2 :eq))
    (try `(vcmppd :eq ,k1 ,zmm1 ,zmm2))
    (try `(vcmpss :eq ,k1 ,xmm1 ,xmm1))
    (try `(vcmpsd :eq ,k1 ,xmm1 ,xmm1))
    ;; Broadcast
    (try `(vbroadcastss ,ymm0 ,xmm1))
    (try `(vbroadcastss ,zmm0 ,xmm1))
    (try `(vbroadcastsd ,zmm0 ,xmm1))
    (try `(vpbroadcastd ,zmm0 ,rax-tn))
    (try `(vpbroadcastq ,zmm0 ,rax-tn))
    ;; Mask transfers
    (test-assemble `(kmovd ,k1 ,rax-tn)
                   "C5FB92C8         KMOVD K1, EAX")
    (test-assemble `(kmovd ,rax-tn ,k1)
                   "C5FB93C1         KMOVD EAX, K1")
    (test-assemble `(kmovq ,k1 ,rax-tn)
                   "C4E1FB92C8       KMOVQ K1, RAX")
    (test-assemble `(kmovq ,rax-tn ,k1)
                   "C4E1FB93C1       KMOVQ RAX, K1")
    ;; Masked arithmetic
    (test-assemble `(vaddps-masked ,zmm0 ,zmm1 ,zmm2 ,k1 :z)
                   "62F174C958C2     VADDPS-MASKED ZMM0, ZMM1, ZMM2 {K1} {z}")
    (try `(vdivps-masked ,zmm0 ,zmm1 ,zmm2 ,k1))
    (try `(vsqrtps-masked ,zmm0 ,zmm1 ,k1))

    ;; AVX-512CD (Conflict Detection)
    (test-assemble `(vpconflictd ,zmm0 ,zmm1)
                   "62F27D48C4C1     VPCONFLICTD ZMM0, ZMM1")
    (test-assemble `(vpconflictq ,zmm0 ,zmm1)
                   "62F2FD48C4C1     VPCONFLICTQ ZMM0, ZMM1")
    (test-assemble `(vplzcntd ,zmm0 ,zmm1)
                   "62F27D4844C1     VPLZCNTD ZMM0, ZMM1")
    (test-assemble `(vplzcntq ,zmm0 ,zmm1)
                   "62F2FD4844C1     VPLZCNTQ ZMM0, ZMM1")
    (test-assemble `(vpconflictd-masked ,zmm0 ,zmm1 ,k1 :z)
                   "62F27DC9C4C1     VPCONFLICTD-MASKED ZMM0, ZMM1 {K1} {z}")
    (test-assemble `(vplzcntd-masked ,zmm0 ,zmm1 ,k1)
                   "62F27D4944C1     VPLZCNTD-MASKED ZMM0, ZMM1 {K1}")
    (try `(vpbroadcastmb2q ,zmm0 ,k1))
    (try `(vpbroadcastmw2d ,zmm0 ,k1))

    ;; AVX-512VNNI (Vector Neural Network Instructions)
    (test-assemble `(vpdpbusd ,zmm0 ,zmm1 ,zmm2)
                   "62F2754850C2     VPDPBUSD ZMM0, ZMM1, ZMM2")
    (test-assemble `(vpdpbusds ,zmm0 ,zmm1 ,zmm2)
                   "62F2754851C2     VPDPBUSDS ZMM0, ZMM1, ZMM2")
    (test-assemble `(vpdpwssd ,zmm0 ,zmm1 ,zmm2)
                   "62F2754852C2     VPDPWSSD ZMM0, ZMM1, ZMM2")
    (test-assemble `(vpdpwssds ,zmm0 ,zmm1 ,zmm2)
                   "62F2754853C2     VPDPWSSDS ZMM0, ZMM1, ZMM2")
    (test-assemble `(vpdpbusd-masked ,zmm0 ,zmm1 ,zmm2 ,k1 :z)
                   "62F275C950C2     VPDPBUSD-MASKED ZMM0, ZMM1, ZMM2 {K1} {z}")

    ;; AVX-512BF16 (Bfloat16 Operations)
    (test-assemble `(vcvtne2ps2bf16 ,zmm0 ,zmm1 ,zmm2)
                   "62F2774872C2     VCVTNE2PS2BF16 ZMM0, ZMM1, ZMM2")
    (test-assemble `(vcvtne2ps2bf16-masked ,zmm0 ,zmm1 ,zmm2 ,k1 :z)
                   "62F277C972C2     VCVTNE2PS2BF16-MASKED ZMM0, ZMM1, ZMM2 {K1} {z}")
    (test-assemble `(vcvtneps2bf16 ,ymm0 ,zmm1)
                   "62F27E4872C1     VCVTNEPS2BF16 ZMM0, ZMM1")
    (test-assemble `(vcvtneps2bf16-masked ,ymm0 ,zmm1 ,k1 :z)
                   "62F27EC972C1     VCVTNEPS2BF16-MASKED ZMM0, ZMM1 {K1} {z}")
    (test-assemble `(vdpbf16ps ,zmm0 ,zmm1 ,zmm2)
                   "62F2764852C2     VDPBF16PS ZMM0, ZMM1, ZMM2")
    (test-assemble `(vdpbf16ps-masked ,zmm0 ,zmm1 ,zmm2 ,k1 :z)
                   "62F276C952C2     VDPBF16PS-MASKED ZMM0, ZMM1, ZMM2 {K1} {z}")))

#+x86-64
(test-util:with-test (:name :avx512-fp16-instructions)
  (let ((k1 (sb-x86-64-asm::get-fpr :kreg 1))
        (k2 (sb-x86-64-asm::get-fpr :kreg 2))
        (zmm0 (sb-x86-64-asm::get-fpr :zmm 0))
        (zmm1 (sb-x86-64-asm::get-fpr :zmm 1))
        (zmm2 (sb-x86-64-asm::get-fpr :zmm 2))
        (ymm0 (sb-x86-64-asm::get-fpr :ymm 0))
        (ymm1 (sb-x86-64-asm::get-fpr :ymm 1))
        (xmm0 (sb-x86-64-asm::get-fpr :xmm 0))
        (xmm1 (sb-x86-64-asm::get-fpr :xmm 1))
        (xmm2 (sb-x86-64-asm::get-fpr :xmm 2)))
    ;; Vector arithmetic (Map 5, W0)
    (test-assemble `(vaddph ,zmm0 ,zmm1 ,zmm2)
                   "62F5744858C2     VADDPH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vaddph-masked ,zmm0 ,zmm1 ,zmm2 ,k1 :z)
                   "62F574C958C2     VADDPH ZMM0, ZMM1, ZMM2 {K1} {z}")
    (test-assemble `(vsubph ,zmm0 ,zmm1 ,zmm2)
                   "62F574485CC2     VSUBPH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vmulph ,zmm0 ,zmm1 ,zmm2)
                   "62F5744859C2     VMULPH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vdivph ,zmm0 ,zmm1 ,zmm2)
                   "62F574485EC2     VDIVPH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vminph ,zmm0 ,zmm1 ,zmm2)
                   "62F574485DC2     VMINPH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vmaxph ,zmm0 ,zmm1 ,zmm2)
                   "62F574485FC2     VMAXPH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vsqrtph ,zmm0 ,zmm1)
                   "62F57C4851C1     VSQRTPH ZMM0, ZMM1")

    ;; Scalar arithmetic (Map 5, #xF3, W0)
    (test-assemble `(vaddsh ,xmm0 ,xmm1 ,xmm2)
                   "62F5760858C2     VADDSH XMM0, XMM1, XMM2")
    (test-assemble `(vsubsh ,xmm0 ,xmm1 ,xmm2)
                   "62F576085CC2     VSUBSH XMM0, XMM1, XMM2")
    (test-assemble `(vmulsh ,xmm0 ,xmm1 ,xmm2)
                   "62F5760859C2     VMULSH XMM0, XMM1, XMM2")
    (test-assemble `(vdivsh ,xmm0 ,xmm1 ,xmm2)
                   "62F576085EC2     VDIVSH XMM0, XMM1, XMM2")
    (test-assemble `(vsqrtsh ,xmm0 ,xmm1 ,xmm2)
                   "62F5760851C2     VSQRTSH XMM0, XMM1, XMM2")

    ;; Map 6 instructions (vrcpph, vrsqrtph, vscalefph, FMA)
    (test-assemble `(vrcpph ,zmm0 ,zmm1)
                   "62F67D484CC1     VRCPPH ZMM0, ZMM1")
    (test-assemble `(vrsqrtph ,zmm0 ,zmm1)
                   "62F67D484EC1     VRSQRTPH ZMM0, ZMM1")
    (test-assemble `(vscalefph ,zmm0 ,zmm1 ,zmm2)
                   "62F675482CC2     VSCALEFPH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vfmadd132ph ,zmm0 ,zmm1 ,zmm2)
                   "62F6754898C2     VFMADD132PH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vfmadd213ph ,zmm0 ,zmm1 ,zmm2)
                   "62F67548A8C2     VFMADD213PH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vfmadd231ph ,zmm0 ,zmm1 ,zmm2)
                   "62F67548B8C2     VFMADD231PH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vfmsub132ph ,zmm0 ,zmm1 ,zmm2)
                   "62F675489AC2     VFMSUB132PH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vfnmadd132ph ,zmm0 ,zmm1 ,zmm2)
                   "62F675489CC2     VFNMADD132PH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vfnmsub132ph ,zmm0 ,zmm1 ,zmm2)
                   "62F675489EC2     VFNMSUB132PH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vfmaddsub132ph ,zmm0 ,zmm1 ,zmm2)
                   "62F6754896C2     VFMADDSUB132PH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vfmsubadd132ph ,zmm0 ,zmm1 ,zmm2)
                   "62F6754897C2     VFMSUBADD132PH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vfcmaddcph ,zmm0 ,zmm1 ,zmm2)
                   "62F6774856C2     VFCMADDCPH ZMM0, ZMM1, ZMM2")
    (test-assemble `(vfmaddcph ,zmm0 ,zmm1 ,zmm2)
                   "62F6764856C2     VFMADDCPH ZMM0, ZMM1, ZMM2")

    ;; Comparisons (vcmpph, vcmpsh)
    (try `(vcmpph :eq ,k1 ,zmm1 ,zmm2))
    (try `(vcmpph :eq ,k1 ,zmm1 ,zmm2 ,k2))
    (try `(vcmpph ,k1 ,zmm1 ,zmm2 :eq))
    (try `(vcmpsh :eq ,k1 ,xmm1 ,xmm2))
    (try `(vcomish ,xmm0 ,xmm1))
    (try `(vucomish ,xmm0 ,xmm1))
    (try `(vfpclassph ,k1 ,zmm1 0))
    (try `(vfpclasssh ,k1 ,xmm1 0))
    (try `(vrndscaleph ,zmm0 ,zmm1 0))
    (try `(vrndscalesh ,xmm0 ,xmm1 ,xmm2 0))

    ;; Conversions
    (test-assemble `(vcvtph2psx ,zmm0 ,ymm1)
                   "62F67D4813C1     VCVTPH2PSX ZMM0, ZMM1")
    (test-assemble `(vcvtps2phx ,ymm0 ,zmm1)
                   "62F57D481DC1     VCVTPS2PHX ZMM0, ZMM1")
    (test-assemble `(vcvtdq2ph ,ymm0 ,zmm1)
                   "62F57C485BC1     VCVTDQ2PH ZMM0, ZMM1")
    (test-assemble `(vcvtph2dq ,zmm0 ,ymm1)
                   "62F57D485BC1     VCVTPH2DQ ZMM0, ZMM1")
    (test-assemble `(vcvtuw2ph ,zmm0 ,zmm1)
                   "62F57F487DC1     VCVTUW2PH ZMM0, ZMM1")
    (test-assemble `(vcvtw2ph ,zmm0 ,zmm1)
                   "62F57E487DC1     VCVTW2PH ZMM0, ZMM1")
    (test-assemble `(vcvtph2w ,zmm0 ,zmm1)
                   "62F57D487DC1     VCVTPH2W ZMM0, ZMM1")
    (test-assemble `(vcvtph2uw ,zmm0 ,zmm1)
                   "62F57C487DC1     VCVTPH2UW ZMM0, ZMM1")

    ;; Moves
    (test-assemble `(vmovw ,xmm0 ,eax)
                   "62F57D086EC0     VMOVW XMM0, EAX")
    (test-assemble `(vmovw ,eax ,xmm0)
                   "62F57D087EC0     VMOVW EAX, XMM0")
    (test-assemble `(vmovsh ,xmm0 ,xmm1 ,xmm2)
                   "62F5760810C2     VMOVSH XMM0, XMM1, XMM2")))


