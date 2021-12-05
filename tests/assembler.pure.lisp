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
    (test-movnti (ea rax-tn) r12-tn "4C0FC320         MOVNTI [RAX], R12")))

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

;;; This seems to be testing that we can find fdefns in static space
;;; which I guess was broken.  immobile-code has no fdefns in static space.
(test-util:with-test (:name :disassemble-static-fdefn
            :skipped-on (or (not :x86-64) :immobile-code))
  (assert (< (get-lisp-obj-address (sb-int:find-fdefn 'sb-impl::sub-gc))
             sb-vm:static-space-end))
  ;; Cause SUB-GC to become un-statically-linked
  (progn (trace sb-impl::sub-gc) (untrace))
  (let ((lines
         (test-util:split-string (with-output-to-string (s)
                         (disassemble 'sb-impl::gc :stream s))
                       #\Newline))
        (found))
    ;; Check that find-called-object looked in static space for FDEFNs
    (dolist (line lines)
      (when (and (search "CALL" line)
                 (search " SUB-GC" line))
        (setq found t)))
    (assert found)))

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
  (test-assemble `(movd ,float0-tn ,rax-tn)
                 "660F6EC0         MOVD XMM0, EAX")
  (test-assemble `(movq ,float0-tn ,eax)
                 "66480F6EC0       MOVQ XMM0, RAX"))

(test-util:with-test (:name :assemble-high-byte-regs :skipped-on (not :x86-64))
  (test-assemble `(cmp (,rdx-tn . :high-byte) 1)
                 "80FE01           CMP DH, 1")
  (test-assemble `(mov (,rdx-tn . :high-byte) (,rcx-tn . :high-byte))
                 "8AF5             MOV DH, CH")
  ;; can not use legacy high byte reg in a REX-prefixed instruction
  (check-does-not-assemble `(movsx (:byte :qword) ,rax-tn (,rbx-tn . :high-byte))))
