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

(in-package sb-vm)

(load "test-util.lisp")
(use-package :test-util)

;; this is architecture-agnostic
(defun test-assemble (inst expect)
  (let ((segment (sb-assem:make-segment :type :regular)))
    (apply (sb-assem::inst-emitter-symbol (car inst)) segment nil (cdr inst))
    (let* ((buf (sb-assem::segment-buffer segment))
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

(with-test (:name :assemble-movti-instruction :skipped-on '(not :x86-64))
  (flet ((test-movnti (dst src expect)
           (test-assemble `(movnti ,dst ,src) expect)))
    (test-movnti (make-ea :dword :base rdi-tn :disp 57) eax-tn
                 "0FC34739         MOVNTI [RDI+57], EAX")
    (test-movnti (make-ea :qword :base rax-tn) r12-tn
                 "4C0FC320         MOVNTI [RAX], R12")))

(with-test (:name :assemble-crc32 :skipped-on '(not :x86-64))
  ;; Destination size = :DWORD
  (test-assemble `(crc32 ,eax-tn ,(make-ea :byte  :base rbp-tn))
                 "F20F38F04500     CRC32 EAX, BYTE PTR [RBP]")
  (test-assemble `(crc32 ,eax-tn ,(make-ea :word  :base rbp-tn))
                 "66F20F38F14500   CRC32 EAX, WORD PTR [RBP]")
  (test-assemble `(crc32 ,eax-tn ,(make-ea :dword :base rbp-tn))
                 "F20F38F14500     CRC32 EAX, DWORD PTR [RBP]")
  ;; these check that the presence of REX does not per se change the width.
  (test-assemble `(crc32 ,r9d-tn ,(make-ea :byte  :base r14-tn :index r15-tn))
                 "F2470F38F00C3E   CRC32 R9D, BYTE PTR [R14+R15]")
  (test-assemble `(crc32 ,r9d-tn ,(make-ea :word  :base r14-tn :index r15-tn))
                 "66F2470F38F10C3E CRC32 R9D, WORD PTR [R14+R15]")
  (test-assemble `(crc32 ,r9d-tn ,(make-ea :dword :base r14-tn :index r15-tn))
                 "F2470F38F10C3E   CRC32 R9D, DWORD PTR [R14+R15]")
  ;; Destination size = :QWORD
  (test-assemble `(crc32 ,rax-tn ,(make-ea :byte  :base rbp-tn))
                 "F2480F38F04500   CRC32 RAX, BYTE PTR [RBP]")
  (test-assemble `(crc32 ,rax-tn ,(make-ea :qword :base rbp-tn))
                 "F2480F38F14500   CRC32 RAX, QWORD PTR [RBP]")
  ;; now with high regs
  (test-assemble `(crc32 ,r9-tn ,(make-ea :byte  :base r14-tn :index r15-tn))
                 "F24F0F38F00C3E   CRC32 R9, BYTE PTR [R14+R15]")
  (test-assemble `(crc32 ,r9-tn ,(make-ea :qword :base r14-tn :index r15-tn))
                 "F24F0F38F10C3E   CRC32 R9, QWORD PTR [R14+R15]"))

(with-test (:name :disassemble-arith-insts :skipped-on '(not (or :x86 :x86-64)))
  (flet ((try (inst expect)
           (let ((p (search "$fp" expect)))
             (when p
               (setq expect
                     (concatenate 'string (subseq expect 0 p)
                                  #+x86 "EBP" #+x86-64 "RBP"
                                  (subseq expect (+ p 3))))))
           (test-assemble inst expect))
         (memref (size) (make-ea size :base #+x86 ebp-tn #+x86-64 rbp-tn)))
    (try `(bt ,(memref :word)  ,ax-tn)  "660FA34500       BT WORD PTR [$fp], AX")
    (try `(bt ,(memref :dword) ,eax-tn) "0FA34500         BT DWORD PTR [$fp], EAX")
    #+x86-64
    (try `(bt ,(memref :qword) ,eax-tn) "480FA34500       BT QWORD PTR [$fp], RAX")
    (try `(bt ,(memref :word)  3) "660FBA650003     BT WORD PTR [$fp], 3")
    (try `(bt ,(memref :dword) 3) "0FBA650003       BT DWORD PTR [$fp], 3")
    #+x86-64
    (try `(bt ,(memref :qword) 3) "480FBA650003     BT QWORD PTR [$fp], 3")
    ;;
    (try `(shld ,eax-tn ,ebx-tn :cl) "0FA5D8           SHLD EAX, EBX, CL")
    (try `(shld ,(memref :word)  ,bx-tn 6)  "660FA45D0006     SHLD [$fp], BX, 6")
    (try `(shld ,(memref :dword) ,ebx-tn 6) "0FA45D0006       SHLD [$fp], EBX, 6")
    #+x86-64
    (try `(shld ,(memref :qword) ,rbx-tn 6) "480FA45D0006     SHLD [$fp], RBX, 6")
    ;;
    (try `(add ,al-tn  #x7f)       "047F             ADD AL, 127")
    (try `(add ,ax-tn  #x7fff)     "6605FF7F         ADD AX, 32767")
    (try `(add ,eax-tn #x7fffffff) "05FFFFFF7F       ADD EAX, 2147483647")
    #+x86-64
    (try `(add ,rax-tn #x7fffffff) "4805FFFFFF7F     ADD RAX, 2147483647")
    ;;
    (try `(add ,bl-tn  #x7f)       "80C37F           ADD BL, 127")
    (try `(add ,bx-tn  #x7fff)     "6681C3FF7F       ADD BX, 32767")
    (try `(add ,ebx-tn #x7fffffff) "81C3FFFFFF7F     ADD EBX, 2147483647")
    #+x86-64
    (try `(add ,rbx-tn #x7fffffff) "4881C3FFFFFF7F   ADD RBX, 2147483647")
    ;;
    (try `(add ,ax-tn  #x7f)       "6683C07F         ADD AX, 127")
    (try `(add ,eax-tn #x7f)       "83C07F           ADD EAX, 127")
    #+x86-64
    (try `(add ,rax-tn #x7f)       "4883C07F         ADD RAX, 127")
    ;;
    (try `(add ,(memref :byte) ,cl-tn)   "004D00           ADD [$fp], CL")
    (try `(add ,(memref :word) ,cx-tn)   "66014D00         ADD [$fp], CX")
    (try `(add ,(memref :dword) ,ecx-tn) "014D00           ADD [$fp], ECX")
    #+x86-64
    (try `(add ,(memref :qword) ,rcx-tn) "48014D00         ADD [$fp], RCX")
    (try `(add ,cl-tn ,(memref :byte))   "024D00           ADD CL, [$fp]")
    (try `(add ,cx-tn ,(memref :word))   "66034D00         ADD CX, [$fp]")
    (try `(add ,ecx-tn ,(memref :dword)) "034D00           ADD ECX, [$fp]")
    #+x86-64
    (try `(add ,rcx-tn ,(memref :qword)) "48034D00         ADD RCX, [$fp]")
    ))
