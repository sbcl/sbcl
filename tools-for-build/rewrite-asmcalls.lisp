(defpackage sb-core-rewriter
  (:use #:cl #:sb-ext #:sb-kernel #:sb-int #:sb-vm #:sb-sys)
  (:import-from sb-x86-64-asm #:jmp #:call #:mov #:get-gpr
                #:machine-ea #:machine-ea-base #:machine-ea-index #:machine-ea-disp))
(in-package sb-core-rewriter)

;;; This file provides a recipe which gets a little bit closer to being able to
;;; emulate #+immobile-space in so far as producing an ELF core is concerned.
;;; The recipe is a bit more complicated than I'd like, but it works.
;;; Let's say you want a core with contiguous text space containing the code
;;; of a quicklisp system. These steps should get you up to the point where
;;; the ELF converter should run (but I haven't tried it).

;;; $ run-sbcl.sh
;;; * (ql:quickload :one-more-re-nightmare-tests)
;;; * (save-lisp-and-die "step1.core")
;;; $ run-sbcl.sh
;;; * (load "tools-for-build/editcore")
;;; * (sb-editcore:move-dynamic-code-to-text-space "step1.core" "step2.core")
;;; * (quit)
;;; $ run-sbcl.sh --core step2.core
;;; * (load "tools-for-build/rewrite-asmcalls")
;;; * (sb-core-rewriter:monkeypatch-all-calls)
;;; * (save-lisp-and-die "step3.core")
;;; Now "step3.core" has a text space, and all lisp-to-lisp calls bypass their FDEFN.
;;; The new core is strictly less featureful than #+immobie-space because global
;;; function redefinition does not work - REMOVE-STATIC-LINKS is missing.

#|
If we were to want verification that the steps did anything,
here are a couple ways (which could be turned into regression tests)

* (assert (null (sb-vm:list-allocated-objects :dynamic :type sb-vm:code-header-widetag)))

$ run-sbcl.sh --core step2.core
(with-open-file (s "dis2.txt" :direction :output) (disassemble 'sb-c::ir1-convert-lambdalike :stream s))
$ run-sbcl.sh --core step3.core
(with-open-file (s "dis3.txt" :direction :output) (disassemble 'sb-c::ir1-convert-lambdalike :stream s))
$ diff dis2.txt dis3.txt
80,84c80,84
< ; 4670:       488B05A9F6FFFF   MOV RAX, [RIP-2391]            ; #<SB-KERNEL:FDEFN SB-INT:VALID-FUNCTION-NAME-P>
< ; 4677:       B902000000       MOV ECX, 2
< ; 467C:       48892C24         MOV [RSP], RBP
< ; 4680:       488BEC           MOV RBP, RSP
< ; 4683:       FF5009           CALL [RAX+9]
---
> ; 4670:       B902000000       MOV ECX, 2
> ; 4675:       48892C24         MOV [RSP], RBP
> ; 4679:       488BEC           MOV RBP, RSP
> ; 467C:       0F1F440000       NOP
> ; 4681:       E8EA0B0100       CALL #x550000175270
... etc
|#

(defun get-code-instruction-model (code)
  (let* ((n-entries (code-n-entries code))
         (insts-sap (code-instructions code))
         (segments
          (sb-disassem::get-code-segments code))
         (dstate (sb-disassem:make-dstate))
         (fun-header-offsets)
         (labels))
    (sb-disassem:label-segments segments dstate)
    (dotimes (i n-entries)
      (let* ((fun (%code-entry-point code i))
             (funbase (sap+ (int-sap (get-lisp-obj-address fun)) (- fun-pointer-lowtag))))
        (push (sap- funbase insts-sap) fun-header-offsets)))
    (setq fun-header-offsets (nreverse fun-header-offsets)
          labels (sort (sb-disassem::dstate-labels dstate) #'< :key #'car))
    (sb-int:collect ((result))
      (dolist (seg segments (coerce (result) 'vector))
        (setf (sb-disassem:dstate-segment dstate) seg
              (sb-disassem:dstate-segment-sap dstate)
              (funcall (sb-disassem:seg-sap-maker seg)))
        (setf (sb-disassem:dstate-cur-offs dstate) 0)
        (loop
          (when (and fun-header-offsets
                     (= (sb-disassem:dstate-cur-addr dstate)
                        (sap-int (sap+ insts-sap (car fun-header-offsets)))))
            (pop fun-header-offsets)
            (incf (sb-disassem:dstate-cur-offs dstate)
                  (* simple-fun-insts-offset n-word-bytes)))
          (let* ((pc (sb-disassem:dstate-cur-addr dstate))
                 (labeled (when (and labels (= pc (caar labels)))
                            (pop labels)
                            t))
                 (inst (sb-disassem:disassemble-instruction dstate))
                 (nbytes (- (sb-disassem:dstate-cur-addr dstate) pc)))
            (result (list* labeled pc nbytes inst)))
          (when (>= (sb-disassem:dstate-cur-offs dstate) (sb-disassem:seg-length seg))
            (return)))))))

;; The extra copy of ASM routines, particularly C-calling trampolines, that now reside in text
;; space have to be modified to correctly reference their C functions. They assume that static
;; space is near alien-linkage space, and so they use this form:
;;   xxxx: E8A1F0EFFF  CALL #x50000060 ; alloc
;; which unforuntately means that after relocating to text space, that instruction refers
;; to random garbage, and more unfortunately there is no room to squeeze in an instruction
;; that encodes to 7 bytes.
;; So we have to create an extra jump "somewhere" that indirects through the linkage table
;; but is callable from the text-space code.
;;; I don't feel like programmatically scanning the asm code to determine these.
;;; Hardcoded is good enough (until it isn't)
(defparameter *c-linkage-redirects*
  (mapcar (lambda (x) (cons x (foreign-symbol-sap x)))
          '("switch_to_arena"
            "alloc"
            "alloc_list"
            "listify_rest_arg"
            "make_list"
            "alloc_funinstance"
            "allocation_tracker_counted"
            "allocation_tracker_sized")))

(defun get-text-space-asm-code-replica ()
  (let* ((v (%make-lisp-obj (+ text-space-start 15)))
         (offs (aref v 0)))
    (%make-lisp-obj (+ text-space-start offs 15))))

(defun monkeypatch-asm ()
  (let* ((freeptr *text-space-free-pointer*)
         (items *c-linkage-redirects*)
         (nelts (length items))
         (nwords (+ 2 (align-up nelts 2)))
         (i (* 2 n-word-bytes))
         (old-code sb-fasl:*assembler-routines*)
         (new-code (get-text-space-asm-code-replica)))
    (setf *text-space-free-pointer* (sap+ freeptr (ash nwords n-word-bytes)))
    (setf (sap-ref-word freeptr 0) simple-array-unsigned-byte-64-widetag
          (sap-ref-word freeptr n-word-bytes) (fixnumize nelts))
    (dolist (item items)
      ;; FF1425nnnnnnnn = CALL [ea]
      (setf (sap-ref-8 freeptr i) #xFF
            (sap-ref-8 freeptr (+ i 1)) #x24
            (sap-ref-8 freeptr (+ i 2)) #x25
            (sap-ref-32 freeptr (+ i 3)) (sap-int (sap+ (cdr item) 8))
            (sap-ref-8 freeptr (+ i 7)) #x90) ; nop
      (incf i 8))
    (sb-c:dis (sap+ freeptr (* 2 n-word-bytes))
              (* nelts n-word-bytes))
    ;; Scan the original blob of code, not the replica.
    ;; Otherwise the "CALL rel32" instructions would be bogus.
    (let ((insts (get-code-instruction-model old-code)))
      (dovector (inst insts)
        (when (eq (fourth inst) 'call)
          (let ((operand (fifth inst)))
            (when (and (integerp operand)
                       (>= operand alien-linkage-table-space-start)
                       (< operand (+ alien-linkage-table-space-start
                                     alien-linkage-table-space-size)))
              (let* ((index
                      ;; Because the new instructions are embedded in
                      ;; a vector of words, hop over the vector header.
                      (+ 2 (position (int-sap operand)
                                     *c-linkage-redirects*
                                     :key #'cdr :test #'sap=)))
                     ;; each entry takes 7 bytes followed by a nop
                     (branch-target (sap+ freeptr (* index n-word-bytes)))
                     (old-next-ip-abs (sap+ (int-sap (second inst)) (third inst)))
                     (old-next-ip-rel (sap- old-next-ip-abs (code-instructions old-code)))
                     (new-next-ip (sap+ (code-instructions new-code)
                                        old-next-ip-rel))
                     (disp (sap- branch-target new-next-ip)))
                (setf (signed-sap-ref-32 new-next-ip -4) disp)
                (format t "~X is ~D. disp=~X~%" inst index disp)))))))))

(defun get-mov-src-constant (code insts i ea)
  (let ((abs-addr (+ (second (svref insts (1+ i))) ; next instruction's address
                     (machine-ea-disp ea))))
    (when (and (not (logtest abs-addr #b111)) ; lispword-aligned
               (>= abs-addr (get-lisp-obj-address code))
               (< abs-addr (sap-int (code-instructions code))))
      (sap-ref-lispobj (int-sap abs-addr) 0))))

(defun locate-constant-move-to-rax (code insts start)
  ;; Look for a MOV to RAX from a code header constant
  ;; Technically this should fail if it finds _any_ instruction
  ;; that affects RAX before it finds the one we're looking for.
  (loop for i downfrom start to 1
        do (let ((inst (svref insts i)))
             (cond ((first inst) (return)) ; labeled statement - fail
                   ((and (eq (fourth inst) 'mov)
                         (eq (fifth inst) #.(get-gpr :qword 0))
                         (typep (sixth inst) '(cons machine-ea (eql :qword))))
                    (let ((ea (car (sixth inst))))
                      (when (and (eq (machine-ea-base ea) :rip)
                                 (minusp (machine-ea-disp ea)))
                        (return
                          (let ((const (get-mov-src-constant code insts i ea)))
                            (when (fdefn-p const)
                              (let ((fun (fdefn-fun const)))
                                (when (simple-fun-p fun)
                                  (values i fun)))))))))))))

(defun replacement-opcode (inst)
  (ecase (fourth inst) ; opcode
    (jmp #xE9)
    (call #xE8)))

(defun rewrite-rax-based-call (code insts inst i &optional print)
  (multiple-value-bind (start callee) (locate-constant-move-to-rax code insts (1- i))
    (when (and start (eq (heap-allocated-p callee) :static))
      (when print
        (format t "~&FDEFN call to ~S:~%" callee)
        (let ((addr (second (svref insts start))) ; starting address
              (end (+ (second inst) (third inst))))
          (sb-c:dis addr (- end addr))))
      ;; Several instructions have to be replaced to make room for the new CALL
      ;; which is a longer than the old, but it's ok since the first MOV
      ;; is eliminated.
      (let* ((sum-lengths
              (loop for j from start to i sum (third (svref insts j))))
             (new-bytes (make-array sum-lengths :element-type '(unsigned-byte 8)))
             (new-index 0))
        (loop for j from (1+ start) below i
              do (let* ((old-inst (svref insts j))
                        (pc (int-sap (second old-inst)))
                        (nbytes (third old-inst)))
                   (dotimes (k nbytes)
                     (setf (aref new-bytes new-index) (sap-ref-8 pc k))
                     (incf new-index))))
        ;; insert padding given that the new call takes 5 bytes to encode
        (let* ((nop-len (- sum-lengths (+ new-index 5)))
               (nop-pattern (ecase nop-len
                              (5 '(#x0f #x1f #x44 #x00 #x00)))))
          (dolist (byte nop-pattern)
            (setf (aref new-bytes new-index) byte)
            (incf new-index)))
        ;; change the call
        (let* ((vaddr (second (svref insts start)))
               (next-pc (+ (second inst) (third inst)))
               (branch-target (simple-fun-entry-sap callee))
               (rel32 (sap- branch-target (int-sap next-pc))))
          ;; (format t "~&Branch displacement becomes ~D~%" rel32)
          (setf (aref new-bytes new-index) (replacement-opcode inst))
          (with-pinned-objects (new-bytes)
            (setf (signed-sap-ref-32 (vector-sap new-bytes) (1+ new-index)) rel32)
            (when print
              (format t "~&Replaced by:~%")
              (let ((s (sb-disassem::make-vector-segment
                        new-bytes 0 sum-lengths
                        :virtual-location vaddr)))
                (sb-disassem::disassemble-segment
                 s *standard-output* (sb-disassem:make-dstate))))
            (%byte-blt new-bytes 0 (int-sap vaddr) 0 sum-lengths)))))))

(defun find-routine-in-text-space (addr)
  ;; this will (for better or for worse) find static fdefns as well as asm routines,
  ;; so we have to figure out which it is.
  (let* ((symbol (sb-disassem::find-assembler-routine addr))
         (data (gethash symbol (sb-fasl::%asm-routine-table sb-fasl:*assembler-routines*))))
    (when (member symbol '(;sb-vm::bignum-to-rcx
                           ;SB-VM::GENERIC-+
                           ;SB-C:UNWIND
                           ;SB-VM::SYS-LIST-ALLOC-TRAMP
                           ))
      (return-from find-routine-in-text-space nil))
    (cond (data
           (let* ((vector (%make-lisp-obj (+ text-space-start other-pointer-lowtag)))
                  (asm-code-start (aref vector 0))
                  (code
                   (%make-lisp-obj (+ text-space-start asm-code-start other-pointer-lowtag))))
             (sap+ (code-instructions code) (car data))))
          (t
           (let* ((fdefn (find-fdefn symbol))
                  (fun (fdefn-fun fdefn)))
             (aver (= addr (+ (get-lisp-obj-address fdefn)
                              (- other-pointer-lowtag)
                              (ash fdefn-raw-addr-slot word-shift))))
             (aver (simple-fun-p fun))
             (simple-fun-entry-sap fun))))))

(defun rewrite-asm-call (inst)
  (let* ((ea (car (fifth inst)))
         (addr (machine-ea-disp ea))
         (branch-target (find-routine-in-text-space addr))
         (new-bytes (make-array 7 :element-type '(unsigned-byte 8))))
    (when (null branch-target)
      (return-from rewrite-asm-call))
    (setf (aref new-bytes 0) #x66 (aref new-bytes 1) #x90) ; 2-byte NOP
    (let* ((vaddr (second inst))
           (next-pc (+ (second inst) (third inst)))
           (rel32 (sap- branch-target (int-sap next-pc))))
      (setf (aref new-bytes 2) (replacement-opcode inst))
      (with-pinned-objects (new-bytes)
        (setf (signed-sap-ref-32 (vector-sap new-bytes) 3) rel32)
        (%byte-blt new-bytes 0 (int-sap vaddr) 0 7)))))

(defun monkeypatch-1 (code &aux (insts (get-code-instruction-model code)))
  (declare (simple-vector insts))
  (do ((i 0 (1+ i)))
      ((>= i (length insts)))
    (let* ((inst (svref insts i))
           (this-op (fourth inst)))
      (when (member this-op '(call jmp))
        ;; is it potentially a call via an fdefn or an asm code indirection?
        (let* ((operand (fifth inst))
               (ea (if (listp operand) (car operand))))
          (when (and (typep operand '(cons machine-ea (eql :qword)))
                     (or (and (eql (machine-ea-base ea) 0) ; [RAX-9]
                              (eql (machine-ea-disp ea) 9)
                              (not (machine-ea-index ea)))
                         (and (not (machine-ea-base ea))
                              (not (machine-ea-index ea))
                              (<= static-space-start (machine-ea-disp ea)
                                  (sap-int *static-space-free-pointer*)))))
            (cond ((eql (machine-ea-base ea) 0) ; based on RAX
                   (rewrite-rax-based-call code insts inst i))
                  (t
                   (rewrite-asm-call inst)))))))))

(defun monkeypatch-all-calls (&optional (cleanup t) print)
  (monkeypatch-asm)
  (let* ((start text-space-start)
         (offsets-vector (%make-lisp-obj (logior start lowtag-mask))))
    (loop for j from 1 below (length offsets-vector)
          do
       (let ((obj (%make-lisp-obj (+ start (aref offsets-vector j) other-pointer-lowtag))))
         (when print
           (format t "Patching ~S~%" obj))
         (monkeypatch-1 obj))))
  (when cleanup
    (delete-package "SB-CORE-REWRITER")))
(export 'monkeypatch-all-calls)
