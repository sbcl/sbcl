;;;; Reorganization of immobile code space.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-X86-64-ASM")

(eval-when (:compile-toplevel) ; not needed outside this file
(defmacro do-text-space-code ((code-var) &body body)
  ;; Loop over all code objects
  `(let* ((call (find-inst #xE8 (get-inst-space)))
          (jmp  (find-inst #xE9 (get-inst-space)))
          (mov-ea (find-inst #x8B (get-inst-space)))
          (mov-imm-acc (find-inst #xB8 (get-inst-space)))
          (dstate (make-dstate nil))
          (code-sap (int-sap 0))
          (seg (sb-disassem::%make-segment :sap-maker (lambda () code-sap))))
     (declare (ignorable mov-ea mov-imm-acc))
     (macrolet ((do-functions ((fun-var addr-var) &body body)
                  ;; Loop over all embedded functions
                  `(dotimes (fun-index (code-n-entries ,',code-var))
                     (let* ((,fun-var (%code-entry-point ,',code-var fun-index))
                            (,addr-var (+ (get-lisp-obj-address ,fun-var)
                                          (- fun-pointer-lowtag)
                                          (ash simple-fun-insts-offset word-shift))))
                       (with-pinned-objects (code-sap) ; Mutate SAP to point to fun
                         (setf (sap-ref-word (int-sap (get-lisp-obj-address code-sap))
                                             (- n-word-bytes other-pointer-lowtag))
                               ,addr-var))
                       (setf (seg-virtual-location seg) ,addr-var
                             (seg-length seg) (%simple-fun-text-len ,fun-var fun-index))
                       ,@body))))
       (sb-vm::map-objects-in-range
        (lambda (,code-var obj-type obj-size)
          (declare (ignore obj-size))
          (when (= obj-type code-header-widetag) ,@body))
        ;; Slowness here is bothersome, especially for UNDO-STATIC-LINKAGE,
        ;; so skip right over all fixedobj pages.
        (ash text-space-start (- n-fixnum-tag-bits))
        (%make-lisp-obj (sap-int *text-space-free-pointer*)))))))

#+immobile-code
(defun sb-vm::collect-immobile-code-relocs ()
  (let ((code-components (make-array 20000 :element-type 'word :fill-pointer 0 :adjustable t))
        (relocs (make-array 100000 :element-type 'word :fill-pointer 0 :adjustable t))
        (seg (sb-disassem::%make-segment
              :sap-maker (lambda () (error "Bad sap maker")) :virtual-location 0))
        (dstate (make-dstate nil)))
    (flet ((scan-function (code sap length extra-offset predicate)
             ;; Extra offset is the amount to add to the offset supplied in the
             ;; lambda to compute the instruction offset relative to the code base.
             ;; Defrag has already stuffed in forwarding pointers when it reads
             ;; this data, which makes code_header_words() inconvenient to use.
             (sb-x86-64-asm::scan-relative-operands
              code (sap-int sap) length dstate seg
              (lambda (offset operand inst)
                (declare (ignore inst))
                (let ((lispobj (if (immobile-space-addr-p operand)
                                   (sb-vm::find-called-object operand)
                                   0)))
                  (vector-push-extend (+ offset extra-offset) relocs)
                  (vector-push-extend (get-lisp-obj-address lispobj) relocs)))
               predicate))
           (finish-component (code start-relocs-index)
             (when (> (fill-pointer relocs) start-relocs-index)
               (vector-push-extend (get-lisp-obj-address code) code-components)
               (vector-push-extend start-relocs-index code-components))))

      ;; Assembler routines contain jumps to immobile code.
      (let ((code sb-fasl:*assembler-routines*)
            (relocs-index (fill-pointer relocs)))
        ;; The whole thing can be disassembled in one stroke since inter-routine
        ;; gaps are encoded as NOPs.
        (multiple-value-bind (start end) (sb-fasl::calc-asm-routine-bounds)
          (scan-function code
                         (sap+ (code-instructions code) start)
                         (- end start)
                         ;; extra offset = header words + start
                         (+ (ash (code-header-words code) word-shift) start)
                         ;; calls from lisp into C code can be ignored, as
                         ;; neither the asssembly routines nor C code will move.
                         #'immobile-space-addr-p))
        (finish-component code relocs-index))

      ;; Immobile space - code components can jump to immobile space,
      ;; read-only space, and C runtime routines.
      (map-allocated-objects
       (lambda (code type size)
         (declare (ignore size))
         (when (and (= type code-header-widetag) (plusp (code-n-entries code)))
           (let ((relocs-index (fill-pointer relocs)))
             (dotimes (i (code-n-entries code))
               ;; simple-funs must be individually scanned so that the
               ;; embedded boxed words are properly skipped over.
               (let* ((fun (%code-entry-point code i))
                      (sap (simple-fun-entry-sap fun)))
                 (scan-function code sap
                                (%simple-fun-text-len fun i)
                                ;; Compute the offset from the base of the code
                                (+ (ash (code-header-words code) word-shift)
                                   (sap- sap (code-instructions code)))
                                #'constantly-t)))
             (finish-component code relocs-index))))
       :immobile))

    ;; Write a delimiter into the array passed to C
    (vector-push-extend 0 code-components)
    (vector-push-extend (fill-pointer relocs) code-components)
    (values code-components relocs)))

(export 'sb-vm::statically-link-core "SB-VM")
(declaim (inline rip-relative-p))
(defun rip-relative-p (modrm-byte) (= (logand modrm-byte #b11000111) #b00000101))

#+immobile-code
(defun sb-vm::statically-link-core (&key callers)
  (do-text-space-code (code)
    (when (or (not callers)
              (and (plusp (code-n-entries code))
                   (member (%simple-fun-name (%code-entry-point code 0)) callers)))
      (let* ((fixups)
             (all-fdefns
              (do ((i sb-vm:code-constants-offset (1+ i))
                   (end (code-header-words code))
                   (list))
                  ((= i end) list)
                (let ((const (code-header-ref code i)))
                  (typecase const
                    (fdefn (push const list))
                    ;; CONS may occur due to previous invocation of statically-link.
                    ((cons fdefn) (push (car const) list))))))
             (observable-fdefns) ; ones that an instruction explicitly dereferences
             (code-begin (- (get-lisp-obj-address code) sb-vm:other-pointer-lowtag))
             (code-end (+ code-begin (sb-vm::code-object-size code)))
             (boxed-begin (+ code-begin (* sb-vm:code-constants-offset sb-vm:n-word-bytes)))
             (boxed-end (+ code-begin (* (code-header-words code) sb-vm:n-word-bytes)))
             (code-insts (code-instructions code)))
        (do-functions (fun addr)
          ;; Loop over function's assembly code
          (dx-flet ((process-inst (chunk inst)
                      (cond
                        ;; find FDEFNs as the source of move-immediate-to-register.
                        ;; There can be false positives, but that's OK.
                        ((and (eq inst mov-imm-acc)
                              ;; ensure not a 64-bit move
                              (eql (sb-disassem::dstate-inst-properties dstate) 0))
                         (let ((value (sap-ref-32 code-sap (1+ (dstate-cur-offs dstate)))))
                           (dolist (fdefn all-fdefns)
                             (when (eql (get-lisp-obj-address fdefn) value)
                               (pushnew fdefn observable-fdefns)))))
                        ;; find FDEFNs in code header as the source of MOV EA to register
                        ((and (eq inst mov-ea)
                              (eql (sap-ref-8 code-sap (dstate-cur-offs dstate)) #x8B)
                              (rip-relative-p (sap-ref-8 code-sap (1+ (dstate-cur-offs dstate)))))
                         (let ((addr (+ (signed-sap-ref-32 code-sap (+ (dstate-cur-offs dstate) 2))
                                        (dstate-next-addr dstate))))
                           (when (and (not (logtest addr (ash sb-vm:lowtag-mask -1))) ; aligned
                                      (<= boxed-begin addr) (< addr boxed-end))
                             (let ((const (sap-ref-lispobj (int-sap addr) 0)))
                               (when (fdefn-p const)
                                 (pushnew const observable-fdefns))))))
                        ;; find FDEFN as the target of JMP/CALL
                        ((or (eq inst jmp) (eq inst call))
                         (let ((target (+ (near-jump-displacement chunk dstate)
                                          (dstate-next-addr dstate))))
                           ;; Can't be an FDEFN if within the same code object
                           (unless (and (<= code-begin target) (< target code-end))
                             (let ((fdefn (dolist (fdefn all-fdefns)
                                            (when (= target (+ (get-lisp-obj-address fdefn)
                                                               ;; KLUDGE: 2 = address of 'jmp' inst
                                                               (- 2 other-pointer-lowtag)))
                                              (return fdefn)))))
                               (when (and fdefn (neq (info :function :inlinep (fdefn-name fdefn))
                                                     'notinline))
                                 (push (cons (+ (sap- code-sap code-insts)
                                                (1+ (sb-disassem:dstate-cur-offs dstate)))
                                             fdefn)
                                       fixups)))))))))
            (map-segment-instructions #'process-inst seg dstate)))
        (sb-vm::statically-link-code-obj code fixups observable-fdefns)))))

;;; While concurrent use of un-statically-link is unlikely, misuse could easily
;;; cause heap corruption. It's preventable by ensuring that this is atomic
;;; with respect to other mutations of the same fdefn.
;;; The issue is that if the fdefn loses the pointer to the underlying code
;;; via (setf fdefn-fun) before we were done removing the static links,
;;; then there could be no remaining pointers visible to GC.
;;; The only way to detect the current set of references is to find uses of the
;;; current jump address, which means we need to fix them *all* before anyone
;;; else gets an opportunity to change the fdefn-fun of this same fdefn again.
(defun sb-impl::undo-static-linkage (fdefn &aux (fun-entry (sb-vm::fdefn-raw-addr fdefn)))
  (unless (sb-vm::fdefn-has-static-callers fdefn)
    (return-from sb-impl::undo-static-linkage))
  (sb-int:with-system-mutex (sb-vm::*static-linker-lock*)
    (do-text-space-code (code)
      ;; Examine only those code components which potentially use FDEFN.
      (binding* ((fdefn-index
                  (multiple-value-bind (fdefns-start count) (code-header-fdefn-range code)
                    (dotimes (i count)
                      (let ((constant (code-header-ref code (+ fdefns-start i))))
                        (when (or (eq constant fdefn)
                                  (and (consp constant) (eq (car constant) fdefn)))
                          (return (+ fdefns-start i))))))
                  :exit-if-null))
        #-immobile-code
        (let ((header-slot-addr
               (+ (get-lisp-obj-address code)
                  (- (ash fdefn-index word-shift) other-pointer-lowtag))))
          (do-functions (fun addr)
            (map-segment-instructions
             (lambda (chunk inst)
               (when (or (eq inst jmp) (eq inst call))
                 ;; If the jump is to FUN-ENTRY, rewrite this call into
                 ;; MOV RAX,[RIP-n] ; CALL [RAX+9]
                 (let ((disp (truly-the (signed-byte 32) (near-jump-displacement chunk dstate))))
                   (when (= (+ disp (dstate-next-addr dstate)) fun-entry)
                     ;; 5 bytes prior has to be the start of a 5-byte NOP.
                     ;; Just trust that it is that.
                     (let ((sap (sap+ (int-sap (dstate-cur-addr dstate)) -5)))
                       (setf (sap-ref-8 sap 0) #x48
                             (sap-ref-8 sap 1) #x8B
                             (sap-ref-8 sap 2) #x05
                             (signed-sap-ref-32 sap 3)
                             (- 3 (- (dstate-next-addr dstate) header-slot-addr))
                             (sap-ref-8 sap 7) #xFF
                             (sap-ref-8 sap 8) (if (eq inst jmp) #x60 #x50)
                             (sap-ref-8 sap 9) 9))))))
             seg dstate)))
        #+immobile-code
        (flet ((fix (sap oldval)
                 (let* ((fdefn-entry (sb-vm::fdefn-entry-address fdefn))
                        (newval (the (signed-byte 32)
                                     (- fdefn-entry (sap-int (sap+ sap 4))))))
                   ;; CMPXCHG is atomic even when misaligned, and x86-64 promises
                   ;; that self-modifying code works correctly, so the fetcher
                   ;; should never see a torn write.
                   (cas (sap-ref-32 sap 0)
                        (ldb (byte 32 0) oldval)
                        (ldb (byte 32 0) newval)))))
          (let ((constant (code-header-ref code fdefn-index)))
            (setf (code-header-ref code fdefn-index) fdefn)
            (if (listp constant) ; list of saved fixup offsets
                (dolist (offset (cdr constant))
                  (let ((sap (sap+ (code-instructions code) offset)))
                    (fix sap (signed-sap-ref-32 sap 0))))
                (do-functions (fun addr)
                  (map-segment-instructions
                   (lambda (chunk inst)
                     (when (or (eq inst jmp) (eq inst call))
                       ;; If the jump is to FUN-ENTRY, change it back to FDEFN-ENTRY
                       ;; TRULY-THE because near-jump-displacement isn't a known fun.
                       (let ((disp (truly-the (signed-byte 32)
                                              (near-jump-displacement chunk dstate))))
                         (when (= (+ disp (dstate-next-addr dstate)) fun-entry)
                           (fix (sap+ (int-sap (dstate-cur-addr dstate)) 1) disp)))))
                   seg dstate)))))))
    (sb-vm::set-fdefn-has-static-callers fdefn 0)))
