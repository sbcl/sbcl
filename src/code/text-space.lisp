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
                                   nil
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

;;; Ensure that any caller of FNAME jumps via the linkage table
;;; and not directly to the current functional binding.
(defun sb-vm::unbypass-linkage (fname fun-entry index &aux (all (permanent-fname-p fname)))
  ;; Permanent names are never mentioned in the code header, so we may have to
  ;; scan all code. Otherwise, only scan code that explicitly uses INDEX.
  (flet ((index-used-p (packed &aux (pos 0))
           (sb-c::do-packed-varints (loc packed pos)
             (when (= loc index)
               (return-from index-used-p t)))))
    (do-text-space-code (code)
      (when (or all (index-used-p (code-header-ref code code-linkage-elts-slot)))
        (do-functions (fun addr)
          (map-segment-instructions
           (lambda (chunk inst)
             (when (or (eq inst jmp) (eq inst call))
               ;; If the jump is to FUN-ENTRY, change it to a linkage space indirection
               (let ((disp (truly-the (signed-byte 32) (near-jump-displacement chunk dstate))))
                 (when (= (+ disp (dstate-next-addr dstate)) fun-entry)
                   (let ((sap (int-sap (dstate-cur-addr dstate)))
                         (cell (sap+ sb-vm::elf-linkage-space (ash index word-shift))))
                     (declare (ignorable cell))
                     #+immobile-space
                     (cond ((eq inst jmp)
                            (aver (= (sap-ref-8 sap 5) #x90)) ; NOP
                            (setf (sap-ref-16 sap 0) #x25ff  ; JMP [RIP+n]
                                  (signed-sap-ref-32 sap 2) (sap- cell (sap+ sap 6))))
                           (t
                            (aver (= (sap-ref-8 sap -1) #x40)) ; REX (no bits)
                            (setf (sap-ref-16 sap -1) #x15ff   ; CALL [RIP+n]
                                  (signed-sap-ref-32 sap 1) (sap- cell (sap+ sap 5)))))
                     #-immobile-space
                     (cond ((eq inst jmp)
                            (aver (= (sap-ref-32 sap -4) #xF0458B49)) ; MOV RAX, [R13-16]
                            (aver (= (sap-ref-8 sap 5) #x90)) ; NOP
                            (setf (sap-ref-16 sap 0) #xa0ff   ; JMP [RAX+n]
                                  (signed-sap-ref-32 sap 2) (ash index word-shift)))

                           (t
                            (aver (= (sap-ref-32 sap -5) #xF0458B49)) ; MOV RAX, [R13-16]
                            (aver (= (sap-ref-8 sap -1) #x40)) ; REX (no bits)
                            (setf (sap-ref-16 sap -1) #x90ff   ; CALL [RAX+n]
                                  (signed-sap-ref-32 sap 1) (ash index word-shift)))))))))
           seg dstate))))))
