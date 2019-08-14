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

(defmacro do-immobile-functions ((code-var fun-var addr-var &key (if t)) &body body)
  ;; Loop over all code objects
  `(let* ((call (find-inst #xE8 (get-inst-space)))
          (jmp  (find-inst #xE9 (get-inst-space)))
          (dstate (make-dstate nil))
          (sap (int-sap 0))
          (seg (sb-disassem::%make-segment :sap-maker (lambda () sap))))
     (sb-vm::map-objects-in-range
      (lambda (,code-var obj-type obj-size)
        (declare (ignore obj-size))
        (when (and (= obj-type code-header-widetag) ,if)
          ;; Loop over all embedded functions
          (dotimes (fun-index (code-n-entries ,code-var))
            (let* ((,fun-var (%code-entry-point ,code-var fun-index))
                   (,addr-var (+ (get-lisp-obj-address ,fun-var)
                                 (- fun-pointer-lowtag)
                                 (ash simple-fun-insts-offset word-shift))))
              (with-pinned-objects (sap) ; Mutate SAP to point to fun
                (setf (sap-ref-word (int-sap (get-lisp-obj-address sap))
                                    (- n-word-bytes other-pointer-lowtag))
                      ,addr-var))
              (setf (seg-virtual-location seg) ,addr-var
                    (seg-length seg) (%simple-fun-text-len ,fun-var fun-index))
              ,@body))))
      ;; Slowness here is bothersome, especially for SB-VM::REMOVE-STATIC-LINKS,
      ;; so skip right over all fixedobj pages.
      (ash varyobj-space-start (- n-fixnum-tag-bits))
      (%make-lisp-obj (sap-int *varyobj-space-free-pointer*)))))

(defun sb-vm::collect-immobile-code-relocs ()
  (let ((code-components
         (make-array 20000 :element-type '(unsigned-byte 32)
                           :fill-pointer 0 :adjustable t))
        (relocs
         (make-array 100000 :element-type '(unsigned-byte 32)
                            :fill-pointer 0 :adjustable t))
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
      (sb-vm:map-allocated-objects
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

(defun sb-vm::statically-link-core (&key callers exclude-callers
                                         callees exclude-callees
                                         verbose)
  (flet ((match-p (name include exclude)
           (and (not (member name exclude :test 'equal))
                (or (not include) (member name include :test 'equal)))))
    (do-immobile-functions (code fun addr)
      (when (match-p (%simple-fun-name fun) callers exclude-callers)
        (dx-let ((printed-fun-name nil)
                 ;; The length of CODE-HEADER-FUNS is an upper-bound on the
                 ;; number of fdefns referenced.
                 (code-header-funs (make-array (code-header-words code))))
          ;; Collect the called functions
          (do ((n 0)
               (i (1- (length code-header-funs)) (1- i)))
              ((< i sb-vm:code-constants-offset)
               (setf (%array-fill-pointer code-header-funs) n))
            (let ((obj (code-header-ref code i)))
              (when (fdefn-p obj)
                (setf (svref code-header-funs n) (fdefn-fun obj))
                (incf n))))
          ;; Compute code bounds
          (let* ((code-begin (- (get-lisp-obj-address code)
                                sb-vm:other-pointer-lowtag))
                 (code-end (+ code-begin (sb-vm::code-object-size code))))
            ;; Loop over function's assembly code
            (dx-flet ((process-inst (chunk inst)
               (when (or (eq inst jmp) (eq inst call))
                 (let ((target (+ (near-jump-displacement chunk dstate)
                                  (dstate-next-addr dstate))))
                   ;; Do not call FIND-CALLED-OBJECT if the target is
                   ;; within the same code object
                   (unless (and (<= code-begin target) (< target code-end))
                     (let ((fdefn (sb-vm::find-called-object target)))
                       (when (and (fdefn-p fdefn)
                                  (let ((callee (fdefn-fun fdefn)))
                                    (and (sb-vm::call-direct-p callee code-header-funs)
                                         (match-p (%fun-name callee)
                                                  callees exclude-callees))))
                         (let ((entry (sb-vm::fdefn-raw-addr fdefn)))
                           (when verbose
                             (let ((*print-pretty* nil))
                               (unless printed-fun-name
                                 (format t "#x~X ~S~%" (get-lisp-obj-address fun) fun)
                                 (setq printed-fun-name t))
                               (format t "  @~x -> ~s [~x]~%"
                                       (dstate-cur-addr dstate) (fdefn-name fdefn) entry)))
                           ;; Set the statically-linked flag
                           (sb-vm::set-fdefn-has-static-callers fdefn 1)
                           ;; Change the machine instruction
                           (setf (signed-sap-ref-32 (int-sap (dstate-cur-addr dstate)) 1)
                                 (- entry (dstate-next-addr dstate)))))))))))
              (map-segment-instructions #'process-inst
                                        seg dstate))))))))

;;; While concurrent use of un-statically-link is unlikely, misuse could easily
;;; cause heap corruption. It's preventable by ensuring that this is atomic
;;; with respect to other mutations of the same fdefn.
;;; The issue is that if the fdefn loses the pointer to the underlying code
;;; via (setf fdefn-fun) before we were done removing the static links,
;;; then there could be no remaining pointers visible to GC.
;;; The only way to detect the current set of references is to find uses of the
;;; current jump address, which means we need to fix them *all* before anyone
;;; else gets an opportunity to change the fdefn-fun of this same fdefn again.
(defun sb-vm::remove-static-links (fdefn)
  (sb-thread::with-system-mutex (sb-c::*static-linker-lock*)
    ;; If the jump is to FUN-ENTRY, change it back to FDEFN-ENTRY
    (let ((fun-entry (sb-vm::fdefn-raw-addr fdefn))
          (fdefn-entry (sb-vm::fdefn-entry-address fdefn)))
      ;; Examine only those code components which potentially use FDEFN.
      (do-immobile-functions
         (code fun addr :if (loop for i downfrom (1- (sb-kernel:code-header-words code))
                                  to sb-vm:code-constants-offset
                                  thereis (eq (sb-kernel:code-header-ref code i)
                                              fdefn)))
        (map-segment-instructions
         (lambda (chunk inst)
           (when (or (eq inst jmp) (eq inst call))
             ;; TRULY-THE because near-jump-displacement isn't a known fun.
             (let ((disp (truly-the (signed-byte 32)
                                    (near-jump-displacement chunk dstate))))
               (when (= fun-entry (+ disp (dstate-next-addr dstate)))
                 (let ((new-disp
                        (the (signed-byte 32)
                             (- fdefn-entry (dstate-next-addr dstate)))))
                   ;; CMPXCHG is atomic even when misaligned, and x86-64 promises
                   ;; that self-modifying code works correctly, so the fetcher
                   ;; should never see a torn write.
                   (%primitive sb-vm::signed-sap-cas-32
                               (int-sap (dstate-cur-addr dstate))
                               1 disp new-disp))))))
         seg dstate)))
    (sb-vm::set-fdefn-has-static-callers fdefn 0))) ; Clear static link flag
