;;;; target-only stuff from CMU CL's src/compiler/x86/insts.lisp
;;;;
;;;; i.e. stuff which was in CMU CL's insts.lisp file, but which in
;;;; the SBCL build process can't be compiled into code for the
;;;; cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!X86-64-ASM")

;;; Prints a memory reference to STREAM. VALUE is a list of
;;; (BASE-REG OFFSET INDEX-REG INDEX-SCALE), where any component may be
;;; missing or nil to indicate that it's not used or has the obvious
;;; default value (e.g., 1 for the index-scale). BASE-REG can be the
;;; symbol RIP or a full register, INDEX-REG a full register. If WIDTH
;;; is non-nil it should be one of the symbols :BYTE, :WORD, :DWORD or
;;; :QWORD; a corresponding size indicator is printed if MODE is :SIZED-REF.
;;; The rationale for supplying WIDTH while eliding a pointer-size qualifier
;;; is that proper dereferencing of RIP-relative constants requires a size,
;;; but in other cases would only add clutter, since a source/destination
;;; register implies a size.
;;;
(defun print-mem-ref (mode value width stream dstate)
  ;; :COMPUTE is used for the LEA instruction - it informs this function
  ;; that the address is not a memory reference below which is confined
  ;; the disassembly - the heuristic for detecting the start of unboxed data.
  ;; LEA is sometimes used to compute the start of a local function for
  ;; allocate-closures, and it points to valid instructions, not data.
  (declare (type (member :ref :sized-ref :compute) mode)
           (type list value)
           (type (member nil :byte :word :dword :qword) width)
           (type stream stream)
           (type disassem-state dstate))
  (when (and width (eq mode :sized-ref))
    (princ width stream)
    (princ '| PTR | stream))
  (write-char #\[ stream)
  (let ((firstp t) (rip-p nil))
    (macrolet ((pel ((var val) &body body)
                 ;; Print an element of the address, maybe with
                 ;; a leading separator.
                 `(let ((,var ,val))
                    ;; Compiler knows that FIRSTP is T in first call to PEL.
                    #-sb-xc-host
                    (declare (muffle-conditions code-deletion-note))
                    (when ,var
                      (unless firstp
                        (write-char #\+ stream))
                      ,@body
                      (setq firstp nil)))))
      (pel (base-reg (first value))
        (cond ((eql 'rip base-reg)
               (setf rip-p t)
               (princ base-reg stream))
              (t
               (print-addr-reg base-reg stream dstate))))
      (pel (index-reg (third value))
        (print-addr-reg index-reg stream dstate)
        (let ((index-scale (fourth value)))
          (when (and index-scale (not (= index-scale 1)))
            (write-char #\* stream)
            (princ index-scale stream))))
      (let ((offset (second value)))
        (when (and offset (or firstp (not (zerop offset))))
          (unless (or firstp (minusp offset))
            (write-char #\+ stream))
          (cond
            (rip-p
             (princ offset stream)
             (unless (eq mode :compute)
               (let ((addr (+ offset (dstate-next-addr dstate))))
                 (or (nth-value
                      1 (note-code-constant-absolute addr dstate width))
                     (maybe-note-assembler-routine addr nil dstate)
                     ;; Show the absolute address and maybe the contents.
                     (note (format nil "[#x~x]~@[ = ~x~]"
                                   addr
                                   (case width
                                     (:qword
                                      (unboxed-constant-ref
                                       dstate
                                       (+ (dstate-next-offs dstate) offset)))))
                           dstate)))))
            (firstp
               (princ16 offset stream)
               (or (minusp offset)
                   (nth-value 1 (note-code-constant-absolute offset dstate))
                   (maybe-note-assembler-routine offset nil dstate)))
            (t
             (princ offset stream)))))))
  (write-char #\] stream)
  #!+sb-thread
  (let ((disp (second value)))
    (when (and (eql (first value) #.(ash (tn-offset sb!vm::thread-base-tn) -1))
               (not (third value)) ; no index
               (typep disp '(integer 0 *)) ; positive displacement
               (seg-code (dstate-segment dstate)))
      ;; Try to reverse-engineer which thread-local binding this is
      (let* ((code (seg-code (dstate-segment dstate)))
             (header-n-words
              (ash (sap-ref-word (int-sap (get-lisp-obj-address code))
                                 (- other-pointer-lowtag)) -8))
             (tls-index (ash disp (- n-fixnum-tag-bits))))
        (loop for word-num from code-constants-offset below header-n-words
              for obj = (code-header-ref code word-num)
              when (and (symbolp obj) (= (symbol-tls-index obj) tls-index))
              do (return-from print-mem-ref
                   (note (lambda (stream) (format stream "tls: ~S" obj))
                         dstate))))
      ;; Or maybe we're looking at the 'struct thread' itself
      (when (< disp max-interrupts)
        (let* ((thread-slots
                (load-time-value
                 (primitive-object-slots
                  (find 'sb!vm::thread *primitive-objects*
                        :key #'primitive-object-name)) t))
               (slot (find (ash disp (- word-shift)) thread-slots
                           :key #'slot-offset)))
          (when slot
            (return-from print-mem-ref
              (note (lambda (stream)
                      (format stream "thread.~(~A~)" (slot-name slot)))
                    dstate))))))))

(defun unboxed-constant-ref (dstate segment-offset)
  (let* ((seg (dstate-segment dstate))
         (code-offset
          (sb!disassem::segment-offs-to-code-offs segment-offset seg))
         (unboxed-range (sb!disassem::seg-unboxed-data-range seg)))
    (and unboxed-range
         (<= (car unboxed-range) code-offset (cdr unboxed-range))
         (sap-ref-int (dstate-segment-sap dstate)
                      segment-offset n-word-bytes
                      (dstate-byte-order dstate)))))
