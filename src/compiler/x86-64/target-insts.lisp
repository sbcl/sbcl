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

(in-package "SB!VM")

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
  (declare (type (member :ref :sized-ref :compute) mode)
           (type list value)
           (type (member nil :byte :word :dword :qword) width)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (when (and width (eq mode :sized-ref))
    (princ width stream)
    (princ '| PTR | stream))
  (write-char #\[ stream)
  (let ((firstp t) (rip-p nil))
    (macrolet ((pel ((var val) &body body)
                 ;; Print an element of the address, maybe with
                 ;; a leading separator.
                 `(let ((,var ,val))
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
               (let ((addr (+ offset (sb!disassem:dstate-next-addr dstate))))
                 (when (plusp addr) ; FIXME: what does this test achieve?
                    (let ((hook (sb!disassem:dstate-get-prop
                                 dstate :rip-relative-mem-ref-hook)))
                      (when hook
                        (funcall hook offset width)))
                    (or (nth-value
                         1 (sb!disassem::note-code-constant-absolute
                            addr dstate width))
                        (sb!disassem:maybe-note-assembler-routine
                         addr nil dstate))))))
            (firstp
             (progn
               (sb!disassem:princ16 offset stream)
               (or (minusp offset)
                   (nth-value 1
                              (sb!disassem::note-code-constant-absolute offset dstate))
                   (sb!disassem:maybe-note-assembler-routine offset
                                                             nil
                                                             dstate))))
            (t
             (princ offset stream)))))))
  (write-char #\] stream)
  #!+sb-thread
  (let ((disp (second value)))
    (when (and (eql (first value) #.(ash (tn-offset thread-base-tn) -1))
               (not (third value)) ; no index
               (typep disp '(integer 0 *)) ; positive displacement
               (sb!disassem::seg-code (sb!disassem:dstate-segment dstate)))
      ;; Try to reverse-engineer which thread-local binding this is
      (let* ((code (sb!disassem::seg-code (sb!disassem:dstate-segment dstate)))
             (header-n-words
              (ash (sap-ref-word (int-sap (get-lisp-obj-address code))
                                 (- other-pointer-lowtag)) -8))
             (tls-index (ash disp (- n-fixnum-tag-bits))))
        (loop for word-num from code-constants-offset below header-n-words
              for obj = (code-header-ref code word-num)
              when (and (symbolp obj) (= (symbol-tls-index obj) tls-index))
              do (return-from print-mem-ref
                   (sb!disassem:note
                    (lambda (stream) (format stream "tls: ~S" obj))
                    dstate))))
      ;; Or maybe we're looking at the 'struct thread' itself
      (when (< disp max-interrupts)
        (let* ((thread-slots (primitive-object-slots
                              (find 'thread *primitive-objects*
                                    :key #'primitive-object-name)))
               (slot (find (ash disp (- word-shift)) thread-slots
                           :key #'slot-offset)))
          (when slot
            (return-from print-mem-ref
              (sb!disassem:note
               (lambda (stream)
                 (format stream "thread.~(~A~)" (slot-name slot)))
               dstate))))))))

(in-package "SB!DISASSEM")

;; Pre-scan a disassembly segment list to find heuristically the start of
;; unboxed constants. This isn't done for disassembly of arbitrary memory,
;; only for Lisp code because that is known to obey the convention that
;; RIP-relative accesses having positive displacement are to unboxed constants.
;; For each reference, record its length so that it will subsequently
;; display the proper number of bytes.

(defun determine-opcode-bounds (seglist dstate)
  (flet ((mem-ref (displacement size more-segments)
           (let ((seg (dstate-segment dstate))
                 ;; compute a segment-relative address of the constant
                 (ref-offset (+ (dstate-next-offs dstate) displacement)))
             ;; Terminate MAP-SEGMENT-INSTRUCTIONS early for this segment
             (setf (seg-opcodes-length seg)
                   (min ref-offset (seg-opcodes-length seg)))
             ;; And for following segments.
             (let ((code-offset (segment-offs-to-code-offs ref-offset seg)))
               (dolist (seg more-segments)
                 (setf (seg-opcodes-length seg)
                       (min (code-offs-to-segment-offs code-offset seg)
                            (seg-opcodes-length seg)))))
             ;; Store the segment-relative address of the reference.
             ;; There should not be duplicate references with different sizes.
             (unless (member ref-offset (seg-unboxed-refs seg) :key #'car)
               (push (cons ref-offset size) (seg-unboxed-refs seg))))))
    (do* ((sink (make-broadcast-stream))
          (tail seglist (cdr tail))
          (seg (car tail) (car tail)))
         ((endp tail))
      (setf (dstate-get-prop dstate :rip-relative-mem-ref-hook)
            (lambda (displacement size)
              (when (plusp displacement) ; displacement forward from RIP
                (mem-ref displacement size (cdr tail)))))
      (let (last-inst-start-ofs last-inst-end-ofs)
        ;; Scan the segment for memory references by "printing",
        ;; which will invoke PRINT-MEM-REF as appropriate.
        (map-segment-instructions
           (lambda (chunk inst)
             (declare (type dchunk chunk) (type instruction inst))
             (let ((printer (inst-printer inst)))
               (when printer
                 (funcall printer chunk inst sink dstate)))
             (setf last-inst-start-ofs (dstate-cur-offs dstate)
                   last-inst-end-ofs (1- (dstate-next-offs dstate))))
           seg dstate)
        ;; Decrease the length further if the last instruction can't be
        ;; completely decoded without crossing into the unboxed constants.
        ;; It's probably zero-fill. "ADD [RAX],AL" encodes as {0,0}
        ;; and is the most likely reason to chop one byte and stop.
        (unless (< last-inst-end-ofs (seg-opcodes-length seg))
          (setf (seg-opcodes-length seg) last-inst-start-ofs)))))
  (setf (dstate-get-prop dstate :rip-relative-mem-ref-hook) nil))

(defun disassemble-unboxed-data (segment stream dstate)
  (unless (< (dstate-cur-offs dstate) (seg-length segment))
    (return-from disassemble-unboxed-data))
  (aver (= (dstate-cur-offs dstate) (seg-opcodes-length segment)))
  ;; Remove refs at addresses outside this segment and sort whatever remains.
  (let ((refs (sort (remove-if (lambda (x) (< (car x) (dstate-cur-offs dstate)))
                               (seg-unboxed-refs segment))
                    #'< :key #'car)))
    (flet ((hexdump (nbytes)
             (print-current-address stream dstate)
             (print-inst nbytes stream dstate)
             (incf (dstate-cur-offs dstate) nbytes)))
      ;; Demarcate just before the first byte of 0-fill (if any) rather than at
      ;; the first location which was referenced as data, because it looks
      ;; nicer to have no incomplete instructions prior to that.
      (format stream "~&; Unboxed data:")
      ;; The way to guarantee we have the exact 'data-start' is to track refs
      ;; from all disassembly segments to all others. This is not trivial,
      ;; so not implemented.
      (let ((data-start (caar refs)))
        (when (and data-start (> data-start (dstate-cur-offs dstate)))
          ;; Padding follows the last decoded instruction
          (hexdump (- data-start (seg-opcodes-length segment)))))
      ;; Print all bytes of each unboxed memory reference on a line.
      (loop
         (when (>= (dstate-cur-offs dstate)
                   (seg-length (dstate-segment dstate)))
           (return))
         (let* ((size (cdar refs)) ; a keyword designating the operand-size
                (nbytes (if size (sb!vm::size-nbyte size))))
           ;; Dump until the next unboxed ref not to exceed seg-length.
           ;; XMM register operations invoke PRINT-MEM-REF with WIDTH=NIL but
           ;; in anticipation of possibly loading a vector register (YMM,ZMM)
           ;; with adjacent packed constants, the REFS list is advanced only
           ; when cur-offs is beyond the current ref.
           (cond (nbytes
                  ;; assert that we don't run past the next ref.
                  (aver (or (endp (cdr refs))
                            (<= (+ (dstate-cur-offs dstate) nbytes)
                                (caadr refs))))
                  (hexdump nbytes))
                 (t
                  (hexdump
                   (min (if (cdr refs)
                            (- (caadr refs) (dstate-cur-offs dstate))
                            16) ; for lack of anything better
                        (- (seg-length (dstate-segment dstate))
                           (dstate-cur-offs dstate))))))) ; clip to seg
         (when (and (cdr refs) (>= (dstate-cur-offs dstate) (caadr refs)))
           (pop refs))))))
