;;;; disassembler-related stuff not needed in cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-DISASSEM")

;;;; FIXME: A lot of stupid package prefixes would go away if DISASSEM
;;;; would use the SB-DI package. And some more would go away if it would
;;;; use SB-SYS (in order to get to the SAP-FOO operators).

(defstruct (instruction (:conc-name inst-)
                        (:constructor
                         make-instruction (name format-name print-name
                                           length mask id printer labeller
                                           prefilter control))
                        (:copier nil))
  (name nil :type symbol :read-only t)
  (format-name nil :type (or symbol string) :read-only t)

  (mask dchunk-zero :type dchunk :read-only t)   ; bits in the inst that are constant
  (id dchunk-zero :type dchunk :read-only t)     ; value of those constant bits

  (length 0 :type disassem-length :read-only t)  ; in bytes

  (print-name nil :type symbol :read-only t)

  ;; disassembly methods
  (prefilter nil :type function :read-only t)
  (labeller nil :type (or list vector) :read-only t)
  (printer nil :type (or null function) :read-only t)
  (control nil :type (or null function) :read-only t)

  ;; instructions that are the same as this instruction but with more
  ;; constraints
  (specializers nil :type list))
(declaim (freeze-type instruction))
(defmethod print-object ((inst instruction) stream)
  (print-unreadable-object (inst stream :type t :identity t)
    (format stream "~A(~A)" (inst-name inst) (inst-format-name inst))))

;;;; combining instructions where one specializes another

;;; Return non-NIL if the instruction SPECIAL is a more specific
;;; version of GENERAL (i.e., the same instruction, but with more
;;; constraints).
(defun inst-specializes-p (special general)
  (declare (type instruction special general))
  (let ((smask (inst-mask special))
        (gmask (inst-mask general)))
    (and (dchunk= (inst-id general)
                  (dchunk-and (inst-id special) gmask))
         (dchunk-strict-superset-p smask gmask))))

;;; a bit arbitrary, but should work ok...
;;;
;;; Return an integer corresponding to the specificity of the
;;; instruction INST.
(defun specializer-rank (inst)
  (declare (type instruction inst))
  (* (dchunk-count-bits (inst-mask inst)) 4))

;;; Order the list of instructions INSTS with more specific (more
;;; constant bits, or same-as argument constains) ones first. Returns
;;; the ordered list.
(defun order-specializers (insts)
  (declare (type list insts))
  (sort insts #'> :key #'specializer-rank))

;;; Given a list of instructions INSTS, Sees if one of these instructions is a
;;; more general form of all the others, in which case they are put into its
;;; specializers list, and it is returned. Otherwise an error is signaled.
(defun try-specializing (insts)
  (declare (type list insts))
  (let ((masters (copy-list insts)))
    (dolist (possible-master insts)
      (dolist (possible-specializer insts)
        (unless (or (eq possible-specializer possible-master)
                    (inst-specializes-p possible-specializer possible-master))
          (setf masters (delete possible-master masters))
          (return)                      ; exit the inner loop
          )))
    (cond ((null masters)
           (bug "~@<Instructions either aren't related or conflict in some way: ~4I~_~S~:>"
                insts))
          ((cdr masters)
           (error "multiple specializing masters: ~S" masters))
          (t
           (let ((master (car masters)))
             (setf (inst-specializers master)
                   (order-specializers (remove master insts)))
             master)))))

;;;; choosing an instruction

(declaim (inline inst-matches-p choose-inst-specialization))

;;; Return non-NIL if all constant-bits in INST match CHUNK.
(defun inst-matches-p (inst chunk)
  (declare (type instruction inst)
           (type dchunk chunk))
  (dchunk= (dchunk-and (inst-mask inst) chunk) (inst-id inst)))

;;; Given an instruction object, INST, and a bit-pattern, CHUNK, pick
;;; the most specific instruction on INST's specializer list whose
;;; constraints are met by CHUNK. If none do, then return INST.
(defun choose-inst-specialization (inst chunk)
  (declare (type instruction inst)
           (type dchunk chunk))
  (or (dolist (spec (inst-specializers inst) nil)
        (declare (type instruction spec))
        (when (inst-matches-p spec chunk)
          (return spec)))
      inst))

;;;; an instruction space holds all known machine instructions in a
;;;; form that can be easily searched

(defstruct (inst-space (:conc-name ispace-)
                       (:copier nil))
  (valid-mask dchunk-zero :type dchunk) ; applies to *children*
  (choices nil :type list))
(declaim (freeze-type inst-space))
(defmethod print-object ((ispace inst-space) stream)
  (print-unreadable-object (ispace stream :type t :identity t)))

;;; now that we've defined the structure, we can declaim the type of
;;; the variable:
(define-load-time-global *disassem-inst-space* nil)
(declaim (type (or null inst-space) *disassem-inst-space*))

(defstruct (inst-space-choice (:conc-name ischoice-)
                              (:copier nil))
  (common-id dchunk-zero :type dchunk)  ; applies to *parent's* mask
  (subspace (missing-arg) :type (or inst-space instruction)))
(declaim (freeze-type inst-space-choice))

;;;; searching for an instruction in instruction space

#-x86-64
(defun pre-decode (chunk dstate)
  (declare (ignore dstate))
  (values chunk 0))

;;; Return the instruction object within INST-SPACE corresponding to the
;;; bit-pattern CHUNK, or NIL if there isn't one.
(defun find-inst (chunk inst-space &optional dstate)
  (declare (type dchunk chunk)
           (type (or null inst-space instruction) inst-space))
  (binding* (((new-chunk length-adjustment)
              (if dstate (pre-decode chunk dstate) (values chunk 0)))
             (inst
              (named-let recurse ((chunk new-chunk) (inst-space inst-space))
                (etypecase inst-space
                  (null nil)
                  (instruction
                   (if (inst-matches-p inst-space chunk)
                       (choose-inst-specialization inst-space chunk)
                       nil))
                  (inst-space
                   (let* ((mask (ispace-valid-mask inst-space))
                          (id (dchunk-and mask chunk)))
                     (declare (type dchunk id mask))
                     (dolist (choice (ispace-choices inst-space))
                       (declare (type inst-space-choice choice))
                       (when (dchunk= id (ischoice-common-id choice))
                         (return (recurse chunk (ischoice-subspace choice)))))))))))
    (if inst
        (values inst (+ (inst-length inst) length-adjustment) new-chunk)
        (values nil 0 chunk))))

;;;; building the instruction space

;;; Returns an instruction-space object corresponding to the list of
;;; instructions INSTS. If the optional parameter INITIAL-MASK is
;;; supplied, only bits it has set are used.
(defun build-inst-space (insts &optional (initial-mask dchunk-one))
  ;; This is done by finding any set of bits that's common to
  ;; all instructions, building an instruction-space node that selects on those
  ;; bits, and recursively handle sets of instructions with a common value for
  ;; these bits (which, since there should be fewer instructions than in INSTS,
  ;; should have some additional set of bits to select on, etc). If there
  ;; are no common bits, or all instructions have the same value within those
  ;; bits, TRY-SPECIALIZING is called, which handles the cases of many
  ;; variations on a single instruction.
  (declare (type list insts)
           (type dchunk initial-mask))
  (cond ((null insts)
         nil)
        ((null (cdr insts))
         (car insts))
        (t
         (let ((vmask (dchunk-copy initial-mask)))
           (dolist (inst insts)
             (dchunk-andf vmask (inst-mask inst)))
           (if (dchunk-zerop vmask)
               (try-specializing insts)
               (let ((buckets nil))
                 (dolist (inst insts)
                   (let* ((common-id (dchunk-and (inst-id inst) vmask))
                          (bucket (assoc common-id buckets :test #'dchunk=)))
                     (cond ((null bucket)
                            (push (list common-id inst) buckets))
                           (t
                            (push inst (cdr bucket))))))
                 (let ((submask (dchunk-clear initial-mask vmask)))
                   (if (= (length buckets) 1)
                       (try-specializing insts)
                       (make-inst-space
                        :valid-mask vmask
                        :choices (mapcar (lambda (bucket)
                                           (make-inst-space-choice
                                            :subspace (build-inst-space
                                                       (cdr bucket)
                                                       submask)
                                            :common-id (car bucket)))
                                         buckets))))))))))

;;;; an inst-space printer for debugging purposes

(defun print-masked-binary (num mask word-size &optional (show word-size))
  (do ((bit (1- word-size) (1- bit)))
      ((< bit 0))
    (write-char (cond ((logbitp bit mask)
                       (if (logbitp bit num) #\1 #\0))
                      ((< bit show) #\x)
                      (t #\space)))))

(defun print-inst-bits (inst)
  (print-masked-binary (inst-id inst)
                       (inst-mask inst)
                       dchunk-bits
                       (bytes-to-bits (inst-length inst))))

;;; Print a nicely-formatted version of INST-SPACE.
(defun print-inst-space (inst-space &optional (indent 0))
  (etypecase inst-space
    (null)
    (instruction
     (format t "~Vt[~A(~A)~40T" indent
             (inst-name inst-space)
             (inst-format-name inst-space))
     (print-inst-bits inst-space)
     (dolist (inst (inst-specializers inst-space))
       (format t "~%~Vt:~A~40T" indent (inst-name inst))
       (print-inst-bits inst))
     (write-char #\])
     (terpri))
    (inst-space
     (format t "~Vt---- ~8,'0X ----~%"
             indent
             (ispace-valid-mask inst-space))
     (map nil
          (lambda (choice)
            (format t "~Vt~8,'0X ==>~%"
                    (+ 2 indent)
                    (ischoice-common-id choice))
            (print-inst-space (ischoice-subspace choice)
                              (+ 4 indent)))
          (ispace-choices inst-space)))))

;;;; (The actual disassembly part follows.)

;;; Code object layout:
;;;     header-word
;;;     code-size (starting from first inst, in bytes)
;;;     entry-points (points to first function header)
;;;     debug-info
;;;     constant1
;;;     constant2
;;;     ...
;;;     <padding to dual-word boundary>
;;;     start of instructions
;;;     ...
;;;     fun-headers and lra's buried in here randomly
;;;     ...
;;;     <padding to dual-word boundary>
;;;
;;; Function header layout (dual word aligned):
;;;     header-word
;;;     self pointer
;;;     name
;;;     arglist
;;;     type
;;;     info
;;;
;;; LRA layout (dual word aligned):
;;;     header-word

(declaim (inline words-to-bytes))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; Convert a word-offset NUM to a byte-offset.
  (defun words-to-bytes (num)
    (declare (type offset num))
    (ash num sb-vm:word-shift))
  ) ; EVAL-WHEN


(defstruct (offs-hook (:copier nil))
  (offset 0 :type offset)
  (fun (missing-arg) :type function)
  (before-address nil :type (member t nil)))
(declaim (freeze-type offs-hook))

(defmethod print-object ((seg segment) stream)
  (print-unreadable-object (seg stream :type t)
    (let ((addr (sap-int (funcall (seg-sap-maker seg)))))
      (format stream "#X~X..~X[~W]~:[ (#X~X)~;~*~]~@[ in ~S~]"
              addr (+ addr (seg-length seg)) (seg-length seg)
              (= (seg-virtual-location seg) addr)
              (seg-virtual-location seg)
              (seg-code seg)))))

;;;; operations on code-components (which hold the instructions for
;;;; one or more functions)

;;;   code     insts      segment       anywhere
;;;      |         |            |              |
;;;      A         B            C              X
;;;
;;; legend: A = 0th word of code object
;;;         B = A + (ASH (CODE-HEADER-WORDS code) WORD-SHIFT)
;;;         C = B + (SEG-INITIAL-OFFSET seg)
;;;         X = arbitrary location >= C
;;; (B and C could be the same location)

;;; Compute X - A given X - C
(defun segment-offs-to-code-offs (offset segment)
  (+ offset
     (ash (code-header-words (seg-code segment)) sb-vm:word-shift)
     (seg-initial-offset segment)))

;;; Compute X - C given X - A
(defun code-offs-to-segment-offs (offset segment)
  (- offset
     (ash (code-header-words (seg-code segment)) sb-vm:word-shift)
     (seg-initial-offset segment)))

;;; Compute X - C given X - B
(defun code-insts-offs-to-segment-offs (offset segment)
  (- offset (seg-initial-offset segment)))


;;; Is ADDRESS aligned on a SIZE byte boundary?
(declaim (inline aligned-p))
(defun aligned-p (address size)
  (declare (type address address)
           (type alignment size))
  (zerop (logand (1- size) address)))

#-(or x86 x86-64 arm64 riscv)
(progn
(defconstant lra-size (words-to-bytes 1))
(defun lra-hook (chunk stream dstate)
  (declare (type dchunk chunk)
           (ignore chunk)
           (type (or null stream) stream)
           (type disassem-state dstate))
  (when (and (aligned-p (dstate-cur-addr dstate)
                        (* 2 sb-vm:n-word-bytes))
             ;; Check type.
             (= (sap-ref-8 (dstate-segment-sap dstate)
                                  (if (eq (dstate-byte-order dstate)
                                          :little-endian)
                                      (dstate-cur-offs dstate)
                                      (+ (dstate-cur-offs dstate)
                                         (1- lra-size))))
                sb-vm:return-pc-widetag))
    (when stream
      (note "possible LRA header" dstate)))
  nil))

;;; Print the fun-header (entry-point) pseudo-instruction at the
;;; current location in DSTATE to STREAM and skip 2 words.
(defun fun-header-hook (fun-index stream dstate)
  (declare (type (or null stream) stream)
           (type disassem-state dstate))
  (when stream
    (let* ((fun (%code-entry-point (seg-code (dstate-segment dstate)) fun-index))
           (name (%simple-fun-name fun))
           (args (%simple-fun-arglist fun)))
      ;; if the function's name conveys its args, don't show ARGS too
      (format stream ".~A ~S~:[~:A~;~]"
              'entry name
              (and (typep name '(cons (eql lambda) (cons list)))
                   (equal args (second name)))
              args)))
  (incf (dstate-next-offs dstate)
        (words-to-bytes sb-vm:simple-fun-insts-offset)))

;;; Return ADDRESS aligned *upward* to a SIZE byte boundary.
;;; KLUDGE: should be ALIGN-UP but old Slime uses it
(declaim (inline align))
(defun align (address size)
  (declare (type address address)
           (type alignment size))
  (logandc1 (1- size) (+ (1- size) address)))

(defun alignment-hook (chunk stream dstate)
  (declare (type dchunk chunk)
           (ignore chunk)
           (type (or null stream) stream)
           (type disassem-state dstate))
  (let ((location (dstate-cur-addr dstate))
        (alignment (dstate-alignment dstate)))
    (unless (aligned-p location alignment)
      (when stream
        (format stream "~A~Vt~W~%" '.align
                (dstate-argument-column dstate)
                alignment))
      (incf (dstate-next-offs dstate)
            (- (align location alignment) location)))
    nil))

(defun rewind-current-segment (dstate segment)
  (declare (type disassem-state dstate)
           (type segment segment))
  (setf (dstate-segment dstate) segment)
  (setf (dstate-inst-properties dstate) 0)
  (setf (dstate-known-register-contents dstate) nil)
  (setf (dstate-cur-offs-hooks dstate)
        (stable-sort (nreverse (copy-list (seg-hooks segment)))
                     (lambda (oh1 oh2)
                       (or (< (offs-hook-offset oh1) (offs-hook-offset oh2))
                           (and (= (offs-hook-offset oh1)
                                   (offs-hook-offset oh2))
                                (offs-hook-before-address oh1)
                                (not (offs-hook-before-address oh2)))))))
  (setf (dstate-cur-offs dstate) 0)
  (setf (dstate-cur-labels dstate) (dstate-labels dstate)))

(defun call-offs-hooks (before-address stream dstate)
  (declare (type (or null stream) stream)
           (type disassem-state dstate))
  (let ((cur-offs (dstate-cur-offs dstate)))
    (setf (dstate-next-offs dstate) cur-offs)
    (loop
      (let ((next-hook (car (dstate-cur-offs-hooks dstate))))
        (when (null next-hook)
          (return))
        (let ((hook-offs (offs-hook-offset next-hook)))
          (when (or (> hook-offs cur-offs)
                    (and (= hook-offs cur-offs)
                         before-address
                         (not (offs-hook-before-address next-hook))))
            (return))
          (unless (< hook-offs cur-offs)
            (funcall (offs-hook-fun next-hook) stream dstate))
          (pop (dstate-cur-offs-hooks dstate))
          (unless (= (dstate-next-offs dstate) cur-offs)
            (return)))))))

(defun call-fun-hooks (chunk stream dstate)
  (let ((hooks (dstate-fun-hooks dstate))
        (cur-offs (dstate-cur-offs dstate)))
    (setf (dstate-next-offs dstate) cur-offs)
    (dolist (hook hooks nil)
      (let ((prefix-p (funcall hook chunk stream dstate)))
        (unless (= (dstate-next-offs dstate) cur-offs)
          (return prefix-p))))))

;;; Print enough spaces to fill the column used for instruction bytes,
;;; assuming that N-BYTES many instruction bytes have already been
;;; printed in it, then print an additional space as separator to the
;;; opcode column.
(defun pad-inst-column (stream n-bytes)
  (declare (type stream stream)
           (type text-width n-bytes))
  (when (> *disassem-inst-column-width* 0)
    (dotimes (i (- *disassem-inst-column-width* (* 2 n-bytes)))
      (write-char #\space stream))
    (write-char #\space stream)))

(defun handle-bogus-instruction (stream dstate prefix-len)
  (let ((alignment (dstate-alignment dstate)))
    (when stream
      (multiple-value-bind (words bytes)
          (truncate alignment sb-vm:n-word-bytes)
        (when (> words 0)
          (print-inst (* words sb-vm:n-word-bytes) stream dstate
                      :trailing-space nil))
        (when (> bytes 0)
          (print-inst bytes stream dstate :trailing-space nil)))
      (pad-inst-column stream (+ prefix-len alignment))
      (decf (dstate-cur-offs dstate) prefix-len)
      (print-bytes (+ prefix-len alignment) stream dstate))
    (incf (dstate-next-offs dstate) alignment)))

;;; Return an object that is automatically returned to the dstate's filtered-arg-pool
;;; after disassembly of the current instruction.
;;; Any given disassembler backend must use the same constructor for its filtered args
;;; that participate in the pool, and must manually initialize those objects, because
;;; the value could be a recycled one from the pool.
(defun new-filtered-arg (dstate constructor)
  (let ((list (dstate-filtered-arg-pool-free dstate)))
    (unless list
      (setf list (list (funcall constructor))))
    (prog1 (car list)
      (setf (dstate-filtered-arg-pool-free dstate) (cdr list))
      (setf (cdr list) (dstate-filtered-arg-pool-in-use dstate)
            (dstate-filtered-arg-pool-in-use dstate) list))))

(defmacro get-dchunk (state)
  (if (= sb-assem:+inst-alignment-bytes+ 1)
      ;; Don't read beyond the segment. This can occur with DISASSEMBLE-MEMORY
      ;; on a function whose code ends in pad bytes that are not an integral
      ;; number of instructions, and maybe you're so unlucky as to be
      ;; on the exact last page of your heap.
      ;; For 8-byte words and 7-byte dchunks, we use SAP-REF-WORD, which reads
      ;; 8 bytes, so make sure the number of bytes to go is 8,
      ;; never mind that dchunk-bits is less.
      `(logand (cond ((>= bytes-remaining sb-vm:n-word-bytes)
                      (sap-ref-word (dstate-segment-sap ,state) (dstate-cur-offs ,state)))
                     (t
                      (setf (dstate-scratch-buf ,state) 0)
                      (%byte-blt (dstate-segment-sap ,state) (dstate-cur-offs ,state)
                                 (struct-slot-sap ,state disassem-state scratch-buf) 0
                                 bytes-remaining)
                      (dstate-scratch-buf ,state)))
               dchunk-one)
      ;; This was some sort of meagre attempt to be endian-agnostic.
      ;; Perhaps it should just use SAP-REF-n directly?
      `(the dchunk (sap-ref-int (dstate-segment-sap ,state)
                                (dstate-cur-offs ,state)
                                (ecase dchunk-bits (32 4) (64 8))
                                (dstate-byte-order ,state)))))

(defun operand (thing dstate)
  (let ((index (dstate-n-operands dstate)))
    (setf (aref (dstate-operands dstate) index) thing
          (dstate-n-operands dstate) (1+ index))))

;;; Decode the instruction at the current ofset in the segment of DSTATE.
;;; Call this only when all of the following hold:
;;;  - *DISASSEM-INST-SPACE* has been constructed
;;;  - the code object referenced in the DSTATE is pinned
;;;  - the instructions are not so near the end of the code that buffer overrun
;;;    could occur. Since any code object containing at least one simple-fun has
;;;    8 bytes of trailer data, this is safe under normal circumstances.
(defun disassemble-instruction (dstate)
  (declare (type disassem-state dstate))
  (setf (dstate-inst-properties dstate) 0)
  (setf (dstate-filtered-arg-pool-in-use dstate) nil)
  (loop
   ;; There is no point to using GET-DCHUNK. How many bytes remain is unknown.
   (multiple-value-bind (inst len chunk)
       (find-inst (logand (sap-ref-word (dstate-segment-sap dstate)
                                        (dstate-cur-offs dstate))
                          dchunk-one)
                  *disassem-inst-space* dstate)
     (aver inst)
     (let ((offs (+ (dstate-cur-offs dstate) len)))
       (setf (dstate-next-offs dstate) offs)
       (funcall (inst-prefilter inst) dstate chunk)
       ;; Grab the revised NEXT-OFFS
       (setf (dstate-cur-offs dstate) (dstate-next-offs dstate))
       ;; Return the first instruction which has a printer.
       ;; On the x86 architecture, this would skip over segment override
       ;; prefixes, and the LOCK, REX, REP prefixes, etc.
       (awhen (inst-printer inst)
         (funcall it chunk inst nil dstate)
         ;; This won't deal with a prefix (i.e. printerless) instruction that
         ;; also has a "control" function.
         ;; That's probably not a meaningful combination.
         (awhen (inst-control inst)
           (funcall it chunk inst nil dstate)
           ;; FIXME: we're not returning the opaque bytes in any way
           (setf (dstate-cur-offs dstate) (dstate-next-offs dstate)))
         (return (prog1 (cons (inst-name inst)
                              (replace (make-list (dstate-n-operands dstate))
                                       (dstate-operands dstate)))
                   (setf (dstate-n-operands dstate) 0))))))))

;;; Iterate through the instructions in SEGMENT, calling FUNCTION for
;;; each instruction, with arguments of CHUNK, STREAM, and DSTATE.
;;; Additionally, unless STREAM is NIL, several items are output to it:
;;; things printed from several hooks, for example labels, and instruction
;;; bytes before FUNCTION is called, notes and a newline afterwards.
;;; Instructions having an INST-PRINTER of NIL are treated as prefix
;;; instructions which makes them print on the same line as the following
;;; instruction, outputting their INST-PRINT-NAME (unless that is NIL)
;;; before FUNCTION is called for the following instruction.
(defun map-segment-instructions (function segment dstate &optional stream)
  (declare (type function function)
           (type segment segment)
           (type disassem-state dstate)
           (type (or null stream) stream))
  (declare (dynamic-extent function))

  (let ((ispace (get-inst-space))
        (prefix-p nil) ; just processed a prefix inst
        (prefix-len 0) ; sum of lengths of any prefix instruction(s)
        (prefix-print-names nil)) ; reverse list of prefixes seen

    (rewind-current-segment dstate segment)

    (with-pinned-objects ((seg-object (dstate-segment dstate))
                          dstate) ; for SAP access to SCRATCH-BUF
     (setf (dstate-segment-sap dstate) (funcall (seg-sap-maker segment)))

     ;; Now commence disssembly of instructions
     (loop
      (when (>= (dstate-cur-offs dstate) (seg-length (dstate-segment dstate)))
        ;; done!
        (when (and stream (> prefix-len 0))
          (pad-inst-column stream prefix-len)
          (decf (dstate-cur-offs dstate) prefix-len)
          (print-bytes prefix-len stream dstate)
          (incf (dstate-cur-offs dstate) prefix-len))
        (return))

      (setf (dstate-next-offs dstate) (dstate-cur-offs dstate))

      (call-offs-hooks t stream dstate)
      (unless (or prefix-p (null stream))
        (print-current-address stream dstate))
      (call-offs-hooks nil stream dstate)

      (unless (> (dstate-next-offs dstate) (dstate-cur-offs dstate))
         (let* ((bytes-remaining (- (seg-length (dstate-segment dstate))
                                    (dstate-cur-offs dstate)))
                (raw-chunk (get-dchunk dstate))
                (fun-prefix-p (call-fun-hooks raw-chunk stream dstate)))
           (declare (index bytes-remaining))
           (if (> (dstate-next-offs dstate) (dstate-cur-offs dstate))
               (setf prefix-p fun-prefix-p)
               (multiple-value-bind (inst len chunk) (find-inst raw-chunk ispace dstate)
                 (cond ((null inst)
                        (handle-bogus-instruction stream dstate prefix-len)
                        (setf prefix-p nil))
                       ;; On x86, the pad bytes at the end of a simple-fun
                       ;; decode as "ADD [RAX], AL" if there are 2 bytes,
                       ;; but if there's only 1 byte, it should show "BYTE 0".
                       ;; There's really nothing we can do about the former.
                       ((> len bytes-remaining)
                        (when stream
                          (print-inst bytes-remaining stream dstate)
                          (print-bytes bytes-remaining stream dstate)
                          (terpri stream))
                        (return))
                       (t
                        (setf (dstate-inst dstate) inst
                              (dstate-next-offs dstate) (+ (dstate-cur-offs dstate) len))
                        (when stream
                          (print-inst len stream dstate :trailing-space nil))
                        (let ((orig-next (dstate-next-offs dstate)))
                          (funcall (inst-prefilter inst) dstate chunk)
                          (setf prefix-p (null (inst-printer inst)))
                          (when stream
                            ;; Print any instruction bytes recognized by
                            ;; the prefilter which calls read-suffix and
                            ;; updates next-offs.
                            (let ((suffix-len (- (dstate-next-offs dstate)
                                                 orig-next)))
                              (when (plusp suffix-len)
                                (print-inst suffix-len stream dstate
                                            :offset len
                                            :trailing-space nil))
                              ;; Keep track of the number of bytes
                              ;; printed so far.
                              (incf prefix-len (+ len suffix-len)))
                            (if prefix-p
                                (awhen (inst-print-name inst)
                                  (push it prefix-print-names))
                                (progn
                                  ;; PREFIX-LEN includes the length of the
                                  ;; current (non-prefix) instruction here.
                                  (pad-inst-column stream prefix-len)
                                  (dolist (name (reverse prefix-print-names))
                                    (princ name stream)
                                    (write-char #\space stream)))))

                          (funcall function chunk inst)

                          (awhen (inst-control inst)
                            (funcall it chunk inst stream dstate)))))))))

      (setf (dstate-cur-offs dstate) (dstate-next-offs dstate))

      (when stream
        (unless prefix-p
          (setf prefix-len 0
                prefix-print-names nil)
          (print-notes-and-newline stream dstate))
        (setf (dstate-output-state dstate) nil))
      (unless prefix-p
        (setf (dstate-filtered-arg-pool-free dstate)
              (nconc (dstate-filtered-arg-pool-free dstate)
                     (dstate-filtered-arg-pool-in-use dstate)))
        (setf (dstate-filtered-arg-pool-in-use dstate) nil)
        (setf (dstate-inst-properties dstate) 0))))))


(defun collect-labelish-operands (args cache)
  (awhen (remove-if-not #'arg-use-label args)
    (let* ((list (mapcar (lambda (arg &aux (fun (arg-use-label arg))
                                           (prefilter (arg-prefilter arg))
                                           (bytes (arg-fields arg)))
                           ;; Require byte specs or a prefilter (or both).
                           ;; Prefilter alone is ok - it can use READ-SUFFIX.
                           ;; Additionally, you can't have :use-label T
                           ;; if multiple fields exist with no prefilter.
                           (aver (or prefilter
                                     (if (eq fun t) (singleton-p bytes) bytes)))
                           ;; If arg has a prefilter, just compute its index,
                           ;; otherwise keep the byte specs for extraction.
                           (coerce (cons (if (eq fun t) #'identity fun)
                                         (if prefilter
                                             (list (posq arg args))
                                             (cons (arg-sign-extend-p arg) bytes)))
                                   'vector))
                         it))
           (repr (if (cdr list) list (car list))) ; usually just 1 item
           (table (assq :labeller cache)))
      (or (find repr (cdr table) :test 'equalp)
          (car (push repr (cdr table)))))))

;;; Make an initial non-printing disassembly pass through DSTATE,
;;; noting any addresses that are referenced by instructions in this
;;; segment.
(defun add-segment-labels (segment dstate)
  ;; add labels at the beginning with a label-number of nil; we'll notice
  ;; later and fill them in (and sort them)
  (declare (type disassem-state dstate))
  (let ((labels (dstate-labels dstate)))
    (map-segment-instructions
     (lambda (chunk inst)
       (declare (type dchunk chunk) (type instruction inst))
       (declare (optimize (sb-c:insert-array-bounds-checks 0)))
       (loop
         ;; item = #(FUNCTION PREFILTERED-VALUE-INDEX)
         ;;      | #(FUNCTION SIGN-EXTEND-P BYTE-SPEC ...)
         for item in (ensure-list (inst-labeller inst))
         do (let* ((item-length (length item))
                   (index/signedp (svref item 1))
                   (adjusted-value
                     (funcall
                      (svref item 0)
                      (flet ((extract-byte (spec-index)
                               (let* ((byte-spec (svref item spec-index))
                                      (integer (dchunk-extract chunk byte-spec)))
                                 (if index/signedp
                                     (sign-extend integer (byte-size byte-spec))
                                     integer))))
                        (case item-length
                          (2 (svref (dstate-filtered-values dstate) index/signedp))
                          (3 (extract-byte 2)) ; extract exactly one byte
                          (t                   ; extract >1 byte.
                           ;; FIXME: this is strictly redundant.
                           ;; You should combine fields in the prefilter
                           ;; so that the labeller receives a single byte.
                           ;; AARCH64 makes use of this though.
                           (loop for i from 2 below item-length
                                 collect (extract-byte i)))))
                      dstate)))
              ;; If non-integer, the value is not a label.
              (when (and (integerp adjusted-value)
                         (not (assoc adjusted-value labels)))
                (push (cons adjusted-value nil) labels)))))
     segment
     dstate)
    ;; erase any notes that got there by accident
    (setf (dstate-notes dstate) nil)
    ;; add labels from code header jump tables.
    ;; CODE-JUMP-TABLE-WORDS = 0 if the architecture doesn't have jump tables.
    (binding* ((code (seg-code segment) :exit-if-null))
      (with-pinned-objects (code)
        (loop with insts = (code-instructions code)
              for i from 1 below (code-jump-table-words code)
              do (pushnew (cons (sap-ref-word insts (ash i sb-vm:word-shift)) nil)
                          labels :key #'car :test #'=))))
    ;; Return the new list
    (setf (dstate-labels dstate) labels)))

;;; If any labels in DSTATE have been added since the last call to
;;; this function, give them label-numbers, enter them in the
;;; hash-table, and make sure the label list is in sorted order.
(defun number-labels (dstate)
  (let ((labels (dstate-labels dstate)))
    (when (and labels (null (cdar labels)))
      ;; at least one label left un-numbered
      (setf labels (sort labels #'< :key #'car))
      (let ((max -1)
            (label-hash (dstate-label-hash dstate)))
        (dolist (label labels)
          (when (not (null (cdr label)))
            (setf max (max max (cdr label)))))
        (dolist (label labels)
          (when (null (cdr label))
            (incf max)
            (setf (cdr label) max)
            (setf (gethash (car label) label-hash)
                  (format nil "L~W" max)))))
      (setf (dstate-labels dstate) labels))))

(defun compute-mask-id (args)
  (let ((mask dchunk-zero)
        (id dchunk-zero))
    (dolist (arg args (values mask id))
      (let ((av (arg-value arg)))
        (when av
          (do ((fields (arg-fields arg) (cdr fields))
               (values (if (atom av) (list av) av) (cdr values)))
              ((null fields))
            (let ((field-mask (dchunk-make-mask (car fields))))
              (when (/= (dchunk-and mask field-mask) dchunk-zero)
                (pd-error "The field ~S in arg ~S overlaps some other field."
                          (car fields)
                          (arg-name arg)))
              (dchunk-insertf id (car fields) (car values))
              (dchunk-orf mask field-mask))))))))

;;;; Utilities for converting a printer specification into lisp code.

;;;; a simple function that helps avoid consing when we're just
;;;; recursively filtering things that usually don't change
(defun sharing-mapcar (fun list)
  (declare (type function fun))
  "A simple (one list arg) mapcar that avoids consing up a new list
  as long as the results of calling FUN on the elements of LIST are
  eq to the original."
  (when list
    (recons list
            (funcall fun (car list))
            (sharing-mapcar fun (cdr list)))))

;;; Return the first non-keyword symbol in a depth-first search of TREE.
(defun find-first-field-name (tree)
  (cond ((null tree)
         nil)
        ((and (symbolp tree) (not (keywordp tree)))
         tree)
        ((atom tree)
         nil)
        ((eq (car tree) 'quote)
         nil)
        (t
         (or (find-first-field-name (car tree))
             (find-first-field-name (cdr tree))))))

(defun compare-fields-form (val-form-1 val-form-2)
  (flet ((listify-fields (fields)
           (cond ((symbolp fields) fields)
                 ((every #'constantp fields) `',fields)
                 (t `(list ,@fields)))))
    (cond ((or (symbolp val-form-1) (symbolp val-form-2))
           `(equal ,(listify-fields val-form-1)
                   ,(listify-fields val-form-2)))
          (t
           `(and ,@(mapcar (lambda (v1 v2) `(= ,v1 ,v2))
                           val-form-1 val-form-2))))))

(defun compile-test (subj test funstate)
  (when (and (consp test) (symbolp (car test)) (not (keywordp (car test))))
    (setf subj (car test)
          test (cdr test)))
  (let ((key (if (consp test) (car test) test))
        (body (if (consp test) (cdr test) nil)))
    (cond ((null key)
           nil)
          ((eq key t)
           t)
          ((eq key :constant)
           (let* ((arg (arg-or-lose subj funstate))
                  (fields (arg-fields arg))
                  (consts body))
             (when (not (= (length fields) (length consts)))
               (pd-error "The number of constants doesn't match number of ~
                          fields in: (~S :constant~{ ~S~})"
                         subj body))
             (compare-fields-form (gen-arg-forms arg :numeric funstate)
                                  consts)))
          ((eq key :positive)
           `(> ,(arg-value-form (arg-or-lose subj funstate) funstate :numeric)
               0))
          ((eq key :negative)
           `(< ,(arg-value-form (arg-or-lose subj funstate) funstate :numeric)
               0))
          ((eq key :test)
           `(,@body ,(arg-value-form (arg-or-lose subj funstate) funstate :numeric)))
          ((eq key :same-as)
           (let ((arg1 (arg-or-lose subj funstate))
                 (arg2 (arg-or-lose (car body) funstate)))
             (unless (and (= (length (arg-fields arg1))
                             (length (arg-fields arg2)))
                          (every (lambda (bs1 bs2)
                                   (= (byte-size bs1) (byte-size bs2)))
                                 (arg-fields arg1)
                                 (arg-fields arg2)))
               (pd-error "can't compare differently sized fields: ~
                          (~S :same-as ~S)" subj (car body)))
             (compare-fields-form (gen-arg-forms arg1 :numeric funstate)
                                  (gen-arg-forms arg2 :numeric funstate))))
          ((eq key :or)
           `(or ,@(mapcar (lambda (sub) (compile-test subj sub funstate))
                          body)))
          ((eq key :and)
           `(and ,@(mapcar (lambda (sub) (compile-test subj sub funstate))
                           body)))
          ((eq key :not)
           `(not ,(compile-test subj (car body) funstate)))
          ((and (consp key) (null body))
           (compile-test subj key funstate))
          (t
           (pd-error "bogus test-form: ~S" test)))))

(defun compile-print (arg-name funstate &optional printer)
  (let* ((arg (arg-or-lose arg-name funstate))
         (printer (or printer (arg-printer arg))))
    (etypecase printer
      (string
       `(local-format-arg ,(arg-value-form arg funstate) ,printer))
      (vector
       `(local-princ (aref ,printer ,(arg-value-form arg funstate :numeric))))
      ((or function (cons (eql function)))
       `(local-call-arg-printer ,(arg-value-form arg funstate) ,printer))
      (boolean
       `(,(if (arg-use-label arg) 'local-princ16 'local-princ)
         ,(arg-value-form arg funstate))))))

(defun compile-printer-body (source funstate)
  (cond ((null source)
         nil)
        ((eq source :name)
         `(local-print-name))
        ((eq source :tab)
         `(local-tab-to-arg-column))
        ((keywordp source)
         (pd-error "unknown printer element: ~S" source))
        ((symbolp source)
         (compile-print source funstate))
        ((atom source)
         ;; Skip the argument separator if disassembling into a model.
         ;; The ", " string is quite consistent amongst backends.
         ;; Making this any more general - such as never printing strings
         ;; into a model - would be prone to information loss.
         (if (and (stringp source) (string= source ", "))
             `(local-print-arg-separator)
             `(local-princ ',source)))
        ((eq (car source) 'quote)
         `(,(if (symbolp (cadr source)) 'local-princ-symbol 'local-princ) ,source))
        ((eq (car source) :using)
         (let ((f (cadr source)))
          (unless (typep f '(or string (cons (eql function) (cons symbol null))))
            (pd-error "The first arg to :USING must be a string or #'function."))
          (compile-print (caddr source) funstate f)))
        ((eq (car source) :plus-integer)
         ;; prints the given field proceed with a + or a -
         (let ((form
                (arg-value-form (arg-or-lose (cadr source) funstate)
                                funstate
                                :numeric)))
           `(progn
              (when (>= ,form 0)
                (local-write-char #\+))
              (local-princ ,form))))
        ((eq (car source) 'function)
         `(local-call-global-printer ,source))
        ((eq (car source) :cond)
         `(cond ,@(mapcar (lambda (clause)
                            `(,(compile-test (find-first-field-name
                                              (cdr clause))
                                             (car clause)
                                             funstate)
                              ,@(compile-printer-list (cdr clause)
                                                      funstate)))
                          (cdr source))))
        ;; :IF, :UNLESS, and :WHEN are replaced by :COND during preprocessing
        (t
         `(progn ,@(compile-printer-list source funstate)))))

(defun compile-printer-list (sources funstate)
  (when sources
    (cons (compile-printer-body (car sources) funstate)
          (compile-printer-list (cdr sources) funstate))))

(defun all-arg-refs-relevant-p (printer args)
  (cond ((or (null printer) (keywordp printer) (eq printer t))
         t)
        ((symbolp printer)
         (find printer args :key #'arg-name))
        ((listp printer)
         (every (lambda (x) (all-arg-refs-relevant-p x args))
                printer))
        (t t)))

(defun pick-printer-choice (choices args)
  (dolist (choice choices
           (pd-error "no suitable choice found in ~S" choices))
    (when (all-arg-refs-relevant-p choice args)
      (return choice))))

(defun preprocess-chooses (printer args)
  (cond ((atom printer)
         printer)
        ((eq (car printer) :choose)
         (pick-printer-choice (cdr printer) args))
        (t
         (sharing-mapcar (lambda (sub) (preprocess-chooses sub args))
                         printer))))

(defun preprocess-test (subj form args)
  (multiple-value-bind (subj test)
      (if (and (consp form) (symbolp (car form)) (not (keywordp (car form))))
          (values (car form) (cdr form))
          (values subj form))
    (let ((key (if (consp test) (car test) test))
          (body (if (consp test) (cdr test) nil)))
      (case key
        (:constant
         (if (null body)
             ;; If no supplied constant values, just any constant is ok,
             ;; just see whether there's some constant value in the arg.
             (not
              (null
               (arg-value
                (or (find subj args :key #'arg-name)
                    (pd-error "unknown argument ~S" subj)))))
             ;; Otherwise, defer to run-time.
             form))
        ((:or :and :not)
         (recons
          form
          subj
          (recons
           test
           key
           (sharing-mapcar
            (lambda (sub-test)
              (preprocess-test subj sub-test args))
            body))))
        (t form)))))

(defun preprocess-conditionals (printer args)
  (if (atom printer)
      printer
      (case (car printer)
        (:unless
         (preprocess-conditionals
          `(:cond ((:not ,(nth 1 printer)) ,@(nthcdr 2 printer)))
          args))
        (:when
         (preprocess-conditionals `(:cond (,(cdr printer))) args))
        (:if
         (preprocess-conditionals
          `(:cond (,(nth 1 printer) ,(nth 2 printer))
                  (t ,(nth 3 printer)))
          args))
        (:cond
         (recons
          printer
          :cond
          (sharing-mapcar
           (lambda (clause)
             (let ((filtered-body
                    (sharing-mapcar
                     (lambda (sub-printer)
                       (preprocess-conditionals sub-printer args))
                     (cdr clause))))
               (recons
                clause
                (preprocess-test (find-first-field-name filtered-body)
                                 (car clause)
                                 args)
                filtered-body)))
           (cdr printer))))
        (quote printer)
        (t
         (sharing-mapcar
          (lambda (sub-printer)
            (preprocess-conditionals sub-printer args))
          printer)))))

;;; Return a version of the disassembly-template PRINTER with
;;; compile-time tests (e.g. :constant without a value), and any
;;; :CHOOSE operators resolved properly for the args ARGS.
;;;
;;; (:CHOOSE Sub*) simply returns the first Sub in which every field
;;; reference refers to a valid arg.
(defun preprocess-printer (printer args)
  (preprocess-conditionals (preprocess-chooses printer args) args))

;;; "Compile" a printer which means first turning the abstract syntax
;;; into a lambda expression that performs various output operations,
;;; and then compiling the lambda.
(defun find-printer-fun (printer-source args cache *current-instruction-flavor*)
  (let* ((source (preprocess-printer printer-source args))
         (funstate (make-funstate args))
         (forms (let ((*!temp-var-counter* 0))
                  (compile-printer-list source funstate)))
         (bindings (make-arg-temp-bindings funstate))
         (guts `(let* ,bindings ,@forms))
         (sub-table (assq :printer cache)))
    (or (cdr (assoc guts (cdr sub-table) :test #'equal))
        (let ((template
     `(named-lambda (inst-printer ,@*current-instruction-flavor*)
        (chunk inst stream dstate
               &aux (chunk (truly-the dchunk chunk))
                    (inst (truly-the instruction inst))
                    (stream (truly-the (or null stream) stream))
                    (dstate (truly-the disassem-state dstate)))
       (macrolet ((local-format-arg (arg fmt)
                    `(funcall (formatter ,fmt) stream ,arg))
                  (local-call-arg-printer (arg fun)
                    ;; If FUN is literally #<FUNCTION THING>, try to turn it back to
                    ;; its name so that we can indirect through an FDEFN, thus allowing
                    ;; redefinition of THING when debugging the disassembler.
                    ;; I looked at how this worked in CMUCL, and it also used to wire
                    ;; in the function object. That was a bug, not a feature, imho.
                    ;; The proper solution is probably to expand the defining macros
                    ;; differently. But some esoteric use might rely on lexical capture.
                    ;; I hope not, though this technique doesn't affect such a use.
                    ;; Another possible solution is to turn ARG-TYPEs into first-class
                    ;; objects, storing function in that object, and always calling
                    ;; the function in it (and allowing redefinition of ARG-TYPEs).
                    ;; That's tricky because the relation between an arg an its type
                    ;; is lost after parsing,
                    ;; because the type is purely a logical construct.
                    (if (and (simple-fun-p fun)
                             (symbolp (%simple-fun-name fun))
                             (eq (symbol-package (%simple-fun-name fun))
                                 sb-assem::*backend-instruction-set-package*))
                        `(,(%simple-fun-name fun) ,arg stream dstate)
                        `(funcall ,fun ,arg stream dstate))))
         ;; All the LOCAL-foo functions print nothing if stream is nil
         (flet ((local-tab-to-arg-column ()
                  (tab (dstate-argument-column dstate) stream))
                (local-print-name ()
                  (when stream (princ (inst-print-name inst) stream)))
                (local-print-arg-separator ()
                  (when stream (princ ", " stream)))
                (local-write-char (ch)
                  (when stream (write-char ch stream)))
                (local-princ-symbol (thing)
                  ;; This special case is reqired so that decoding x86 JMP and CMOV
                  ;; to an instruction model does not insert a superfluous symbol.
                  ;; I hope this is right. We really only want to elide symbols
                  ;; that are not inside a :COND or other selector.
                  (when stream (princ thing stream)))
                (local-princ (thing)
                  (if stream
                      (princ thing stream)
                      (operand thing dstate)))
                (local-princ16 (thing)
                  (if stream
                      (princ16 thing stream)
                      (operand thing dstate)))
                (local-call-global-printer (fun)
                  (funcall fun chunk inst stream dstate))
                (local-filtered-value (offset)
                  (declare (type filtered-value-index offset))
                  (aref (dstate-filtered-values dstate) offset))
                (local-extract (bytespec)
                  (dchunk-extract chunk bytespec))
                (lookup-label (lab)
                  (or (gethash lab (dstate-label-hash dstate))
                      lab))
                (adjust-label (val adjust-fun)
                  (funcall adjust-fun val dstate)))
           (declare (ignorable #'local-tab-to-arg-column
                               #'local-print-name #'local-print-arg-separator
                               #'local-princ #'local-princ16 #'local-princ-symbol
                               #'local-write-char
                               #'local-call-global-printer
                               #'local-extract
                               #'local-filtered-value
                               #'lookup-label #'adjust-label)
                    (inline local-tab-to-arg-column
                            local-print-arg-separator
                            local-princ local-princ16 local-princ-symbol
                            local-call-global-printer
                            local-filtered-value local-extract
                            lookup-label adjust-label))
           :body)))))
          (cdar (push (cons guts (compile nil (subst guts :body template)))
                      (cdr sub-table)))))))

(defun collect-inst-variants (base-name package variants cache)
  (loop for printer in variants
        for index from 1
        collect
     (destructuring-bind (format-name
                          (&rest arg-constraints)
                          &optional (printer :default)
                          &key (print-name (intern (string-upcase base-name) package))
                               control)
         printer
       (declare (type (or symbol string) print-name))
       (let* ((format (format-or-lose format-name))
              (args (copy-list (format-args format)))
              (format-length (bytes-to-bits (format-length format))))
         (dolist (constraint arg-constraints)
           (destructuring-bind (name . props) constraint
             (let ((cell (member name args :key #'arg-name))
                   (arg))
               (if cell
                   (setf (car cell) (setf arg (copy-structure (car cell))))
                   (setf args (nconc args (list (setf arg (%make-arg name))))))
               (apply #'modify-arg
                      arg format-length (and props (cons :value props))))))
         (multiple-value-bind (mask id) (compute-mask-id args)
           (make-instruction
                   base-name format-name print-name
                   (format-length format) mask id
                   (awhen (if (eq printer :default)
                              (format-default-printer format)
                              printer)
                     (find-printer-fun it args cache (list base-name index)))
                   (collect-labelish-operands args cache)
                   (compute-prefilter args cache)
                   control))))))

(defun !compile-inst-printers ()
  (let ((package sb-assem::*backend-instruction-set-package*)
        (cache (list (list :printer) (list :prefilter) (list :labeller))))
    (do-symbols (symbol package)
      (awhen (get symbol 'instruction-flavors)
        (setf (get symbol 'instructions)
              (collect-inst-variants symbol package it cache))))
    (unless (sb-impl::!c-runtime-noinform-p)
      (format t "~&Disassembler: ~{~D printers, ~D prefilters, ~D labelers~}~%"
              (mapcar (lambda (x) (length (cdr x))) cache)))))

;;; Get the instruction-space, creating it if necessary.
(defun get-inst-space (&key (package sb-assem::*backend-instruction-set-package*)
                            force)
  (let ((ispace *disassem-inst-space*))
    (when (or force (null ispace))
      (let ((insts nil))
        (do-symbols (symbol package)
          (setq insts (nconc (copy-list (get symbol 'instructions)) insts)))
        (setf ispace (sb-vm:without-arena "disassem" (build-inst-space insts))))
      (setf *disassem-inst-space* ispace))
    ispace))

(defun set-location-printing-range (dstate from length)
  (setf (dstate-addr-print-len dstate) ; in characters
        ;; 4 bits per hex digit
        (ceiling (integer-length (logxor from (+ from length))) 4)))

(defconstant label-column-width 7)

;;; Print the current address in DSTATE to STREAM, plus any labels that
;;; correspond to it, and leave the cursor in the instruction column.
(defun print-current-address (stream dstate)
  (declare (type stream stream)
           (type disassem-state dstate))
  (let* ((location (dstate-cur-addr dstate))
         (location-column-width *disassem-location-column-width*)
         (plen ; the number of rightmost hex chars of this address to print
          (or (dstate-addr-print-len dstate)
              ;; Usually we've already set the width, but in case not...
              (let ((seg (dstate-segment dstate)))
                (set-location-printing-range
                 dstate (seg-virtual-location seg) (seg-length seg))))))

    (if (eq (dstate-output-state dstate) :beginning) ; on the first line
        (if location-column-width
            ;; If there's a user-specified width, force that number of hex chars
            ;; regardless of whether it's greater or smaller than PLEN.
            (setq plen location-column-width)
            ;; No specified width. The PLEN of this line becomes the width.
            ;; Adjust the DSTATE's argument column for it.
            (incf (dstate-argument-column dstate)
                  (setq location-column-width plen)))
        ;; not the first line
        (if location-column-width
            ;; A specified width smaller than that required clips significant
            ;; digits, but larger should not cause leading zeros to appear.
            (setq plen (min plen location-column-width))
            ;; Otherwise use the previously computed addr-print-len
            (setq location-column-width plen)))

    (incf location-column-width 2) ; account for leading "; "
    (fresh-line stream)
    (princ "; " stream)

    ;; print the location
    ;; [this is equivalent to (format stream "~V,'0x:" plen printed-value), but
    ;;  usually avoids any consing]
    ;; FIXME: if this cruft is actually a speed win, the format-string compiler
    ;; should be improved to obviate the obfuscation. If it is not a win,
    ;; we should just replace it with the above format string already.
    (tab0 (- location-column-width plen) stream)
    (let* ((printed-bits (* 4 plen))
           (printed-value (ldb (byte printed-bits 0) location))
           (leading-zeros
            (truncate (- printed-bits (integer-length printed-value)) 4)))
      (dotimes (i leading-zeros)
        (write-char #\0 stream))
      (unless (zerop printed-value)
        (write printed-value :stream stream :base 16 :radix nil))
      (unless (zerop plen)
        (write-char #\: stream)))

    ;; print any labels
    (loop
      (let* ((next-label (car (dstate-cur-labels dstate)))
             (label-location (car next-label)))
        (when (or (null label-location) (> label-location location))
          (return))
        (unless (< label-location location)
          (format stream " L~W:" (cdr next-label)))
        (pop (dstate-cur-labels dstate))))

    ;; move to the instruction column
    (tab0 (+ location-column-width 1 label-column-width) stream)
    ))

(macrolet ((with-print-restrictions (&rest body)
             `(let ((*print-pretty* t)
                    ;; Truncating end-of-line notes is not very informative, certainly
                    ;; now that so many FDEFNs have compound names like
                    ;;  #<SB-KERNEL:FDEFN (SB-IMPL::SPECIALIZED-XEP
                    ;;                     F ..))
                    ;; hence the extremely generous overriding value for right-margin.
                    (*print-right-margin* 200)
                    (*print-lines* 2)
                    (*print-length* 4)
                    (*print-level* 4))
                ,@body)))

;;; Print a newline to STREAM, inserting any pending notes in DSTATE
;;; as end-of-line comments. If there is more than one note, a
;;; separate line will be used for each one.
(defun print-notes-and-newline (stream dstate)
  (declare (type stream stream)
           (type disassem-state dstate))
  (flet ((print-note (note)
           (format stream "~Vt " *disassem-note-column*)
           (pprint-logical-block (stream nil :per-line-prefix "; ")
             (etypecase note
               (string
                (write-string note stream))
               (function
                (funcall note stream))))
           (terpri stream)))
    (let ((notes (dstate-notes dstate)))
      (when notes
        (with-print-restrictions
            (if (listp notes)
                (dolist (note (dstate-notes dstate))
                  (print-note note))
                (print-note notes)))
        (setf (dstate-notes dstate) nil))))
  (fresh-line stream))

(defun prin1-short (thing stream)
  (with-print-restrictions
    (prin1 thing stream)))
) ; end MACROLET

;;; Print NUM instruction bytes to STREAM as hex values.
(defun print-inst (num stream dstate &key (offset 0) (trailing-space t))
  (when (> *disassem-inst-column-width* 0)
    (let ((sap (dstate-segment-sap dstate))
          (start-offs (+ offset (dstate-cur-offs dstate))))
      (dotimes (offs num)
        (format stream "~2,'0x" (sap-ref-8 sap (+ offs start-offs))))
      (when trailing-space
        (pad-inst-column stream num)))))

;;; Disassemble NUM bytes to STREAM as simple `BYTE' instructions.
(defun print-bytes (num stream dstate)
  (declare (type offset num)
           (type stream stream)
           (type disassem-state dstate))
  (format stream "~A~Vt" 'BYTE (dstate-argument-column dstate))
  (let ((sap (dstate-segment-sap dstate))
        (start-offs (dstate-cur-offs dstate)))
    (dotimes (offs num)
      (unless (zerop offs)
        (write-string ", " stream))
      (format stream "#X~2,'0x" (sap-ref-8 sap (+ offs start-offs))))))

(defvar *default-dstate-hooks*
  (list* #-(or x86 x86-64 arm64 riscv) #'lra-hook nil))

;;; Make a disassembler-state object.
(defun make-dstate (&optional (fun-hooks *default-dstate-hooks*))
  (declare (inline %make-dstate))
  (let ((alignment sb-assem:+inst-alignment-bytes+)
        (arg-column
         (+ 2 ; for the leading "; " on each line
            (or *disassem-location-column-width* 0)
            1
            label-column-width
            *disassem-inst-column-width*
            (if (zerop *disassem-inst-column-width*) 0 1)
            *disassem-opcode-column-width*)))

    (when (> alignment 1)
      (push #'alignment-hook fun-hooks))

    (%make-dstate alignment arg-column fun-hooks)))

;;; Logically or MASK into the set of instruction properties in DSTATE.
(defun dstate-setprop (dstate mask)
  (setf (dstate-inst-properties dstate) (logior mask (dstate-inst-properties dstate))))

;;; Return non-NIL if any bit in MASK
;;; is in the set of instruction properties in DSTATE.
(defun dstate-getprop (dstate mask)
  (logtest mask (dstate-inst-properties dstate)))

(defun add-fun-header-hooks (segment)
  (declare (type segment segment))
  (dotimes (i (code-n-entries (seg-code segment)))
    (let* ((fun (%code-entry-point (seg-code segment) i))
           (length (seg-length segment))
           (code-offs (%fun-code-offset fun))
           (offset (code-offs-to-segment-offs code-offs segment)))
      (when (<= 0 offset length)
        ;; Up to 2 words (less a byte) of padding might be present to align the
        ;; next simple-fun. Limit on OFFSET is to avoid incorrect triggering
        ;; in case of unexpected weirdness.
        (when (< 0 offset (* sb-vm:n-word-bytes 2))
          (push (make-offs-hook
                 :fun (lambda (stream dstate)
                         (when stream
                           (format stream ".SKIP ~D" offset))
                          (incf (dstate-next-offs dstate) offset))
                 :offset 0) ; at 0 bytes into this seg, skip OFFSET bytes
                (seg-hooks segment)))
        (unless (minusp offset)
          (push (make-offs-hook
                 :offset offset
                 :fun (let ((i i)) ; capture the _current_ I, not the final value
                        (lambda (stream dstate) (fun-header-hook i stream dstate))))
                (seg-hooks segment)))))))

;;; A SAP-MAKER is a no-argument function that returns a SAP.

(declaim (inline sap-maker))
(defun sap-maker (function input offset)
  (declare (optimize (speed 3))
           (muffle-conditions compiler-note)
           (type (function (t) system-area-pointer) function)
           (type offset offset))
  (let ((old-sap (sap+ (funcall function input) offset)))
    (declare (type system-area-pointer old-sap))
    (lambda ()
      (let ((new-addr
             (+ (sap-int (funcall function input)) offset)))
        ;; Saving the sap like this avoids consing except when the sap
        ;; changes (because the sap-int, arith, etc., get inlined).
        (declare (type address new-addr))
        (if (= (sap-int old-sap) new-addr)
            old-sap
            (setf old-sap (int-sap new-addr)))))))

(defun vector-sap-maker (vector offset)
  (declare (optimize (speed 3))
           (type offset offset))
  (sap-maker #'vector-sap vector offset))

(defun code-sap-maker (code offset)
  (declare (optimize (speed 3))
           (type code-component code)
           (type offset offset))
  (sap-maker #'code-instructions code offset))

(defun memory-sap-maker (address)
  (declare (optimize (speed 3))
           (muffle-conditions compiler-note)
           (type address address))
  (let ((sap (int-sap address)))
    (lambda () sap)))

(defstruct (source-form-cache (:conc-name sfcache-)
                              (:copier nil))
  (debug-source nil :type (or null sb-di:debug-source))
  (toplevel-form-index -1 :type fixnum)
  (last-location-retrieved nil :type (or null sb-di:code-location))
  (last-form-retrieved -1 :type fixnum))

;;; Return a memory segment located at the system-area-pointer returned by
;;; SAP-MAKER and LENGTH bytes long in the disassem-state object DSTATE.
;;; OBJECT is the object to pin (possibly NIL) when calling the SAP-MAKER.
;;; INITIAL-RAW-BYTES is the number of leading bytes of the segment
;;; that are not machine instructions.

;;; &KEY arguments include :VIRTUAL-LOCATION (by default the same as
;;; the address), :DEBUG-FUN, :SOURCE-FORM-CACHE (a
;;; SOURCE-FORM-CACHE object), and :HOOKS (a list of OFFS-HOOK
;;; objects).
;;; INITIAL-OFFSET is the displacement into the instruction bytes
;;; of CODE (if supplied) that the segment begins at.
;;;
;;; Technically we need to pin OBJECT around all of calls of MAKE-SEGMENT
;;; with that same object. Otherwise, the VIRTUAL-LOCATION slots could come
;;; out inconsistently across them. It's unlikely to happen, but if it did,
;;; that would tend to lead to buggy disassemblies due to subtlety of
;;; absolute addressing in MAP-SEGMENT-INSTRUCTIONS that supposedly hides
;;; the movability of the underlying object.
(defun make-segment (object sap-maker length
                     &key
                     code (initial-offset 0) virtual-location
                     debug-fun source-form-cache
                     hooks)
  (declare (type (function () system-area-pointer) sap-maker)
           (type disassem-length length)
           (type (or null address) virtual-location)
           (type (or null sb-di:debug-fun) debug-fun)
           (type (or null source-form-cache) source-form-cache))
  (let ((segment
         (%make-segment
           :object object
           :sap-maker sap-maker
           :length length
           :virtual-location (or virtual-location
                                 (sap-int (funcall sap-maker)))
           :hooks hooks
           :code code
           :initial-offset initial-offset ; an offset into CODE
           :debug-fun debug-fun)))
    (add-debugging-hooks segment debug-fun source-form-cache)
    (when code
      (add-fun-header-hooks segment))
    segment))

(defun make-vector-segment (vector offset &rest args)
  (declare (type vector vector)
           (type offset offset))
  (apply #'make-segment vector (vector-sap-maker vector offset) args))

(defun make-code-segment (code offset length &rest args)
  (declare (type code-component code)
           (type offset offset))
  (apply #'make-segment code
         (code-sap-maker code offset) length
         ;; For displaying PCs as if the code object's instruction area
         ;; had an origin address of 0, uncomment this next line:
         ;; :virtual-location offset
         :code code :initial-offset offset args))

(defun make-memory-segment (code address &rest args)
  (declare (type address address))
  (apply #'make-segment code (memory-sap-maker address) args))

;;; just for fun
(defun print-fun-headers (function)
  (declare (type compiled-function function))
  (let* ((self (%fun-fun function))
         (code (fun-code-header self)))
    (format t "Code-header ~S: size: ~S~%" code (%code-code-size code))
    (loop for i below (code-n-entries code)
          for fun = (%code-entry-point code i)
       do
        ;; There is function header fun-offset words from the
        ;; code header.
      (format t "Fun-header ~S at offset #x~X (bytes):~% ~S ~A => ~S~%"
              fun
              (%fun-code-offset fun)
              (%simple-fun-name fun)
              (%simple-fun-arglist fun)
              (%simple-fun-type fun)))))

;;; getting at the source code...

(defun get-different-source-form (loc context &optional cache)
  (if (and cache
           (eq (sb-di:code-location-debug-source loc)
               (sfcache-debug-source cache))
           (eq (sb-di:code-location-toplevel-form-offset loc)
               (sfcache-toplevel-form-index cache))
           (or (eql (sb-di:code-location-form-number loc)
                    (sfcache-last-form-retrieved cache))
               (awhen (sfcache-last-location-retrieved cache)
                 (sb-di:code-location= loc it))))
      (values nil nil)
      (let ((form (sb-debug::code-location-source-form loc context nil)))
        (when cache
          (setf (sfcache-debug-source cache)
                (sb-di:code-location-debug-source loc))
          (setf (sfcache-toplevel-form-index cache)
                (sb-di:code-location-toplevel-form-offset loc))
          (setf (sfcache-last-form-retrieved cache)
                (sb-di:code-location-form-number loc))
          (setf (sfcache-last-location-retrieved cache) loc))
        (values form t))))

;;;; stuff to use debugging info to augment the disassembly

(defun code-fun-map (code)
  (declare (type code-component code))
  (sb-di::get-debug-info-fun-map (%code-debug-info code)))

;;; Assuming that CODE-OBJ is pinned, return true if ADDR is anywhere
;;; between the tagged pointer and the first occuring simple-fun.
(defun points-to-code-constant-p (addr code-obj)
  (declare (type word addr) (type code-component code-obj))
  (<= (get-lisp-obj-address code-obj)
      addr
      (get-lisp-obj-address (%code-entry-point code-obj 0))))

(defstruct (location-group (:copier nil) (:predicate nil))
  ;; This was (VECTOR (OR LIST FIXNUM)) but that doesn't have any
  ;; specialization other than T, and the cross-compiler has trouble
  ;; with (SB-XC:TYPEP #() '(VECTOR (OR LIST FIXNUM)))
  (locations #() :type simple-vector))

;;; Return the vector of DEBUG-VARs currently associated with DSTATE.
(defun dstate-debug-vars (dstate)
  (declare (type disassem-state dstate))
  (storage-info-debug-vars (seg-storage-info (dstate-segment dstate))))

;;; Given the OFFSET of a location within the location-group called
;;; LG-NAME, see whether there's a current mapping to a source
;;; variable in DSTATE, and if so, return the offset of that variable
;;; in the current debug-var vector.
(defun find-valid-storage-location (offset lg-name dstate)
  (declare (type offset offset)
           (type symbol lg-name)
           (type disassem-state dstate))
  (let* ((storage-info
          (seg-storage-info (dstate-segment dstate)))
         (location-group
          (and storage-info
               (cdr (assoc lg-name (storage-info-groups storage-info)))))
         (currently-valid
          (dstate-current-valid-locations dstate)))
    (and location-group
         (not (null currently-valid))
         (let ((locations (location-group-locations location-group)))
           (and (< offset (length locations))
                (let ((used-by (aref locations offset)))
                  (and used-by
                       (let ((debug-var-num
                              (typecase used-by
                                (fixnum
                                 (and (not
                                       (zerop (bit currently-valid used-by)))
                                      used-by))
                                (list
                                 (some (lambda (num)
                                         (and (not
                                               (zerop
                                                (bit currently-valid num)))
                                              num))
                                       used-by)))))
                         (and debug-var-num
                              (progn
                                ;; Found a valid storage reference!
                                ;; can't use it again until it's revalidated...
                                (setf (bit (dstate-current-valid-locations
                                            dstate)
                                           debug-var-num)
                                      0)
                                debug-var-num))
                         ))))))))

;;; Return a STORAGE-INFO struction describing the object-to-source
;;; variable mappings from DEBUG-FUN.
(defun storage-info-for-debug-fun (debug-fun)
  (declare (type sb-di:debug-fun debug-fun)
           (inline make-storage-info))
  (let ((sc-vec sb-c:*backend-sc-numbers*)
        (groups nil)
        (debug-vars (sb-di::debug-fun-debug-vars debug-fun)))
    (and debug-vars
         (dotimes (debug-var-offset
                   (length debug-vars)
                   (make-storage-info :groups groups
                                      :debug-vars debug-vars))
           (let ((debug-var (aref debug-vars debug-var-offset)))
             #+nil
             (format t ";;; At offset ~W: ~S~%" debug-var-offset debug-var)
             (let* ((sc+offset
                     (sb-di::compiled-debug-var-sc+offset debug-var))
                    (sb-name
                     (sb-c:sb-name
                      (sb-c:sc-sb (aref sc-vec
                                        (sb-c:sc+offset-scn sc+offset))))))
               #+nil
               (format t ";;; SET: ~S[~W]~%"
                       sb-name (sb-c:sc+offset-offset sc+offset))
               (unless (null sb-name)
                 (let ((group (cdr (assoc sb-name groups))))
                   (when (null group)
                     (setf group (make-location-group))
                     (push `(,sb-name . ,group) groups))
                   (let* ((locations (location-group-locations group))
                          (length (length locations))
                          (offset (sb-c:sc+offset-offset sc+offset)))
                     (when (>= offset length)
                       (setf locations (adjust-array locations
                                                     (max (* 2 length) (1+ offset))
                                                     :initial-element nil)
                             (location-group-locations group) locations))
                     (let ((already-there (aref locations offset)))
                       (cond ((null already-there)
                              (setf (aref locations offset) debug-var-offset))
                             ((eql already-there debug-var-offset))
                             (t
                              (if (listp already-there)
                                  (pushnew debug-var-offset
                                           (aref locations offset))
                                  (setf (aref locations offset)
                                        (list debug-var-offset
                                              already-there)))))
                       )))))))
         )))

(defun source-available-p (debug-fun)
  (handler-case
      (sb-di:do-debug-fun-blocks (block debug-fun)
        (declare (ignore block))
        (return t))
    (sb-di:no-debug-blocks () nil)))

(defun print-block-boundary (stream dstate)
  (let ((os (dstate-output-state dstate)))
    (when (not (eq os :beginning))
      (when (not (eq os :block-boundary))
        (terpri stream))
      (setf (dstate-output-state dstate)
            :block-boundary))))

;;; Add hooks to track the source code in SEGMENT during disassembly.
;;; SFCACHE can be either NIL or it can be a SOURCE-FORM-CACHE
;;; structure, in which case it is used to cache forms from files.
(defun add-source-tracking-hooks (segment debug-fun &optional sfcache)
  (declare (type segment segment)
           (type (or null sb-di:debug-fun) debug-fun)
           (type (or null source-form-cache) sfcache))
  (let ((last-block-pc -1))
    (flet ((add-hook (pc fun &optional before-address)
             (push (make-offs-hook
                    :offset (code-insts-offs-to-segment-offs pc segment)
                    :fun fun
                    :before-address before-address)
                   (seg-hooks segment))))
      (handler-case
          (sb-di:do-debug-fun-blocks (block debug-fun)
            (let ((first-location-in-block-p t))
              (sb-di:do-debug-block-locations (loc block)
                (let ((pc (sb-di::compiled-code-location-pc loc)))

                  ;; Put blank lines in at block boundaries
                  (when (and first-location-in-block-p
                             (/= pc last-block-pc))
                    (setf first-location-in-block-p nil)
                    (add-hook pc
                              (lambda (stream dstate)
                                (print-block-boundary stream dstate))
                              t)
                    (setf last-block-pc pc))

                  ;; Print out corresponding source; this information is not
                  ;; all that accurate, but it's better than nothing
                  (unless (zerop (sb-di:code-location-form-number loc))
                    (multiple-value-bind (form new)
                        (get-different-source-form loc 0 sfcache)
                      (when new
                         (let ((at-block-begin (= pc last-block-pc)))
                           (add-hook
                            pc
                            (lambda (stream dstate)
                              (declare (ignore dstate))
                              (when stream
                                (unless at-block-begin
                                  (terpri stream))
                                (format stream ";;; [~W] "
                                        (sb-di:code-location-form-number
                                         loc))
                                (prin1-short form stream)
                                (terpri stream)
                                (terpri stream)))
                            t)))))

                  ;; Keep track of variable live-ness as best we can.
                  (let ((live-set
                         (copy-seq (sb-di::compiled-code-location-live-set
                                    loc))))
                    (add-hook
                     pc
                     (lambda (stream dstate)
                       (declare (ignore stream))
                       (setf (dstate-current-valid-locations dstate)
                             live-set)
                       #+nil
                       (note (lambda (stream)
                               (let ((*print-length* nil))
                                 (format stream "live set: ~S"
                                         live-set)))
                             dstate))))
                  ))))
        (sb-di:no-debug-blocks () nil)))))

;;; Disabled because it produces poor annotations, especially around
;;; macros.
(defvar *disassemble-annotate* nil
  "Annotate DISASSEMBLE output with source code.")

(defun add-debugging-hooks (segment debug-fun &optional sfcache)
  (when debug-fun
    (setf (seg-storage-info segment)
          (storage-info-for-debug-fun debug-fun))
    (when *disassemble-annotate*
      (add-source-tracking-hooks segment debug-fun sfcache))))


;;; Return a list of the segments of memory containing machine code
;;; instructions for FUNCTION.
(defun get-fun-segments (function)
  (declare (type compiled-function function))
  (let* ((function (%fun-fun function))
         (code (fun-code-header function))
         (fun-map (code-fun-map code))
         (sfcache (make-source-form-cache))
         (fun-start (sb-di::function-start-pc-offset function)))
    (let ((first-block-seen-p nil)
          (nil-block-seen-p nil)
          (last-offset 0)
          (last-debug-fun nil)
          (segments nil))
      (flet ((add-seg (offs len df)
               (when (> len 0)
                 (push (make-code-segment code offs len
                                          :debug-fun df
                                          :source-form-cache sfcache)
                       segments))))
        (dotimes (fmap-index (length fun-map))
          (let ((fmap-entry (aref fun-map fmap-index)))
            (etypecase fmap-entry
              (integer
               (when first-block-seen-p
                 (add-seg last-offset
                          (- fmap-entry last-offset)
                          last-debug-fun)
                 (setf last-debug-fun nil))
               (setf last-offset fmap-entry))
              (sb-c::compiled-debug-fun
               (let ((kind (sb-c::compiled-debug-fun-kind fmap-entry)))
                 #+nil
                 (format t ";;; SAW ~S ~S ~S,~S ~W,~W~%"
                         name kind first-block-seen-p nil-block-seen-p
                         last-offset
                         (sb-c::compiled-debug-fun-start-pc fmap-entry))
                 (cond ((and (>= last-offset fun-start)
                             (null kind)
                             (not first-block-seen-p))
                        (setf first-block-seen-p t))
                       ((eq kind :external)
                        (when first-block-seen-p
                          (return)))
                       ((eq kind nil)
                        (let ((name (sb-c::compiled-debug-fun-name fmap-entry)))
                          (cond ((and (typep name '(cons (eql sb-impl::specialized-xep)))
                                      (eq (%fun-name function)
                                          (second name)))
                                 (setf first-block-seen-p t))
                                (nil-block-seen-p
                                 (return))
                                (first-block-seen-p
                                 (setf nil-block-seen-p t))))))
                 (setf last-debug-fun
                       (sb-di::make-compiled-debug-fun fmap-entry code)))))))
        (let ((max-offset (%code-text-size code)))
          (when (and first-block-seen-p last-debug-fun)
            (add-seg last-offset
                     (- max-offset last-offset)
                     last-debug-fun))
          (if (null segments) ; FIXME: when does this happen? Comment PLEASE
              (let ((offs (sb-di::function-start-pc-offset function)))
                (list
                 (make-code-segment code offs (- max-offset offs))))
              (nreverse segments)))))))

;;; Return a list of the segments of memory containing machine code
;;; instructions for the code-component CODE. If START-OFFSET and/or
;;; LENGTH is supplied, only that part of the code-segment is used
;;; (but these are constrained to lie within the code-segment).
(defun get-code-segments (code
                          &optional
                          (start-offset 0)
                          (length (%code-text-size code)))
  (declare (type code-component code)
           (type offset start-offset)
           (type disassem-length length))
  (unless (sb-c::compiled-debug-info-p (%code-debug-info code))
    (return-from get-code-segments
      (if (typep (%code-debug-info code) '(or hash-table (cons hash-table)))
          (collect ((segs))
            (dohash ((name locs) (sb-fasl::%asm-routine-table code))
              (destructuring-bind (start end . index) locs
                (declare (ignore index))
                (let ((seg (make-code-segment code start (- (1+ end) start))))
                  (push (make-offs-hook :offset 0
                                        :fun (lambda (stream dstate)
                                               (declare (ignore stream))
                                               (note (string name) dstate)))
                        (seg-hooks seg))
                  (segs seg))))
            (sort (segs) #'< :key #'seg-virtual-location))
          (list (make-code-segment code start-offset length)))))
  (let ((segments nil)
        (sfcache (make-source-form-cache))
        (last-offset (code-n-unboxed-data-bytes code))
        (last-debug-fun nil))
    (flet ((add-seg (offs len df)
             (let* ((restricted-offs
                     (min (max start-offset offs) (+ start-offset length)))
                    (restricted-len
                     (- (min (max start-offset (+ offs len))
                             (+ start-offset length))
                        restricted-offs)))
               (when (plusp restricted-len)
                 (push (make-code-segment code
                                          restricted-offs restricted-len
                                          :debug-fun df
                                          :source-form-cache sfcache)
                       segments)))))
      (dovector (fun-map-entry (code-fun-map code))
        (etypecase fun-map-entry
          (integer
           (add-seg last-offset (- fun-map-entry last-offset)
                    last-debug-fun)
           (setf last-debug-fun nil)
           (setf last-offset fun-map-entry))
          (sb-c::compiled-debug-fun
           (setf last-debug-fun
                 (sb-di::make-compiled-debug-fun fun-map-entry code)))))
      (when last-debug-fun
        (add-seg last-offset
                 (- (%code-text-size code) last-offset)
                 last-debug-fun)))
    (nreverse segments)))

;;; Compute labels for all the memory segments in SEGLIST and adds
;;; them to DSTATE. It's important to call this function with all the
;;; segments you're interested in, so that it can find references from
;;; one to another.
(defun label-segments (seglist dstate)
  (declare (type list seglist)
           (type disassem-state dstate))
  (dolist (seg seglist)
    (add-segment-labels seg dstate))
  ;; Now remove any labels that don't point anywhere in the segments
  ;; we have.
  (setf (dstate-labels dstate)
        (remove-if (lambda (lab)
                     (not
                      ;; Ok, this is bogus when you want to show the code
                      ;; as if the origin were 0 (perhaps to compare
                      ;; two disssemblies that should be the same).
                      ;; Maybe that's a another good reason to store
                      ;; labels as relativized.
                      (some (lambda (seg)
                              (let ((start (seg-virtual-location seg)))
                                (<= start
                                    (car lab)
                                    (+ start (seg-length seg)))))
                            seglist)))
                   (dstate-labels dstate))))

;;; Disassemble the machine code instructions in SEGMENT to STREAM.
(defun disassemble-segment (segment stream dstate)
  (declare (type segment segment)
           (type stream stream)
           (type disassem-state dstate))
  (let ((*print-pretty* nil) ; otherwise the pp conses hugely
        ;; million-element bit vectors are no fun to see
        (*print-vector-length*
         (min (if (integerp *print-vector-length*)
                  *print-vector-length*
                  most-positive-fixnum)
              100)))
    (number-labels dstate)
    (map-segment-instructions
     (lambda (chunk inst)
       (declare (type dchunk chunk) (type instruction inst))
       (awhen (inst-printer inst)
         (funcall it chunk inst stream dstate)
         (setf (dstate-previous-chunk dstate) chunk)))
     segment
     dstate
     stream)))

;;; Disassemble the machine code instructions in each memory segment
;;; in SEGMENTS in turn to STREAM. Return NIL.
(defun disassemble-segments (segments stream dstate)
  (declare (type list segments)
           (type stream stream)
           (type disassem-state dstate))
  (unless (null segments)
    (let ((n-segments (length segments))
          (first (car segments))
          (last (car (last segments))))
      (flet ((print-segment-name (segment)
               (let* ((debug-fun (seg-debug-fun segment))
                      (name (and debug-fun (sb-di:debug-fun-name debug-fun))))
                 (when name
                   (format stream " ~Vt " *disassem-note-column*)
                   (pprint-logical-block (stream nil :per-line-prefix "; ")
                     (case (sb-c::compiled-debug-fun-kind
                            (sb-di::compiled-debug-fun-compiler-debug-fun debug-fun))
                       (:external
                        (format stream "(XEP ~s)" name))
                       (:optional
                        (format stream "(&OPTIONAL ~s)" name))
                       (:more
                        (format stream "(&MORE ~s)" name))
                       (t (prin1 name stream))))))))
        ;; One origin per segment is printed. As with the per-line display,
        ;; the segment is thought of as immovable for rendering of addresses,
        ;; though in fact the disassembler transiently allows movement.
        (format stream "~&; Size: ~a bytes. Origin: #x~x~@[ (segment 1 of ~D)~]"
                (reduce #'+ segments :key #'seg-length)
                (seg-virtual-location first)
                (if (> n-segments 1) n-segments))
        (print-segment-name (first segments))
        (set-location-printing-range dstate
                                     (seg-virtual-location first)
                                     (- (+ (seg-virtual-location last)
                                           (seg-length last))
                                        (seg-virtual-location first)))
        (setf (dstate-output-state dstate) :beginning)
        (let ((i 0))
          (dolist (seg segments)
            (when (> (incf i) 1)
              (format stream "~&; Origin #x~x (segment ~D of ~D)"
                      (seg-virtual-location seg) i n-segments)
              (print-segment-name seg))
            (disassemble-segment seg stream dstate)))))))


;;;; top level functions

;;; Disassemble the machine code instructions for FUNCTION.
(defun disassemble-fun (fun &key
                            (stream *standard-output*)
                            (use-labels t))
  (declare (type compiled-function fun)
           (type stream stream)
           (type boolean use-labels))
  (let* ((dstate (make-dstate))
         (segments (get-fun-segments fun)))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)))

(defun get-compiled-funs (thing)
  (named-let recurse ((fun (cond ((legal-fun-name-p thing)
                                  (or (and (symbolp thing) (macro-function thing))
                                      (fdefinition thing)))
                                 ((sb-pcl::method-p thing)
                                  (sb-mop:method-function thing))
                                 (t thing))))
    (etypecase fun
      ((or (cons (member lambda named-lambda)) interpreted-function)
       (awhen (compile nil fun)
         (list it)))
      (sb-pcl::%method-function
       ;; user's code is in the fast-function
       (recurse (sb-pcl::%method-function-fast-function fun)))
      (funcallable-instance
       (list (%funcallable-instance-fun fun)))
      (function
       (list fun)))))

(defun disassemble (object &key (stream *standard-output*) (use-labels t))
  "Disassemble the compiled code associated with OBJECT, which can be a
  function, a lambda expression, or a symbol with a function definition. If
  it is not already compiled, the compiler is called to produce something to
  disassemble."
  (if (typep object 'code-component)
      (disassemble-code-component object :stream stream :use-labels use-labels)
      (flet ((disassemble1 (fun)
               (format stream "~&; disassembly for ~S" (%fun-name fun))
               (disassemble-fun fun
                                :stream stream
                                :use-labels use-labels)))
        (mapc #'disassemble1 (get-compiled-funs object))))
  nil)

;;; Disassembles the given area of memory starting at ADDRESS and
;;; LENGTH long. Note that if CODE-COMPONENT is NIL and this memory
;;; could move during a GC, you'd better disable it around the call to
;;; this function.
;;; FIXME: either remove CODE-COMPONENT from this interface or explain
;;; how it could be used. It doesn't make sense to pass in an ADDRESS
;;; unless CODE-COMPONENT was already pinned.
(defun disassemble-memory (address
                           length
                           &key
                           (stream *standard-output*)
                           code-component
                           (use-labels t))
  (declare (type (or address system-area-pointer) address)
           (type disassem-length length)
           (type stream stream)
           (type (or null code-component) code-component)
           (type boolean use-labels))
  (let* ((address
          (if (system-area-pointer-p address)
              (sap-int address)
              address))
         (dstate (make-dstate code-component))
         (segments
          (if code-component
              (let ((code-offs
                     (- address
                        (sap-int
                         (code-instructions code-component)))))
                (when (or (< code-offs 0)
                          ;; Allow displaying beyond code-text-size
                          ;; but not beyond code-code-size.
                          (> code-offs (%code-code-size code-component)))
                  (error "address ~X not in the code component ~S"
                         address code-component))
                (get-code-segments code-component code-offs length))
              (list (make-memory-segment code-component address length)))))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)))

;;; Disassemble the machine code instructions associated with
;;; CODE-COMPONENT (this may include multiple entry points).
(defun disassemble-code-component (thing &key (stream *standard-output*)
                                              (use-labels t))
  (declare (type stream stream)
           (type boolean use-labels))
  (let* ((code-component
          (etypecase thing
           (function (fun-code-header (%fun-fun thing)))
           (code-component thing)))
         (dstate (make-dstate))
         (segments (get-code-segments code-component)))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)
    (let ((n (code-jump-table-words code-component)))
      (when (> n 1)
        (let ((sap (code-instructions code-component)))
          (format stream "; Jump table (~d entries) @ ~X~%" (1- n)
                  (sap-int (sap+ sap sb-vm:n-word-bytes)))
          (dotimes (i (1- n))
            (let ((a (sap-ref-word sap (ash (1+ i) sb-vm:word-shift))))
              (format stream "; ~vt~v,'02x = ~a~%"
                      (+ label-column-width
                         (dstate-addr-print-len dstate)
                         3) ; i don't know what 3 means
                      (* 2 sb-vm:n-word-bytes)
                      a
                      (gethash a (dstate-label-hash dstate))))))))))

;;; This convenience function has two syntaxes depending on what OBJECT is:
;;;   (DIS OBJ &optional STREAM)
;;;   (DIS ADDR|SAP LENGTH &optional STREAM)
(defun sb-c:dis (object &optional length (stream *standard-output* streamp))
  (typecase object
   ((or address system-area-pointer)
    (aver length)
    (disassemble-memory object length :stream stream))
   (t
    (aver (not streamp))
    (when length
      (setq stream length))
    (dolist (thing (cond ((code-component-p object) (list object))
                         ((and (symbolp object) (special-operator-p object))
                          ;; What could it do- disassemble the interpreter?
                          (error "Can't disassemble a special operator"))
                         (t (get-compiled-funs object))))
      (when (code-component-p object)
        (let* ((base (- (get-lisp-obj-address object) sb-vm:other-pointer-lowtag))
               (insts (code-instructions object)))
        (format stream "~&; Base: ~x Data: ~x~%" base (sap-int insts))))
      (disassemble-code-component thing :stream stream)))))

;;;; code to disassemble assembler segments

;;; Disassemble the machine code instructions associated with
;;; BYTES (a vector of assembly-unit) between each of RANGES.
(defun disassemble-assem-segment (bytes ranges stream)
  (declare (type stream stream))
  (let* ((dstate (make-dstate))
         (disassem-segments
          (mapcar (lambda (range &aux (from (car range)) (to (cdr range)))
                    (make-vector-segment bytes from (- to from)
                                         :virtual-location
                                         (- from (caar ranges))))
                  ranges)))
    (label-segments disassem-segments dstate)
    (disassemble-segments disassem-segments stream dstate)))

;;; routines to find things in the Lisp environment

;;; an alist of (SYMBOL-SLOT-OFFSET . ACCESS-FUN-NAME) for slots
;;; in a symbol object that we know about
(define-load-time-global *grokked-symbol-slots*
  (sort (copy-list `((,sb-vm:symbol-value-slot . symbol-value)
                     (,sb-vm:symbol-info-slot . symbol-info)
                     (,sb-vm:symbol-name-slot . symbol-name)))
        #'<
        :key #'car))

;;; Given ADDRESS, try and figure out if which slot of which symbol is
;;; being referred to. Of course we can just give up, so it's not a
;;; big deal... Return two values, the symbol and the name of the
;;; access function of the slot.
(defun grok-symbol-slot-ref (address)
  (declare (type address address))
  (if (not (aligned-p address sb-vm:n-word-bytes))
      (values nil nil)
      (do ((slots-tail *grokked-symbol-slots* (cdr slots-tail)))
          ((null slots-tail)
           (values nil nil))
        (let* ((field (car slots-tail))
               (slot-offset (words-to-bytes (car field)))
               (maybe-symbol-addr (- address slot-offset))
               (maybe-symbol
                (make-lisp-obj (+ maybe-symbol-addr sb-vm:other-pointer-lowtag)
                               nil)))
          (when (symbolp maybe-symbol)
            (return (values maybe-symbol (cdr field))))))))

;;; Given a BYTE-OFFSET from NIL, try and figure out which slot of
;;; which symbol is being referred to. Of course we can just give up,
;;; so it's not a big deal... Return two values, the symbol and the
;;; access function.
(defun grok-nil-indexed-symbol-slot-ref (byte-offset)
  (declare (type offset byte-offset))
  (grok-symbol-slot-ref (+ sb-vm:nil-value byte-offset)))

(define-load-time-global *assembler-routines-by-addr* nil)

;;; Return the name of the primitive Lisp assembler routine that contains
;;; ADDRESS, or foreign symbol located at ADDRESS, or NIL if there isn't one.
;;; If found, and the answer is an assembler routine, also return the displacement
;;; from the start of the containing routine as a secondary value.
(defun find-assembler-routine (address &aux (addr->name *assembler-routines-by-addr*))
  (declare (type address address))
  (when (null addr->name)
    (setf addr->name (make-hash-table) *assembler-routines-by-addr* addr->name)
    (flet ((invert (name->addr addr-xform)
             (maphash (lambda (name address)
                        (setf (gethash (funcall addr-xform address) addr->name) name))
                      name->addr)))
      (let ((code sb-fasl:*assembler-routines*))
        (invert (sb-fasl::%asm-routine-table code)
                (lambda (x) (sap-int (sap+ (code-instructions code) (car x)))))))
    #-linkage-space
    (dovector (name sb-vm::+all-static-fdefns+)
      (let ((address (+ sb-vm:nil-value (sb-vm:static-fun-offset name))))
        (setf (gethash address addr->name) name)))
    ;; Not really a routine, but it uses the similar logic for annotations
    #+(and sb-safepoint (not x86-64))
    (setf (gethash (- sb-vm:static-space-start sb-vm:gc-safepoint-trap-offset) addr->name)
          "safepoint"))
  (let ((found (gethash address addr->name)))
    (cond (found
           (values found 0))
          (t
           (let* ((code sb-fasl:*assembler-routines*)
                  (hashtable (sb-fasl::%asm-routine-table code))
                  (start (sap-int (code-instructions code)))
                  (end (+ start (1- (%code-text-size code)))))
             (when (<= start address end) ; it has to be an asm routine
               (let* ((offset (- address start))
                      (index (unless (logtest address (1- sb-vm:n-word-bytes))
                               (floor offset sb-vm:n-word-bytes))))
                 (declare (ignorable index))
                 (dohash ((name locs) hashtable)
                   (when (<= (car locs) offset (cadr locs))
                     (return-from find-assembler-routine
                      (values name (- address (+ start (car locs))))))
                   #+(or x86 x86-64)
                   (when (eql index (cddr locs))
                     (return-from find-assembler-routine
                      (values name 0)))))))
           (values nil nil)))))

;;;; some handy function for machine-dependent code to use...

(defun sap-ref-int (sap offset length byte-order)
  (declare (type system-area-pointer sap)
           (fixnum offset)
           (type (member 1 2 4 8) length)
           (type (member :little-endian :big-endian) byte-order))
  (if (or (eq length 1)
          (and (eq byte-order #+big-endian :big-endian #+little-endian :little-endian)
               ;; unaligned loads are ok for these
               #-(or (and arm (not openbsd)) ; openbsd enables alignment faults
                     arm64 ppc ppc64 x86 x86-64)
               (not (logtest (1- length) (sap-int (sap+ sap offset))))))
      (locally
       (declare (optimize (safety 0))) ; disregard shadow memory for msan
       (case length
         (8 (sap-ref-64 sap offset))
         (4 (sap-ref-32 sap offset))
         (2 (sap-ref-16 sap offset))
         (1 (sap-ref-8 sap offset))))
      (binding* (((offset increment)
                  (cond ((eq byte-order :big-endian) (values offset +1))
                        (t (values (+ offset (1- length)) -1))))
                 (val 0))
        (dotimes (i length val)
          (declare (index i))
          (setq val (logior (ash val 8) (sap-ref-8 sap offset)))
          (incf offset increment)))))

;;; Extract a trailing field starting at NEXT-OFFS, and update NEXT-OFFS.
(defun read-suffix (length dstate)
  (declare (type (member 8 16 32 64) length)
           (type disassem-state dstate)
           (optimize (speed 3) (safety 0)))
  (let ((length (ecase length (8 1) (16 2) (32 4) (64 8))))
    (declare (type (unsigned-byte 4) length))
    (prog1
      (sap-ref-int (dstate-segment-sap dstate)
                   (dstate-next-offs dstate)
                   length
                   (dstate-byte-order dstate))
      (incf (dstate-next-offs dstate) length))))

(defun read-signed-suffix (length dstate)
  (declare (type (member 8 16 32 64) length)
           (type disassem-state dstate)
           (optimize (speed 3) (safety 0)))
  (sign-extend (read-suffix length dstate) length))


;;;; optional routines to make notes about code

;;; Store NOTE (which can be either a string or a function with a
;;; single stream argument) to be printed as an end-of-line comment
;;; after the current instruction is disassembled.
(defun note (note dstate)
  (declare (type (or string function) note)
           (type disassem-state dstate))
  (let ((notes (dstate-notes dstate)))
    (setf (dstate-notes dstate) (typecase notes
                                  (null note)
                                  (cons (nconc notes (list note)))
                                  (t (list notes note))))))

(defun prin1-quoted-short (thing stream)
  (if (self-evaluating-p thing)
      (prin1-short thing stream)
      (prin1-short `',thing stream)))

(defun tab (column stream)
  (when stream
    (funcall (formatter "~V,1t") stream column))
  nil)
(defun tab0 (column stream)
  (funcall (formatter "~V,0t") stream column)
  nil)

(defun princ16 (value stream)
  (write value :stream stream :radix t :base 16 :escape nil))

;;; Store a note about the lisp constant at LOCATION in the code object
;;; being disassembled, to be printed as an end-of-line comment.
;;; The interpretation of LOCATION depends on HOW as follows:
;;; - if :INDEX, then LOCATION is directly the argument to CODE-HEADER-REF.
;;; - if :RELATIVE, then it is is a byte displacement beyond CODE.
;;; - if :ABSOLUTE, then it is an address (I'm not sure if this an address
;;    beyond DSTATE-SEGMENT-SAP or SEGMENT-VIRTUAL-LOCATION when those differ)
;;; In any case, if the offset indicates a location outside of the
;;; boxed constants, nothing is printed.

(defun note-code-constant (location dstate &optional (how :relative))
  (declare (type disassem-state dstate))
  (multiple-value-bind (const valid)
      (code-constant-value location dstate how)
    (when valid
      (note (lambda (stream) (prin1-quoted-short const stream)) dstate)
      (values const t))))

(defun code-constant-value (location dstate &optional (how :relative))
  (declare (type disassem-state dstate))
  (binding* ((code (seg-code (dstate-segment dstate)))
             ((addr index)
              (ecase how
               (:relative
                ;; When CODE-TN has a lowtag (as it usually does), we add it in here.
                ;; x86-64 and arm64 do not have a code-tn, but they behave like ppc64
                ;; in that the displacement is relative to the base of the code.
                (let ((addr (+ location
                               #-(or x86-64 ppc64 arm64) sb-vm:other-pointer-lowtag)))
                  (values addr (ash addr (- sb-vm:word-shift)))))
               (:absolute
                ;; Concerning object movement:
                ;; Since we've already decided what the ADDR is, there is nothing that
                ;; has to be done to pin objects or disable GC here - if the object
                ;; is movable, then ADDR is already potentially wrong unless the caller
                ;; took care of immobilizing the DSTATE's code blob.
                ;; It's OK to compute a bogus address (when CODE is NIL). It's just math.
                (values location
                        (ash (- location (- (get-lisp-obj-address code)
                                            sb-vm:other-pointer-lowtag))
                             (- sb-vm:word-shift))))
               (:index
                (values nil location)))))
    ;; Cautiously avoid reading any word index that is not within the
    ;; boxed portion of the header.
    ;; The metadata at index 1 is not considered a valid index.
    (cond ((and code (< 1 index (code-header-words code)))
           (when addr ; ADDR must be word-aligned to be sensible
             (aver (not (logtest addr (ash sb-vm:lowtag-mask -1)))))
           (values (code-header-ref code index) t))
          (t
           (values nil nil)))))

(defun find-code-constant-from-interior-pointer (value dstate)
  (let* ((seg (dstate-segment dstate))
         (code (seg-code seg)))
    (when code
      ;; check for exact pointers first
      (loop for i from sb-vm:code-constants-offset
            below (code-header-words code)
            when (eql (get-lisp-obj-address (code-header-ref code i))
                      value)
            do (return-from find-code-constant-from-interior-pointer
                 (code-header-ref code i)))
      (let ((callables (seg-code-callables seg)))
        (when (eq callables :?)
          (setq callables nil)
          (loop for i from sb-vm:code-constants-offset below (code-header-words code)
                do (let* ((const (code-header-ref code i))
                          (fdefn (if (typep const '(cons fdefn)) (car const) const))
                          (fun (typecase fdefn
                                 (fdefn (fdefn-fun fdefn))
                                 (function fdefn))))
                     ;; This collection will only make sense for immobile objects,
                     ;; but it's fine to collect it.
                     (when (fdefn-p fdefn)
                       (push const callables))
                     (when (functionp fun)
                       (push fun callables))))
          ;; Sorting downward finds the right entry point if there are multiple callees
          ;; in a code object using the trivial algorithm below.
          (setf callables (sort callables #'> :key
                                ;; KLUDGE: need a stub, #'GET-LISP-OBJ-ADDRESS won't work
                                (lambda (x) (get-lisp-obj-address x)))
                (seg-code-callables seg) callables))
        (dolist (thing callables)
          (when (<= (logandc2 (get-lisp-obj-address thing) sb-vm:lowtag-mask)
                    value
                    (let ((base-obj
                            (if (simple-fun-p thing) (fun-code-header thing) thing)))
                      (+ (logandc2 (get-lisp-obj-address base-obj) sb-vm:lowtag-mask)
                         (primitive-object-size base-obj)
                         -1)))
            (return thing)))))))

;;; If the memory address located NIL-BYTE-OFFSET bytes from the
;;; constant NIL is a valid slot in a symbol, store a note describing
;;; which symbol and slot, to be printed as an end-of-line comment
;;; after the current instruction is disassembled. Returns non-NIL iff
;;; a note was recorded.
(defun maybe-note-nil-indexed-symbol-slot-ref (nil-byte-offset dstate)
  (declare (type offset nil-byte-offset)
           (type disassem-state dstate))
  (multiple-value-bind (symbol access-fun)
      (grok-nil-indexed-symbol-slot-ref nil-byte-offset)
    (when access-fun
      (note (lambda (stream)
              (prin1 (if (eq access-fun 'symbol-value)
                         symbol
                         `(,access-fun ',symbol))
                     stream))
            dstate))
    access-fun))

;;; If the memory address located NIL-BYTE-OFFSET bytes from the
;;; constant NIL is a valid lisp object, store a note describing which
;;; symbol and slot, to be printed as an end-of-line comment after the
;;; current instruction is disassembled. Returns non-NIL iff a note
;;; was recorded.
;;; If the address is the start of an assembly routine, print it as
;;; a symbol without a quote.
(defun maybe-note-nil-indexed-object (nil-byte-offset dstate)
  (declare (type offset nil-byte-offset)
           (type disassem-state dstate))
  (binding* ((addr (+ sb-vm:nil-value nil-byte-offset))
             ((obj validp) (make-lisp-obj addr nil)))
    (when validp
      ;; ambiguous case - the backend could potentially use NIL
      ;; to compute certain arbitrary fixnums.
      (awhen (and (fixnump obj) (find-assembler-routine addr))
        (note (lambda (stream) (prin1-short it stream)) dstate)
        (return-from maybe-note-nil-indexed-object t))
      (note (lambda (stream) (prin1-quoted-short obj stream)) dstate)
      t)))

;;; If ADDRESS is the address of a primitive assembler routine or
;;; foreign symbol, store a note describing which one, to be printed
;;; as an end-of-line comment after the current instruction is
;;; disassembled. Returns non-NIL iff a note was recorded. If
;;; NOTE-ADDRESS-P is non-NIL, a note of the address is also made.
(defun maybe-note-assembler-routine (address note-address-p dstate)
  (declare (type disassem-state dstate))
  (unless (typep address 'address)
    (return-from maybe-note-assembler-routine nil))
  (multiple-value-bind (name offs) (find-assembler-routine address)
    (unless name
      (setq name (sap-foreign-symbol (int-sap address))))
    (when name
      (when (eql offs 0)
        (setq offs nil))
      (note (lambda (stream)
              (cond (note-address-p
                     (format stream "#x~8,'0x: ~a~@[ +~d~]" address name offs))
                    (offs
                     (format stream "~a +~d" name offs))
                    ((stringp name)
                     (princ name stream))
                    (t
                     (prin1 name stream))))
            dstate))
    name))

;;; If there's a valid mapping from OFFSET in the storage class
;;; SC-NAME to a source variable, make a note of the source-variable
;;; name, to be printed as an end-of-line comment after the current
;;; instruction is disassembled. Returns non-NIL iff a note was
;;; recorded.
(defun maybe-note-single-storage-ref (offset sc-name dstate)
  (declare (type offset offset)
           (type symbol sc-name)
           (type disassem-state dstate))
  (let ((storage-location
         (find-valid-storage-location offset sc-name dstate)))
    (when storage-location
      (note (lambda (stream)
              (princ (sb-di:debug-var-symbol
                      (aref (storage-info-debug-vars
                             (seg-storage-info (dstate-segment dstate)))
                            storage-location))
                     stream))
            dstate)
      t)))

;;; If there's a valid mapping from OFFSET in the storage-base called
;;; SB-NAME to a source variable, make a note equating ASSOC-WITH with
;;; the source-variable name, to be printed as an end-of-line comment
;;; after the current instruction is disassembled. Returns non-NIL iff
;;; a note was recorded.
(defun maybe-note-associated-storage-ref (offset sb-name assoc-with dstate)
  (declare (type offset offset)
           (type symbol sb-name)
           (type (or symbol string) assoc-with)
           (type disassem-state dstate))
  (let ((storage-location
         (find-valid-storage-location offset sb-name dstate)))
    (when storage-location
      (note (lambda (stream)
              (format stream "~A = ~S"
                      assoc-with
                      (sb-di:debug-var-symbol
                       (aref (dstate-debug-vars dstate)
                             storage-location))))
            dstate)
      t)))

(defun maybe-note-static-symbol (address dstate)
  (declare (type disassem-state dstate))
  (when (or (not (typep address `(unsigned-byte ,sb-vm:n-machine-word-bits)))
            (eql address 0))
    (return-from maybe-note-static-symbol))
  (let ((symbol
         (block found
           (when (eq address sb-vm:nil-value)
             (return-from found nil))
           (when (< address (sap-int sb-vm:*static-space-free-pointer*))
             (dovector (symbol sb-vm:+static-symbols+)
               (when (= (get-lisp-obj-address symbol) address)
                 (return-from found symbol))))
          (return-from maybe-note-static-symbol))))
    (note (lambda (s) (prin1 symbol s)) dstate)))

(defun get-internal-error-name (errnum)
  (and (array-in-bounds-p sb-c:+backend-internal-errors+ errnum)
       (cadr (svref sb-c:+backend-internal-errors+ errnum))))

(defun get-random-tn-name (sc+offset)
  (let ((sc (sb-c:sc+offset-scn sc+offset))
        (offset (sb-c:sc+offset-offset sc+offset)))
    (if (= sc sb-vm:immediate-sc-number)
        (princ-to-string offset)
        (sb-c:location-print-name
         (sb-c:make-random-tn (svref sb-c:*backend-sc-numbers* sc) offset)))))

;;; When called from an error break instruction's :DISASSEM-CONTROL (or
;;; :DISASSEM-PRINTER) function, will correctly deal with printing the
;;; arguments to the break.
;;;
;;; ERROR-PARSE-FUN should be a function that accepts:
;;;   1) a SYSTEM-AREA-POINTER
;;;   2) a BYTE-OFFSET from the SAP to begin at
;;; It should read information from the SAP starting at BYTE-OFFSET, and
;;; return five values:
;;;   1) the error number
;;;   2) the total length, in bytes, of the information
;;;   3) a list of SC-OFFSETs of the locations of the error parameters
;;;   4) a list of the length (as read from the SAP), in bytes, of each
;;;      of the return values.
;;;   5) a boolean indicating whether to disassemble 1 byte prior to
;;;      decoding the SC+OFFSETs.  (This byte is literally the byte in
;;;      memory, which is distinct from the 'error number')
(defun handle-break-args (error-parse-fun trap-number stream dstate)
  (declare (type function error-parse-fun)
           (type (or null stream) stream)
           (type disassem-state dstate))
  (multiple-value-bind (errnum adjust sc+offsets lengths error-byte)
      (funcall error-parse-fun
               (dstate-segment-sap dstate)
               (dstate-next-offs dstate)
               trap-number
               (null stream))
    (when stream
       (setf (dstate-cur-offs dstate)
             (dstate-next-offs dstate))
       (flet ((emit-err-arg ()
                (let ((num (pop lengths)))
                  (unless (zerop num)
                    (print-notes-and-newline stream dstate)
                    (print-current-address stream dstate)
                    (print-inst num stream dstate)
                    (print-bytes num stream dstate)
                    (incf (dstate-cur-offs dstate) num))))
              (emit-note (note)
                (when note
                  (note (string note) dstate))))
         (when error-byte
           (emit-err-arg))
         (emit-note (get-internal-error-name errnum))
         (dolist (sc+offset sc+offsets)
           (emit-err-arg)
           (if (= (sb-c:sc+offset-scn sc+offset) sb-vm:constant-sc-number)
               (note-code-constant (sb-c:sc+offset-offset sc+offset) dstate :index)
               (emit-note (get-random-tn-name sc+offset))))))
    (incf (dstate-next-offs dstate) adjust)))

;;; arm64 stores an error-number in the instruction bytes,
;;; so can't easily share this code.
;;; But probably we should just add the conditionalization in here.
#-arm64
(defun snarf-error-junk (sap offset trap-number &optional length-only)
  (let* ((index offset)
         (error-byte t)
         (error-number (cond ((>= trap-number sb-vm:error-trap)
                              (setf error-byte nil)
                              (- trap-number sb-vm:error-trap))
                             (t
                              (incf index)
                              (sap-ref-8 sap offset))))
         (length (sb-kernel::error-length error-number)))
    (declare (type system-area-pointer sap)
             (type (unsigned-byte 8) length))
    (cond (length-only
           (loop repeat length do (sb-c:sap-read-var-integerf sap index))
           (values 0 (- index offset) nil nil error-byte))
          (t
           (collect ((sc+offsets)
                     (lengths))
             (when error-byte
               (lengths 1)) ;; error-number
             (loop repeat length do
                   (let ((old-index index))
                     (sc+offsets (sb-c:sap-read-var-integerf sap index))
                     (lengths (- index old-index))))
             (values error-number
                     (- index offset)
                     (sc+offsets)
                     (lengths)
                     error-byte))))))

;;; Compute a lambda which receives a DSTATE and DCHUNK, and for each arg
;;; in ARGS that gets some calculation performed, performs all such
;;; calculations.
;;; Arguments with no field specification can be used to parse suffix bytes.
;;; The lambda stores all results into DSTATE and possibly updates NEXT-OFFS.
(defun compute-prefilter (args cache)
  (let ((repr
         ;; Start with an abstract representation of all the
         ;; filtering actions performed in ARGS.
         (mapcan (lambda (arg)
                   (awhen (arg-prefilter arg)
                     (list (list* it
                                  (posq arg args)
                                  (arg-sign-extend-p arg)
                                  (arg-fields arg)))))
                 args)))
    (when (null repr)
      (return-from compute-prefilter
        (lambda (x y)
          ;; do nothing at all, as efficiently as possible
          (declare (optimize (safety 0)) (ignore x y)))))
    ;; Try to find a lambda that matches the abstract representation
    (awhen (assoc repr (cdr (assq :prefilter cache)) :test 'equal)
      (return-from compute-prefilter (cdr it)))
    (let* ((actions
            (mapcan
             (lambda (filter-spec)
               (destructuring-bind (function arg-index sign-extend . fields) filter-spec
                 `((svref (dstate-filtered-values dstate) ,arg-index)
                   (funcall
                    ,function
                    dstate
                    ,@(mapcar (lambda (byte)
                                (let ((int `(dchunk-extract chunk ',byte)))
                                  (if sign-extend
                                      `(sign-extend ,int ,(byte-size byte))
                                      int)))
                              fields)))))
             repr))
           (compiled
            (compile nil `(lambda (dstate chunk)
                            (let ((chunk (truly-the dchunk chunk))
                                  (dstate (truly-the disassem-state dstate)))
                              (declare (ignorable chunk))
                              (setf ,@actions))))))
      (push (cons repr compiled) (cdr (assq :prefilter cache)))
      compiled)))

(defun !remove-bootstrap-symbols ()
  ;; Remove INSTRUCTION-FLAVORS properties, which are the inputs to
  ;; computing the INSTRUCTION instances.
  (do-symbols (symbol sb-assem::*backend-instruction-set-package*)
    (remf (symbol-plist symbol) 'instruction-flavors))
  ;; Remove compile-time-only metadata. This preserves compatibility with the
  ;; older disassembler macros which wrapped GEN-ARG-TYPE-DEF-FORM and such
  ;; in (EVAL-WHEN (:COMPILE-TOPLEVEL :EXECUTE)), which in turn required that
  ;; all prefilters, labellers, and printers be defined at cross-compile-time.
  ;; A consequence of :LOAD-TOPLEVEL not being there was that was not possible
  ;; to add instruction definitions to an image without also recompiling
  ;; the backend's "insts" file. It also was not possible to incrementally
  ;; recompile and/or use slam.sh because of a bunch of mostly harmless bugs
  ;; in the function cache (a/k/a identical-code-folding) logic that was only
  ;; guaranteed to do the right thing from a clean compile. Additionally,
  ;; you had to use (GET-INST-SPACE :FORCE T) to pick up new definitions.
  ;; Given those considerations which made extending a running disassembler
  ;; nontrivial, the code-generating code is not so useful after the
  ;; initial instruction space is built, so it can all be removed.
  ;; But if you need all these macros to exist for some reason,
  ;; then define the following feature to keep them:
  #-sb-retain-assembler-macros
  (do-symbols (symbol sb-assem::*backend-instruction-set-package*)
    (remf (symbol-plist symbol) 'arg-type)
    (remf (symbol-plist symbol) 'inst-format)))

;; Remove macros that only make sense with metadata available.
;; Tree shaker will remove everything that the macros depended on.
(push '("SB-DISASSEM" define-arg-type define-instruction-format
        gen-arg-forms %gen-arg-forms)
      *!removable-symbols*)
