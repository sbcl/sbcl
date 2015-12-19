;;;; disassembler-related stuff not needed in cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!DISASSEM")

;;;; FIXME: A lot of stupid package prefixes would go away if DISASSEM
;;;; would use the SB!DI package. And some more would go away if it would
;;;; use SB!SYS (in order to get to the SAP-FOO operators).

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

(defun specialization-error (insts)
  (bug
   "~@<Instructions either aren't related or conflict in some way: ~4I~_~S~:>"
   insts))

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
           (specialization-error insts))
          ((cdr masters)
           (error "multiple specializing masters: ~S" masters))
          (t
           (let ((master (car masters)))
             (setf (inst-specializers master)
                   (order-specializers (remove master insts)))
             master)))))

;;;; choosing an instruction

#!-sb-fluid (declaim (inline inst-matches-p choose-inst-specialization))

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

;;;; searching for an instruction in instruction space

;;; Return the instruction object within INST-SPACE corresponding to the
;;; bit-pattern CHUNK, or NIL if there isn't one.
(defun find-inst (chunk inst-space)
  (declare (type dchunk chunk)
           (type (or null inst-space instruction) inst-space))
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
           (return (find-inst chunk (ischoice-subspace choice)))))))))

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
;;;     next pointer (next function header)
;;;     name
;;;     arglist
;;;     type
;;;
;;; LRA layout (dual word aligned):
;;;     header-word

#!-sb-fluid (declaim (inline words-to-bytes bytes-to-words))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; Convert a word-offset NUM to a byte-offset.
  (defun words-to-bytes (num)
    (declare (type offset num))
    (ash num sb!vm:word-shift))
  ) ; EVAL-WHEN

;;; Convert a byte-offset NUM to a word-offset.
(defun bytes-to-words (num)
  (declare (type offset num))
  (ash num (- sb!vm:word-shift)))

(defconstant lra-size (words-to-bytes 1))

(defstruct (offs-hook (:copier nil))
  (offset 0 :type offset)
  (fun (missing-arg) :type function)
  (before-address nil :type (member t nil)))

(def!method print-object ((seg segment) stream)
  (print-unreadable-object (seg stream :type t)
    (let ((addr (sb!sys:sap-int (funcall (seg-sap-maker seg)))))
      (format stream "#X~X..~X[~W]~:[ (#X~X)~;~*~]~@[ in ~S~]"
              addr (+ addr (seg-length seg)) (seg-length seg)
              (= (seg-virtual-location seg) addr)
              (seg-virtual-location seg)
              (seg-code seg)))))

;;;; function ops

(defun fun-self (fun)
  (declare (type compiled-function fun))
  (sb!kernel:%simple-fun-self (sb!kernel:%fun-fun fun)))

(defun fun-code (fun)
  (declare (type compiled-function fun))
  (sb!kernel:fun-code-header (fun-self fun)))

(defun fun-next (fun)
  (declare (type compiled-function fun))
  (sb!kernel:%simple-fun-next (sb!kernel:%fun-fun fun)))

(defun fun-address (fun)
  (declare (type compiled-function fun))
  (- (sb!kernel:get-lisp-obj-address (sb!kernel:%fun-fun fun)) sb!vm:fun-pointer-lowtag))

;;; the offset of FUNCTION from the start of its code-component's
;;; instruction area
(defun fun-insts-offset (function)
  (declare (type compiled-function function))
  (- (fun-address function)
     (sb!sys:sap-int (sb!kernel:code-instructions (fun-code function)))))

;;; the offset of FUNCTION from the start of its code-component
(defun fun-offset (function)
  (declare (type compiled-function function))
  (words-to-bytes (sb!kernel:get-closure-length function)))

;;;; operations on code-components (which hold the instructions for
;;;; one or more functions)

;;; Return the length of the instruction area in CODE-COMPONENT.
(defun code-inst-area-length (code-component)
  (declare (type sb!kernel:code-component code-component))
  (sb!kernel:%code-code-size code-component))

;;; Return the address of the instruction area in CODE-COMPONENT.
(defun code-inst-area-address (code-component)
  (declare (type sb!kernel:code-component code-component))
  (sb!sys:sap-int (sb!kernel:code-instructions code-component)))

(defun segment-offs-to-code-offs (offset segment)
  (sb!sys:without-gcing
   (let* ((seg-base-addr (sb!sys:sap-int (funcall (seg-sap-maker segment))))
          (code-addr
           (logandc1 sb!vm:lowtag-mask
                     (sb!kernel:get-lisp-obj-address (seg-code segment))))
          (addr (+ offset seg-base-addr)))
     (declare (type address seg-base-addr code-addr addr))
     (- addr code-addr))))

(defun code-offs-to-segment-offs (offset segment)
  (sb!sys:without-gcing
   (let* ((seg-base-addr (sb!sys:sap-int (funcall (seg-sap-maker segment))))
          (code-addr
           (logandc1 sb!vm:lowtag-mask
                     (sb!kernel:get-lisp-obj-address (seg-code segment))))
          (addr (+ offset code-addr)))
     (declare (type address seg-base-addr code-addr addr))
     (- addr seg-base-addr))))

(defun code-insts-offs-to-segment-offs (offset segment)
  (sb!sys:without-gcing
   (let* ((seg-base-addr (sb!sys:sap-int (funcall (seg-sap-maker segment))))
          (code-insts-addr
           (sb!sys:sap-int (sb!kernel:code-instructions (seg-code segment))))
          (addr (+ offset code-insts-addr)))
     (declare (type address seg-base-addr code-insts-addr addr))
     (- addr seg-base-addr))))

(defun lra-hook (chunk stream dstate)
  (declare (type dchunk chunk)
           (ignore chunk)
           (type (or null stream) stream)
           (type disassem-state dstate))
  (when (and (aligned-p (+ (seg-virtual-location (dstate-segment dstate))
                           (dstate-cur-offs dstate))
                        (* 2 sb!vm:n-word-bytes))
             ;; Check type.
             (= (sb!sys:sap-ref-8 (dstate-segment-sap dstate)
                                  (if (eq (dstate-byte-order dstate)
                                          :little-endian)
                                      (dstate-cur-offs dstate)
                                      (+ (dstate-cur-offs dstate)
                                         (1- lra-size))))
                sb!vm:return-pc-header-widetag))
    (unless (null stream)
      (note "possible LRA header" dstate)))
  nil)

;;; Print the fun-header (entry-point) pseudo-instruction at the
;;; current location in DSTATE to STREAM.
(defun fun-header-hook (stream dstate)
  (declare (type (or null stream) stream)
           (type disassem-state dstate))
  (unless (null stream)
    (let* ((seg (dstate-segment dstate))
           (code (seg-code seg))
           (woffs
            (bytes-to-words
             (segment-offs-to-code-offs (dstate-cur-offs dstate) seg)))
           (name
            (sb!kernel:code-header-ref code
                                       (+ woffs
                                          sb!vm:simple-fun-name-slot)))
           (args
            (sb!kernel:code-header-ref code
                                       (+ woffs
                                          sb!vm:simple-fun-arglist-slot)))
           (type
            (sb!kernel:code-header-ref code
                                       (+ woffs
                                          sb!vm:simple-fun-type-slot))))
      ;; if the function's name conveys its args, don't show ARGS too
      (format stream ".~A ~S~:[~:A~;~]" 'entry name
              (and (typep name '(cons (eql lambda) (cons list)))
                   (equal args (second name)))
              args)
      (note (lambda (stream)
              (format stream "~:S" type)) ; use format to print NIL as ()
            dstate)))
  (incf (dstate-next-offs dstate)
        (words-to-bytes sb!vm:simple-fun-code-offset)))

(defun alignment-hook (chunk stream dstate)
  (declare (type dchunk chunk)
           (ignore chunk)
           (type (or null stream) stream)
           (type disassem-state dstate))
  (let ((location
         (+ (seg-virtual-location (dstate-segment dstate))
            (dstate-cur-offs dstate)))
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
  (setf (dstate-inst-properties dstate) nil)
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
    (unless (null stream)
      (multiple-value-bind (words bytes)
          (truncate alignment sb!vm:n-word-bytes)
        (when (> words 0)
          (print-inst (* words sb!vm:n-word-bytes) stream dstate
                      :trailing-space nil))
        (when (> bytes 0)
          (print-inst bytes stream dstate :trailing-space nil)))
      (pad-inst-column stream (+ prefix-len alignment))
      (decf (dstate-cur-offs dstate) prefix-len)
      (print-bytes (+ prefix-len alignment) stream dstate))
    (incf (dstate-next-offs dstate) alignment)))

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

  (let ((ispace (get-inst-space))
        (data-end-offset
         ;; If the segment starts with unboxed data,
         ;; dump some number of words using the .WORD pseudo-ops.
         (if (and (seg-unboxed-data-range segment)
                  (= (segment-offs-to-code-offs 0 segment)
                     (car (seg-unboxed-data-range segment))))
             (code-offs-to-segment-offs (cdr (seg-unboxed-data-range segment))
                                        segment)
             0)) ; sentinel value
        (prefix-p nil) ; just processed a prefix inst
        (prefix-len 0) ; sum of lengths of any prefix instruction(s)
        (prefix-print-names nil)) ; reverse list of prefixes seen

    (rewind-current-segment dstate segment)

    (loop
      (when (>= (dstate-cur-offs dstate)
                (seg-opcodes-length (dstate-segment dstate)))
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

      (when (< (dstate-cur-offs dstate) data-end-offset)
        (sb!sys:without-gcing
         (format stream "~A  #x~v,'0x" '.word
                 (* 2 sb!vm:n-word-bytes)
                 (sap-ref-int (funcall (seg-sap-maker segment))
                              (dstate-cur-offs dstate)
                              sb!vm:n-word-bytes
                              (dstate-byte-order dstate))))
        (setf (dstate-next-offs dstate)
              (+ (dstate-cur-offs dstate) sb!vm:n-word-bytes)))

      (unless (> (dstate-next-offs dstate) (dstate-cur-offs dstate))
        (sb!sys:without-gcing
         (setf (dstate-segment-sap dstate) (funcall (seg-sap-maker segment)))

         (let* ((chunk
                 (sap-ref-dchunk (dstate-segment-sap dstate)
                                 (dstate-cur-offs dstate)
                                 (dstate-byte-order dstate)))
                (fun-prefix-p (call-fun-hooks chunk stream dstate)))
           (if (> (dstate-next-offs dstate) (dstate-cur-offs dstate))
               (setf prefix-p fun-prefix-p)
               (let ((inst (find-inst chunk ispace)))
                 (cond ((null inst)
                        (handle-bogus-instruction stream dstate prefix-len)
                        (setf prefix-p nil))
                       (t
                        (setf (dstate-next-offs dstate)
                              (+ (dstate-cur-offs dstate)
                                 (inst-length inst)))
                        (let ((orig-next (dstate-next-offs dstate))
                              (prefilter (inst-prefilter inst))
                              (control (inst-control inst)))
                          (print-inst (inst-length inst) stream dstate
                                      :trailing-space nil)
                          (when prefilter
                            (funcall prefilter chunk dstate))

                          (setf prefix-p (null (inst-printer inst)))

                          (when stream
                            ;; Print any instruction bytes recognized by
                            ;; the prefilter which calls read-suffix and
                            ;; updates next-offs.
                            (let ((suffix-len (- (dstate-next-offs dstate)
                                                 orig-next)))
                              (when (plusp suffix-len)
                                (print-inst suffix-len stream dstate
                                            :offset (inst-length inst)
                                            :trailing-space nil))
                              ;; Keep track of the number of bytes
                              ;; printed so far.
                              (incf prefix-len (+ (inst-length inst)
                                                  suffix-len)))
                            (if prefix-p
                                (let ((name (inst-print-name inst)))
                                  (when name
                                    (push name prefix-print-names)))
                                (progn
                                  ;; PREFIX-LEN includes the length of the
                                  ;; current (non-prefix) instruction here.
                                  (pad-inst-column stream prefix-len)
                                  (dolist (name (reverse prefix-print-names))
                                    (princ name stream)
                                    (write-char #\space stream)))))

                          (funcall function chunk inst)

                          (when control
                            (funcall control chunk inst stream dstate))))))))))

      (setf (dstate-cur-offs dstate) (dstate-next-offs dstate))

      (when stream
        (unless prefix-p
          (setf prefix-len 0
                prefix-print-names nil)
          (print-notes-and-newline stream dstate))
        (setf (dstate-output-state dstate) nil))
      (unless prefix-p
        (setf (dstate-inst-properties dstate) nil)))))


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
       (let ((labeller (inst-labeller inst)))
         (when labeller
           (setf labels (funcall labeller chunk labels dstate)))))
     segment
     dstate)
    (setf (dstate-labels dstate) labels)
    ;; erase any notes that got there by accident
    (setf (dstate-notes dstate) nil)))

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

;;; Get the instruction-space, creating it if necessary.
(defun get-inst-space (&key (package sb!assem::*backend-instruction-set-package*)
                            force)
  (let ((ispace *disassem-inst-space*))
    (when (or force (null ispace))
      (let ((insts nil))
        (do-symbols (name package)
          (let ((inst-flavors (get name :disassembler)))
            (dolist (flav inst-flavors)
              (push flav insts))))
        (setf ispace (build-inst-space insts)))
      (setf *disassem-inst-space* ispace))
    ispace))

;;;; Add global hooks.

(defun add-offs-hook (segment addr hook)
  (let ((entry (cons addr hook)))
    (if (null (seg-hooks segment))
        (setf (seg-hooks segment) (list entry))
        (push entry (cdr (last (seg-hooks segment)))))))

(defun add-offs-note-hook (segment addr note)
  (add-offs-hook segment
                 addr
                 (lambda (stream dstate)
                   (declare (type (or null stream) stream)
                            (type disassem-state dstate))
                   (when stream
                     (note note dstate)))))

(defun add-offs-comment-hook (segment addr comment)
  (add-offs-hook segment
                 addr
                 (lambda (stream dstate)
                   (declare (type (or null stream) stream)
                            (ignore dstate))
                   (when stream
                     (write-string ";;; " stream)
                     (etypecase comment
                       (string
                        (write-string comment stream))
                       (function
                        (funcall comment stream)))
                     (terpri stream)))))

(defun add-fun-hook (dstate function)
  (push function (dstate-fun-hooks dstate)))

(defun set-location-printing-range (dstate from length)
  (setf (dstate-addr-print-len dstate) ; in characters
        ;; 4 bits per hex digit
        (ceiling (integer-length (logxor from (+ from length))) 4)))

;;; Print the current address in DSTATE to STREAM, plus any labels that
;;; correspond to it, and leave the cursor in the instruction column.
(defun print-current-address (stream dstate)
  (declare (type stream stream)
           (type disassem-state dstate))
  (let* ((location
          (+ (seg-virtual-location (dstate-segment dstate))
             (dstate-cur-offs dstate)))
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

(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro with-print-restrictions (&rest body)
    `(let ((*print-pretty* t)
           (*print-lines* 2)
           (*print-length* 4)
           (*print-level* 3))
       ,@body)))

;;; Print a newline to STREAM, inserting any pending notes in DSTATE
;;; as end-of-line comments. If there is more than one note, a
;;; separate line will be used for each one.
(defun print-notes-and-newline (stream dstate)
  (declare (type stream stream)
           (type disassem-state dstate))
  (with-print-restrictions
    (dolist (note (dstate-notes dstate))
      (format stream "~Vt " *disassem-note-column*)
      (pprint-logical-block (stream nil :per-line-prefix "; ")
      (etypecase note
        (string
         (write-string note stream))
        (function
         (funcall note stream))))
      (terpri stream))
    (fresh-line stream)
    (setf (dstate-notes dstate) nil)))

;;; Print NUM instruction bytes to STREAM as hex values.
(defun print-inst (num stream dstate &key (offset 0) (trailing-space t))
  (when (> *disassem-inst-column-width* 0)
    (let ((sap (dstate-segment-sap dstate))
          (start-offs (+ offset (dstate-cur-offs dstate))))
      (dotimes (offs num)
        (format stream "~2,'0x" (sb!sys:sap-ref-8 sap (+ offs start-offs))))
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
      (format stream "#X~2,'0x" (sb!sys:sap-ref-8 sap (+ offs start-offs))))))

;;; Disassemble NUM machine-words to STREAM as simple `WORD' instructions.
(defun print-words (num stream dstate)
  (declare (type offset num)
           (type stream stream)
           (type disassem-state dstate))
  (format stream "~A~Vt" 'WORD (dstate-argument-column dstate))
  (let ((sap (dstate-segment-sap dstate))
        (start-offs (dstate-cur-offs dstate))
        (byte-order (dstate-byte-order dstate)))
    (dotimes (word-offs num)
      (unless (zerop word-offs)
        (write-string ", " stream))
      (let ((word 0) (bit-shift 0))
        (dotimes (byte-offs sb!vm:n-word-bytes)
          (let ((byte
                 (sb!sys:sap-ref-8
                        sap
                        (+ start-offs
                           (* word-offs sb!vm:n-word-bytes)
                           byte-offs))))
            (setf word
                  (if (eq byte-order :big-endian)
                      (+ (ash word sb!vm:n-byte-bits) byte)
                      (+ word (ash byte bit-shift))))
            (incf bit-shift sb!vm:n-byte-bits)))
        (format stream "#X~V,'0X" (ash sb!vm:n-word-bits -2) word)))))

(defvar *default-dstate-hooks* (list #'lra-hook))

;;; Make a disassembler-state object.
(defun make-dstate (&optional (fun-hooks *default-dstate-hooks*))
  (let ((alignment *disassem-inst-alignment-bytes*)
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

    (%make-dstate :fun-hooks fun-hooks
                  :argument-column arg-column
                  :alignment alignment
                  :byte-order sb!c:*backend-byte-order*)))

(defun add-fun-header-hooks (segment)
  (declare (type segment segment))
  (do ((fun (awhen (seg-code segment) (sb!kernel:%code-entry-points it))
            (fun-next fun))
       (length (seg-length segment)))
      ((null fun))
    (let ((offset (code-offs-to-segment-offs (fun-offset fun) segment)))
      (when (<= 0 offset length)
        ;; Up to 2 words of zeros might be present to align the next
        ;; simple-fun. Limit on OFFSET is to avoid incorrect triggering
        ;; in case of unexpected weirdness. FIXME: verify all zero bytes
        (when (< 0 offset (* sb!vm:n-word-bytes 2))
          (push (make-offs-hook
                 :fun (lambda (stream dstate)
                         (when stream
                           (format stream ".SKIP ~D" offset))
                          (incf (dstate-next-offs dstate) offset))
                 :offset 0) ; at 0 bytes into this seg, skip OFFSET bytes
                (seg-hooks segment)))
        (push (make-offs-hook :offset offset :fun #'fun-header-hook)
              (seg-hooks segment))))))

;;; A SAP-MAKER is a no-argument function that returns a SAP.

;; FIXME: Are the objects we are taking saps for always pinned?
#!-sb-fluid (declaim (inline sap-maker))
(defun sap-maker (function input offset)
  (declare (optimize (speed 3))
           (muffle-conditions compiler-note)
           (type (function (t) sb!sys:system-area-pointer) function)
           (type offset offset))
  (let ((old-sap (sb!sys:sap+ (funcall function input) offset)))
    (declare (type sb!sys:system-area-pointer old-sap))
    (lambda ()
      (let ((new-addr
             (+ (sb!sys:sap-int (funcall function input)) offset)))
        ;; Saving the sap like this avoids consing except when the sap
        ;; changes (because the sap-int, arith, etc., get inlined).
        (declare (type address new-addr))
        (if (= (sb!sys:sap-int old-sap) new-addr)
            old-sap
            (setf old-sap (sb!sys:int-sap new-addr)))))))

(defun vector-sap-maker (vector offset)
  (declare (optimize (speed 3))
           (type offset offset))
  (sap-maker #'sb!sys:vector-sap vector offset))

(defun code-sap-maker (code offset)
  (declare (optimize (speed 3))
           (type sb!kernel:code-component code)
           (type offset offset))
  (sap-maker #'sb!kernel:code-instructions code offset))

(defun memory-sap-maker (address)
  (declare (optimize (speed 3))
           (muffle-conditions compiler-note)
           (type address address))
  (let ((sap (sb!sys:int-sap address)))
    (lambda () sap)))

(defstruct (source-form-cache (:conc-name sfcache-)
                              (:copier nil))
  (debug-source nil :type (or null sb!di:debug-source))
  (toplevel-form-index -1 :type fixnum)
  (last-location-retrieved nil :type (or null sb!di:code-location))
  (last-form-retrieved -1 :type fixnum))

;;; Return a memory segment located at the system-area-pointer returned by
;;; SAP-MAKER and LENGTH bytes long in the disassem-state object DSTATE.
;;;
;;; &KEY arguments include :VIRTUAL-LOCATION (by default the same as
;;; the address), :DEBUG-FUN, :SOURCE-FORM-CACHE (a
;;; SOURCE-FORM-CACHE object), and :HOOKS (a list of OFFS-HOOK
;;; objects).
(defun make-segment (sap-maker length
                     &key
                     code virtual-location
                     debug-fun source-form-cache
                     hooks)
  (declare (type (function () sb!sys:system-area-pointer) sap-maker)
           (type disassem-length length)
           (type (or null address) virtual-location)
           (type (or null sb!di:debug-fun) debug-fun)
           (type (or null source-form-cache) source-form-cache))
  (let* ((segment
          (%make-segment
           :sap-maker sap-maker
           :length length
           :opcodes-length length
           :virtual-location (or virtual-location
                                 (sb!sys:sap-int (funcall sap-maker)))
           :hooks hooks
           :code code
           :unboxed-data-range
           (and code
                (let ((n-words (sb!kernel:code-n-unboxed-data-words code))
                      (start (sb!kernel:get-header-data code)))
                  (and (plusp n-words)
                       (cons (* sb!vm:n-word-bytes start)
                             (* sb!vm:n-word-bytes (+ start n-words)))))))))
    (add-debugging-hooks segment debug-fun source-form-cache)
    (add-fun-header-hooks segment)
    segment))

(defun make-vector-segment (vector offset &rest args)
  (declare (type vector vector)
           (type offset offset)
           (inline make-segment))
  (apply #'make-segment (vector-sap-maker vector offset) args))

(defun make-code-segment (code offset length &rest args)
  (declare (type sb!kernel:code-component code)
           (type offset offset)
           (inline make-segment))
  (apply #'make-segment (code-sap-maker code offset) length :code code args))

(defun make-memory-segment (address &rest args)
  (declare (type address address)
           (inline make-segment))
  (apply #'make-segment (memory-sap-maker address) args))

;;; just for fun
(defun print-fun-headers (function)
  (declare (type compiled-function function))
  (let* ((self (fun-self function))
         (code (sb!kernel:fun-code-header self)))
    (format t "Code-header ~S: size: ~S~%"
            code
            (sb!kernel:%code-code-size code))
    (do ((fun (sb!kernel:code-header-ref code sb!vm:code-entry-points-slot)
              (fun-next fun)))
        ((null fun))
      (let ((fun-offset (sb!kernel:get-closure-length fun)))
        ;; There is function header fun-offset words from the
        ;; code header.
        (format t "Fun-header ~S at offset ~W (words): ~S~A => ~S~%"
                fun
                fun-offset
                (sb!kernel:code-header-ref
                 code (+ fun-offset sb!vm:simple-fun-name-slot))
                (sb!kernel:code-header-ref
                 code (+ fun-offset sb!vm:simple-fun-arglist-slot))
                (sb!kernel:code-header-ref
                 code (+ fun-offset sb!vm:simple-fun-type-slot)))))))

;;; getting at the source code...

(defun get-different-source-form (loc context &optional cache)
  (if (and cache
           (eq (sb!di:code-location-debug-source loc)
               (sfcache-debug-source cache))
           (eq (sb!di:code-location-toplevel-form-offset loc)
               (sfcache-toplevel-form-index cache))
           (or (eql (sb!di:code-location-form-number loc)
                    (sfcache-last-form-retrieved cache))
               (awhen (sfcache-last-location-retrieved cache)
                 (sb!di:code-location= loc it))))
      (values nil nil)
      (let ((form (sb!debug::code-location-source-form loc context nil)))
        (when cache
          (setf (sfcache-debug-source cache)
                (sb!di:code-location-debug-source loc))
          (setf (sfcache-toplevel-form-index cache)
                (sb!di:code-location-toplevel-form-offset loc))
          (setf (sfcache-last-form-retrieved cache)
                (sb!di:code-location-form-number loc))
          (setf (sfcache-last-location-retrieved cache) loc))
        (values form t))))

;;;; stuff to use debugging info to augment the disassembly

(defun code-fun-map (code)
  (declare (type sb!kernel:code-component code))
  (sb!c::compiled-debug-info-fun-map (sb!kernel:%code-debug-info code)))

(defstruct (location-group (:copier nil) (:predicate nil))
  ;; This was (VECTOR (OR LIST FIXNUM)) but that doesn't have any
  ;; specialization other than T, and the cross-compiler has trouble
  ;; with (SB!XC:TYPEP #() '(VECTOR (OR LIST FIXNUM)))
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

;;; Return a new vector which has the same contents as the old one
;;; VEC, plus new cells (for a total size of NEW-LEN). The additional
;;; elements are initialized to INITIAL-ELEMENT.
(defun grow-vector (vec new-len &optional initial-element)
  (declare (type vector vec)
           (type fixnum new-len))
  (let ((new
         (make-sequence `(vector ,(array-element-type vec) ,new-len)
                        new-len
                        :initial-element initial-element)))
    (dotimes (i (length vec))
      (setf (aref new i) (aref vec i)))
    new))

;;; Return a STORAGE-INFO struction describing the object-to-source
;;; variable mappings from DEBUG-FUN.
(defun storage-info-for-debug-fun (debug-fun)
  (declare (type sb!di:debug-fun debug-fun))
  (let ((sc-vec sb!c::*backend-sc-numbers*)
        (groups nil)
        (debug-vars (sb!di::debug-fun-debug-vars
                     debug-fun)))
    (and debug-vars
         (dotimes (debug-var-offset
                   (length debug-vars)
                   (make-storage-info :groups groups
                                      :debug-vars debug-vars))
           (let ((debug-var (aref debug-vars debug-var-offset)))
             #+nil
             (format t ";;; At offset ~W: ~S~%" debug-var-offset debug-var)
             (let* ((sc-offset
                     (sb!di::compiled-debug-var-sc-offset debug-var))
                    (sb-name
                     (sb!c:sb-name
                      (sb!c:sc-sb (aref sc-vec
                                        (sb!c:sc-offset-scn sc-offset))))))
               #+nil
               (format t ";;; SET: ~S[~W]~%"
                       sb-name (sb!c:sc-offset-offset sc-offset))
               (unless (null sb-name)
                 (let ((group (cdr (assoc sb-name groups))))
                   (when (null group)
                     (setf group (make-location-group))
                     (push `(,sb-name . ,group) groups))
                   (let* ((locations (location-group-locations group))
                          (length (length locations))
                          (offset (sb!c:sc-offset-offset sc-offset)))
                     (when (>= offset length)
                       (setf locations
                             (grow-vector locations
                                          (max (* 2 length)
                                               (1+ offset))
                                          nil)
                             (location-group-locations group)
                             locations))
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
      (sb!di:do-debug-fun-blocks (block debug-fun)
        (declare (ignore block))
        (return t))
    (sb!di:no-debug-blocks () nil)))

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
           (type (or null sb!di:debug-fun) debug-fun)
           (type (or null source-form-cache) sfcache))
  (let ((last-block-pc -1))
    (flet ((add-hook (pc fun &optional before-address)
             (push (make-offs-hook
                    :offset (code-insts-offs-to-segment-offs pc segment)
                    :fun fun
                    :before-address before-address)
                   (seg-hooks segment))))
      (handler-case
          (sb!di:do-debug-fun-blocks (block debug-fun)
            (let ((first-location-in-block-p t))
              (sb!di:do-debug-block-locations (loc block)
                (let ((pc (sb!di::compiled-code-location-pc loc)))

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
                  (unless (zerop (sb!di:code-location-form-number loc))
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
                                        (sb!di:code-location-form-number
                                         loc))
                                (prin1-short form stream)
                                (terpri stream)
                                (terpri stream)))
                            t)))))

                  ;; Keep track of variable live-ness as best we can.
                  (let ((live-set
                         (copy-seq (sb!di::compiled-code-location-live-set
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
        (sb!di:no-debug-blocks () nil)))))

(defvar *disassemble-annotate* t
  #!+sb-doc
  "Annotate DISASSEMBLE output with source code.")

(defun add-debugging-hooks (segment debug-fun &optional sfcache)
  (when debug-fun
    (setf (seg-storage-info segment)
          (storage-info-for-debug-fun debug-fun))
    (when *disassemble-annotate*
      (add-source-tracking-hooks segment debug-fun sfcache))
    (let ((kind (sb!di:debug-fun-kind debug-fun)))
      (flet ((add-new-hook (n)
               (push (make-offs-hook
                      :offset 0
                      :fun (lambda (stream dstate)
                             (declare (ignore stream))
                             (note n dstate)))
                     (seg-hooks segment))))
        (case kind
          (:external)
          ((nil)
           (add-new-hook "no-arg-parsing entry point"))
          (t
           (add-new-hook (lambda (stream)
                           (format stream "~S entry point" kind)))))))))

;;; Return a list of the segments of memory containing machine code
;;; instructions for FUNCTION.
(defun get-fun-segments (function)
  (declare (type compiled-function function))
  (let* ((function (fun-self function))
         (code (fun-code function))
         (fun-map (code-fun-map code))
         (fname (sb!kernel:%simple-fun-name function))
         (sfcache (make-source-form-cache)))
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
              (sb!c::compiled-debug-fun
               (let ((name (sb!c::compiled-debug-fun-name fmap-entry))
                     (kind (sb!c::compiled-debug-fun-kind fmap-entry)))
                 #+nil
                 (format t ";;; SAW ~S ~S ~S,~S ~W,~W~%"
                         name kind first-block-seen-p nil-block-seen-p
                         last-offset
                         (sb!c::compiled-debug-fun-start-pc fmap-entry))
                 (cond (#+nil (eq last-offset fun-offset)
                              (and (equal name fname) (not first-block-seen-p))
                              (setf first-block-seen-p t))
                       ((eq kind :external)
                        (when first-block-seen-p
                          (return)))
                       ((eq kind nil)
                        (when nil-block-seen-p
                          (return))
                        (when first-block-seen-p
                          (setf nil-block-seen-p t))))
                 (setf last-debug-fun
                       (sb!di::make-compiled-debug-fun fmap-entry code)))))))
        (let ((max-offset (code-inst-area-length code)))
          (when (and first-block-seen-p last-debug-fun)
            (add-seg last-offset
                     (- max-offset last-offset)
                     last-debug-fun))
          (if (null segments)
              (let ((offs (fun-insts-offset function)))
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
                          (length (code-inst-area-length code)))
  (declare (type sb!kernel:code-component code)
           (type offset start-offset)
           (type disassem-length length))
  (let ((segments nil))
    (when (sb!kernel:%code-debug-info code)
      (let ((fun-map (code-fun-map code))
            (sfcache (make-source-form-cache)))
        (let ((last-offset 0)
              (last-debug-fun nil))
          (flet ((add-seg (offs len df)
                   (let* ((restricted-offs
                           (min (max start-offset offs)
                                (+ start-offset length)))
                          (restricted-len
                           (- (min (max start-offset (+ offs len))
                                   (+ start-offset length))
                              restricted-offs)))
                     (when (> restricted-len 0)
                       (push (make-code-segment code
                                                restricted-offs restricted-len
                                                :debug-fun df
                                                :source-form-cache sfcache)
                             segments)))))
            (dotimes (fun-map-index (length fun-map))
              (let ((fun-map-entry (aref fun-map fun-map-index)))
                (etypecase fun-map-entry
                  (integer
                   (add-seg last-offset (- fun-map-entry last-offset)
                            last-debug-fun)
                   (setf last-debug-fun nil)
                   (setf last-offset fun-map-entry))
                  (sb!c::compiled-debug-fun
                   (setf last-debug-fun
                         (sb!di::make-compiled-debug-fun fun-map-entry
                                                         code))))))
            (when last-debug-fun
              (add-seg last-offset
                       (- (code-inst-area-length code) last-offset)
                       last-debug-fun))))))
    (if (null segments)
        (list (make-code-segment code start-offset length))
        (nreverse segments))))

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
  (let ((*print-pretty* nil)) ; otherwise the pp conses hugely
    (number-labels dstate)
    (map-segment-instructions
     (lambda (chunk inst)
       (declare (type dchunk chunk) (type instruction inst))
       (let ((printer (inst-printer inst)))
         (when printer
           (funcall printer chunk inst stream dstate))))
     segment
     dstate
     stream)))

;;; Disassemble the machine code instructions in each memory segment
;;; in SEGMENTS in turn to STREAM.
(defun disassemble-segments (segments stream dstate)
  (declare (type list segments)
           (type stream stream)
           (type disassem-state dstate))
  (unless (null segments)
    (let ((n-segments (length segments))
          (first (car segments))
          (last (car (last segments))))
      ;; One origin per segment is printed. As with the per-line display,
      ;; the segment is thought of as immovable for rendering of addresses,
      ;; though in fact the disassembler transiently allows movement.
      (format stream "~&; Size: ~a bytes. Origin: #x~x~@[ (segment 1 of ~D)~]"
              (reduce #'+ segments :key #'seg-length)
              (seg-virtual-location first)
              (if (> n-segments 1) n-segments))
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
                    (seg-virtual-location seg) i n-segments))
          (disassemble-segment seg stream dstate))))))


;;;; top level functions

;;; Disassemble the machine code instructions for FUNCTION.
(defun disassemble-fun (fun &key
                            (stream *standard-output*)
                            (use-labels t))
  (declare (type compiled-function fun)
           (type stream stream)
           (type (member t nil) use-labels))
  (let* ((dstate (make-dstate))
         (segments (get-fun-segments fun)))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)))

(defun valid-extended-function-designators-for-disassemble-p (thing)
  (typecase thing
    ((satisfies legal-fun-name-p)
     (compiled-funs-or-lose (fdefinition thing) thing))
    (sb!pcl::%method-function
         ;; in a %METHOD-FUNCTION, the user code is in the fast function, so
         ;; we to disassemble both.
         ;; FIXME: interpreted methods need to be compiled as above.
         (list thing (sb!pcl::%method-function-fast-function thing)))
    ((or (cons (eql lambda))
         #!+sb-fasteval sb!interpreter:interpreted-function
         #!+sb-eval sb!eval:interpreted-function)
     (compile nil thing))
    (function thing)
    (t nil)))

(defun compiled-funs-or-lose (thing &optional (name thing))
  (let ((funs (valid-extended-function-designators-for-disassemble-p thing)))
    (if funs
        funs
        (error 'simple-type-error
               :datum thing
               :expected-type '(satisfies valid-extended-function-designators-for-disassemble-p)
               :format-control "Can't make a compiled function from ~S"
               :format-arguments (list name)))))

(defun disassemble (object &key
                           (stream *standard-output*)
                           (use-labels t))
  #!+sb-doc
  "Disassemble the compiled code associated with OBJECT, which can be a
  function, a lambda expression, or a symbol with a function definition. If
  it is not already compiled, the compiler is called to produce something to
  disassemble."
  (declare (type (or function symbol cons) object)
           (type (or (member t) stream) stream)
           (type (member t nil) use-labels))
  (flet ((disassemble1 (fun)
           (format stream "~&; disassembly for ~S" (sb!kernel:%fun-name fun))
           (disassemble-fun fun
                            :stream stream
                            :use-labels use-labels)))
    (mapc #'disassemble1 (ensure-list (compiled-funs-or-lose object))))
  nil)

;;; Disassembles the given area of memory starting at ADDRESS and
;;; LENGTH long. Note that if CODE-COMPONENT is NIL and this memory
;;; could move during a GC, you'd better disable it around the call to
;;; this function.
(defun disassemble-memory (address
                           length
                           &key
                           (stream *standard-output*)
                           code-component
                           (use-labels t))
  (declare (type (or address sb!sys:system-area-pointer) address)
           (type disassem-length length)
           (type stream stream)
           (type (or null sb!kernel:code-component) code-component)
           (type (member t nil) use-labels))
  (let* ((address
          (if (sb!sys:system-area-pointer-p address)
              (sb!sys:sap-int address)
              address))
         (dstate (make-dstate))
         (segments
          (if code-component
              (let ((code-offs
                     (- address
                        (sb!sys:sap-int
                         (sb!kernel:code-instructions code-component)))))
                (when (or (< code-offs 0)
                          (> code-offs (code-inst-area-length code-component)))
                  (error "address ~X not in the code component ~S"
                         address code-component))
                (get-code-segments code-component code-offs length))
              (list (make-memory-segment address length)))))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)))

;;; Disassemble the machine code instructions associated with
;;; CODE-COMPONENT (this may include multiple entry points).
(defun disassemble-code-component (code-component &key
                                                  (stream *standard-output*)
                                                  (use-labels t))
  (declare (type (or sb!kernel:code-component compiled-function)
                 code-component)
           (type stream stream)
           (type (member t nil) use-labels))
  (let* ((code-component
          (if (functionp code-component)
              (fun-code code-component)
              code-component))
         (dstate (make-dstate))
         (segments (get-code-segments code-component)))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)))

;;;; code to disassemble assembler segments

(defun assem-segment-to-disassem-segment (assem-segment)
  (declare (type sb!assem:segment assem-segment))
  (let ((contents (sb!assem:segment-contents-as-vector assem-segment)))
    (make-vector-segment contents 0 (length contents) :virtual-location 0)))

;;; Disassemble the machine code instructions associated with
;;; ASSEM-SEGMENT (of type assem:segment).
(defun disassemble-assem-segment (assem-segment stream)
  (declare (type sb!assem:segment assem-segment)
           (type stream stream))
  (let ((dstate (make-dstate))
        (disassem-segments
         (list (assem-segment-to-disassem-segment assem-segment))))
    (label-segments disassem-segments dstate)
    (disassemble-segments disassem-segments stream dstate)))

;;; routines to find things in the Lisp environment

;;; an alist of (SYMBOL-SLOT-OFFSET . ACCESS-FUN-NAME) for slots
;;; in a symbol object that we know about
(defparameter *grokked-symbol-slots*
  (sort (copy-list `((,sb!vm:symbol-value-slot . symbol-value)
                     (,sb!vm:symbol-info-slot . symbol-info)
                     (,sb!vm:symbol-name-slot . symbol-name)
                     (,sb!vm:symbol-package-slot . symbol-package)))
        #'<
        :key #'car))

;;; Given ADDRESS, try and figure out if which slot of which symbol is
;;; being referred to. Of course we can just give up, so it's not a
;;; big deal... Return two values, the symbol and the name of the
;;; access function of the slot.
(defun grok-symbol-slot-ref (address)
  (declare (type address address))
  (if (not (aligned-p address sb!vm:n-word-bytes))
      (values nil nil)
      (do ((slots-tail *grokked-symbol-slots* (cdr slots-tail)))
          ((null slots-tail)
           (values nil nil))
        (let* ((field (car slots-tail))
               (slot-offset (words-to-bytes (car field)))
               (maybe-symbol-addr (- address slot-offset))
               (maybe-symbol
                (sb!kernel:make-lisp-obj
                 (+ maybe-symbol-addr sb!vm:other-pointer-lowtag))))
          (when (symbolp maybe-symbol)
            (return (values maybe-symbol (cdr field))))))))

(defvar *address-of-nil-object* (sb!kernel:get-lisp-obj-address nil))

;;; Given a BYTE-OFFSET from NIL, try and figure out which slot of
;;; which symbol is being referred to. Of course we can just give up,
;;; so it's not a big deal... Return two values, the symbol and the
;;; access function.
(defun grok-nil-indexed-symbol-slot-ref (byte-offset)
  (declare (type offset byte-offset))
  (grok-symbol-slot-ref (+ *address-of-nil-object* byte-offset)))

;;; Return the Lisp object located BYTE-OFFSET from NIL.
(defun get-nil-indexed-object (byte-offset)
  (declare (type offset byte-offset))
  (sb!kernel:make-lisp-obj (+ *address-of-nil-object* byte-offset)))

;;; Return two values; the Lisp object located at BYTE-OFFSET in the
;;; constant area of the code-object in the current segment and T, or
;;; NIL and NIL if there is no code-object in the current segment.
(defun get-code-constant (byte-offset dstate)
  (declare (type offset byte-offset)
           (type disassem-state dstate))
  (let ((code (seg-code (dstate-segment dstate))))
    (if code
        (values
         (sb!kernel:code-header-ref code
                                    (ash (+ byte-offset
                                            sb!vm:other-pointer-lowtag)
                                         (- sb!vm:word-shift)))
         t)
        (values nil nil))))

(defun get-code-constant-absolute (addr dstate &optional width)
  (declare (type address addr))
  (declare (type disassem-state dstate))
  (declare (ignore width))
  (let ((code (seg-code (dstate-segment dstate))))
    (if (null code)
      (return-from get-code-constant-absolute (values nil nil)))
    ;; This WITHOUT-GCING, while not technically broken, is extremely deceptive
    ;; because if it is really needed, then this function has a broken API.
    ;; Since ADDR comes in as absolute, CODE must not move between the caller's
    ;; computation and the comparison below. But we're already in WITHOUT-GCING
    ;; in MAP-SEGMENT-INSTRUCTIONS, so, who cares, I guess?
    (sb!sys:without-gcing
     (let* ((n-header-bytes (* (sb!kernel:get-header-data code) sb!vm:n-word-bytes))
            (header-addr (- (sb!kernel:get-lisp-obj-address code)
                            sb!vm:other-pointer-lowtag))
            (code-start (+ header-addr n-header-bytes)))
         (cond ((< header-addr addr code-start)
                (values (sb!sys:sap-ref-lispobj (sb!sys:int-sap addr) 0) t))
               (t
                (values nil nil)))))))

(defvar *assembler-routines-by-addr* nil)

(defvar *foreign-symbols-by-addr* nil)

;;; Build an address-name hash-table from the name-address hash
(defun invert-address-hash (htable &optional (addr-hash (make-hash-table)))
  (maphash (lambda (name address)
             (setf (gethash address addr-hash) name))
           htable)
  addr-hash)

;;; Return the name of the primitive Lisp assembler routine or foreign
;;; symbol located at ADDRESS, or NIL if there isn't one.
(defun find-assembler-routine (address)
  (declare (type address address))
  (when (null *assembler-routines-by-addr*)
    (setf *assembler-routines-by-addr*
          (invert-address-hash sb!fasl:*assembler-routines*))
    #!-sb-dynamic-core
    (setf *assembler-routines-by-addr*
          (invert-address-hash sb!sys:*static-foreign-symbols*
                               *assembler-routines-by-addr*))
    (loop for static in sb!vm:*static-funs*
          for address = (+ sb!vm::nil-value
                           (sb!vm::static-fun-offset static))
          do
          (setf (gethash address *assembler-routines-by-addr*)
                static))
    ;; Not really a routine, but it uses the similar logic for annotations
    #!+sb-safepoint
    (setf (gethash sb!vm::gc-safepoint-page-addr *assembler-routines-by-addr*)
          "safepoint"))
  (gethash address *assembler-routines-by-addr*))

;;;; some handy function for machine-dependent code to use...

#!-sb-fluid (declaim (maybe-inline sap-ref-int read-suffix))

(defun sap-ref-int (sap offset length byte-order)
  (declare (type sb!sys:system-area-pointer sap)
           (type (unsigned-byte 16) offset)
           (type (member 1 2 4 8) length)
           (type (member :little-endian :big-endian) byte-order)
           (muffle-conditions compiler-note) ; integer coercion, oh well
           (optimize (speed 3) (safety 0)))
  (ecase length
    (1 (sb!sys:sap-ref-8 sap offset))
    (2 (if (eq byte-order :big-endian)
           (+ (ash (sb!sys:sap-ref-8 sap offset) 8)
              (sb!sys:sap-ref-8 sap (+ offset 1)))
           (+ (ash (sb!sys:sap-ref-8 sap (+ offset 1)) 8)
              (sb!sys:sap-ref-8 sap offset))))
    (4 (if (eq byte-order :big-endian)
           (+ (ash (sb!sys:sap-ref-8 sap offset) 24)
              (ash (sb!sys:sap-ref-8 sap (+ 1 offset)) 16)
              (ash (sb!sys:sap-ref-8 sap (+ 2 offset)) 8)
              (sb!sys:sap-ref-8 sap (+ 3 offset)))
           (+ (sb!sys:sap-ref-8 sap offset)
              (ash (sb!sys:sap-ref-8 sap (+ 1 offset)) 8)
              (ash (sb!sys:sap-ref-8 sap (+ 2 offset)) 16)
              (ash (sb!sys:sap-ref-8 sap (+ 3 offset)) 24))))
    (8 (if (eq byte-order :big-endian)
           (+ (ash (sb!sys:sap-ref-8 sap offset) 56)
              (ash (sb!sys:sap-ref-8 sap (+ 1 offset)) 48)
              (ash (sb!sys:sap-ref-8 sap (+ 2 offset)) 40)
              (ash (sb!sys:sap-ref-8 sap (+ 3 offset)) 32)
              (ash (sb!sys:sap-ref-8 sap (+ 4 offset)) 24)
              (ash (sb!sys:sap-ref-8 sap (+ 5 offset)) 16)
              (ash (sb!sys:sap-ref-8 sap (+ 6 offset)) 8)
              (sb!sys:sap-ref-8 sap (+ 7 offset)))
           (+ (sb!sys:sap-ref-8 sap offset)
              (ash (sb!sys:sap-ref-8 sap (+ 1 offset)) 8)
              (ash (sb!sys:sap-ref-8 sap (+ 2 offset)) 16)
              (ash (sb!sys:sap-ref-8 sap (+ 3 offset)) 24)
              (ash (sb!sys:sap-ref-8 sap (+ 4 offset)) 32)
              (ash (sb!sys:sap-ref-8 sap (+ 5 offset)) 40)
              (ash (sb!sys:sap-ref-8 sap (+ 6 offset)) 48)
              (ash (sb!sys:sap-ref-8 sap (+ 7 offset)) 56))))))

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

;;;; optional routines to make notes about code

;;; Store NOTE (which can be either a string or a function with a
;;; single stream argument) to be printed as an end-of-line comment
;;; after the current instruction is disassembled.
(defun note (note dstate)
  (declare (type (or string function) note)
           (type disassem-state dstate))
  (push note (dstate-notes dstate)))

(defun prin1-short (thing stream)
  (with-print-restrictions
    (prin1 thing stream)))

(defun prin1-quoted-short (thing stream)
  (if (self-evaluating-p thing)
      (prin1-short thing stream)
      (prin1-short `',thing stream)))

;;; Store a note about the lisp constant located BYTE-OFFSET bytes
;;; from the current code-component, to be printed as an end-of-line
;;; comment after the current instruction is disassembled.
(defun note-code-constant (byte-offset dstate)
  (declare (type offset byte-offset)
           (type disassem-state dstate))
  (multiple-value-bind (const valid)
      (get-code-constant byte-offset dstate)
    (when valid
      (note (lambda (stream)
              (prin1-quoted-short const stream))
            dstate))
    const))

;;; Store a note about the lisp constant located at ADDR in the
;;; current code-component, to be printed as an end-of-line comment
;;; after the current instruction is disassembled.
(defun note-code-constant-absolute (addr dstate &optional width)
  (declare (type address addr)
           (type disassem-state dstate))
  (multiple-value-bind (const valid)
      (get-code-constant-absolute addr dstate width)
    (when valid
      (note (lambda (stream)
              (prin1-quoted-short const stream))
            dstate))
    (values const valid)))

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
(defun maybe-note-nil-indexed-object (nil-byte-offset dstate)
  (declare (type offset nil-byte-offset)
           (type disassem-state dstate))
  (let ((obj (get-nil-indexed-object nil-byte-offset)))
    (note (lambda (stream)
            (prin1-quoted-short obj stream))
          dstate)
    t))

;;; If ADDRESS is the address of a primitive assembler routine or
;;; foreign symbol, store a note describing which one, to be printed
;;; as an end-of-line comment after the current instruction is
;;; disassembled. Returns non-NIL iff a note was recorded. If
;;; NOTE-ADDRESS-P is non-NIL, a note of the address is also made.
(defun maybe-note-assembler-routine (address note-address-p dstate)
  (declare (type disassem-state dstate))
  (unless (typep address 'address)
    (return-from maybe-note-assembler-routine nil))
  (let ((name (or
               (find-assembler-routine address)
               #!+linkage-table
               (sb!sys:sap-foreign-symbol (sb!sys:int-sap address)))))
    (unless (null name)
      (note (lambda (stream)
              (if note-address-p
                  (format stream "#x~8,'0x: ~a" address name)
                  (princ name stream)))
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
              (princ (sb!di:debug-var-symbol
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
                      (sb!di:debug-var-symbol
                       (aref (dstate-debug-vars dstate)
                             storage-location))))
            dstate)
      t)))

(defun maybe-note-static-symbol (offset dstate)
  (dolist (symbol sb!vm:*static-symbols*)
    (when (= (sb!kernel:get-lisp-obj-address symbol) offset)
      (return (note (lambda (s) (prin1 symbol s)) dstate)))))

(defun get-internal-error-name (errnum)
  (cdr (svref sb!c:+backend-internal-errors+ errnum)))

(defun get-sc-name (sc-offs)
  (sb!c:location-print-name
   ;; FIXME: This seems like an awful lot of computation just to get a name.
   ;; Couldn't we just use lookup in *BACKEND-SC-NAMES*, without having to cons
   ;; up a new object?
   (sb!c:make-random-tn :kind :normal
                        :sc (svref sb!c:*backend-sc-numbers*
                                   (sb!c:sc-offset-scn sc-offs))
                        :offset (sb!c:sc-offset-offset sc-offs))))

;;; When called from an error break instruction's :DISASSEM-CONTROL (or
;;; :DISASSEM-PRINTER) function, will correctly deal with printing the
;;; arguments to the break.
;;;
;;; ERROR-PARSE-FUN should be a function that accepts:
;;;   1) a SYSTEM-AREA-POINTER
;;;   2) a BYTE-OFFSET from the SAP to begin at
;;;   3) optionally, LENGTH-ONLY, which if non-NIL, means to only return
;;;      the byte length of the arguments (to avoid unnecessary consing)
;;; It should read information from the SAP starting at BYTE-OFFSET, and
;;; return four values:
;;;   1) the error number
;;;   2) the total length, in bytes, of the information
;;;   3) a list of SC-OFFSETs of the locations of the error parameters
;;;   4) a list of the length (as read from the SAP), in bytes, of each
;;;      of the return values.
(defun handle-break-args (error-parse-fun stream dstate)
  (declare (type function error-parse-fun)
           (type (or null stream) stream)
           (type disassem-state dstate))
  (multiple-value-bind (errnum adjust sc-offsets lengths)
      (funcall error-parse-fun
               (dstate-segment-sap dstate)
               (dstate-next-offs dstate)
               (null stream))
    (when stream
      (setf (dstate-cur-offs dstate)
            (dstate-next-offs dstate))
      (flet ((emit-err-arg ()
               (let ((num (pop lengths)))
                 (print-notes-and-newline stream dstate)
                 (print-current-address stream dstate)
                 (print-inst num stream dstate)
                 (print-bytes num stream dstate)
                 (incf (dstate-cur-offs dstate) num)))
             (emit-note (note)
               (when note
                 (note note dstate))))
        (emit-err-arg)
        ;; ARM64 encodes the error number in BRK instruction itself
        #!-arm64
        (emit-err-arg)
        (emit-note (symbol-name (get-internal-error-name errnum)))
        (dolist (sc-offs sc-offsets)
          (emit-err-arg)
          (if (= (sb!c:sc-offset-scn sc-offs)
                 sb!vm:constant-sc-number)
              (note-code-constant (* (1- (sb!c:sc-offset-offset sc-offs))
                                     sb!vm:n-word-bytes)
                                  dstate)
              (emit-note (get-sc-name sc-offs))))))
    (incf (dstate-next-offs dstate)
          adjust)))
