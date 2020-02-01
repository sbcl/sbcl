;;;; disassembler structures not needed in cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-DISASSEM")

(defstruct (storage-info (:copier nil))
  (groups nil :type list)               ; alist of (name . location-group)
  (debug-vars #() :type vector))

(defstruct (segment (:conc-name seg-)
                    (:constructor %make-segment)
                    (:copier nil))
  ;; the object that should be pinned when calling the sap-maker
  (object nil)
  (sap-maker (missing-arg) :type (function () system-area-pointer))
  ;; Length in bytes of the range of memory covered by this segment.
  (length 0 :type disassem-length)
  ;; This is somewhat bogus if we make more than one segment for a given
  ;; code object (which always happens) and if the object can move due to GC
  ;; between calls to the segment constructor. If moved, then segments
  ;; get different virtual locations despite having the same sap-maker.
  ;; Depending on whether we are trying to use this address to dereference
  ;; data (so we want a physical address) versus show the logical address
  ;; within the code at which instructions occur, as if the code never moved,
  ;; then we want different things here. In practice, the code probably can't
  ;; move, so the two possible meanings of the slot are the same in effect.
  ;; The only way to find a GC-related bug would be to insert some GC calls
  ;; at several points in the disassembler, while also ensuring no conservative
  ;; references exist to the code, and seeing what happens with regard to the
  ;; addresses that are printed and/or the ability to display unboxed constants
  ;; from the code header; it seems like an exercise in futility.
  (virtual-location 0 :type address)
  (storage-info nil :type (or null storage-info))
  (code nil :type (or null code-component))
  ;; list of function and fdefn constants extracted from code header
  (code-callables :?)
  ;; the byte offset beyond CODE-INSTRUCTIONS of CODE which
  ;; corresponds to offset 0 in this segment
  (initial-offset 0 :type index)
  (hooks nil :type list)
  debug-fun)

;;; All state during disassembly. We store some seemingly redundant
;;; information so that we can allow garbage collect during disassembly and
;;; not get tripped up by a code block being moved...
(defstruct (disassem-state (:conc-name dstate-)
                           (:constructor %make-dstate
                               (alignment argument-column fun-hooks))
                           (:copier nil))
  ;; to avoid buffer overrun at segment end, we might need to copy bytes
  ;; here first because we access memory in chunks larger than 1 byte.
  (scratch-buf 0 :type sb-vm:word)
  ;; offset of current pos in segment
  (cur-offs 0 :type offset)
  ;; offset of next position
  (next-offs 0 :type offset)
  ;; a sap pointing to our segment
  (segment-sap (int-sap 0) :type system-area-pointer)
  ;; the current segment
  (segment nil :type (or null segment))
  ;; true if disassembling non-lisp code, which disables interpretation
  ;; of bytes after a trap instruction as SC+OFFSETs.
  (foreign-code-p nil)
  ;; true (the default) if PC-relative jumps should be decoded as absolute.
  ;; No effect if the target disassembler does not implement the choice.
  (absolutize-jumps t)
  ;; what to align to in most cases
  (alignment sb-vm:n-word-bytes :type alignment)
  (byte-order sb-c:*backend-byte-order*
              :type (member :big-endian :little-endian))
  ;; current instruction as found in instruction space
  (inst)
  (operands (make-array 10) :read-only t) ; enough for anybody
  (n-operands 0)
  ;; for user code to track decoded bits, cleared each time after a
  ;; non-prefix instruction is processed
  (inst-properties 0 :type fixnum)
  (filtered-values (make-array max-filtered-value-index)
                   :type filtered-value-vector)
  ;; to avoid consing decoded values, a prefilter can keep a chain
  ;; of objects in these slots. The objects returned here
  ;; are reusable for the next instruction.
  (filtered-arg-pool-in-use)
  (filtered-arg-pool-free)
  ;; used for prettifying printing
  (addr-print-len nil :type (or null (integer 0 20)))
  (argument-column 0 :type column)
  ;; to make output look nicer
  (output-state :beginning
                :type (member :beginning
                              :block-boundary
                              nil))

  ;; alist of (address . label-number)
  (labels nil :type list)
  ;; same as LABELS slot data, but in a different form
  (label-hash (make-hash-table) :type hash-table)
  ;; list of function
  (fun-hooks nil :type list)

  ;; alist of (address . label-number), popped as it's used
  (cur-labels nil :type list)
  ;; OFFS-HOOKs, popped as they're used
  (cur-offs-hooks nil :type list)

  ;; for the current location
  (notes nil :type list)

  ;; currently active source variables
  (current-valid-locations nil :type (or null (vector bit))))

(declaim (freeze-type disassem-state))
(defmethod print-object ((dstate disassem-state) stream)
  (print-unreadable-object (dstate stream :type t)
    (format stream
            "+~W~@[ in ~S~]"
            (dstate-cur-offs dstate)
            (dstate-segment dstate))))

;;; Return the absolute address of the current instruction in DSTATE.
(defun dstate-cur-addr (dstate)
  (the address (+ (seg-virtual-location (dstate-segment dstate))
                  (dstate-cur-offs dstate))))

;;; Return the absolute address of the next instruction in DSTATE.
(defun dstate-next-addr (dstate)
  (the address (+ (seg-virtual-location (dstate-segment dstate))
                  (dstate-next-offs dstate))))
